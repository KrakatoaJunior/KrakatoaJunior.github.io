library(dplyr)          # Data wrangling
library(gganimate)      # Animation
library(ggnewscale)
library(ggplot2)        # Plotting
library(ggrepel)
library(glue)
library(grid)
library(MASS)
library(mclust)         # Gaussian Mixture Models
library(mice)           # Multiple imputation
library(mvtnorm)        # Multivariate normal density for contours
library(purrr)
library(RColorBrewer)   # Color palettes
library(ragg)
library(readr)          # CSV reading
library(rlang)
library(scales)
library(stringr)
library(tibble)
library(tidyr)

set.seed(500)

### load data
df <- read_csv("WHI_Inflation.csv")

# Checking manually leads to the knowledge that Cyprus has 2 rows with repeated years,
# Choose the rows that are more consistent
df <- df[-285,]
df <- df[-287,]

### Imputation

# Getting the numeric column names
num_vars <- df %>% dplyr::select(where(is.numeric)) %>% names()

# Getting the range of years
all_years <- seq(min(df$Year, na.rm=TRUE), max(df$Year, na.rm=TRUE))

# Check for missingness
colSums(is.na(df)) / nrow(df) * 100

# Remove grossly missing columns
df <- df %>% dplyr::select(-`Official Core Consumer Price Inflation`) %>% dplyr::select(-`Producer Price Inflation`)

# Remove columns with lots of dummy variables
df_noCountrynoRegion <- df %>% dplyr::select(-Country) %>% dplyr::select(-`Continent/Region`)

# Checking for significance with multiple linear regression, without dummy variables
mlr <- lm(`Score` ~ ., data = df_noCountrynoRegion)
summary(mlr)

# only get the columns with significance
df <- df %>%
  dplyr::select(-`Headline Consumer Price Inflation`) %>%
  dplyr::select(-`Energy Consumer Price Inflation`)%>%
  dplyr::select(-`Food Consumer Price Inflation`) %>%
  dplyr::select(-`GDP deflator Index growth rate`)

sig_vars <- c("Score","GDP per Capita","Social support","Healthy life expectancy at birth","Freedom to make life choices","Generosity","Perceptions of corruption")

## Getting ready for imputation by inserting blanks

# filling in missing years
df_full <- df %>%
  group_by(Country) %>%
  tidyr::complete(Year = all_years) %>%   # creates missing year rows with NAs
  ungroup()

# backfilling country/region column, the only column other than country that's categorical
if ("Continent/Region" %in% names(df_full)) {
  df_full <- df_full %>%
    group_by(Country) %>%
    tidyr::fill(`Continent/Region`, .direction = "downup") %>%
    ungroup()
}

## Imputation by country function
country_imputation <- function(country, m = 10){
  
  # method vector for MICE
  method <- make.method(country)
  if ("Year" %in% names(country)){ method["Year"] <- "" } else message("'Year' is not detected while making method vector")
  if ("Country" %in% names(country)){ method["Country"] <- "" } else message("'Country' is not detected while making method vector")
  if ("Continent/Region" %in% names(country)){ method["Continent/Region"] <- "" } else message("'Continent/Region' is not detected while making method vector")
  for (variable in sig_vars) {
    if (variable %in% names(country)){ method[variable] <- "pmm"} else message(sprintf("'%s' is not detected while making method vector", variable))
  }
  
  # predictor matrix for MICE
  P <- make.predictorMatrix(country)
  diag(P) <- 0
  if ("Year" %in% colnames(P)){
    P["Year", ] <- 0;
    P[, "Year"] <- 1 }
  else message("'Year' is not detected while making predictor matrix")
  if ("Country" %in% colnames(P)){
    P["Country", ] <- 0;
    P[, "Country"] <- 0 }
  else message("'Country' is not detected while making predictor matrix")
  if ("Continent/Region" %in% colnames(P)) {
    P["Continent/Region", ] <- 0;
    P[, "Continent/Region"] <- 1 }
  else message("'Year' is not detected while making predictor matrix")
  if ("Score" %in% colnames(P)) {
    P[, "Score"] <- 0}  # outcome shouldn’t predict others
  # all others not specified is ["", 1] and [1, ""]
  
  imputed_country <-  mice(
    data            = country,
    m               = m,
    method          = method,
    predictorMatrix = P,
    printFlag       = FALSE
  )
  return(imputed_country)
}

# Split by country
imp_by_country <- df_full %>%
  group_split(Country, .keep = TRUE)

# The 'stochastic' in stochastic linear regression, each imputation will be pooled later to ease out imputation errors
m = 10

# Empty vector of size m to store the m different imputations
completed_by_m <- vector("list", m)

## The function call. It takes a while for m > 1
imputed_by_country <- map(imp_by_country, ~ country_imputation(.x, m = m))

# Build m completed datasets by stitching each country’s i-th completion
for (i in seq_len(m)) {
  completed_by_m[[i]] <- imputed_by_country %>%
    map(~ if (is.null(.x)) {NULL} else complete(.x, i)) %>%
    compact() %>%
    bind_rows() %>%
    arrange(Country, Year)
}

# Check for NAs
sapply(completed_by_m, function(df) any(is.na(df)))
sapply(completed_by_m, function(df) sum(is.na(df)))
lapply(completed_by_m, function(df) colSums(is.na(df)))
# By country
na_totals_by_country <- lapply(completed_by_m, function(df) {
  df %>%
    group_by(Country) %>%
    summarise(total_NAs = sum(is.na(across(everything()))), .groups = "drop") %>%
    filter(total_NAs > 0)
})
print(na_totals_by_country[[1]], n = Inf)
# Seems like there are countries with mostly missing values across years that mice doesnt know what to do with

# By sig. vars.
na_sig_vars <- lapply(completed_by_m, function(df) {
  df %>%
    summarise(across(all_of(sig_vars), ~ sum(is.na(.)), .names = "na_{.col}")) %>%
    pivot_longer(
      cols = everything(),
      names_to = "Variable",
      values_to = "NAs"
    ) %>%
    filter(NAs > 0) %>%
    arrange(desc(NAs))
})
print(na_sig_vars[[1]], n = Inf)
# Columns look fine

## removing the missing rows ##

# flag each imputation's countries which has MORE THAN 10 NAs (across years) #
countries_more_than_10NAs <- lapply(na_totals_by_country, function(df) {
  df %>%
    filter(total_NAs > 10) %>%
    pull(Country)
})
countries_more_than_10NAs[[1]]

# Countries that have ANY NAs in list form
countries_any_NAs <- lapply(na_totals_by_country, function(df) {df %>% pull(Country)})
countries_any_NAs[[1]]

# Countries that have LESS THAN 10 NAs in list form
countries_less_than_10NAs <- setdiff(countries_any_NAs[[1]], countries_more_than_10NAs[[1]])

# Drop countries with ANY missing values #
completed_by_m <- lapply(completed_by_m, function(df) {
  df %>% filter(!Country %in% unlist(countries_any_NAs))
})

## Second round of imputations, with the goal of stitching back countries
## with less missing values into first round of imputations ##
target_regions <- df_full %>%
  filter(Country %in% countries_less_than_10NAs) %>%
  pull(`Continent/Region`) %>%
  unique()

# imputing based on region, using stochastic PMM instead of stochastic regression
region_imputation <- function(df, m = 10, seed = 500) {
  df %>%
    group_split(`Continent/Region`, .keep = TRUE) %>%    # split by region
    purrr::map(function(block) {
      # build method vector safely
      meth <- make.method(block)
      # default: do not impute ID-like vars
      if ("Year" %in% names(meth))    meth["Year"]    <- ""
      if ("Country" %in% names(meth)) meth["Country"] <- ""
      if ("Continent/Region" %in% names(meth)) meth["Continent/Region"] <- ""
      
      # set PMM for vars we actually have in this block
      want <- sig_vars[sig_vars %in% names(meth)]
      meth[want] <- "pmm"
      # ensure Score is imputed with PMM
      if ("Score" %in% names(meth)) meth["Score"] <- "pmm"
      
      # predictor matrix: Region/Country don’t predict anything;
      # Score is predicted by others but doesn’t predict others
      P <- make.predictorMatrix(block)
      diag(P) <- 0
      if ("Country" %in% colnames(P))          { P[,"Country"] <- 0; P["Country",] <- 0 }
      if ("Continent/Region" %in% colnames(P)) { P[,"Continent/Region"] <- 0; P["Continent/Region",] <- 0 }
      if ("Score" %in% colnames(P))            { P[, "Score"]  <- 1; P["Score", ] <- 0 }
      if ("Year" %in% colnames(P))             { P["Year", ]   <- 0; P[, "Year"]  <- 1 }  # Year helps predict others
      
      mice(block, m = m, method = meth, predictorMatrix = P,
           printFlag = FALSE, seed = seed)
    }) %>%
    purrr::map(complete, action = "long") %>%
    dplyr::bind_rows()
}

# Run region-based imputation only for the target countries
reimputed_targets <- df_full %>%
  filter(`Continent/Region` %in% target_regions) %>%
  region_imputation(m = 10)

# NA check
sapply(reimputed_targets, function(col) any(is.na(col)))

# stitching back with the original completed_by_m for the other countries

# Make sure reimputed_targets is the long form with .imp (1..m) present
stopifnot(all(c(".imp", ".id") %in% names(reimputed_targets)))
available_imps <- sort(unique(reimputed_targets$.imp))

df_imputed <- lapply(seq_len(m), function(i) {
  d <- completed_by_m[[i]]
  
  # take only the i-th imputation of the reimputed targets
  r_i <- reimputed_targets %>%
    dplyr::filter(.imp == i, Country %in% countries_less_than_10NAs) %>%
    dplyr::select(-.imp, -.id)
  
  # if reimputation didn’t produce any rows for this i, keep the original d
  if (nrow(r_i) == 0) {
    message(sprintf("No reimputed rows for .imp == %d; keeping original for this imputation.", i))
    return(d)
  }
  
  # align schemas: add any missing cols to r_i; drop extras; reorder to match d
  missing_in_r <- setdiff(names(d), names(r_i))
  if (length(missing_in_r) > 0) {
    for (nm in missing_in_r) r_i[[nm]] <- NA
  }
  r_i <- r_i[, names(d), drop = FALSE]
  
  # replace the target countries’ rows with reimputed ones
  d %>%
    dplyr::filter(!Country %in% countries_less_than_10NAs) %>%
    dplyr::bind_rows(r_i) %>%
    dplyr::arrange(Country, Year)
})
### Imputation done ###

### Visualization time! ###

# collapse imputations -> average (as you had)
df_avg <- bind_rows(completed_by_m, .id = "imputation") %>%
  group_by(Country, Year, `Continent/Region`) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE), .groups = "drop")

vars_pca <- c(
  "GDP per Capita","Social support","Healthy life expectancy at birth",
  "Freedom to make life choices","Generosity","Perceptions of corruption","Score"
)

X <- df_avg %>% dplyr::select(all_of(vars_pca))

# --- PCA with 2 components ---
pca <- prcomp(X, center = TRUE, scale. = TRUE)

scores_2D <- as.data.frame(pca$x[,1:2]) %>%
  setNames(c("PC1","PC2")) %>%
  bind_cols(df_avg %>% dplyr::select(Country, Year, `Continent/Region`))

# loadings in PC1–PC2 plane
load_2D <- as.data.frame(pca$rotation[,1:2])
load_2D$Variable <- rownames(load_2D)

# GMM clustering on PC1–PC2 (fit once globally)
global_fit <- Mclust(scores_2D %>% dplyr::select(PC1,PC2))
scores_2D$Cluster <- predict(global_fit, newdata = scores_2D %>% dplyr::select(PC1,PC2))$classification

target_year <- 2023L

last_per_cty <- scores_2D %>%
  dplyr::group_by(Country) %>%
  dplyr::slice_max(Year, n = 1, with_ties = FALSE) %>%
  dplyr::ungroup()

pad_2023 <- last_per_cty %>%
  dplyr::filter(Year < target_year) %>%
  dplyr::mutate(Year = target_year)

scores_2D_ext <- dplyr::bind_rows(scores_2D, pad_2023) %>%
  dplyr::arrange(Year, Country)

# predict clusters for the padded rows (same model)
scores_2D_ext$Cluster <- predict(
  global_fit,
  newdata = dplyr::select(scores_2D_ext, PC1, PC2)
)$classification

# ranges & arrow scaling (use extended data)
xr <- range(scores_2D_ext$PC1, na.rm=TRUE)
yr <- range(scores_2D_ext$PC2, na.rm=TRUE)

# --- Variable arrows by true magnitude in PC1–PC2 (correlation biplot style) ---

# 1) correlations of variables with PC1/PC2:
#    For prcomp(..., scale.=TRUE), cor(var_j, PC_i) = loading_{j,i} * sdev[i]
var_corr <- sweep(pca$rotation[, 1:2, drop = FALSE], 2, pca$sdev[1:2], `*`)

load_2D <- as.data.frame(var_corr)
colnames(load_2D)[1:2] <- c("PC1", "PC2")          # ensure standard names
load_2D$Variable <- rownames(var_corr)

# 2) be extra sure these are numeric (guards against weird coercions)
load_2D <- load_2D %>%
  dplyr::mutate(
    PC1 = as.numeric(PC1),
    PC2 = as.numeric(PC2)
  )

# 3) magnitudes in the PC1–PC2 plane
load_2D <- load_2D %>%
  dplyr::mutate(mag = sqrt(PC1^2 + PC2^2))

# 4) scale all arrows proportionally so the longest ≈ 35% of plot span
L_max   <- 0.35 * min(diff(xr), diff(yr))
max_mag <- suppressWarnings(max(load_2D$mag, na.rm = TRUE))
sfac    <- if (is.finite(max_mag) && !is.na(max_mag) && max_mag > 0) L_max / max_mag else 0

load_2D <- load_2D %>%
  dplyr::mutate(
    xend = PC1 * sfac,
    yend = PC2 * sfac
  )

# split: remove GDP per Capita + Social support from the base layer
special <- c("GDP per Capita", "Social support", "Healthy life expectancy at birth")
load_2D_rest <- dplyr::filter(load_2D, !Variable %in% special)
load_2D_triplet <- dplyr::filter(load_2D, Variable %in% special)
load_2D_score <- load_2D_rest %>% dplyr::filter(Variable == "Score")
load_2D_rest <- load_2D_rest %>% dplyr::filter(!Variable == "Score")

# cluster palette (from extended data)
G <- length(unique(scores_2D_ext$Cluster))
pal <- c("#0072B2", "#E69F00", "#009E73", "#D55E00", "#CC79A7")
lev <- sort(unique(scores_2D_ext$Cluster))
pal_named <- setNames(pal[seq_along(lev)], lev)

# ---- plot ----
p <- ggplot(scores_2D_ext, aes(PC1, PC2, group = Country)) +
  geom_hline(yintercept=0, color="grey90", linewidth=0.4) +
  geom_vline(xintercept=0, color="grey90", linewidth=0.4) +
  
  # variable loadings
  # Bottom 3
  geom_segment(
    data = load_2D_rest, inherit.aes = FALSE,
    aes(x = 0, y = 0, xend = xend, yend = yend),
    color = "grey40", linewidth = 0.6,
    arrow = arrow(length = unit(0.15, "cm"))
  ) +
  geom_text(
    data = load_2D_rest, inherit.aes = FALSE,
    aes(x = xend, y = yend, label = Variable),
    color = "grey20", size = 3, vjust = -0.4,
    nudge_x = 0.3,
    nudge_y = -0.2
  ) +
  # Top 3
  geom_segment(
    data = load_2D_triplet, inherit.aes = FALSE,
    aes(x = 0, y = 0, xend = xend, yend = yend),
    color = "grey40", linewidth = 0.6,
    arrow = arrow(length = unit(0.15, "cm"))
  ) +
  geom_text(
    data = load_2D_triplet, inherit.aes = FALSE,
    aes(x = xend, y = yend, label = Variable),
#    nudge_x = c(0.55, 0.5, 1.05),  # order matches rows in load_2D_pair
#    nudge_y = c(0.08, 0.17, 0.05),
    nudge_x = c(.59, -0.5, 1.18),  # order matches rows in load_2D_pair
    nudge_y = c(0.05, 0.1, 0.02),
    color = "grey20", size = 3
  )+
  # Score
  geom_segment(
    data = load_2D_score, inherit.aes = FALSE,
    aes(x = 0, y = 0, xend = xend, yend = yend),
    color = "grey40", linewidth = 0.6,
    arrow = arrow(length = unit(0.15, "cm"))
  ) +
  geom_text(
    data = load_2D_score, inherit.aes = FALSE,
    aes(x = xend, y = yend, label = Variable),
    nudge_x = 0.25,
    nudge_y = 0.05,
    color = "grey20", size = 3
  )+
  
  # country labels colored by cluster
  geom_text(aes(label = Country,
                color = factor(Cluster)),
            size = 2.3, alpha = 0.9, key_glyph = "point") +
  
  scale_color_manual(values = pal_named, name = "Cluster", drop = FALSE) +
  
  scale_x_continuous(limits = xr, expand = expansion(mult = 0.06)) +
  scale_y_continuous(limits = yr, expand = expansion(mult = 0.06)) +
  coord_equal() +
  labs(title = "GMM on PCA (2D)", subtitle = "Yearly states") +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank()) +
  
  # BIG bottom-left year label that flips only at end of transition
  labs(tag = "{ifelse(is.na(previous_state), closest_state, previous_state)}") +
  theme(
    plot.tag = element_text(size = 36, face = "bold"),
    plot.tag.position = c(0.01, 0.02)  # bottom-left in [0,1]
  ) +
  
  # animate by discrete Year with easing
  gganimate::transition_states(
    Year, transition_length = 4, state_length = 1, wrap = FALSE
  ) +
  gganimate::ease_aes("quadratic-in-out")

gganimate::animate(
  p,
  nframes = 450, fps = 30, end_pause = 30,   # hold on 2023 a bit
  width = 1800, height = 1200, res = 150,
  def = ragg::agg_png(),
  renderer = gganimate::av_renderer("gmm_pca2D_yearlabel_to2023.mp4")
)
