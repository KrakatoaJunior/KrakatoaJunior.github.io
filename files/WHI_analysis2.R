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
library(jsonlite)

set.seed(500)

### load data
df <- read_csv("WHI_Inflation.csv")

# checking with 2 keys
n_occur <- data.frame(table(df$Country, df$Year))

# which unique combination have duplicates
n_occur[n_occur$Freq > 1,] 

# bring up those entries in the df
df_dupes <- df[df$Country %in% n_occur$Var1[n_occur$Freq > 1] & df$Year %in% n_occur$Var2[n_occur$Freq > 1],]
show(df_dupes)
#View(df_dupes)
#toJSON(setNames(as.data.frame(df_dupes),colnames(df_dupes)))

# Putting the mismatches side by side
# flag number of distinct values per group for each column
var_flags <- df_dupes %>%
  group_by(Country, Year) %>%
  summarise(across(everything(), ~ n_distinct(.x)), .groups = "drop")

# columns that vary in at least one group
cols_to_keep <- var_flags %>%
  summarise(across(everything(), ~ any(. > 1))) %>%
  pivot_longer(everything(), names_to = "col", values_to = "varies") %>%
  filter(varies) %>%
  pull(col)

# filter original data to keep only keys + mismatched columns
df_dupes_mismatch <- df_dupes %>%
  dplyr::select(all_of(cols_to_keep))
#View(df_dupes_mismatch)
#toJSON(setNames(as.data.frame(df_dupes_mismatch),colnames(df_dupes_mismatch)))

cols_to_keep_depvars <- setdiff(cols_to_keep, c("Country", "Year"))

# Loop through each variable and plot
for (var in cols_to_keep_depvars) {
  cyprus <- df %>% filter(Country == "Cyprus")
  
  p <- ggplot(cyprus, aes(x = Year, y = .data[[var]])) +
    geom_point(color="green" +
    scale_x_continuous(breaks = seq(min(cyprus$Year), max(cyprus$Year), by = 1)) +
    labs(title = paste("Cyprus:", var), y = var)
  
  print(p)   # important in loops to display plots
}

# Choose the rows that are more consistent with the rest of its country's other years
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

# collapse imputations -> average
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

# 2) be extra sure these are numeric (guards against weird coercion)
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

# split: remove from the base layer
special <- c("GDP per Capita", "Social support", "Healthy life expectancy at birth")

# everything else except the three
load_2D_rest <- dplyr::filter(load_2D, !Variable %in% special)

# the three
load_2D_triplet <- dplyr::filter(load_2D, Variable %in% special)

# just score
load_2D_score <- load_2D_rest %>% dplyr::filter(Variable == "Score")

# Generosity, Perceptions of corruption, and Freedom to Make Life choices 
load_2D_rest <- load_2D_rest %>% dplyr::filter(!Variable == "Score")

# cluster palette (from extended data)
G <- length(unique(scores_2D_ext$Cluster))
pal <- c("#0072B2", "#E69F00", "#009E73", "#D55E00", "#CC79A7")
lev <- sort(unique(scores_2D_ext$Cluster))
pal_named <- setNames(pal[seq_along(lev)], lev)

# ---- plot ----

p_plot <- function(render_w = 1920, render_h = 1080){
  sf <- render_w / 1920 # original width
  p <- ggplot(scores_2D_ext, aes(PC1, PC2, group = Country)) +
    geom_hline(yintercept=0, color="grey90", linewidth=0.4*sf) +
    geom_vline(xintercept=0, color="grey90", linewidth=0.4*sf) +
    
    ## variable loadings — shrink arrows & labels a bit ##
    
    # GDP per Capita, Social Support, Healthy Life Expectancy at Birth
    geom_segment(
      data = load_2D_triplet, inherit.aes = FALSE,
      aes(x = 0, y = 0, xend = xend, yend = yend),
      color = "grey40", linewidth = 0.6*sf,
      arrow = arrow(length = unit(0.12*sf, "cm"))
    ) +
    geom_text(
      data = load_2D_triplet, inherit.aes = FALSE,
      aes(x = xend, y = yend, label = Variable),
      nudge_x = c(.59, -0.5, 1.18) * sf,
      nudge_y = c(0.05, 0.10, 0.02) * sf,
      color = "grey20", size = 3*sf
    ) +
    
    # 
    geom_segment(
      data = load_2D_rest, inherit.aes = FALSE,
      aes(x = 0, y = 0, xend = xend, yend = yend),
      color = "grey40", linewidth = 0.6*sf,
      arrow = arrow(length = unit(0.12*sf, "cm"))
    ) +
    geom_text(
      data = load_2D_rest, inherit.aes = FALSE,
      aes(x = xend, y = yend, label = Variable),
      color = "grey20", size = 3*sf, vjust = -0.4,
      nudge_x = 0.25*sf, nudge_y = -0.18*sf
    ) +
    
    # 
    geom_segment(
      data = load_2D_score, inherit.aes = FALSE,
      aes(x = 0, y = 0, xend = xend, yend = yend),
      color = "grey40", linewidth = 0.6*sf,
      arrow = arrow(length = unit(0.12*sf, "cm"))
    ) +
    geom_text(
      data = load_2D_score, inherit.aes = FALSE,
      aes(x = xend, y = yend, label = Variable),
      nudge_x = 0.22*sf, nudge_y = 0.05*sf,
      color = "grey20", size = 3*sf
    ) +
    
    # country labels colored by cluster
    geom_text(
      aes(label = Country, color = factor(Cluster)),
      size = 2.0*sf, alpha = 0.9, key_glyph = "point"
    ) +
    
    scale_color_manual(values = pal_named, name = "Cluster", drop = FALSE) +
    
    scale_x_continuous(limits = xr, expand = expansion(mult = 0.06)) +
    scale_y_continuous(limits = yr, expand = expansion(mult = 0.06)) +
    coord_equal() +
    labs(title = "GMM on PCA (2D)", subtitle = "Yearly states") +
    
    # Base font
    theme_minimal(base_size = 12*sf) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      
      # Legend moves to bottom and wraps into two rows
      legend.position   = "bottom",
      legend.title      = element_text(size = 10*sf, face = "bold"),
      legend.text       = element_text(size = 9*sf),
      legend.key.width  = unit(14*sf, "pt"),
      legend.key.height = unit(10*sf, "pt"),
      legend.spacing.y  = unit(2*sf, "pt"),
      legend.spacing.x  = unit(6*sf, "pt"),
      
      # BIG bottom-left year label
      plot.tag          = element_text(size = 36*sf, face = "bold"),
      plot.tag.position = c(0.012, 0.025)
    ) +
    
    # Wrap legend items (2 rows) and legend points
    guides(
      color = guide_legend(
        nrow = 2,
        byrow = TRUE,
        override.aes = list(size = 2.5*sf)
      )
    ) +
    
    # animate by discrete Year with easing
    gganimate::transition_states(
      Year, transition_length = 4, state_length = 1, wrap = FALSE
    ) +
    gganimate::ease_aes("quadratic-in-out") +
    labs(tag = "{ifelse(is.na(previous_state), closest_state, previous_state)}")
}
  
##### Animation functions #####
###############################

# 1 #
animate_default <- function(){
  p_plot(1800, 1200)
  gganimate::animate(
    p,
    nframes = 450, fps = 30, end_pause = 30,   # hold on 2023 a bit
    width = 1800, height = 1200, res = 150,
    def = ragg::agg_png(),
    renderer = gganimate::av_renderer("gmm_pca2D_yearlabel_to2023.mp4")
  )
}

# 2 #
animate_desktop <- function(){
  p_plot()
  gganimate::animate(
    p,
    nframes   = 450,
    fps       = 30,
    end_pause = 30,              # hold on last frame
    width     = 1800,
    height    = 1200,
    res       = 150,
    device    = "ragg_png",      # high-quality PNG frames via ragg
    renderer  = gganimate::ffmpeg_renderer(
      format  = "mp4",
      options = list(
        vcodec   = "libx264",    # H.264 encoder
        pix_fmt  = "yuv420p",    # widest playback compatibility
        crf      = 20,           # quality (lower = better, ~18–23 typical)
        preset   = "slow",       # speed/quality tradeoff: ultrafast…placebo
        movflags = "+faststart"  # put moov atom first for web streaming
      )
    )
  )
}

# 3 #
animate_mobile <- function(){
  p_plot(1280, 720)
  gganimate::animate(
    p, nframes=450, fps=30, end_pause=30, width=1280, height=720, res=120,
    device="ragg_png",
    renderer=gganimate::ffmpeg_renderer(
      format="mp4",
      options=list(vcodec="libx264", pix_fmt="yuv420p", crf=22, preset="slow", movflags="+faststart")
    )
  )
}

# function call with empty brackets e.g.animate_mobile()

### Exporting for interactive website ###

# --- EXPORT PCA + GMM ARTIFACTS ---------------------------------------------
# deps
library(readr)
library(jsonlite)
dir.create("artifacts", showWarnings = FALSE)

# 1) Save whole fitted objects (best for reuse)
saveRDS(pca,        file = "artifacts/pca.rds")
saveRDS(global_fit, file = "artifacts/gmm_mclust.rds")

# 2) PCA exports --------------------------------------------------------------
# 2a) Scores (use the extended table you animated with)
scores_out <- scores_2D_ext[, c("Country","Year","PC1","PC2","Cluster","Continent/Region")]
write_csv(scores_out, "artifacts/pca_scores_2D_ext.csv")

# 2b) Loadings for PC1-PC2 (you already built load_2D)
write_csv(load_2D[, c("Variable","PC1","PC2")], "artifacts/pca_loadings_2D.csv")

# 2c) Explained variance
expl_var   <- pca$sdev^2
expl_tbl   <- data.frame(
  PC       = paste0("PC", seq_along(expl_var)),
  variance = expl_var,
  var_prop = expl_var / sum(expl_var)
)
write_csv(expl_tbl, "artifacts/pca_variance.csv")

# 2d) Preprocessing (centering/scaling) to apply the same transform later
pre_tbl <- data.frame(variable = names(pca$center),
                      center   = as.numeric(pca$center),
                      scale    = as.numeric(pca$scale))
write_csv(pre_tbl, "artifacts/pca_preprocess.csv")

# 3) GMM exports (mclust) ----------------------------------------------------
# Hard labels & soft responsibilities for the extended set
gmm_pred_all <- predict(global_fit, newdata = dplyr::select(scores_2D_ext, PC1, PC2))
labels_tbl <- data.frame(id = seq_len(nrow(scores_2D_ext)),
                         Country = scores_2D_ext$Country,
                         Year    = scores_2D_ext$Year,
                         cluster = gmm_pred_all$classification)
write_csv(labels_tbl, "artifacts/gmm_labels.csv")

resp_tbl <- as.data.frame(gmm_pred_all$z)
colnames(resp_tbl) <- paste0("comp_", seq_len(ncol(resp_tbl)))
resp_tbl <- cbind(scores_2D_ext[c("Country","Year")], resp_tbl)
write_csv(resp_tbl, "artifacts/gmm_responsibilities.csv")

# Parameters: means (in PC space), covariances, and weights
means_mat <- t(global_fit$parameters$mean)                # K x 2
colnames(means_mat) <- c("PC1","PC2")
means_tbl <- data.frame(component = seq_len(nrow(means_mat)), means_mat)
write_csv(means_tbl, "artifacts/gmm_means.csv")

weights_tbl <- data.frame(component = seq_along(global_fit$parameters$pro),
                          weight    = global_fit$parameters$pro)
write_csv(weights_tbl, "artifacts/gmm_weights.csv")

# Covariances: 2x2xK array — easiest to keep as RDS; also provide flattened CSV
cov_arr <- global_fit$parameters$variance$sigma
saveRDS(cov_arr, "artifacts/gmm_covariances.rds")

# Flatten each 2x2 into a row (PC1_PC1, PC1_PC2, PC2_PC1, PC2_PC2)
flatten_cov <- function(S) {
  data.frame(PC1_PC1 = S[1,1], PC1_PC2 = S[1,2], PC2_PC1 = S[2,1], PC2_PC2 = S[2,2])
}
cov_tbl <- do.call(rbind, lapply(seq_len(dim(cov_arr)[3]), function(k) flatten_cov(cov_arr[,,k])))
cov_tbl$component <- seq_len(nrow(cov_tbl))
cov_tbl <- cov_tbl[, c("component","PC1_PC1","PC1_PC2","PC2_PC1","PC2_PC2")]
write_csv(cov_tbl, "artifacts/gmm_covariances_flat.csv")

# Fit stats
fit_tbl <- data.frame(loglik = global_fit$loglik,
                      df     = global_fit$df,
                      BIC    = global_fit$bic)
write_csv(fit_tbl, "artifacts/gmm_fitstats.csv")

# 4) (Optional) JSON bundles for a webpage -----------------------------------
write_json(scores_out,  "artifacts/pca_scores_2D_ext.json",  dataframe = "rows", pretty = TRUE)
write_json(load_2D,     "artifacts/pca_loadings_2D.json",    dataframe = "rows", pretty = TRUE)
write_json(means_tbl,   "artifacts/gmm_means.json",          dataframe = "rows", pretty = TRUE)
write_json(weights_tbl, "artifacts/gmm_weights.json",        dataframe = "rows", pretty = TRUE)
write_json(cov_tbl,     "artifacts/gmm_covariances.json",    dataframe = "rows", pretty = TRUE)
write_json(resp_tbl,    "artifacts/gmm_responsibilities.json", dataframe = "rows", pretty = TRUE)

