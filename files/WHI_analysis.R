library(dplyr)          # Data wrangling
library(ggplot2)        # Plotting
library(gganimate)      # Animation
library(mice)           # Multiple imputation
library(mvtnorm)        # Multivariate normal density for contours
library(mclust)         # Gaussian Mixture Models
library(RColorBrewer)   # Color palettes
library(readr)          # CSV reading

set.seed(500)

# load data
df <- read_csv("WHI_Inflation.csv")

# Checking for significance
df_noCountrynoRegion <- df %>% select(-Country) %>% select(-`Continent/Region`)
model_all <- lm(`Score` ~ ., data = df_noCountrynoRegion)
summary(model_all)

#perform multiple imputation on data
imputed_data <- mice(df, m = 5, method = 'pmm')

### GMM
# We divide the data into 5 to get random probable sets - lessening imputation bias
df1 <- complete(imputed_data, 1)
df2 <- complete(imputed_data, 2)
df3 <- complete(imputed_data, 3)
df4 <- complete(imputed_data, 4)
df5 <- complete(imputed_data, 5)

# Feature selection and scaling
features1 <- df1 %>% 
  select(`GDP per Capita`, `Social support`, `Healthy life expectancy at birth`,
         `Freedom to make life choices`, `Generosity`, `Perceptions of corruption`,
         `Food Consumer Price Inflation`) %>%
  scale() %>% as.data.frame()
features2 <- df2 %>% 
  select(`GDP per Capita`, `Social support`, `Healthy life expectancy at birth`,
         `Freedom to make life choices`, `Generosity`, `Perceptions of corruption`,
         `Food Consumer Price Inflation`) %>%
  scale() %>% as.data.frame()
features3 <- df3 %>% 
  select(`GDP per Capita`, `Social support`, `Healthy life expectancy at birth`,
         `Freedom to make life choices`, `Generosity`, `Perceptions of corruption`,
         `Food Consumer Price Inflation`) %>%
  scale() %>% as.data.frame()
features4 <- df4 %>% 
  select(`GDP per Capita`, `Social support`, `Healthy life expectancy at birth`,
         `Freedom to make life choices`, `Generosity`, `Perceptions of corruption`,
         `Food Consumer Price Inflation`) %>%
  scale() %>% as.data.frame()
features5 <- df5 %>% 
  select(`GDP per Capita`, `Social support`, `Healthy life expectancy at birth`,
         `Freedom to make life choices`, `Generosity`, `Perceptions of corruption`,
         `Food Consumer Price Inflation`) %>%
  scale() %>% as.data.frame()

# Fit GMM on each dataset
model1 <- Mclust(features1)
model2 <- Mclust(features2)
model3 <- Mclust(features3)
model4 <- Mclust(features4)
model5 <- Mclust(features5)

# Extract classifications
clusters1 <- model1$classification
clusters2 <- model2$classification
clusters3 <- model3$classification
clusters4 <- model4$classification
clusters5 <- model5$classification

# Combine clusterings into a matrix
clusters_all <- cbind(clusters1, clusters2, clusters3, clusters4, clusters5)
clusters_all

# Consensus: majority vote per row
consensus_clusters <- apply(clusters_all, 1, function(x) {
  as.numeric(names(sort(table(x), decreasing = TRUE))[1])
})
consensus_clusters

# Automatically selects the best number of clusters based on BIC
model <- Mclust(consensus_clusters)
summary(model)

plot(model, what = "BIC") # Best number of clusters is 1, not good
# Divide dataset further by year -> time series
# Use PCA

### Visualizing using PCA
## Creating custom PRcomp object
pca_list <- lapply(list(features1, features2, features3, features4, features5), prcomp)

# PCA coordinate values for each Principal Component axis
avg_scores <- Reduce("+", lapply(pca_list, function(pca) pca$x)) / 5

# PCA rotation values
avg_rotation <- Reduce("+", lapply(pca_list, function(p) p$rotation)) / length(pca_list)

# Average variance of each point
avg_variance <- avg_variance <- Reduce("+", lapply(pca_list, function(p) p$sdev^2)) / length(pca_list)

# Eigenvalues <-> proportion of variance to the sum of variance
prop_var <- avg_variance / sum(avg_variance)

# Add on the GMM clusters, country and year
pca_result <- list(
  x = avg_scores,
  rotation = avg_rotation,
  sdev = sqrt(avg_variance),
  prop_var = prop_var
)
class(pca_result) <- "prcomp"

## Scree plot
barplot(prop_var * 100,
        names.arg = paste0("PC", 1:length(prop_var)),
        ylab = "Variance Explained (%)",
        main = "Scree Plot (Averaged PCA)")
# 1st PC explains 37% variance, 2nd PC explains 17% of variance, etc

# Extract the first 2 PCs (for visualization purposes ONLY, we should actually choose ~3 or 4 PCs)
pca_scores <- as.data.frame(avg_scores[, 1:2])
colnames(pca_scores) <- c("PC1", "PC2")
loadings <- as.data.frame(pca_result$rotation[, 1:2])
loadings$Feature <- rownames(loadings)

## Contour plotting
# Create our grid
pca_grid <- expand.grid(
  PC1 = seq(min(pca_scores$PC1), max(pca_scores$PC1), length.out = 100),
  PC2 = seq(min(pca_scores$PC2), max(pca_scores$PC2), length.out = 100)
)

# Prepare plot data
plot_data <- pca_scores %>%
  mutate(Cluster = as.factor(consensus_clusters),
         Country = df1$Country,
         Year = df1$Year,
         Region = df1$`Continent/Region`) %>%
  arrange(Country, Year)

# List of years
years <- unique(df1$Year)
cluster_levels <- unique(consensus_clusters)

arrow_scale <- 5

# Generate contour data per year
contour_data_by_year <- lapply(years, function(y) {
  pd <- plot_data %>% filter(Year == y)
  
  lapply(cluster_levels, function(k) {
    cluster_points <- pd %>% filter(Cluster == k)
    if (nrow(cluster_points) < 2) return(NULL)
    
    mu <- colMeans(cluster_points[, c("PC1", "PC2")])
    sig <- cov(cluster_points[, c("PC1", "PC2")])
    if (any(is.na(sig)) || det(sig) < 1e-10) sig <- sig + diag(1e-6, 2)
    
    density <- dmvnorm(pca_grid, mean = mu, sigma = sig)
    
    data.frame(pca_grid,
               Density = density,
               Cluster = as.factor(k),
               Year = y)
  }) %>% bind_rows()
})

# Bind all years into one dataframe
collated_CD <- do.call(rbind, contour_data_by_year)
collated_CD

## Animating time-series
# Get full set of cluster levels
cluster_levels <- sort(unique(plot_data$Cluster))

# Assign a fixed color palette (use RColorBrewer or manual)
palette <- brewer.pal(n = length(cluster_levels), name = "Set1")

# The ggplot object to iterate over
p <- ggplot(plot_data, aes(x = PC1, y = PC2, color = Cluster, group = Country)) +
  geom_point(size = 2.5, alpha = 0.8) +
  geom_text(aes(label = Country), color = "darkgrey", size = 2.5, check_overlap = TRUE) +
  geom_segment(data = loadings,
               aes(x = 0, y = 0, 
                   xend = PC1 * arrow_scale, 
                   yend = PC2 * arrow_scale),
               inherit.aes = FALSE,
               arrow = arrow(length = unit(0.2, "cm")),
               color = "black") +
  geom_text(data = loadings,
            aes(x = PC1 * arrow_scale, 
                y = PC2 * arrow_scale, 
                label = Feature),
            inherit.aes = FALSE,
            color = "black", size = 4) +
  theme_minimal() +
  labs(title = "PCA of WHI Data with GMM Clusters",
       subtitle = "Year: {frame_time}",
       x = "Principal Component 1",
       y = "Principal Component 2",
       color = "Cluster") +
  transition_time(Year) +
  scale_color_manual(values = setNames(palette, cluster_levels)) +
  enter_fade() +
  exit_fade() +
  ease_aes("linear")

gganimate::animate(p, nframes = 600, fps = 30, width = 900, height = 600,
        renderer = gifski_renderer("whi_pca_gmm.gif"))

