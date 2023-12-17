#Project work 3
library(factoextra)
library(cluster)
library(tidyr)
library(readxl)
library(ggplot2)
library(corrplot)
library(caret)



### Preprocessing:

# 0. Loading and viewing dataset
Wine_quality <- read_excel("Wine_quality.xlsx")
View(Wine_quality)

# 1. Plot histograms to illustrate the distribution of wine quality in both red and white wine types.

# Plot for both wines
quality_hist <- ggplot(Wine_quality, aes(x = quality)) +
  geom_histogram(binwidth = 0.5, fill = "pink", color = "pink") +
  labs(title = "Distribution of Quality Variable (All wines)",
       x = "Quality",
       y = "Frequency")
print(quality_hist)

# Plot for Red Wine
red_plot <- ggplot(Wine_quality[Wine_quality$type == "red", ], aes(x = quality)) +
  geom_bar(stat = "count", fill = "violetred4") +
  labs(title = "Red Wine Quality Distribution", x = "Quality", y = "Count")

# Plot for White Wine
white_plot <- ggplot(Wine_quality[Wine_quality$type == "white", ], aes(x = quality)) +
  geom_bar(stat = "count", fill = "beige") +
  labs(title = "White Wine Quality Distribution", x = "Quality", y = "Count")

# Display the plots side by side
library(gridExtra)
grid.arrange(red_plot, white_plot, ncol = 2)



# 2. Assess the variance of input variables. Identify and consider (if needed) removing variables
#with exceptionally low variance compared to others. 

selected_columns <- c("fixed.acidity", "volatile.acidity", "citric.acid", "residual.sugar", "chlorides", "free.sulfur.dioxide", "total.sulfur.dioxide", "density", "sulphates","alcohol")
variances <- apply(Wine_quality[, selected_columns], 2, var)

for (i in seq_along(selected_columns)) {
  print(paste("Variance of", selected_columns[i], ":", variances[i]))
}

# Density - low variance
boxplot(Wine_quality$density, main = "Density Levels")

# Deleting density and output columns
df <- Wine_quality[, -which(names(Wine_quality) == "density")]
df <- df[, -which(names(df) == "type")]
df <- df[, -which(names(df) == "quality")]


# 3. Conduct correlation analysis among input variables and identify highly correlated columns and
#visualize the results. Discuss your approach for managing correlated variables (if needed).

# Compute the correlation matrix
cor_matrix <- cor(df)
print(cor_matrix)

# Visualize the correlation matrix using corrplot
corrplot(cor_matrix, method = "color", type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)

highly_correlated <- findCorrelation(cor_matrix, cutoff = -0.4)
pairs(df[, highly_correlated])



### Clustering 
#Apply k-means clustering to analyze wine quality data. Utilize only input
#variables and exclude output variables (quality and type) from the analysis.

#ready dataset: df

# 1. Scaling
scaled_df <- scale(df)
View(scaled_df)

# 2. Elbow and Silhouette
df_clust=data.frame(scaled_df)
fviz_nbclust(df, kmeans, method="wss") + 
  geom_vline(xintercept = 2, linetype = 2) +
  labs(subtitle = "Elbow Method")
fviz_nbclust(df, kmeans, method="silhouette")+
  labs(subtitle = "Silhouette Method")

# 3. K-means
km_res <- kmeans(df_clust, 2, nstart = 25)
aggregate(df, by=list(cluster=km_res$cluster),mean)
km_res
table(km_res$cluster)

# Visualize
fviz_cluster(km_res, scaled_df, ellipse.type = "norm") 

# Assuming 'type' column represents the wine type ('red' or 'white')
wine_type <- Wine_quality$type

# Combine cluster assignments with the wine type
clustered_data <- cbind(df_clust, Cluster = km_res$cluster, Type = wine_type)

# Create a table of the distribution of wine types in each cluster
cluster_distribution <- table(clustered_data$Cluster, clustered_data$Type)

# Convert the table to a data frame for ggplot
cluster_distribution_df <- as.data.frame.matrix(cluster_distribution)
cluster_distribution_df$Cluster <- rownames(cluster_distribution_df)

# Reshape the data for ggplot
cluster_distribution_df_long <- tidyr::gather(cluster_distribution_df, key = "Type", value = "Count", -Cluster)

# Create a bar plot
ggplot(cluster_distribution_df_long, aes(x = Cluster, y = Count, fill = Type)) +
  geom_bar(stat = "identity") +
  labs(title = "Distribution of Wines in Clusters",
       x = "Cluster",
       y = "Count",
       fill = "Wine Type")


### Classification
#Classification: Utilize K nearest neighbor classification algorithm to predict the wine type based
#on the input variables (do not use the quality variable)
# Deleting density and output columns


library(ggplot2)

# Assuming your dataset is named your_data
# Replace your_data with your actual dataset name

# Melt the data into long format
melted_data <- reshape2::melt(Wine_quality)

# Create boxplots with facets
ggplot(melted_data, aes(x = variable, y = value)) +
  geom_boxplot() +
  facet_wrap(~variable, scales = "free") +
  labs(title = "Boxplot of All Variables",
       caption = "Source: your_data dataset.") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


