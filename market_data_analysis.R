market_data<- read.csv("C:/Users/_PBCR_/Desktop/Kaggle/market_data.csv")
#summary Statistics
summary(market_data)
#correlation 
# Calculate the correlation matrix
correlation_matrix <- cor(market_data)
correlation_matrix
# Create a correlation map with labels
library(corrplot)

corrplot(correlation_matrix, method = "color", type = "upper", tl.col = "black", tl.srt = 45)
# Price vs. Sales Analysis
plot(market_data$Price, market_data$Sale, xlab = "Price", ylab = "Sales", main = "Price vs. Sales")
# Create scatter plots for each advertising channel vs. sales
library(ggplot2)
# Radio
ggplot(market_data, aes(x = Radio, y = Sale)) +
  geom_point() +
  labs(x = "Radio Ads Spending", y = "Sales") +
  ggtitle("Radio Ads vs. Sales")

# In-Store Spending
ggplot(market_data, aes(x = InStrSpending, y = Sale)) +
  geom_point() +
  labs(x = "In-Store Spending", y = "Sales") +
  ggtitle("In-Store Spending vs. Sales")

# TV
ggplot(market_data, aes(x = TVSpending, y = Sale)) +
  geom_point() +
  labs(x = "TV Ads Spending", y = "Sales") +
  ggtitle("TV Ads vs. Sales")

# Online Advertising
ggplot(market_data, aes(x = OnlineAdsSpending, y = Sale)) +
  geom_point() +
  labs(x = "Online Ads Spending", y = "Sales") +
  ggtitle("Online Ads vs. Sales")


#regression Analysis 
model <- lm(Sale ~ ., data = market_data)
summary(model)



# Select the features you want to use for segmentation
selected_features <- market_data[, c("InStrSpending", "Discount", "TVSpending", "StockRate", "Price", "Radio", "OnlineAdsSpending")]

# Standardize the data (important for k-means)
scaled_data <- scale(selected_features)

# Determine the number of clusters (k)
# You can use methods like the elbow method to choose the optimal k
# In this example, let's assume you want to create 3 clusters
k <- 3

# Perform k-means clustering
kmeans_result <- kmeans(scaled_data, centers = k)

# Add the cluster assignments to the original dataset
market_data$Cluster <- kmeans_result$cluster

# Summary of cluster assignments
table(market_data$Cluster)

# Visualize the clusters (e.g., using PCA)
library(ggplot2)
pca_data <- prcomp(scaled_data, scale = TRUE)
pca_df <- as.data.frame(pca_data$x)
pca_df$Cluster <- market_data$Cluster
ggplot(pca_df, aes(x = PC1, y = PC2, color = Cluster)) +
  geom_point() +
  labs(x = "Principal Component 1", y = "Principal Component 2") +
  ggtitle("Customer Segmentation")

