library(readr)
library(dplyr)
library(ggplot2)
library(FactoMineR)

url <- "https://koalaverse.github.io/homlr/data/my_basket.csv"
my_basket <- read_csv(url)
dim(my_basket)

numeric_basket <- my_basket %>% select(where(is.numeric))

pca_result <- prcomp(numeric_basket, scale. = TRUE, center = TRUE)

scree_data <- data.frame(
  PC = 1:length(pca_result$sdev),
  Variance = pca_result$sdev^2,
  PropVar = (pca_result$sdev^2) / sum(pca_result$sdev^2)
)

ggplot(scree_data, aes(x = PC, y = PropVar)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_line(aes(y = cumsum(PropVar)), color = "red", size = 1) +
  geom_point(aes(y = cumsum(PropVar)), color = "red", size = 2) +
  labs(title = "PCA Scree Plot", x = "Principal Component", y = "Proportion of Variance Explained") +
  theme_minimal()

loadings_df <- data.frame(
  feature = rownames(pca_result$rotation),
  PC1 = pca_result$rotation[,1]
)

ggplot(loadings_df, aes(x = PC1, y = reorder(feature, PC1))) +
  geom_point(size = 3, color = "steelblue") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  labs(title = "PCA Loadings Plot (PC1)", x = "PC1 Loading", y = "Feature") +
  theme_minimal()

biplot(pca_result, main = "PCA Biplot")

pca_factominer <- PCA(numeric_basket, scale.unit = TRUE, graph = FALSE)