#https://www.geeksforgeeks.org/r-language/principal-component-analysis-with-r-programming/
library(dplyr); library(ggplot2); library(ggfortify); library(factoextra)

str(mtcars)
my_pca <- prcomp(mtcars, scale. = TRUE)
summary(my_pca)
biplot(my_pca, scale = 0, main = "Base R Biplot")

# Scree plot
fviz_eig(my_pca, addlabels = TRUE, ylim = c(0, 50))

# PC1-PC2 scores plot
autoplot(my_pca, data = mtcars, colour = "cyl",
         loadings = TRUE, loadings.label = TRUE, loadings.label.size = 3,
         label = TRUE, label.size = 3) +
  theme_minimal() + labs(title = "PCA Scores + Loadings")

autoplot(my_pca, choice = "jitter", loadings = TRUE, loadings.label = TRUE,
         loadings.label.size = 4) + theme_minimal()

# Cumulative variance
propve <- (my_pca$sdev^2 / sum(my_pca$sdev^2)) * 100
tibble(PC = 1:length(propve), Var = cumsum(propve)) %>%
  ggplot(aes(PC, Var)) + geom_line(color = "steelblue") + geom_point() +
  labs(title = "Cumulative Variance Explained (%)") + ylim(0, 100)
