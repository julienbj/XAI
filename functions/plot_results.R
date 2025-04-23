library(ggplot2)
library(ggbeeswarm)
library(reshape2)

plot_results <- function(results){
  shap_df <- melt(as.data.frame(results$true_shap), variable.name = "Feature", value.name = "SHAP")
  
  ggplot(shap_df, aes(x = SHAP, y = Feature, color= Feature)) +
    geom_beeswarm(cex = 0.5) +
    theme_minimal() +
    labs(title = "True SHAP Values", x = "SHAP Value", y = "Feature") +
    theme(legend.position = "none")
}