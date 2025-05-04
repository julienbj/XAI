library(ggplot2)
library(ggbeeswarm)
library(reshape2)
source("colors.R")

plot_results <- function(results, rho_values=c(0.0, 0.3, 0.7, 0.9)) {
  my_colors = c(uioblue1, color2, uioblue2, color4, uioblue3)
  
  par(mfrow = c(2, 2),
      oma = c(6, 6, 4, 2),
      mar = c(4, 4, 2, 1))
  
  for (i in 1:4){
    df <- as.data.frame(results[[i]])
    
    # Determine which axes to draw
    draw_x_axis <- i %in% c(1, 2)  # top row
    draw_y_axis <- i %in% c(1, 3)  # left column
  
    
    boxplot(df, main = bquote(paste(rho, " = ", .(format(rho_values[i], nsmall = 1)))), 
            xlab = "", ylab = "",
            col = my_colors,
            border = "black",
            horizontal = TRUE, 
            las = 1, 
            axes = FALSE)
  }
  
  if (draw_y_axis) axis(2, las = 1)
  if (draw_x_axis) axis(1)
  box()
  
  mtext("Mean Absolute error", side = 1, outer = TRUE, line = 3, cex = 1)  # x-label
  mtext("Methods", side = 2, outer = TRUE, line = 3, cex = 1)  # y-label
  
  par(mfrow = c(1, 1))
}

plot_results_single <- function(results, rho_val){
  my_colors = c(uioblue1, color2, uioblue2, color4, uioblue3)

  boxplot(results, main = bquote(paste(rho, " = ", rho_val)),
          xlab = "Mean Absolute Error", ylab = "Methods",
          col = my_colors,
          border = "black",
          horizontal = TRUE,
          las = 1)
}


