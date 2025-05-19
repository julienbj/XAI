library(ggplot2)
library(dplyr)
library(scales)

source("functions/colors.R")

#Create MAE plots,  saves to file if filename is given
plot_mae <- function(results, plottitle, filename="", blue=F, rhos=c("0.0", "0.1", "0.3", "0.5", "0.7", "0.9")){
  results <- lapply(results, `[[`, "mae")

  if (blue) {
    my_colors <- c(uioblue1, color2, uioblue2, color4, uioblue3)
  } else {
    my_colors <- c(uioblue2, uiopink2, uioorange2, uiogreen2, uioyellow)
  }
  
  n <- length(results)
  maxv <- max(unlist(results, recursive = TRUE, use.names = FALSE))
  minv <- min(unlist(results, recursive = TRUE, use.names = FALSE))
  
  if(nzchar(filename)){
    path <- paste("results/plots/", filename, ".pdf", sep="")
    pdf(path, family = "serif",
        width  = 6.3,      
        height = 5.0,
        paper  = "special")
  }
  
  par(mfrow = c(3, 2),
      oma = c(4, 9, 4, 2),
      mar = c(4, 2, 2, 1)
      )
  
  for (i in 1:n){
    df <- results[[i]]
    df_title <- bquote(paste(rho, " = ", .(rhos[i])))
    
    if (i %in% c(1, 3, 5)){
      boxplot(df, horizontal=T, col = my_colors, las=2, main = df_title)
    } else {
      boxplot(df, horizontal=T, col = my_colors, las=2, names=F, main = df_title)
    }
  }
  
  mtext(plottitle, outer = TRUE, cex = 1.5, font = 1, line = 1)
  mtext("MEAN AVERAGE ERROR", side = 1, outer = TRUE, line = 0.5, cex = 0.8)
  mtext("METHOD", side = 2, outer = TRUE, line = 6, cex = 0.8)
  
  if(nzchar(filename)){
    dev.off()
  }
}

#Create time plots, saves to file if filename is given
plot_times <- function(m3, m4, m5, m6, m7, m8, m9, blue=F, filename=""){
  if (blue) {
    my_colors <- c("independence" = uioblue1, "empirical"= color2, "gaussian" = uioblue2, copula = "color4")
  } else {
    my_colors <- c("independence" = uioblue2, "empirical" = uiopink2, "gaussian" = uioorange2, "copula" = uiogreen2)
  }
  
  combined <- sort_data(m3, m4, m5, m6, m7, m8, m9)
  
  summary_data <- combined %>%
    group_by(Features, Type) %>%
    summarise(
      mean = mean(Times),
      sd = sd(Times))
  
  summary_data$Type <- factor(
    summary_data$Type,
    levels = c("copula", "gaussian", "empirical", "independence"))
  
  if(nzchar(filename)){
    path <- paste("results/plots/", filename, ".pdf", sep="")
    pdf(path, family="serif",         
        width  = 6.3,      
        height = 5.0,
        paper  = "special")
  }
    
  print(ggplot(summary_data, aes(x = Features, y = log(mean), color = Type, fill = Type)) +
    geom_line(linewidth = 1) +
    geom_ribbon(aes(ymin = log(mean - sd), ymax = log(mean + sd)), alpha = 0.2, color = NA) +
    scale_color_manual(values = my_colors) +
    scale_fill_manual(values = my_colors) +
    scale_x_continuous(breaks = c(3, 4, 5, 6, 7, 8, 9)) +
    theme_minimal() +
    labs(title = "Computational runtime per estimation method", y = expression("Time (s, " * log[10] * ")"), x = "Features (M)") +
    theme(legend.title = element_blank(), plot.title = element_text(hjust = 0.35, size = 14),
    plot.title.position = "plot", axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14))  +
    guides(color = guide_legend(reverse = FALSE),
           fill  = guide_legend(reverse = FALSE)))
    
  if(nzchar(filename)){
    dev.off()
  }
}

#Helper function to sort data before plotting time
sort_data <- function(m3, m4, m5, m6, m7, m8, m9){
  # m3 <- lapply(m3, `[[`, "times")[[rho_nr]]
  # m4 <- lapply(m4, `[[`, "times")[[rho_nr]]
  # m5 <- lapply(m5, `[[`, "times")[[rho_nr]]

  make_df <- function(method_name, f3, f4, f5, f6, f7, f8, f9) {
    data.frame(
      Features = rep(c(3, 4, 5, 6, 7, 8, 9), times = c(length(f3), length(f4), length(f5), length(f6), length(f7), length(f8), length(f9))),
      Times = c(f3, f4, f5, f6, f7, f8, f9),
      Type = method_name
    )
  }
  
  combined <- bind_rows(
    make_df("copula", m3$copula, m4$copula, m5$copula, m6$copula, m7$copula, m8$copula, m9$copula),
    make_df("empirical", m3$empirical, m4$empirical, m5$empirical, m6$empirical, m7$empirical, m8$empirical, m9$empirical),
    make_df("gaussian", m3$gaussian, m4$gaussian, m5$gaussian, m6$gaussian, m7$gaussian, m8$gaussian, m9$gaussian),
    make_df("independence", m3$independence, m4$independence, m5$independence, m6$independence, m7$independence, m8$independence, m9$independence)
  )
  
  return(combined)
}

#Will provide a string with results as a latex-table 
to_latex_string <- function(results, type,
                            which  = c("means", "sds"),
                            digits = 1) {
  which <- match.arg(which)         
  
  tbl   <- create_all_table(results, type)[[which]]   
  mat   <- do.call(cbind, tbl)
  
  rownames(mat) <- c("Independence", "Empirical", "Gaussian", "Copula")
  
  fmt   <- formatC(mat, format = "e", digits = digits)
  lines <- sapply(seq_len(nrow(fmt)), function(i)
    paste(c(rownames(fmt)[i], fmt[i, ]), collapse = " & "))
  
  paste0(lines, " \\\\", collapse = "\n")
  
  create_table <- function(results, type, rho_nr){
    ind <- lapply(results, `[[`, type)[[rho_nr]]$independence
    emp <- lapply(results, `[[`, type)[[rho_nr]]$empirical
    gau <- lapply(results, `[[`, type)[[rho_nr]]$gaussian
    cop <- lapply(results, `[[`, type)[[rho_nr]]$copula
    
    mean_list <- c(independence = mean(ind), empirical = mean(emp), gaussian = mean(gau), copula = mean(cop))
    sd_list <- c(independence = sd(ind), empirical = sd(emp), gaussian = sd(gau), copula = sd(cop))
    
    return (list(mean_list = mean_list, sd_list = sd_list ))
  }
  
  create_all_table <- function(results, type){
    list_mean <- list()
    list_sd <- list()
    for (rho_nr in 1:length(results)) {
      res <- create_table(results, type, rho_nr)
      list_mean[[rho_nr]] <- res$mean_list
      list_sd[[rho_nr]] <-  res$sd_list
    }
    
    return(list(means = list_mean, sds = list_sd))
  }
}
