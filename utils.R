# Helper function for ACF and PACF - returns both ggplot and plotly versions
plot_acf_pacf <- function(model, conf_level = 0.95) {
  # Calculate residuals and ACF/PACF
  residuals <- resid(model)
  acf_result <- acf(residuals, plot = FALSE)
  pacf_result <- pacf(residuals, plot = FALSE)
  
  # Calculate critical lines
  ciline <- qnorm((1 - conf_level) / 2) / sqrt(length(residuals))
  
  # Create ACF plot (ggplot version)
  acf_df <- data.frame(lag = acf_result$lag, acf = acf_result$acf)
  acf_plot_ggplot <- ggplot(data = acf_df, aes(x = lag, y = acf)) +
    geom_hline(aes(yintercept = 0)) +
    geom_segment(mapping = aes(xend = lag, yend = 0)) +
    geom_hline(aes(yintercept = ciline), linetype = 2, color = "blue") +
    geom_hline(aes(yintercept = -ciline), linetype = 2, color = "blue") +
    labs(
      title = "Autocorrelation Function (ACF)",
      x = "Lag", y = "ACF"
    ) +
    theme_bw()
  
  # Create PACF plot (ggplot version)
  pacf_df <- data.frame(lag = pacf_result$lag, acf = pacf_result$acf)
  pacf_plot_ggplot <- ggplot(data = pacf_df, aes(x = lag, y = acf)) +
    geom_hline(aes(yintercept = 0)) +
    geom_segment(mapping = aes(xend = lag, yend = 0)) +
    geom_hline(aes(yintercept = ciline), linetype = 2, color = "blue") +
    geom_hline(aes(yintercept = -ciline), linetype = 2, color = "blue") +
    labs(
      title = "Partial Autocorrelation Function (PACF)",
      x = "Lag", y = "PACF"
    ) +
    theme_bw()
  
  # Return both ggplot and plotly versions
  return(list(
    acf_plot = ggplotly(acf_plot_ggplot, width = 425, height = 425),
    acf_plot_ggplot = acf_plot_ggplot,
    pacf_plot = ggplotly(pacf_plot_ggplot, width = 425, height = 425),
    pacf_plot_ggplot = pacf_plot_ggplot
  ))
}

# Helper fucntion for plotting residuals
create_residual_plot_from_model <- function(glm_model, dataset, subject_id, timevar, outcome) {
  # Extract residuals
  dtresidL <- subset(dataset, select = c(subject_id, timevar, outcome))
  dtresidL$residuals <- glm_model$residuals
  dtresidW <- pivot_wider(dtresidL,
                          id_cols = subject_id, 
                          names_from = timevar, 
                          values_from = residuals,
                          names_prefix = "resid_"
  )
  
  # Create correlation matrices
  resCor <- cor(dtresidW[, -1])
  
  # Create a list of vectors from the matrix
  listCor <- map(-(ncol(resCor) - 1):(ncol(resCor) - 1), 
                 ~ mean(resCor[outer(1:ncol(resCor), 1:ncol(resCor), "-") == .x]))
  
  # Create a data frame from listCor
  tp <- ncol(resCor)
  lag <- 1:(tp - 1)
  means <- unlist(listCor[1:(tp - 1)])
  dat <- data.frame(lag, means = rev(means))
  
  # Create the ggplot object
  p <- ggplot(dat, aes(x = lag, y = means)) +
    geom_line() +
    scale_x_continuous(breaks = lag) +
    ylim(c(-1, 1)) +
    xlab("Lag") +
    ylab("Mean") +
    ggtitle("Correlation structure") +
    theme_bw()
  
  return(p)
}

