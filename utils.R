
# Function for ACF and PACF
plot_acf_pacf <- function(model, conf_level = 0.95) {
  # Calculate residuals and ACF/PACF
  residuals <- resid(model)
  acf_result <- acf(residuals, plot = FALSE)
  pacf_result <- pacf(residuals, plot = FALSE)
  
  # Calculate critical lines
  ciline <- qnorm((1 - conf_level) / 2) / sqrt(length(residuals))
  
  # Create ACF plot
  acf_df <- data.frame(lag = acf_result$lag, acf = acf_result$acf)
  acf_plot <- ggplot(data = acf_df, aes(x = lag, y = acf)) +
    geom_hline(aes(yintercept = 0)) +
    geom_segment(mapping = aes(xend = lag, yend = 0)) +
    geom_hline(aes(yintercept = ciline), linetype = 2, color = "blue") +
    geom_hline(aes(yintercept = -ciline), linetype = 2, color = "blue") +
    labs(
      title = "Autocorrelation Function (ACF)",
      x = "Lag", y = "ACF"
    ) +
    theme_bw()
  
  # Create PACF plot
  pacf_df <- data.frame(lag = pacf_result$lag, acf = pacf_result$acf)
  pacf_plot <- ggplot(data = pacf_df, aes(x = lag, y = acf)) +
    geom_hline(aes(yintercept = 0)) +
    geom_segment(mapping = aes(xend = lag, yend = 0)) +
    geom_hline(aes(yintercept = ciline), linetype = 2, color = "blue") +
    geom_hline(aes(yintercept = -ciline), linetype = 2, color = "blue") +
    labs(
      title = "Partial Autocorrelation Function (PACF)",
      x = "Lag", y = "PACF"
    ) +
    theme_bw()
  
  return(list(acf_plot = ggplotly(acf_plot, width = 425, height = 425), pacf_plot = ggplotly(pacf_plot, width = 425, height = 425)))
}