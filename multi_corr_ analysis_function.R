
# =============================================================================
# Add DS multi-correlation-analysis function
# =============================================================================

# Main analysis function
multi_corr_analysis <- function(model, x_var = NULL, y_var = NULL, x_name = "X", y_name = "Y") {

  # Help message when called without arguments
  if(missing(model)) {
    cat("USAGE EXAMPLES:\n")
    cat("Simple regression: multi_corr_analysis(model1, x_name='Education', y_name='Income')\n")
    cat("Multiple regression: multi_corr_analysis(model2, x_var='education', x_name='Education', y_name='Income')\n")
    cat("Arguments:\n")
    cat("  model = your lm() model object\n")
    cat("  x_var = name of predictor variable (for multiple regression)\n")
    cat("  x_name, y_name = labels for output\n")
    return(invisible())
  }

  # Extract data from model
  model_data <- model$model
  y <- model_data[,1]  # First column is always the response

  # For correlation, use the primary predictor
  if(is.null(x_var)) {
    x <- model_data[,2]  # Second column for simple models
  } else {
    x <- model_data[[x_var]]  # Specified predictor for complex models
  }

  # Get all measures
  correlation <- cor(x, y)

  # Get slope for the specified predictor
  if(is.null(x_var)) {
    slope <- coef(model)[2]  # Simple case
  } else {
    slope <- coef(model)[x_var]  # Named coefficient
  }

  r_squared <- summary(model)$r.squared
  rse <- summary(model)$sigma

  # Calculate MAE
  predictions <- predict(model)
  mae <- mean(abs(y - predictions))

  # Create clean output
  results <- data.frame(
    Measure = c("Correlation", "Slope", "R²", "RSE", "MAE"),
    Value = c(
      round(correlation, 3),
      round(slope, 3),
      round(r_squared, 3),
      round(rse, 3),
      round(mae, 3)
    ),
    Interpretation = c(
      "Relationship strength (-1 to 1)",
      paste(y_name, "change per", x_name, "unit"),
      "% model improves predictions",
      paste("Typical prediction error in", y_name, "units"),
      paste("Average prediction error in", y_name, "units")
    )
  )

  return(results)
}

# Helper function for ggplot-ready stats
extract_model_stats_for_ggplot <- function(model, x_var = NULL, x_name = "X", y_name = "Y") {
  stats <- multi_corr_analysis(model, x_var, y_var = NULL, x_name, y_name)

  # Return named list for easy access
  list(
    r2 = stats$Value[stats$Measure == "R²"],
    slope = stats$Value[stats$Measure == "Slope"],
    correlation = stats$Value[stats$Measure == "Correlation"],
    rse = stats$Value[stats$Measure == "RSE"],
    mae = stats$Value[stats$Measure == "MAE"]
  )
}

# Quick help function
mca_help <- function() {
  cat("=== MULTI_CORR_ANALYSIS QUICK REFERENCE ===\n\n")
  cat("SIMPLE REGRESSION:\n")
  cat("model1 <- lm(y ~ x, data = df)\n")
  cat("multi_corr_analysis(model1, x_name = 'Predictor', y_name = 'Outcome')\n\n")
  cat("multi_corr_analysis(job_data$education, job_data$income, 'Education', 'Income)\n\n")

  cat("MULTIPLE REGRESSION:\n")
  cat("model2 <- lm(y ~ x + z, data = df)\n")
  cat("multi_corr_analysis(model2, x_var = 'x', x_name = 'Predictor', y_name = 'Outcome')\n\n")

  cat("FOR GGPLOT SUBTITLES:\n")
  cat("stats <- extract_model_stats(model1, x_name = 'Education')\n")
  cat("subtitle = paste0('R² = ', stats$r2, ' | Slope = ', stats$slope)\n\n")
}


