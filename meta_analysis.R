# Comprehensive Meta-Analysis Script - FIXED VERSION
# Fixed clean funnel plot - removed problematic Egger line, kept statistics
# Author: Enhanced for comprehensive meta-analysis with detailed outputs
# Date: 2025

# Clear console and set options
cat("\014")
rm(list = ls())
options(stringsAsFactors = FALSE)

# Enhanced error handling
options(error = function() {
  calls <- sys.calls()
  if (length(calls) >= 2) {
    cat("Error occurred in:\n")
    print(calls[[length(calls) - 1]])
  }
  traceback()
})

# Load required libraries with error handling
cat("=== LOADING REQUIRED PACKAGES ===\n")
if (!require("pacman")) {
  cat("Installing pacman package manager...\n")
  install.packages("pacman")
}

# Load all packages and track which ones succeed
packages_to_load <- c(
  "meta", "metafor", "dplyr", "ggplot2", "gridExtra",
  "RColorBrewer", "viridis", "readr", "stringr", "purrr",
  "forestplot", "grid", "ggrepel", "patchwork", "scales"
)

# Try to load Cairo separately as it's optional
cairo_available <- FALSE
if (.Platform$OS.type == "windows" || Sys.info()["sysname"] == "Linux") {
  if (requireNamespace("Cairo", quietly = TRUE)) {
    library(Cairo)
    cairo_available <- TRUE
    cat("Cairo graphics loaded successfully\n")
  } else {
    cat("Cairo not available - using standard graphics\n")
  }
}

# Load main packages
pacman::p_load(char = packages_to_load)
cat("All required packages loaded successfully\n\n")

# Set graphics options based on Cairo availability
if (cairo_available) {
  options(
    device = function(...) {
      Cairo::CairoPNG(..., dpi = 300, pointsize = 12)
    }
  )
} else {
  options(device = png)
}

# Modern color palette with consistent theme
modern_colors <- c(
  primary = "#2E86AB",      # Blue
  secondary = "#A23B72",    # Purple-pink
  accent1 = "#F18F01",      # Orange
  accent2 = "#C73E1D",      # Red
  neutral = "#6C757D",      # Gray
  success = "#28A745",      # Green
  info = "#17A2B8",         # Cyan
  warning = "#FFC107",      # Yellow
  egger_line = "#FF6B6B",   # Red for Egger's line
  ci_band = "#FFE66D"       # Yellow for CI band
)

# Consistent font settings
base_font_size <- 12
title_font_size <- 16
subtitle_font_size <- 14
axis_font_size <- 11
label_font_size <- 10

# Global variable for results directory
results_dir <- NULL

# Function to check data structure and provide diagnostics
check_data_structure <- function(data) {
  cat("\n=== DATA STRUCTURE CHECK ===\n")
  cat("Dimensions:", nrow(data), "rows x", ncol(data), "columns\n")
  cat("Column names:\n")
  print(names(data))
  
  # Check for required columns
  required_cols <- c(
    "Intervention_Mean", "Intervention_SD", "Intervention_N",
    "Control_Mean", "Control_SD", "Control_N"
  )
  
  # Case-insensitive check
  data_cols_lower <- tolower(names(data))
  found_cols <- character()
  missing_cols <- character()
  
  for (col in required_cols) {
    if (tolower(col) %in% data_cols_lower) {
      found_cols <- c(found_cols, col)
    } else {
      missing_cols <- c(missing_cols, col)
    }
  }
  
  cat("\nRequired columns found:", paste(found_cols, collapse = ", "), "\n")
  if (length(missing_cols) > 0) {
    cat("MISSING REQUIRED COLUMNS:", paste(missing_cols, collapse = ", "), "\n")
  }
  
  # Check data types
  cat("\nData types:\n")
  for (col in names(data)) {
    cat(sprintf("  %s: %s\n", col, class(data[[col]])[1]))
  }
  
  # Check for complete cases
  if (length(found_cols) == length(required_cols)) {
    complete_cases <- complete.cases(data[, required_cols])
    cat("\nComplete cases for meta-analysis:", sum(complete_cases), "out of", nrow(data), "\n")
  }
  
  return(list(found = found_cols, missing = missing_cols))
}

# Function to read and prepare data with enhanced error handling
prepare_data <- function(file_path) {
  cat("\n=== READING DATA ===\n")
  
  # Check if file exists
  if (!file.exists(file_path)) {
    cat("ERROR: File not found:", file_path, "\n")
    cat("Current working directory:", getwd(), "\n")
    cat("Files in current directory:\n")
    print(list.files(pattern = "*.csv"))
    
    # Offer to create sample data
    response <- readline("Would you like to create sample data for testing? (y/n): ")
    if (tolower(response) == "y") {
      return(create_sample_data())
    } else {
      stop("No data file found and sample data declined")
    }
  }
  
  cat("Reading file:", file_path, "\n")
  
  # Try multiple reading methods
  data <- NULL
  
  # Method 1: readr
  tryCatch({
    data <- read_csv(file_path, show_col_types = FALSE)
    cat("Successfully read data using read_csv\n")
  }, error = function(e) {
    cat("read_csv failed:", e$message, "\n")
  })
  
  # Method 2: base R
  if (is.null(data)) {
    tryCatch({
      data <- read.csv(file_path, stringsAsFactors = FALSE)
      cat("Successfully read data using read.csv\n")
    }, error = function(e) {
      cat("read.csv failed:", e$message, "\n")
    })
  }
  
  # Method 3: with different encodings
  if (is.null(data)) {
    encodings <- c("UTF-8", "latin1", "Windows-1252")
    for (enc in encodings) {
      tryCatch({
        data <- read.csv(file_path, stringsAsFactors = FALSE, fileEncoding = enc)
        cat("Successfully read data using encoding:", enc, "\n")
        break
      }, error = function(e) {
        cat("Failed with encoding", enc, "\n")
      })
    }
  }
  
  if (is.null(data)) {
    stop("Could not read data file with any method")
  }
  
  # Check data structure
  col_check <- check_data_structure(data)
  
  # Clean column names (standardize)
  colnames(data) <- str_replace_all(colnames(data), " ", "_")
  colnames(data) <- str_replace_all(colnames(data), "[()]", "")
  
  # Fix case sensitivity issues
  name_mapping <- c(
    "intervention_mean" = "Intervention_Mean",
    "intervention_sd" = "Intervention_SD",
    "intervention_n" = "Intervention_N",
    "control_mean" = "Control_Mean",
    "control_sd" = "Control_SD",
    "control_n" = "Control_N"
  )
  
  for (old_name in names(name_mapping)) {
    idx <- which(tolower(names(data)) == old_name)
    if (length(idx) > 0) {
      names(data)[idx[1]] <- name_mapping[old_name]
    }
  }
  
  # Ensure numeric columns are numeric
  numeric_cols <- c("Intervention_Mean", "Intervention_SD", "Intervention_N",
                    "Control_Mean", "Control_SD", "Control_N", "Year")
  
  for (col in numeric_cols) {
    if (col %in% names(data) && !is.numeric(data[[col]])) {
      cat("Converting", col, "to numeric\n")
      data[[col]] <- as.numeric(as.character(data[[col]]))
    }
  }
  
  # Add missing optional columns with defaults
  if (!"Author" %in% names(data)) {
    data$Author <- paste0("Study", 1:nrow(data))
    cat("Added default Author column\n")
  }
  if (!"Year" %in% names(data)) {
    data$Year <- 2020
    cat("Added default Year column\n")
  }
  if (!"Outcome" %in% names(data)) {
    data$Outcome <- "Primary Outcome"
    cat("Added default Outcome column\n")
  }
  
  # Create study identifier and handle multiple entries per study
  data <- data %>%
    group_by(Author, Year, Outcome) %>%
    mutate(
      study_count = row_number(),
      study_suffix = case_when(
        study_count == 1 ~ "",
        study_count == 2 ~ "b",
        study_count == 3 ~ "c",
        study_count == 4 ~ "d",
        study_count == 5 ~ "e",
        TRUE ~ paste0(letters[study_count])
      ),
      study_id = paste0(Author, " (", Year, ")", study_suffix)
    ) %>%
    ungroup() %>%
    arrange(desc(Year), Author, study_count)
  
  # Final data summary
  cat("\n=== DATA SUMMARY ===\n")
  cat("Total rows:", nrow(data), "\n")
  cat("Unique outcomes:", length(unique(data$Outcome)), "\n")
  cat("Outcomes:", paste(unique(data$Outcome), collapse = ", "), "\n")
  
  return(data)
}

# Function to create sample data for testing
create_sample_data <- function() {
  cat("\nCreating sample data for demonstration...\n")
  
  set.seed(123)
  n_studies <- 20
  
  sample_data <- data.frame(
    Author = rep(paste0("Author", 1:10), 2),
    Year = rep(2015:2024, 2),
    Outcome = rep(c("HbA1c (%)", "Weight (kg)"), each = 10),
    Outcome_Unit = rep(c("%", "kg"), each = 10),
    Drug = rep(c("Semaglutide", "Liraglutide"), 10),
    Dosage = rep(c("1.0 mg", "1.8 mg"), 10),
    Control = rep("Placebo", n_studies),
    Intervention_Mean = c(
      rnorm(10, -1.5, 0.3),  # HbA1c reduction
      rnorm(10, -5.2, 1.2)   # Weight reduction
    ),
    Intervention_SD = c(
      runif(10, 0.8, 1.2),
      runif(10, 3.5, 4.5)
    ),
    Intervention_N = sample(50:150, n_studies, replace = TRUE),
    Control_Mean = c(
      rnorm(10, -0.3, 0.2),
      rnorm(10, -0.8, 0.5)
    ),
    Control_SD = c(
      runif(10, 0.9, 1.3),
      runif(10, 3.8, 4.8)
    ),
    Control_N = sample(50:150, n_studies, replace = TRUE),
    Country = sample(c("USA", "UK", "Germany", "Japan"), n_studies, replace = TRUE),
    Study_Duration_weeks = sample(c(12, 24, 26, 52), n_studies, replace = TRUE),
    Disease_Stage = sample(c("Early", "Advanced"), n_studies, replace = TRUE)
  )
  
  cat("Sample data created with", nrow(sample_data), "studies\n")
  return(sample_data)
}

# Function to determine effect size type (MD vs SMD)
determine_effect_size <- function(outcome_data) {
  units <- unique(outcome_data$Outcome_Unit[!is.na(outcome_data$Outcome_Unit)])
  if (length(units) <= 1) {
    return("MD")  # Mean Difference for same units
  } else {
    return("SMD") # Standardized Mean Difference for different units
  }
}

# Function to create results directory structure
create_results_structure <- function() {
  # Create main results directory with timestamp
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  results_dir <<- paste0("Results_", timestamp)
  
  if (!dir.exists(results_dir)) {
    dir.create(results_dir, recursive = TRUE)
  }
  
  cat("Created main results directory:", results_dir, "\n")
  return(results_dir)
}

# Function to create outcome-specific subdirectory
create_outcome_directory <- function(outcome_name) {
  safe_outcome_name <- str_replace_all(outcome_name, "[^A-Za-z0-9]", "_")
  outcome_dir <- file.path(results_dir, safe_outcome_name)
  
  if (!dir.exists(outcome_dir)) {
    dir.create(outcome_dir, recursive = TRUE)
  }
  
  return(outcome_dir)
}

# Enhanced function to create forest plot with consistent formatting
create_forest_plot <- function(escalc_result, fixed_model, random_model, outcome_name, effect_label, effect_type) {
  
  cat("Creating forest plots...\n")
  
  # Create outcome directory
  outcome_dir <- create_outcome_directory(outcome_name)
  
  # Create Fixed Effects Forest Plot
  tryCatch({
    create_single_forest_plot(escalc_result, fixed_model, outcome_name, effect_label, 
                              effect_type, "Fixed", outcome_dir)
  }, error = function(e) {
    cat("Error creating fixed effects plot:", e$message, "\n")
  })
  
  # Create Random Effects Forest Plot  
  tryCatch({
    create_single_forest_plot(escalc_result, random_model, outcome_name, effect_label, 
                              effect_type, "Random", outcome_dir)
  }, error = function(e) {
    cat("Error creating random effects plot:", e$message, "\n")
  })
}

# Function to create a single forest plot
create_single_forest_plot <- function(escalc_result, model, outcome_name, effect_label, 
                                      effect_type, model_type, outcome_dir) {
  
  filename <- file.path(outcome_dir, paste0("forest_plot_", tolower(model_type), "_effects.png"))
  
  # Calculate plot dimensions
  n_studies <- length(escalc_result$study_id)
  plot_height <- max(8, 4 + 0.3 * n_studies)
  
  # Create plot using basic graphics if Cairo not available
  if (cairo_available) {
    Cairo::CairoPNG(filename, width = 20, height = plot_height, units = "in", res = 300)
  } else {
    png(filename, width = 20 * 300, height = plot_height * 300, res = 300)
  }
  
  # Use metafor's forest function
  tryCatch({
    forest(model, 
           slab = escalc_result$study_id,
           xlim = c(-10, 10),
           ylim = c(-1.5, n_studies + 3),
           xlab = effect_label,
           mlab = paste(model_type, "Effects Model"),
           header = TRUE,
           shade = TRUE,
           annotate = TRUE,
           addfit = TRUE,
           addcred = (model_type == "Random"),
           col = modern_colors["primary"],
           border = modern_colors["primary"],
           lty = c("solid", "blank"),
           fonts = "sans",
           cex = 0.8,
           cex.lab = 1,
           cex.axis = 0.9,
           main = paste(model_type, "Effects Meta-Analysis:", outcome_name))
    
    # Add sample sizes if available
    if (all(c("Intervention_N", "Control_N") %in% names(escalc_result))) {
      text(-10, n_studies + 1.5, "n (Int/Ctrl)", font = 2, adj = 0, cex = 0.7)
      for (i in 1:n_studies) {
        text(-10, n_studies - i + 1, 
             paste0(escalc_result$Intervention_N[i], "/", escalc_result$Control_N[i]), 
             adj = 0, cex = 0.7)
      }
    }
    
    # Add heterogeneity statistics
    if (model_type == "Random") {
      mtext(paste0("I² = ", round(model$I2, 1), "%, τ² = ", round(model$tau2, 3),
                   ", Q = ", round(model$QE, 2), ", p = ", 
                   ifelse(model$QEp < 0.001, "<0.001", sprintf("%.3f", model$QEp))), 
            side = 1, line = 4, cex = 0.8)
    }
    
  }, error = function(e) {
    # Fallback to even simpler plot
    plot(0, 0, type = "n", xlim = c(0, 1), ylim = c(0, 1), 
         xlab = "", ylab = "", axes = FALSE)
    text(0.5, 0.5, paste("Forest plot for", outcome_name, "\n", 
                         model_type, "Effects\n",
                         "Effect =", round(model$beta[1], 3), "\n",
                         "p =", round(model$pval, 4)), 
         cex = 1.5, adj = 0.5)
  })
  
  dev.off()
  
  cat(paste(model_type, "effects forest plot saved\n"))
}

# Enhanced function to create funnel plot with rich Egger's test information
create_enhanced_funnel_plot_with_egger <- function(escalc_result, outcome_name, effect_type) {
  cat("Creating enhanced funnel plot with comprehensive Egger's test...\n")
  
  outcome_dir <- create_outcome_directory(outcome_name)
  
  # Perform comprehensive Egger's test
  egger_test <- regtest(escalc_result$yi, escalc_result$vi, model = "lm")
  
  # Calculate additional statistics
  se_values <- sqrt(escalc_result$vi)
  effect_sizes <- escalc_result$yi
  
  # Fit regression line for visualization
  reg_data <- data.frame(
    se = se_values,
    effect = effect_sizes
  )
  
  # Calculate prediction intervals for the regression
  se_range <- seq(min(se_values), max(se_values), length.out = 100)
  
  # Create the enhanced funnel plot
  filename <- file.path(outcome_dir, "enhanced_funnel_plot_with_egger.png")
  
  if (cairo_available) {
    Cairo::CairoPNG(filename, width = 14, height = 12, units = "in", res = 300)
  } else {
    png(filename, width = 14 * 300, height = 12 * 300, res = 300)
  }
  
  # Set up layout for multiple panels
  par(mfrow = c(2, 2), mar = c(4, 4, 3, 2))
  
  # Panel 1: Standard funnel plot with Egger's line
  funnel(escalc_result$yi, escalc_result$vi,
         xlab = paste(effect_type, "Effect Size"),
         ylab = "Standard Error",
         main = paste("Funnel Plot with Egger's Test\n", outcome_name),
         back = "white",
         shade = "white",
         hlines = "white",
         col = modern_colors["primary"],
         pch = 19,
         cex = 1.2)
  
  # Add Egger's regression line
  if (!is.na(egger_test$pval)) {
    # Calculate line parameters
    intercept <- egger_test$fit$coefficients[1]
    slope <- egger_test$fit$coefficients[2]
    
    # Draw regression line using CORRECT transformation
    se_pred <- seq(min(se_values), max(se_values), length.out = 50)
    effect_pred <- intercept * se_pred + slope
    lines(effect_pred, se_pred, col = modern_colors["egger_line"], lwd = 2, lty = 2)
    
    # Add legend
    legend("topright", 
           legend = c("Studies", "Egger's Line"),
           col = c(modern_colors["primary"], modern_colors["egger_line"]),
           pch = c(19, NA),
           lty = c(NA, 2),
           lwd = c(NA, 2),
           cex = 0.8)
  }
  
  # Add comprehensive statistics
  stats_text <- paste0(
    "Egger's Test Results:\n",
    "Intercept: ", round(egger_test$fit$coefficients[1], 3), "\n",
    "SE: ", round(sqrt(diag(vcov(egger_test$fit)))[1], 3), "\n",
    "t-value: ", round(egger_test$fit$coefficients[1] / sqrt(diag(vcov(egger_test$fit)))[1], 3), "\n",
    "p-value: ", ifelse(egger_test$pval < 0.001, "< 0.001", sprintf("%.3f", egger_test$pval)), "\n",
    "Interpretation: ", ifelse(egger_test$pval < 0.05, "Asymmetry detected", "No asymmetry")
  )
  
  mtext(stats_text, side = 1, line = -8, adj = 0, cex = 0.7, col = "darkblue")
  
  # Panel 2: Residuals vs fitted plot
  if (!is.na(egger_test$pval)) {
    plot(fitted(egger_test$fit), residuals(egger_test$fit),
         xlab = "Fitted Values", ylab = "Residuals",
         main = "Residuals vs Fitted",
         pch = 19, col = modern_colors["secondary"])
    abline(h = 0, lty = 2)
  }
  
  # Panel 3: Normal Q-Q plot of residuals
  if (!is.na(egger_test$pval)) {
    qqnorm(residuals(egger_test$fit), 
           main = "Q-Q Plot of Residuals",
           pch = 19, col = modern_colors["accent1"])
    qqline(residuals(egger_test$fit), col = modern_colors["egger_line"], lwd = 2)
  }
  
  # Panel 4: Effect sizes vs precision (1/SE)
  precision <- 1 / se_values
  plot(effect_sizes, precision,
       xlab = paste(effect_type, "Effect Size"),
       ylab = "Precision (1/SE)",
       main = "Precision Plot",
       pch = 19, col = modern_colors["success"],
       cex = 1.2)
  abline(v = 0, lty = 2)
  
  dev.off()
  
  # Create a second detailed Egger's plot
  filename2 <- file.path(outcome_dir, "egger_detailed_analysis.png")
  
  if (cairo_available) {
    Cairo::CairoPNG(filename2, width = 16, height = 10, units = "in", res = 300)
  } else {
    png(filename2, width = 16 * 300, height = 10 * 300, res = 300)
  }
  
  par(mfrow = c(1, 2), mar = c(5, 5, 4, 2))
  
  # Detailed Egger's regression plot
  plot(se_values, effect_sizes,
       xlab = "Standard Error",
       ylab = paste(effect_type, "Effect Size"),
       main = paste("Egger's Regression Analysis\n", outcome_name),
       pch = 19,
       col = modern_colors["primary"],
       cex = 1.5)
  
  # Add regression line and confidence intervals
  if (!is.na(egger_test$pval)) {
    # Regression line
    abline(egger_test$fit, col = modern_colors["egger_line"], lwd = 3)
    
    # Confidence intervals
    pred_data <- data.frame(se = se_range)
    names(pred_data)[1] <- names(egger_test$fit$model)[2]  # Match predictor name
    
    tryCatch({
      pred_intervals <- predict(egger_test$fit, pred_data, interval = "confidence")
      lines(se_range, pred_intervals[,"lwr"], col = modern_colors["warning"], lty = 2, lwd = 2)
      lines(se_range, pred_intervals[,"upr"], col = modern_colors["warning"], lty = 2, lwd = 2)
    }, error = function(e) {
      cat("Could not add confidence intervals:", e$message, "\n")
    })
    
    # Add text with regression equation
    equation_text <- paste0(
      "Regression Equation:\n",
      "Effect = ", round(egger_test$fit$coefficients[1], 3), 
      " + ", round(egger_test$fit$coefficients[2], 3), " × SE\n\n",
      "R² = ", round(summary(egger_test$fit)$r.squared, 3), "\n",
      "Adjusted R² = ", round(summary(egger_test$fit)$adj.r.squared, 3), "\n\n",
      "Publication Bias Assessment:\n",
      ifelse(egger_test$pval < 0.001, "Strong evidence of asymmetry (p < 0.001)",
             ifelse(egger_test$pval < 0.01, "Moderate evidence of asymmetry (p < 0.01)",
                    ifelse(egger_test$pval < 0.05, "Some evidence of asymmetry (p < 0.05)",
                           "No evidence of asymmetry (p ≥ 0.05)")))
    )
    
    legend("topleft", legend = equation_text, bty = "n", cex = 0.8)
  }
  
  # Publication bias interpretation plot
  bias_colors <- ifelse(egger_test$pval < 0.001, modern_colors["accent2"],
                        ifelse(egger_test$pval < 0.01, modern_colors["warning"],
                               ifelse(egger_test$pval < 0.05, modern_colors["accent1"],
                                      modern_colors["success"])))
  
  plot(0, 0, type = "n", xlim = c(0, 1), ylim = c(0, 1),
       xlab = "", ylab = "", axes = FALSE,
       main = "Publication Bias Interpretation")
  
  # Add interpretation text
  interpretation_text <- paste0(
    "EGGER'S TEST INTERPRETATION\n\n",
    "Test Statistic: ", round(egger_test$fit$coefficients[1] / sqrt(diag(vcov(egger_test$fit)))[1], 3), "\n",
    "p-value: ", ifelse(egger_test$pval < 0.001, "< 0.001", sprintf("%.3f", egger_test$pval)), "\n\n",
    "What this means:\n",
    "• Egger's test examines funnel plot asymmetry\n",
    "• Significant p-value suggests publication bias\n",
    "• Non-significant p-value suggests no bias\n\n",
    "Conclusion:\n",
    ifelse(egger_test$pval < 0.05,
           "⚠️ CAUTION: Possible publication bias detected\nResults should be interpreted carefully",
           "✓ No evidence of publication bias\nResults appear robust"),
    "\n\nNote: Egger's test has limited power with <10 studies"
  )
  
  text(0.5, 0.5, interpretation_text, adj = 0.5, cex = 1.1, col = bias_colors)
  
  dev.off()
  
  cat("Enhanced funnel plots saved\n")
  
  # Return comprehensive results
  return(list(
    intercept = egger_test$fit$coefficients[1],
    se = sqrt(diag(vcov(egger_test$fit)))[1],
    slope = egger_test$fit$coefficients[2],
    t_stat = egger_test$fit$coefficients[1] / sqrt(diag(vcov(egger_test$fit)))[1],
    p_value = egger_test$pval,
    significant = egger_test$pval < 0.05,
    r_squared = summary(egger_test$fit)$r.squared,
    regression_equation = paste0("Effect = ", round(egger_test$fit$coefficients[1], 3), 
                                 " + ", round(egger_test$fit$coefficients[2], 3), " × SE"),
    interpretation = ifelse(egger_test$pval < 0.001, "Strong evidence of asymmetry",
                            ifelse(egger_test$pval < 0.01, "Moderate evidence of asymmetry",
                                   ifelse(egger_test$pval < 0.05, "Some evidence of asymmetry",
                                          "No evidence of asymmetry")))
  ))
}

# FIXED FUNCTION: Create clean funnel plot with Egger test statistics (NO LINE)
create_simple_funnel_with_egger <- function(escalc_result, outcome_name, effect_type) {
  cat("Creating clean funnel plot with Egger test statistics (no line)...\n")
  
  outcome_dir <- create_outcome_directory(outcome_name)
  
  # Perform Egger's test
  egger_test <- regtest(escalc_result$yi, escalc_result$vi, model = "lm")
  
  # Calculate values for plotting
  se_values <- sqrt(escalc_result$vi)
  effect_sizes <- escalc_result$yi
  
  # Create the plot
  filename <- file.path(outcome_dir, "clean_funnel_plot_egger.png")
  
  if (cairo_available) {
    Cairo::CairoPNG(filename, width = 12, height = 10, units = "in", res = 300)
  } else {
    png(filename, width = 12 * 300, height = 10 * 300, res = 300)
  }
  
  # Set up plotting area
  par(mar = c(5, 5, 4, 2))
  
  # Create funnel plot
  # Invert y-axis for traditional funnel plot appearance
  max_se <- max(se_values) * 1.1
  plot_xlim <- range(effect_sizes) + c(-0.5, 0.5) * diff(range(effect_sizes))
  
  plot(effect_sizes, se_values,
       xlim = plot_xlim,
       ylim = c(max_se, 0),
       xlab = paste(effect_type, "Effect Size"),
       ylab = "Standard Error",
       main = paste("Funnel Plot with Egger's Test\n", outcome_name),
       pch = 19, cex = 1.5, col = modern_colors["primary"])
  
  # Add vertical line at zero effect
  abline(v = 0, lty = 2, lwd = 2, col = "gray60")
  
  # Calculate and add overall effect line
  overall_model <- rma(yi, vi, data = escalc_result)
  abline(v = overall_model$beta[1], lty = 2, lwd = 2, col = modern_colors["accent2"])
  
  # REMOVED: All Egger line drawing code - just keep the statistics
  
  # Add confidence funnel (pseudo-confidence interval)
  if (!is.na(overall_model$beta[1])) {
    se_funnel <- seq(0, max_se, length.out = 50)
    ci_lower <- overall_model$beta[1] - 1.96 * se_funnel
    ci_upper <- overall_model$beta[1] + 1.96 * se_funnel
    
    lines(ci_lower, se_funnel, lty = 3, lwd = 1, col = "gray70")
    lines(ci_upper, se_funnel, lty = 3, lwd = 1, col = "gray70")
  }
  
  # Updated legend without Egger line
  legend_items <- c("Studies", "Overall Effect", "Null Effect", "95% CI Funnel")
  legend_colors <- c(modern_colors["primary"], modern_colors["accent2"], "gray60", "gray70")
  legend_pch <- c(19, NA, NA, NA)
  legend_lty <- c(NA, 2, 2, 3)
  legend_lwd <- c(NA, 2, 2, 1)
  
  legend("topright", 
         legend = legend_items,
         col = legend_colors,
         pch = legend_pch,
         lty = legend_lty,
         lwd = legend_lwd,
         cex = 1, bg = "white")
  
  # Add Egger test results as text - ALWAYS show if test is available
  if (!is.na(egger_test$pval)) {
    egger_text <- paste0(
      "Egger's Test for Publication Bias:\n",
      "Intercept = ", sprintf("%.3f", egger_test$fit$coefficients[1]), "\n",
      "SE = ", sprintf("%.3f", sqrt(diag(vcov(egger_test$fit)))[1]), "\n",
      "t = ", sprintf("%.3f", egger_test$fit$coefficients[1] / sqrt(diag(vcov(egger_test$fit)))[1]), "\n",
      "p = ", ifelse(egger_test$pval < 0.001, "< 0.001", sprintf("%.3f", egger_test$pval)), "\n\n",
      "Interpretation:\n",
      ifelse(egger_test$pval < 0.05, 
             "⚠️ Asymmetry detected\n(possible publication bias)",
             "✓ No asymmetry detected\n(no evidence of bias)"), "\n\n",
      "Note: Egger line removed for clarity\nStatistics remain valid"
    )
    
    # Position text box
    text_x <- plot_xlim[1] + 0.02 * diff(plot_xlim)
    text_y <- max_se * 0.45
    
    # Add text with appropriate color
    text_color <- ifelse(egger_test$pval < 0.05, "darkred", "darkgreen")
    
    # Add text
    text(text_x, text_y, egger_text, 
         adj = c(0, 1), cex = 0.85, 
         col = text_color,
         family = "mono")
  } else {
    # Show message if Egger test couldn't be performed
    text_x <- plot_xlim[1] + 0.02 * diff(plot_xlim)
    text_y <- max_se * 0.35
    
    no_test_text <- "Egger's Test:\nNot available\n(insufficient data or error)"
    text(text_x, text_y, no_test_text, 
         adj = c(0, 1), cex = 0.85, 
         col = "gray50",
         family = "mono")
  }
  
  # Add sample size information if available
  if (all(c("Intervention_N", "Control_N") %in% names(escalc_result))) {
    total_n <- sum(escalc_result$Intervention_N + escalc_result$Control_N)
    n_studies <- nrow(escalc_result)
    
    subtitle_text <- paste0("Studies: ", n_studies, " | Total N: ", total_n)
    mtext(subtitle_text, side = 3, line = 0.5, cex = 0.9, col = "gray40")
  }
  
  dev.off()
  
  cat("Clean funnel plot with Egger test statistics (no line) saved\n")
  
  return(list(
    egger_results = egger_test,
    filename = filename,
    line_drawn = FALSE  # Always FALSE now since we removed the line
  ))
}

# ENHANCED FUNCTION: Comprehensive sensitivity analysis
perform_sensitivity_analysis <- function(escalc_result, outcome_name, effect_type) {
  cat("Performing comprehensive sensitivity analysis...\n")
  
  outcome_dir <- create_outcome_directory(outcome_name)
  
  # Fit the main model
  main_model <- rma(yi, vi, data = escalc_result)
  
  # Perform leave-one-out analysis
  loo_analysis <- leave1out(main_model)
  
  # Create comprehensive sensitivity plots
  filename <- file.path(outcome_dir, "comprehensive_sensitivity_analysis.png")
  
  if (cairo_available) {
    Cairo::CairoPNG(filename, width = 16, height = 12, units = "in", res = 300)
  } else {
    png(filename, width = 16 * 300, height = 12 * 300, res = 300)
  }
  
  # Set up layout for multiple panels
  par(mfrow = c(2, 2), mar = c(5, 5, 4, 2))
  
  # Panel 1: Leave-one-out forest plot
  tryCatch({
    # Extract data for manual plotting if automatic fails
    loo_effects <- loo_analysis$estimate
    loo_ci_lb <- loo_analysis$ci.lb
    loo_ci_ub <- loo_analysis$ci.ub
    study_names <- escalc_result$study_id
    
    # Create leave-one-out plot
    n_studies <- length(loo_effects)
    plot_range <- range(c(loo_ci_lb, loo_ci_ub, main_model$ci.lb, main_model$ci.ub))
    
    plot(loo_effects, 1:n_studies, 
         xlim = plot_range,
         ylim = c(0.5, n_studies + 1.5),
         xlab = paste("Effect Size (", effect_type, ")", sep = ""),
         ylab = "Study Omitted",
         main = paste("Leave-One-Out Analysis\n", outcome_name),
         pch = 19, cex = 1.2, col = modern_colors["primary"])
    
    # Add confidence intervals
    for (i in 1:n_studies) {
      lines(c(loo_ci_lb[i], loo_ci_ub[i]), c(i, i), lwd = 2, col = modern_colors["primary"])
    }
    
    # Add overall effect line
    abline(v = main_model$beta[1], lty = 2, lwd = 2, col = modern_colors["accent2"])
    abline(v = main_model$ci.lb, lty = 3, lwd = 1, col = modern_colors["accent2"])
    abline(v = main_model$ci.ub, lty = 3, lwd = 1, col = modern_colors["accent2"])
    
    # Add study labels
    axis(2, at = 1:n_studies, labels = paste("Omit:", substr(study_names, 1, 20)), 
         las = 2, cex.axis = 0.7)
    
    # Add vertical line at zero
    abline(v = 0, lty = 1, lwd = 1, col = "gray50")
    
    # Add legend
    legend("topright", 
           legend = c("Leave-one-out estimate", "Overall estimate", "95% CI"),
           col = c(modern_colors["primary"], modern_colors["accent2"], modern_colors["accent2"]),
           pch = c(19, NA, NA),
           lty = c(NA, 2, 3),
           lwd = c(NA, 2, 1),
           cex = 0.8)
    
  }, error = function(e) {
    plot(0, 0, type = "n", main = "Leave-One-Out Analysis", xlab = "", ylab = "")
    text(0, 0, paste("Error in leave-one-out plot:", e$message), cex = 1.2)
  })
  
  # Panel 2: Influence diagnostics
  tryCatch({
    # Calculate influence measures
    influence_vals <- abs(loo_analysis$estimate - main_model$beta[1])
    
    plot(1:length(influence_vals), influence_vals,
         xlab = "Study Number", ylab = "Influence (Change in Effect)",
         main = "Study Influence on Overall Effect",
         pch = 19, cex = 1.5, col = modern_colors["secondary"])
    
    # Add study labels for most influential
    max_influence_idx <- which.max(influence_vals)
    text(max_influence_idx, influence_vals[max_influence_idx], 
         labels = paste("Most Influential:\n", study_names[max_influence_idx]), 
         pos = 3, cex = 0.8, col = modern_colors["accent2"])
    
    # Add threshold line (arbitrary: 50% of overall effect)
    threshold <- 0.5 * abs(main_model$beta[1])
    abline(h = threshold, lty = 2, col = modern_colors["warning"])
    text(length(influence_vals) * 0.8, threshold, 
         "50% of overall effect", pos = 3, cex = 0.8)
    
  }, error = function(e) {
    plot(0, 0, type = "n", main = "Influence Diagnostics", xlab = "", ylab = "")
    text(0, 0, paste("Error in influence plot:", e$message), cex = 1.2)
  })
  
  # Panel 3: Cumulative meta-analysis
  tryCatch({
    # Order studies by year if available, otherwise by effect size
    if ("Year" %in% names(escalc_result)) {
      order_idx <- order(escalc_result$Year)
    } else {
      order_idx <- order(escalc_result$yi)
    }
    
    # Perform cumulative analysis
    cum_effects <- numeric(nrow(escalc_result))
    cum_ci_lb <- numeric(nrow(escalc_result))
    cum_ci_ub <- numeric(nrow(escalc_result))
    
    for (i in 1:nrow(escalc_result)) {
      subset_data <- escalc_result[order_idx[1:i], ]
      temp_model <- rma(yi, vi, data = subset_data)
      cum_effects[i] <- temp_model$beta[1]
      cum_ci_lb[i] <- temp_model$ci.lb
      cum_ci_ub[i] <- temp_model$ci.ub
    }
    
    plot_range_cum <- range(c(cum_ci_lb, cum_ci_ub))
    
    plot(1:length(cum_effects), cum_effects,
         ylim = plot_range_cum,
         xlab = "Number of Studies", ylab = paste("Cumulative Effect Size (", effect_type, ")", sep = ""),
         main = "Cumulative Meta-Analysis",
         pch = 19, cex = 1.2, col = modern_colors["success"], type = "b")
    
    # Add confidence bands
    polygon(c(1:length(cum_effects), rev(1:length(cum_effects))),
            c(cum_ci_lb, rev(cum_ci_ub)),
            col = paste0(modern_colors["success"], "30"), border = NA)
    
    # Add final overall effect line
    abline(h = main_model$beta[1], lty = 2, lwd = 2, col = modern_colors["accent2"])
    abline(h = 0, lty = 1, lwd = 1, col = "gray50")
    
  }, error = function(e) {
    plot(0, 0, type = "n", main = "Cumulative Meta-Analysis", xlab = "", ylab = "")
    text(0, 0, paste("Error in cumulative plot:", e$message), cex = 1.2)
  })
  
  # Panel 4: Outlier detection
  tryCatch({
    # Calculate standardized residuals
    residuals_std <- rstandard(main_model)$resid
    
    plot(1:length(residuals_std), residuals_std,
         xlab = "Study Number", ylab = "Standardized Residuals",
         main = "Outlier Detection",
         pch = 19, cex = 1.5, col = modern_colors["info"])
    
    # Add threshold lines for outliers (±2 and ±3)
    abline(h = c(-3, -2, 0, 2, 3), lty = c(2, 3, 1, 3, 2), 
           col = c("red", "orange", "gray50", "orange", "red"))
    
    # Label potential outliers
    outliers_idx <- which(abs(residuals_std) > 2)
    if (length(outliers_idx) > 0) {
      text(outliers_idx, residuals_std[outliers_idx], 
           labels = substr(study_names[outliers_idx], 1, 15), 
           pos = 3, cex = 0.7, col = "red")
    }
    
    # Add legend
    legend("topright", 
           legend = c("±2 SD", "±3 SD"),
           lty = c(3, 2),
           col = c("orange", "red"),
           cex = 0.8)
    
  }, error = function(e) {
    plot(0, 0, type = "n", main = "Outlier Detection", xlab = "", ylab = "")
    text(0, 0, paste("Error in outlier plot:", e$message), cex = 1.2)
  })
  
  dev.off()
  
  # Create a separate detailed leave-one-out plot
  filename2 <- file.path(outcome_dir, "detailed_leave_one_out.png")
  
  if (cairo_available) {
    Cairo::CairoPNG(filename2, width = 12, height = 8, units = "in", res = 300)
  } else {
    png(filename2, width = 12 * 300, height = 8 * 300, res = 300)
  }
  
  # Create detailed leave-one-out plot using metafor's built-in function
  tryCatch({
    forest(loo_analysis, 
           slab = paste("Omitting:", escalc_result$study_id),
           header = TRUE,
           main = paste("Leave-One-Out Sensitivity Analysis:", outcome_name),
           xlab = paste("Effect Size (", effect_type, ")", sep = ""),
           col = modern_colors["primary"],
           border = modern_colors["primary"])
  }, error = function(e) {
    # Fallback manual plot
    par(mar = c(5, 8, 4, 2))
    loo_effects <- loo_analysis$estimate
    loo_ci_lb <- loo_analysis$ci.lb
    loo_ci_ub <- loo_analysis$ci.ub
    study_names <- escalc_result$study_id
    
    plot(loo_effects, 1:length(loo_effects), 
         xlim = range(c(loo_ci_lb, loo_ci_ub)),
         xlab = paste("Effect Size (", effect_type, ")", sep = ""),
         ylab = "",
         main = paste("Leave-One-Out Analysis:", outcome_name),
         pch = 19, cex = 1.2, col = modern_colors["primary"],
         yaxt = "n")
    
    # Add confidence intervals
    for (i in 1:length(loo_effects)) {
      lines(c(loo_ci_lb[i], loo_ci_ub[i]), c(i, i), lwd = 2, col = modern_colors["primary"])
    }
    
    # Add study labels
    axis(2, at = 1:length(loo_effects), 
         labels = paste("Omit:", substr(study_names, 1, 25)), 
         las = 2, cex.axis = 0.8)
    
    # Add overall effect
    abline(v = main_model$beta[1], lty = 2, lwd = 2, col = modern_colors["accent2"])
    abline(v = 0, lty = 1, col = "gray50")
  })
  
  dev.off()
  
  cat("Comprehensive sensitivity analysis plots saved\n")
  
  # Calculate influence measures and return results
  influence_vals <- abs(loo_analysis$estimate - main_model$beta[1])
  max_influence_idx <- which.max(influence_vals)
  
  # Identify outliers
  residuals_std <- rstandard(main_model)$resid
  outliers_idx <- which(abs(residuals_std) > 2)
  
  return(list(
    loo_results = loo_analysis,
    max_influence_study = escalc_result$study_id[max_influence_idx],
    max_influence_value = influence_vals[max_influence_idx],
    robust = max(influence_vals) < 0.5 * abs(main_model$beta[1]),
    outliers = if(length(outliers_idx) > 0) escalc_result$study_id[outliers_idx] else "None",
    n_outliers = length(outliers_idx),
    influence_values = influence_vals,
    standardized_residuals = residuals_std
  ))
}

# Enhanced function to export comprehensive results to CSV
export_comprehensive_results_to_csv <- function(escalc_result, fixed_model, random_model, outcome_name, 
                                                effect_type, egger_results, sensitivity_results) {
  cat("Exporting comprehensive results to CSV...\n")
  
  outcome_dir <- create_outcome_directory(outcome_name)
  
  # 1. Individual study results with comprehensive information
  study_results <- data.frame(
    Study_ID = escalc_result$study_id,
    
    # Effect size information
    Effect_Size = escalc_result$yi,
    Standard_Error = sqrt(escalc_result$vi),
    Variance = escalc_result$vi,
    Lower_CI_95 = escalc_result$yi - 1.96 * sqrt(escalc_result$vi),
    Upper_CI_95 = escalc_result$yi + 1.96 * sqrt(escalc_result$vi),
    
    # Z-scores and p-values for individual studies
    Z_Score = escalc_result$yi / sqrt(escalc_result$vi),
    P_Value_Individual = 2 * (1 - pnorm(abs(escalc_result$yi / sqrt(escalc_result$vi)))),
    
    # Weights
    Weight_Fixed_Percent = round(weights(fixed_model), 2),
    Weight_Random_Percent = round(weights(random_model), 2),
    
    # Precision measures
    Precision_1_over_SE = 1 / sqrt(escalc_result$vi),
    Sample_Size_Total = escalc_result$Intervention_N + escalc_result$Control_N
  )
  
  # Add all original data columns if they exist
  original_cols <- c("Author", "Year", "Drug", "Dosage", "Control",
                     "Intervention_Mean", "Intervention_SD", "Intervention_N",
                     "Control_Mean", "Control_SD", "Control_N",
                     "Country", "Study_Duration_weeks", "Disease_Stage", "Outcome_Unit")
  
  for (col in original_cols) {
    if (col %in% names(escalc_result)) {
      study_results[[col]] <- escalc_result[[col]]
    }
  }
  
  # Add sensitivity analysis results if available
  if (!is.null(sensitivity_results)) {
    if ("influence_values" %in% names(sensitivity_results)) {
      study_results$Influence_Value <- sensitivity_results$influence_values
    }
    if ("standardized_residuals" %in% names(sensitivity_results)) {
      study_results$Standardized_Residual <- sensitivity_results$standardized_residuals
      study_results$Potential_Outlier <- abs(sensitivity_results$standardized_residuals) > 2
    }
  }
  
  # Save individual study results
  write.csv(study_results, file.path(outcome_dir, "comprehensive_study_results.csv"), row.names = FALSE)
  
  # 2. Comprehensive overall results comparing fixed and random effects
  overall_results <- data.frame(
    Outcome = outcome_name,
    Effect_Size_Type = effect_type,
    Number_of_Studies = nrow(escalc_result),
    Total_Sample_Size = sum(escalc_result$Intervention_N + escalc_result$Control_N),
    
    # Fixed Effects Model Results
    Fixed_Effect_Size = fixed_model$beta[1],
    Fixed_SE = fixed_model$se,
    Fixed_CI_Lower_95 = fixed_model$ci.lb,
    Fixed_CI_Upper_95 = fixed_model$ci.ub,
    Fixed_Z_Value = fixed_model$zval,
    Fixed_P_Value = fixed_model$pval,
    Fixed_Significant = fixed_model$pval < 0.05,
    
    # Random Effects Model Results
    Random_Effect_Size = random_model$beta[1],
    Random_SE = random_model$se,
    Random_CI_Lower_95 = random_model$ci.lb,
    Random_CI_Upper_95 = random_model$ci.ub,
    Random_Z_Value = random_model$zval,
    Random_P_Value = random_model$pval,
    Random_Significant = random_model$pval < 0.05,
    
    # Heterogeneity Statistics
    I_Squared_Percent = random_model$I2,
    Tau_Squared = random_model$tau2,
    Tau = sqrt(random_model$tau2),
    Q_Statistic = random_model$QE,
    Q_DF = random_model$k - 1,
    Q_P_Value = random_model$QEp,
    Heterogeneity_Significant = random_model$QEp < 0.05,
    
    # Heterogeneity Interpretation
    Heterogeneity_Level = case_when(
      random_model$I2 < 25 ~ "Low",
      random_model$I2 < 50 ~ "Moderate",
      random_model$I2 < 75 ~ "Substantial",
      TRUE ~ "Considerable"
    ),
    
    # Model Comparison
    Effect_Size_Difference = abs(fixed_model$beta[1] - random_model$beta[1]),
    Prefer_Random_Effects = random_model$I2 > 25,
    
    # Publication Bias (Egger's Test)
    Egger_Intercept = ifelse(is.null(egger_results), NA, egger_results$intercept),
    Egger_SE = ifelse(is.null(egger_results), NA, egger_results$se),
    Egger_T_Statistic = ifelse(is.null(egger_results), NA, egger_results$t_stat),
    Egger_P_Value = ifelse(is.null(egger_results), NA, egger_results$p_value),
    Egger_Significant = ifelse(is.null(egger_results), NA, egger_results$significant),
    Publication_Bias_Evidence = ifelse(is.null(egger_results), NA, egger_results$interpretation),
    
    # Sensitivity Analysis
    Most_Influential_Study = ifelse(is.null(sensitivity_results), NA, sensitivity_results$max_influence_study),
    Max_Influence_Value = ifelse(is.null(sensitivity_results), NA, sensitivity_results$max_influence_value),
    Results_Robust = ifelse(is.null(sensitivity_results), NA, sensitivity_results$robust),
    Number_of_Outliers = ifelse(is.null(sensitivity_results), NA, sensitivity_results$n_outliers),
    
    # Quality Assessment
    Adequate_Sample_Size = nrow(escalc_result) >= 5,
    Low_Risk_of_Bias = !is.null(egger_results) && !egger_results$significant,
    
    # Recommendation
    Primary_Model_Recommendation = ifelse(random_model$I2 > 25, "Random Effects", "Fixed Effects"),
    
    # Effect Size Interpretation (for SMD)
    Effect_Size_Magnitude = if(effect_type == "SMD") {
      case_when(
        abs(random_model$beta[1]) < 0.2 ~ "Negligible",
        abs(random_model$beta[1]) < 0.5 ~ "Small",
        abs(random_model$beta[1]) < 0.8 ~ "Medium",
        TRUE ~ "Large"
      )
    } else {
      "N/A (Mean Difference)"
    }
  )
  
  write.csv(overall_results, file.path(outcome_dir, "comprehensive_overall_results.csv"), row.names = FALSE)
  
  # 3. Model comparison table
  model_comparison <- data.frame(
    Model = c("Fixed Effects", "Random Effects"),
    Effect_Size = c(fixed_model$beta[1], random_model$beta[1]),
    Standard_Error = c(fixed_model$se, random_model$se),
    CI_Lower_95 = c(fixed_model$ci.lb, random_model$ci.lb),
    CI_Upper_95 = c(fixed_model$ci.ub, random_model$ci.ub),
    Z_Value = c(fixed_model$zval, random_model$zval),
    P_Value = c(fixed_model$pval, random_model$pval),
    AIC = c(fixed_model$fit.stats$AIC, random_model$fit.stats$AIC),
    BIC = c(fixed_model$fit.stats$BIC, random_model$fit.stats$BIC),
    Preferred = c(
      random_model$I2 <= 25,  # Prefer fixed if low heterogeneity
      random_model$I2 > 25    # Prefer random if high heterogeneity
    )
  )
  
  write.csv(model_comparison, file.path(outcome_dir, "model_comparison.csv"), row.names = FALSE)
  
  # 4. Publication bias assessment
  if (!is.null(egger_results)) {
    bias_assessment <- data.frame(
      Test = "Egger's Test",
      Statistic = egger_results$t_stat,
      P_Value = egger_results$p_value,
      Interpretation = egger_results$interpretation,
      Recommendation = ifelse(egger_results$significant,
                              "Caution: Possible publication bias. Consider additional bias assessment methods.",
                              "No evidence of publication bias detected."),
      Regression_Equation = egger_results$regression_equation,
      R_Squared = egger_results$r_squared
    )
    
    write.csv(bias_assessment, file.path(outcome_dir, "publication_bias_assessment.csv"), row.names = FALSE)
  }
  
  # 5. Sensitivity analysis summary if available
  if (!is.null(sensitivity_results)) {
    sensitivity_summary <- data.frame(
      Analysis_Type = c("Leave-One-Out", "Outlier Detection", "Influence Analysis"),
      Most_Influential_Study = c(sensitivity_results$max_influence_study, 
                                 if(sensitivity_results$n_outliers > 0) paste(sensitivity_results$outliers, collapse = ", ") else "None",
                                 sensitivity_results$max_influence_study),
      Impact_Value = c(sensitivity_results$max_influence_value, 
                       sensitivity_results$n_outliers,
                       sensitivity_results$max_influence_value),
      Results_Robust = c(sensitivity_results$robust, 
                         sensitivity_results$n_outliers == 0,
                         sensitivity_results$robust),
      Notes = c("Maximum change in effect size when removing one study",
                "Studies with standardized residuals > 2",
                "Overall assessment of result stability")
    )
    
    write.csv(sensitivity_summary, file.path(outcome_dir, "sensitivity_analysis_summary.csv"), row.names = FALSE)
  }
  
  cat("Comprehensive CSV files saved\n")
  
  return(list(
    study_results = study_results,
    overall_results = overall_results,
    model_comparison = model_comparison,
    bias_assessment = if(!is.null(egger_results)) bias_assessment else NULL,
    sensitivity_summary = if(!is.null(sensitivity_results)) sensitivity_summary else NULL
  ))
}

# Function to perform meta-analysis for one outcome
perform_meta_analysis <- function(outcome_data, outcome_name, all_data) {
  cat(paste("\n=== ANALYZING OUTCOME:", outcome_name, "===\n"))
  
  # Remove rows with missing essential data
  clean_data <- outcome_data %>%
    filter(
      !is.na(Intervention_Mean) & !is.na(Intervention_SD) & !is.na(Intervention_N) &
        !is.na(Control_Mean) & !is.na(Control_SD) & !is.na(Control_N)
    )
  
  cat("Total studies for this outcome:", nrow(outcome_data), "\n")
  cat("Studies with complete data:", nrow(clean_data), "\n")
  
  if (nrow(clean_data) < 2) {
    cat(paste("SKIPPING", outcome_name, "- insufficient data (n =", nrow(clean_data), ")\n"))
    return(NULL)
  }
  
  # Show first few rows of data
  cat("\nFirst 3 studies:\n")
  print(clean_data[1:min(3, nrow(clean_data)), c("study_id", "Intervention_Mean", "Control_Mean")])
  
  # Determine effect size type
  effect_type <- determine_effect_size(clean_data)
  cat("Effect size type:", effect_type, "\n")
  
  # Calculate effect sizes using metafor
  tryCatch({
    if (effect_type == "SMD") {
      escalc_result <- escalc(
        measure = "SMD",
        m1i = clean_data$Intervention_Mean,
        sd1i = clean_data$Intervention_SD,
        n1i = clean_data$Intervention_N,
        m2i = clean_data$Control_Mean,
        sd2i = clean_data$Control_SD,
        n2i = clean_data$Control_N,
        data = clean_data
      )
      effect_label <- "Std. Mean Difference"
    } else {
      escalc_result <- escalc(
        measure = "MD",
        m1i = clean_data$Intervention_Mean,
        sd1i = clean_data$Intervention_SD,
        n1i = clean_data$Intervention_N,
        m2i = clean_data$Control_Mean,
        sd2i = clean_data$Control_SD,
        n2i = clean_data$Control_N,
        data = clean_data
      )
      effect_label <- "Mean Difference"
    }
    
    cat("Effect sizes calculated successfully\n")
    
    # Perform meta-analysis with both fixed and random effects
    fixed_model <- rma(yi, vi, data = escalc_result, method = "FE")
    random_model <- rma(yi, vi, data = escalc_result, method = "REML")
    
    cat("\nMeta-analysis results:\n")
    cat("Fixed effect:", round(fixed_model$beta[1], 3), 
        "p =", round(fixed_model$pval, 4), "\n")
    cat("Random effect:", round(random_model$beta[1], 3), 
        "p =", round(random_model$pval, 4), "\n")
    cat("I-squared:", round(random_model$I2, 1), "%\n")
    
    # Create enhanced forest plot
    tryCatch({
      create_forest_plot(escalc_result, fixed_model, random_model, outcome_name, effect_label, effect_type)
    }, error = function(e) {
      cat("WARNING: Could not create forest plot:", e$message, "\n")
    })
    
    # Create enhanced funnel plot with rich Egger's test information
    egger_results <- tryCatch({
      create_enhanced_funnel_plot_with_egger(escalc_result, outcome_name, effect_type)
    }, error = function(e) {
      cat("WARNING: Could not create enhanced funnel plot:", e$message, "\n")
      list(intercept = NA, se = NA, t_stat = NA, p_value = NA, significant = NA,
           regression_equation = NA, interpretation = NA)
    })
    
    # Create separate clean funnel plot with Egger test (FIXED VERSION - NO LINE)
    clean_funnel_results <- tryCatch({
      result <- create_simple_funnel_with_egger(escalc_result, outcome_name, effect_type)
      cat("Clean funnel plot created without Egger line (statistics only)\n")
      result
    }, error = function(e) {
      cat("WARNING: Could not create clean funnel plot:", e$message, "\n")
      NULL
    })
    
    # Perform comprehensive sensitivity analysis
    sensitivity_results <- tryCatch({
      perform_sensitivity_analysis(escalc_result, outcome_name, effect_type)
    }, error = function(e) {
      cat("WARNING: Could not perform sensitivity analysis:", e$message, "\n")
      list(max_influence_study = NA, max_influence_value = NA, robust = NA, 
           outliers = "None", n_outliers = 0)
    })
    
    # Export comprehensive results to CSV files
    csv_results <- tryCatch({
      export_comprehensive_results_to_csv(escalc_result, fixed_model, random_model, outcome_name, 
                                          effect_type, egger_results, sensitivity_results)
    }, error = function(e) {
      cat("WARNING: Could not export CSV results:", e$message, "\n")
      NULL
    })
    
    # Return results summary
    return(list(
      outcome = outcome_name,
      effect_type = effect_type,
      n_studies = nrow(clean_data),
      fixed_model = fixed_model,
      random_model = random_model,
      escalc_result = escalc_result,
      csv_results = csv_results,
      egger_results = egger_results,
      clean_funnel_results = clean_funnel_results,
      sensitivity_results = sensitivity_results,
      clean_data = clean_data
    ))
    
  }, error = function(e) {
    cat("ERROR in meta-analysis:", e$message, "\n")
    return(NULL)
  })
}

# Function to perform meta-regression
perform_meta_regression <- function(results_list) {
  cat("\n=== PERFORMING META-REGRESSION ===\n")
  
  # Combine all data
  all_data <- data.frame()
  
  for (result in results_list) {
    if (!is.null(result) && !is.null(result$escalc_result)) {
      temp_data <- result$escalc_result
      temp_data$outcome <- result$outcome
      if (nrow(all_data) == 0) {
        all_data <- temp_data
      } else {
        # Match columns
        common_cols <- intersect(names(all_data), names(temp_data))
        all_data <- rbind(all_data[, common_cols], temp_data[, common_cols])
      }
    }
  }
  
  if (nrow(all_data) == 0) {
    cat("No data available for meta-regression\n")
    return(NULL)
  }
  
  cat("Combined data for meta-regression:", nrow(all_data), "studies\n")
  
  # Prepare variables
  all_data$total_n <- all_data$Intervention_N + all_data$Control_N
  
  # Try meta-regression by year if available
  if ("Year" %in% names(all_data) && length(unique(all_data$Year)) > 1) {
    cat("\nTrying meta-regression by year...\n")
    tryCatch({
      year_model <- rma(yi ~ Year, vi, data = all_data)
      cat("Year coefficient:", round(year_model$beta[2], 4), 
          "p =", round(year_model$pval[2], 4), "\n")
    }, error = function(e) {
      cat("Could not perform year regression:", e$message, "\n")
    })
  }
  
  # Create meta-regression plots
  filename <- file.path(results_dir, "meta_regression_comprehensive.png")
  
  if (cairo_available) {
    Cairo::CairoPNG(filename, width = 16, height = 12, units = "in", res = 300)
  } else {
    png(filename, width = 16 * 300, height = 12 * 300, res = 300)
  }
  
  par(mfrow = c(2, 3), mar = c(4, 4, 3, 2))
  
  # Plot 1: Effect sizes by outcome
  if (length(unique(all_data$outcome)) > 1) {
    boxplot(yi ~ outcome, data = all_data,
            main = "Effect Sizes by Outcome",
            ylab = "Effect Size",
            col = modern_colors["accent1"],
            las = 2)
    abline(h = 0, lty = 2)
  }
  
  # Plot 2: Effect sizes by year
  if ("Year" %in% names(all_data)) {
    plot(all_data$Year, all_data$yi,
         main = "Effect Sizes by Year",
         xlab = "Year", ylab = "Effect Size",
         pch = 19, col = modern_colors["primary"],
         cex = 1.2)
    abline(h = 0, lty = 2)
    if (exists("year_model")) {
      abline(year_model$beta[1], year_model$beta[2], col = "red", lwd = 2)
    }
  }
  
  # Plot 3: Effect sizes by sample size
  plot(all_data$total_n, all_data$yi,
       main = "Effect Sizes by Sample Size",
       xlab = "Total Sample Size", ylab = "Effect Size",
       pch = 19, col = modern_colors["secondary"],
       cex = 1.2)
  abline(h = 0, lty = 2)
  
  # Plot 4: Effect sizes by country (if available)
  if ("Country" %in% names(all_data) && length(unique(all_data$Country)) > 1) {
    boxplot(yi ~ Country, data = all_data,
            main = "Effect Sizes by Country",
            ylab = "Effect Size",
            col = modern_colors["success"],
            las = 2)
    abline(h = 0, lty = 2)
  }
  
  # Plot 5: Precision vs effect size
  precision <- 1 / sqrt(all_data$vi)
  plot(all_data$yi, precision,
       main = "Precision vs Effect Size",
       xlab = "Effect Size", ylab = "Precision (1/SE)",
       pch = 19, col = modern_colors["info"],
       cex = 1.2)
  abline(v = 0, lty = 2)
  
  # Plot 6: Study duration vs effect size (if available)
  if ("Study_Duration_weeks" %in% names(all_data)) {
    plot(all_data$Study_Duration_weeks, all_data$yi,
         main = "Effect Size by Study Duration",
         xlab = "Study Duration (weeks)", ylab = "Effect Size",
         pch = 19, col = modern_colors["warning"],
         cex = 1.2)
    abline(h = 0, lty = 2)
  }
  
  dev.off()
  
  cat("Comprehensive meta-regression plots saved\n")
  
  return(all_data)
}

# Enhanced function to create summary report
create_summary_report <- function(results_list) {
  cat("\n=== CREATING COMPREHENSIVE SUMMARY REPORT ===\n")
  
  summary_file <- file.path(results_dir, "comprehensive_meta_analysis_report.txt")
  
  sink(summary_file)
  cat("COMPREHENSIVE META-ANALYSIS SUMMARY REPORT\n")
  cat("Generated on:", as.character(Sys.time()), "\n")
  cat("Script version: FIXED - Clean funnel plot without Egger line\n")
  cat(paste(rep("=", 80), collapse = ""), "\n\n")
  
  successful_analyses <- 0
  
  for (result in results_list) {
    if (!is.null(result)) {
      successful_analyses <- successful_analyses + 1
      
      cat("OUTCOME:", result$outcome, "\n")
      cat(paste(rep("-", 60), collapse = ""), "\n")
      cat("Effect Size Type:", result$effect_type, "\n")
      cat("Number of Studies:", result$n_studies, "\n")
      cat("Total Sample Size:", sum(result$escalc_result$Intervention_N + result$escalc_result$Control_N), "\n\n")
      
      cat("FIXED EFFECTS MODEL:\n")
      cat("Effect Size:", sprintf("%.3f", result$fixed_model$beta[1]), "\n")
      cat("Standard Error:", sprintf("%.3f", result$fixed_model$se), "\n")
      cat("95% CI: [", sprintf("%.3f", result$fixed_model$ci.lb), 
          ", ", sprintf("%.3f", result$fixed_model$ci.ub), "]\n")
      cat("Z-value:", sprintf("%.3f", result$fixed_model$zval), "\n")
      cat("p-value:", ifelse(result$fixed_model$pval < 0.001, "<0.001", 
                             sprintf("%.3f", result$fixed_model$pval)), "\n")
      cat("Significant:", ifelse(result$fixed_model$pval < 0.05, "YES", "NO"), "\n\n")
      
      cat("RANDOM EFFECTS MODEL:\n")
      cat("Effect Size:", sprintf("%.3f", result$random_model$beta[1]), "\n")
      cat("Standard Error:", sprintf("%.3f", result$random_model$se), "\n")
      cat("95% CI: [", sprintf("%.3f", result$random_model$ci.lb), 
          ", ", sprintf("%.3f", result$random_model$ci.ub), "]\n")
      cat("Z-value:", sprintf("%.3f", result$random_model$zval), "\n")
      cat("p-value:", ifelse(result$random_model$pval < 0.001, "<0.001", 
                             sprintf("%.3f", result$random_model$pval)), "\n")
      cat("Significant:", ifelse(result$random_model$pval < 0.05, "YES", "NO"), "\n\n")
      
      cat("HETEROGENEITY ASSESSMENT:\n")
      cat("I-squared:", sprintf("%.1f%%", result$random_model$I2), "\n")
      cat("Tau-squared:", sprintf("%.3f", result$random_model$tau2), "\n")
      cat("Tau:", sprintf("%.3f", sqrt(result$random_model$tau2)), "\n")
      cat("Q-statistic:", sprintf("%.2f", result$random_model$QE), "\n")
      cat("Q p-value:", ifelse(result$random_model$QEp < 0.001, "<0.001", 
                               sprintf("%.3f", result$random_model$QEp)), "\n")
      
      heterogeneity_level <- case_when(
        result$random_model$I2 < 25 ~ "Low",
        result$random_model$I2 < 50 ~ "Moderate", 
        result$random_model$I2 < 75 ~ "Substantial",
        TRUE ~ "Considerable"
      )
      cat("Heterogeneity Level:", heterogeneity_level, "\n\n")
      
      if (!is.null(result$egger_results)) {
        cat("PUBLICATION BIAS ASSESSMENT (EGGER'S TEST):\n")
        cat("Intercept:", sprintf("%.3f", result$egger_results$intercept), "\n")
        cat("Standard Error:", sprintf("%.3f", result$egger_results$se), "\n")
        cat("t-statistic:", sprintf("%.3f", result$egger_results$t_stat), "\n")
        cat("p-value:", ifelse(is.na(result$egger_results$p_value), "NA",
                               ifelse(result$egger_results$p_value < 0.001, "<0.001", 
                                      sprintf("%.3f", result$egger_results$p_value))), "\n")
        cat("Interpretation:", result$egger_results$interpretation, "\n")
        cat("Regression Equation:", result$egger_results$regression_equation, "\n")
        cat("Clean Funnel Plot: Line removed for clarity, statistics remain valid\n")
        cat("\n")
      }
      
      if (!is.null(result$sensitivity_results)) {
        cat("SENSITIVITY ANALYSIS:\n")
        cat("Most Influential Study:", result$sensitivity_results$max_influence_study, "\n")
        cat("Maximum Influence Value:", sprintf("%.3f", result$sensitivity_results$max_influence_value), "\n")
        cat("Results Robust:", ifelse(result$sensitivity_results$robust, "YES", "NO"), "\n")
        cat("Number of Outliers:", result$sensitivity_results$n_outliers, "\n")
        if (result$sensitivity_results$n_outliers > 0) {
          cat("Outlier Studies:", paste(result$sensitivity_results$outliers, collapse = ", "), "\n")
        }
        cat("\n")
      }
      
      cat("RECOMMENDATION:\n")
      recommended_model <- ifelse(result$random_model$I2 > 25, "Random Effects", "Fixed Effects")
      cat("Primary Model:", recommended_model, "\n")
      
      if (result$effect_type == "SMD") {
        magnitude <- case_when(
          abs(result$random_model$beta[1]) < 0.2 ~ "Negligible",
          abs(result$random_model$beta[1]) < 0.5 ~ "Small",
          abs(result$random_model$beta[1]) < 0.8 ~ "Medium",
          TRUE ~ "Large"
        )
        cat("Effect Size Magnitude:", magnitude, "\n")
      }
      
      cat("\n", paste(rep("=", 80), collapse = ""), "\n\n")
    }
  }
  
  cat("OVERALL SUMMARY:\n")
  cat("Total outcomes analyzed:", successful_analyses, "\n")
  cat("Analysis completed on:", as.character(Sys.time()), "\n")
  cat("\nFiles generated:\n")
  cat("- Forest plots (fixed and random effects)\n")
  cat("- Enhanced funnel plots with comprehensive Egger's test\n")
  cat("- Clean funnel plots with Egger test statistics (line removed for clarity)\n")
  cat("- Comprehensive sensitivity analysis plots (4-panel + detailed leave-one-out)\n")
  cat("- Comprehensive CSV results files\n")
  cat("- Publication bias assessment\n")
  cat("- Model comparison tables\n")
  cat("- Sensitivity analysis summaries\n")
  
  sink()
  
  # Create enhanced CSV summary
  create_master_summary_csv(results_list)
  
  cat("Comprehensive summary report saved\n")
}

# Function to create master summary CSV
create_master_summary_csv <- function(results_list) {
  all_outcomes_summary <- data.frame()
  
  for (result in results_list) {
    if (!is.null(result)) {
      outcome_row <- data.frame(
        Outcome = result$outcome,
        Effect_Size_Type = result$effect_type,
        Number_of_Studies = result$n_studies,
        Total_Sample_Size = sum(result$escalc_result$Intervention_N + result$escalc_result$Control_N),
        
        # Fixed Effects Results
        Fixed_Effect_Size = result$fixed_model$beta[1],
        Fixed_CI_Lower = result$fixed_model$ci.lb,
        Fixed_CI_Upper = result$fixed_model$ci.ub,
        Fixed_P_Value = result$fixed_model$pval,
        Fixed_Significant = result$fixed_model$pval < 0.05,
        
        # Random Effects Results
        Random_Effect_Size = result$random_model$beta[1],
        Random_CI_Lower = result$random_model$ci.lb,
        Random_CI_Upper = result$random_model$ci.ub,
        Random_P_Value = result$random_model$pval,
        Random_Significant = result$random_model$pval < 0.05,
        
        # Heterogeneity
        I_Squared = result$random_model$I2,
        Heterogeneity_Level = case_when(
          result$random_model$I2 < 25 ~ "Low",
          result$random_model$I2 < 50 ~ "Moderate",
          result$random_model$I2 < 75 ~ "Substantial",
          TRUE ~ "Considerable"
        ),
        
        # Publication Bias
        Egger_P_Value = ifelse(is.null(result$egger_results), NA, result$egger_results$p_value),
        Publication_Bias_Evidence = ifelse(is.null(result$egger_results), NA, 
                                           result$egger_results$interpretation),
        
        # Sensitivity Analysis
        Most_Influential_Study = ifelse(is.null(result$sensitivity_results), NA, 
                                        result$sensitivity_results$max_influence_study),
        Number_of_Outliers = ifelse(is.null(result$sensitivity_results), NA, 
                                    result$sensitivity_results$n_outliers),
        
        # Recommendations
        Recommended_Model = ifelse(result$random_model$I2 > 25, "Random Effects", "Fixed Effects"),
        
        # Effect Size Magnitude (for SMD)
        Effect_Size_Magnitude = if(result$effect_type == "SMD") {
          case_when(
            abs(result$random_model$beta[1]) < 0.2 ~ "Negligible",
            abs(result$random_model$beta[1]) < 0.5 ~ "Small",
            abs(result$random_model$beta[1]) < 0.8 ~ "Medium",
            TRUE ~ "Large"
          )
        } else {
          "N/A (Mean Difference)"
        },
        
        # Quality indicators
        Adequate_Sample_Size = result$n_studies >= 5,
        Results_Robust = ifelse(is.null(result$sensitivity_results), NA, 
                                result$sensitivity_results$robust),
        
        # Note about clean funnel plot
        Clean_Funnel_Note = "Egger line removed for clarity; statistics remain valid"
      )
      
      if (nrow(all_outcomes_summary) == 0) {
        all_outcomes_summary <- outcome_row
      } else {
        all_outcomes_summary <- rbind(all_outcomes_summary, outcome_row)
      }
    }
  }
  
  if (nrow(all_outcomes_summary) > 0) {
    write.csv(all_outcomes_summary, file.path(results_dir, "master_outcomes_summary.csv"), 
              row.names = FALSE)
  }
}

# Main execution function
main <- function() {
  
  cat("\n", paste(rep("=", 60), collapse = ""), "\n")
  cat("   FIXED COMPREHENSIVE META-ANALYSIS SCRIPT\n")
  cat("   Version: 3.3 - Clean Funnel Plot WITHOUT Egger Line\n")
  cat(paste(rep("=", 60), collapse = ""), "\n\n")
  
  # Check if the required file exists
  input_file <- "continuous_data - Copy.csv"
  
  # Create results directory structure
  create_results_structure()
  
  # Prepare data
  data <- tryCatch({
    prepare_data(input_file)
  }, error = function(e) {
    cat("\nERROR:", e$message, "\n")
    return(NULL)
  })
  
  if (is.null(data)) {
    stop("Could not prepare data for analysis")
  }
  
  # Get unique outcomes
  outcomes <- unique(data$Outcome[!is.na(data$Outcome)])
  cat("\n=== STARTING FIXED ANALYSIS ===\n")
  cat("Found", length(outcomes), "unique outcomes\n")
  cat("Results will be saved in:", results_dir, "\n")
  cat("Fixed features:\n")
  cat("- Rich Egger's test visualization with regression analysis (enhanced plot)\n")
  cat("- Clean funnel plots with Egger STATISTICS ONLY (line removed for clarity)\n")
  cat("- Fixed comprehensive sensitivity analysis with 4 panels\n")
  cat("- Detailed leave-one-out analysis\n")
  cat("- Comprehensive CSV exports with all model information\n")
  cat("- Publication bias assessment with interpretation\n")
  cat("- Model comparison tables\n")
  cat("- Outlier detection and influence analysis\n")
  cat("- PROBLEM FIXED: No more incorrect Egger lines in clean funnel plots\n\n")
  
  # Perform meta-analysis for each outcome
  results_list <- list()
  
  for (i in seq_along(outcomes)) {
    cat("\n--- Processing outcome", i, "of", length(outcomes), "---\n")
    outcome_data <- data %>% filter(Outcome == outcomes[i])
    result <- perform_meta_analysis(outcome_data, outcomes[i], data)
    results_list[[outcomes[i]]] <- result
  }
  
  # Count successful analyses
  successful <- sum(!sapply(results_list, is.null))
  cat("\n=== ANALYSIS COMPLETE ===\n")
  cat("Successfully analyzed", successful, "out of", length(outcomes), "outcomes\n")
  
  if (successful > 0) {
    # Perform meta-regression
    tryCatch({
      perform_meta_regression(results_list)
    }, error = function(e) {
      cat("Could not perform meta-regression:", e$message, "\n")
    })
    
    # Create comprehensive summary report
    create_summary_report(results_list)
    
    cat("\n=== RESULTS SUMMARY ===\n")
    cat(sprintf("%-30s %-10s %-10s %-12s %-15s %-20s %-12s\n", 
                "Outcome", "N Studies", "Effect", "p-value", "I²", "Pub. Bias", "Outliers"))
    cat(paste(rep("-", 110), collapse = ""), "\n")
    
    for (result in results_list) {
      if (!is.null(result)) {
        sig_text <- ifelse(result$random_model$pval < 0.05, "*", " ")
        bias_text <- ifelse(!is.null(result$egger_results) && result$egger_results$significant, 
                            "Evidence", "None")
        outliers_text <- ifelse(!is.null(result$sensitivity_results), 
                                result$sensitivity_results$n_outliers, "NA")
        cat(sprintf("%-30s %-10d %-9.3f %-11.3f %-14.1f%% %-20s %-11s %s\n", 
                    substr(result$outcome, 1, 29),
                    result$n_studies,
                    result$random_model$beta[1], 
                    result$random_model$pval,
                    result$random_model$I2,
                    bias_text,
                    outliers_text,
                    sig_text))
      }
    }
    cat("\n* = Statistically significant (p < 0.05)\n")
  }
  
  cat("\n=== SCRIPT COMPLETE ===\n")
  cat("Results directory:", results_dir, "\n")
  cat("Fixed outputs generated:\n")
  cat("✓ Forest plots with comprehensive statistics\n")
  cat("✓ Enhanced funnel plots with rich Egger's test visualization (with line)\n")
  cat("✓ Clean funnel plots with Egger STATISTICS ONLY (line removed)\n")
  cat("✓ Fixed comprehensive sensitivity analysis (4-panel plots)\n")
  cat("✓ Detailed leave-one-out sensitivity plots\n")
  cat("✓ Comprehensive CSV files with all model information\n")
  cat("✓ Publication bias assessment with interpretation\n")
  cat("✓ Model comparison tables\n")
  cat("✓ Outlier detection and influence analysis\n")
  cat("✓ Meta-regression analysis\n")
  cat("✓ Master summary report\n")
  cat("✓ FIXED: Clean funnel plots show only valid Egger statistics\n")
  
  return(list(
    results = results_list,
    data = data
  ))
}

# Execute the main function
cat("FIXED Meta-analysis script loaded successfully!\n")
cat("Fix: Clean funnel plot now shows Egger statistics without problematic line\n")
cat("Starting analysis...\n\n")

# Run with error handling
final_results <- tryCatch({
  main()
}, error = function(e) {
  cat("\n!!! CRITICAL ERROR !!!\n")
  cat("Error message:", e$message, "\n")
  cat("\nFull error trace:\n")
  traceback()
  NULL
})

if (!is.null(final_results)) {
  cat("\n✓ Fixed analysis completed successfully!\n")
  cat("To access results in R, use: final_results$results\n")
  cat("All fixed outputs saved in:", results_dir, "\n")
  cat("Clean funnel plots now show Egger statistics without incorrect lines\n")
} else {
  cat("\n✗ Analysis failed. Please check error messages above.\n")
}

# END OF FIXED SCRIPT
