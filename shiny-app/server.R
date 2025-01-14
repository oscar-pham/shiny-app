library(shiny)
library(ggplot2)
library(DT)
library(sortable)

#----------------------------------------------------------------------------

df <- read.csv("df_cleaned.csv", header = TRUE, sep = ",")

# Convert all character columns to factors and non-character columns to numeric
df[] <- lapply(df, function(x) {
  if (is.character(x)) {
    # Convert "NA" strings to actual NA before converting to factor
    x[x == "NA"] <- NA
    as.factor(x)  # Convert character columns to factors
  } else {
    as.numeric(x)  # Convert non-character columns to numeric
  }
})

# Remove rows with any NA values (including "NA" strings now converted)
df <- na.omit(df)

#----------------------------------------------------------------------------

# Helper function for permutation test
perm_test <- function(sample1, sample2, n_perm = 1000, welch = FALSE, alternative = "greater") {
  combined <- c(sample1, sample2)
  n1 <- length(sample1)
  n2 <- length(sample2)
  
  # Observed t-statistic
  observed_t <- t.test(sample1, sample2, var.equal = !welch, alternative = alternative)$statistic
  
  # Permutations
  perm_t_stats <- replicate(n_perm, {
    permuted <- sample(combined)
    perm_sample1 <- permuted[1:n1]
    perm_sample2 <- permuted[(n1 + 1):(n1 + n2)]
    
    t.test(perm_sample1, perm_sample2, var.equal = !welch, alternative = alternative)$statistic
  })
  
  # Calculate p-value
  p_value <- mean(abs(perm_t_stats) >= abs(observed_t))
  
  return(list(statistic = observed_t, p_value = p_value))
}

simulate_test_statistics <- function(var1, var2, n = 1000) {
  test_statistics <- replicate(n, {
    # Create a contingency table from the sampled data
    contingency_table <- table(sample(var1, length(var1), replace = TRUE), 
                               sample(var2, length(var2), replace = TRUE))
    
    # Ensure that the table has no zero rows or columns before applying chisq.test
    if (all(rowSums(contingency_table) > 0) && all(colSums(contingency_table) > 0)) {
      chisq_test <- suppressWarnings(chisq.test(contingency_table))
      return(chisq_test$statistic)  # Return the test statistic instead of the p-value
    } else {
      return(NA)  # Return NA for invalid tables
    }
  })
  
  # Remove NA values from test_statistics to ensure valid data
  test_statistics <- test_statistics[!is.na(test_statistics)]
  return(test_statistics)
}

conclusion_function <- function(p_value, alpha) {
  if (p_value < alpha) {
    return(paste("Since the p-value (", formatC(p_value, digits = 5), ") is less than the significance level (α = ", alpha, "), we reject the null hypothesis."))
  } else {
    return(paste("Since the p-value (", formatC(p_value, digits = 5), ") is greater than or equal to the significance level (α = ", alpha, "), we do not reject the null hypothesis."))
  }
}

#----------------------------------------------------------------------------

server <- function(input, output, session) {
  observe({
    numeric_vars <- names(df)[sapply(df, is.numeric)]
    factor_vars <- names(df)[sapply(df, is.factor)]
    
    updateSelectInput(session, "quantitative_var", choices = numeric_vars)
    updateSelectInput(session, "categorical_var", choices = factor_vars)
    updateSelectInput(session, "var_gof", 
                      choices = names(df)[sapply(df, is.factor)], 
                      selected = names(df)[sapply(df, is.factor)][1])
  })
  
  # UI elements for selecting variables
  output$var_select_1 <- renderUI({
    if (input$select == 1) {
      selectInput("var1", "Choose the first categorical variable", choices = names(df)[sapply(df, is.factor)], selected = names(df)[1])
    }
  })
  
  # UI elements for selecting variables
  output$var_select_num3 <- renderUI({
    selectInput("numerical", "Choose a numerical variable", choices = names(df)[sapply(df, is.numeric)], selected = names(df)[1])

  })
  
  # UI elements for selecting variables
  output$var_select_num4 <- renderUI({
    selectInput("numerical", "Choose a numerical variable", choices = names(df)[sapply(df, is.numeric)], selected = names(df)[1])
    
  })
  
  # Render the UI element for categorical and numerical variable selection
  output$var_select_cat2 <- renderUI({
    selectInput("categorical", "Choose a categorical variable (grouping):", 
                choices = names(df)[sapply(df, is.factor)], 
                selected = names(df)[sapply(df, is.factor)][1])
  })
  
  output$var_select_num5 <- renderUI({
    selectInput("numerical", "Choose a numerical variable (values):", 
                choices = names(df)[sapply(df, is.numeric)], 
                selected = names(df)[sapply(df, is.numeric)][1])
  })
  
  # UI elements for selecting variables
  output$var_select_num1 <- renderUI({
    if (input$plotting == 3) {
      selectInput("numerical1", "Choose the first numerical variable", choices = names(df)[sapply(df, is.numeric)], selected = names(df)[1])
    }
  })
  
  # UI elements for selecting variables
  output$var_select_num2 <- renderUI({
    if (input$plotting == 3) {
      selectInput("numerical2", "Choose the second numerical variable", choices = names(df)[sapply(df, is.numeric)], selected = names(df)[1])
    }
  })
  
  # UI elements for selecting variables
  output$var_select_cat <- renderUI({
    if (input$plotting == 1) {
      selectInput("categorical1", "Choose a categorical variable", choices = names(df)[sapply(df, is.factor)], selected = names(df)[1])
    }
  })
  
  # UI elements for selecting variables
  output$var_select_acat <- renderUI({
    if (input$plotting == 1) {
      selectInput("categorical2", "Choose another categorical variable", choices = names(df)[sapply(df, is.factor)], selected = names(df)[1])
    }
  })
  
  output$var_select_2 <- renderUI({
    if (input$select == 1) {
      selectInput("var2", "Choose the second categorical variable", choices = names(df)[sapply(df, is.factor)], selected = names(df)[2])
    }
  })
  
  # Reactive expression to calculate test results
  test_results <- reactive({
    if (input$select == 1 && !is.null(input$var1) && !is.null(input$var2)) {
      var1 <- df[[input$var1]]
      var2 <- df[[input$var2]]
      contingency_table <- table(var1, var2)
      
      # Ensure valid table (no zero rows or columns)
      if (all(rowSums(contingency_table) > 0) && all(colSums(contingency_table) > 0)) {
        # Perform Chi-Square test, with simulation if expected counts < 5
        simulated <- any(suppressWarnings(chisq.test(contingency_table)$expected < 5))
        chisq_test <- suppressWarnings(chisq.test(contingency_table, simulate.p.value = simulated))
        
        return(list(
          test = chisq_test,
          simulated = simulated,
          observed_table = chisq_test$observed,
          expected_table = chisq_test$expected,
          var1 = var1,
          var2 = var2
        ))
      } else {
        return(NULL)  # Return NULL if the table is invalid
      }
    } else {
      return(NULL)
    }
  })
  
  # Reactive expression to calculate expected values based on selected distribution
  expected_values_calculation <- reactive({
    req(input$var_gof)
    
    observed <- table(df[[input$var_gof]])
    categories <- seq_along(observed)  # Handle non-numeric categories with indices
    
    if (input$distribution == "normal") {
      mean_val <- mean(as.numeric(observed))
      sd_val <- sd(as.numeric(observed))
      expected <- dnorm(categories, mean = mean_val, sd = sd_val)
    } else if (input$distribution == "poisson") {
      lambda <- mean(as.numeric(observed))
      expected <- dpois(categories, lambda = lambda)
    } else if (input$distribution == "custom") {
      expected_proportions <- as.numeric(unlist(strsplit(input$expected_values, ",")))
      if (length(expected_proportions) != length(observed)) {
        showNotification("Expected proportions must match the number of categories.", type = "error")
        return(NULL)
      }
      expected <- expected_proportions
    }
    
    expected <- expected / sum(expected) * sum(observed)  # Rescale to match observed counts
    return(list(observed = observed, expected = expected))
  })
  
gof_results <- reactive({
  req(input$var_gof, input$distribution)
  
  # Step 1: Calculate expected and observed values
  results <- expected_values_calculation()
  if (is.null(results)) return(NULL)
  
  observed <- results$observed
  expected <- results$expected
  
  # Step 2: Check if expected values contain missing or zero values
  if (any(is.na(expected)) || any(expected == 0)) {
    showNotification("Expected values contain missing or zero values. Test cannot be performed.", type = "error")
    return(NULL)
  }
  
  # Step 3: Check if any expected values are less than 5
  use_simulation <- any(expected < 5)
  
  # Step 4: Perform the Chi-Square test with or without simulation
  chisq_test <- tryCatch({
    if (use_simulation) {
      chisq.test(observed, p = expected / sum(expected), rescale.p = TRUE, simulate.p.value = TRUE, B = 2000)
    } else {
      chisq.test(observed, p = expected / sum(expected), rescale.p = TRUE)
    }
  }, error = function(e) {
    showNotification(paste("Error performing Chi-Square test:", e$message), type = "error")
    return(NULL)
  })
    
    # Step 5: Return the test results, include whether simulation was used
    return(list(test = chisq_test, observed = observed, expected = expected, simulated = use_simulation))
  })
  
  # Display test results and warnings
  output$t_result <- renderUI({
    results <- test_results()
    if (!is.null(results)) {
      HTML(paste0("<strong>Test Statistic:</strong> ", round(results$test$statistic, 5)))
    }
  })
  
  output$p_result <- renderUI({
    results <- test_results()
    if (!is.null(results)) {
      HTML(paste0("<strong>P-value:</strong> ", formatC(results$test$p.value, digits = 5)))
    }
  })
  
  # Plot histogram for simulated p-values or normal p-value graph
  output$plot_pvalues <- renderPlot({
    results <- test_results()
    if (!is.null(results)) {
      if (results$simulated) {
        # Simulated p-values: Generate a histogram
        t_stat <- simulate_test_statistics(results$var1, results$var2)
        
        if (length(t_stat) > 0) {  # Ensure there are valid p-values
          ggplot(data.frame(t_stat), aes(x = t_stat)) +
            geom_histogram(binwidth = 0.75, fill = "grey", color = "black") +
            geom_vline(aes(xintercept = results$test$statistic), color = "red", linetype = "dashed", size = 1.2) +  # Vertical line for test statistic
            labs(title = "Histogram of Simulated P-values", x = "Values", y = "Frequency") +
            theme(
              plot.title = element_text(size = 20, hjust = 0.5, face = "bold"),  # Title size and center alignment
              axis.title.x = element_text(size = 14),  # Adjust x-axis title size
              axis.title.y = element_text(size = 14)   # Adjust y-axis title size
            )
        } else {
          # If no valid p-values, show a message
          ggplot() + 
            annotate("text", x = 0.5, y = 0.5, label = "No valid simulated p-values", size = 6) +
            theme_void()
        }
      }
    }
  })
  
  # Display Chi-Square p-value graph (for non-simulated p-values)
  output$plot_pvalue_graph <- renderPlot({
    results <- test_results()
    
    if (!is.null(results) && !results$simulated) {
      # Extract p-value and test statistic
      p_value <- results$test$p.value
      test_stat <- results$test$statistic
      df <- results$test$parameter  # Degrees of freedom
      
      # Define a range of x values (Chi-Square statistic values)
      x_vals <- seq(0, max(test_stat * 1.5, 10), length.out = 1000)  # Adjust the range based on the test_stat
      
      # Create a data frame for shading the tail
      tail_data <- data.frame(x = seq(test_stat, max(x_vals), length.out = 500))
      
      # Plot the Chi-Square distribution and the shaded tail
      ggplot(data.frame(x = x_vals), aes(x = x)) +
        stat_function(fun = dchisq, args = list(df = df), color = "grey", size = 1) +  # Plot Chi-Square distribution
        geom_vline(aes(xintercept = test_stat), color = "red", linetype = "dashed", size = 1.2) +  # Vertical line for test statistic
        geom_area(data = tail_data, aes(x = x, y = dchisq(x, df = df)), fill = "red", alpha = 0.4) +  # Shading the tail
        labs(title = "Chi-Square Distribution with Test Statistic and P-Value Shaded", 
             x = "Chi-Square Value", y = "Density") +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 20, hjust = 0.5, face = "bold"),  # Title size and center alignment
          axis.title.x = element_text(size = 14),  # Adjust x-axis title size
          axis.title.y = element_text(size = 14)   # Adjust y-axis title size
        )
    }
  })
  
  output$simulation_flag <- reactive({
    req(input$select)
    results <- NULL
    if (input$select == 1){
    results <- test_results()
    }
    else if (input$select == 2){
      results <- gof_results()
      print(results)
    }
    if (!is.null(results)) {
      return(results$simulated)
    }
    return(FALSE)  # Default to false if no results
  })
  
  outputOptions(output, "simulation_flag", suspendWhenHidden = FALSE)
  
  # Render expected contingency table with 'table-danger' for rows with values < 5
  output$expected_table <- DT::renderDataTable({
    results <- test_results()
    
    if (!is.null(results)) {
      expected_table <- as.data.frame.matrix(results$expected_table)
      
      # Add rownames to the table
      expected_table$RowNames <- rownames(expected_table)
      expected_table <- expected_table[, c(ncol(expected_table), 1:(ncol(expected_table) - 1))]  # Reorder so rownames come first
      
      # Prepare the row callback to add 'table-danger' to rows where any cell < 5
      row_callback <- JS(
        "function(row, data, index) {",
        "  var min_value = Math.min.apply(null, data.slice(1));",  # Skip the first column (row names)
        "  if (min_value < 5) {",
        "    $(row).addClass('table-danger');",  # Add 'table-danger' class
        "  }",
        "}"
      )
      
      # Create the datatable
      datatable(
        expected_table, 
        options = list(
          dom = 't',          # Show only the table, no pagination, no search bar
          paging = FALSE,     # Disable pagination
          searching = FALSE,  # Disable search
          ordering = TRUE,    # Enable column sorting
          autoWidth = TRUE,   # Automatically adjust column width
          rowCallback = row_callback  # Use the row callback to apply 'table-danger' class
        ),
        rownames = FALSE,  # Already included in the table
        class = 'table table-hover table-striped'
      )
    }
  })
  
  # Display dynamic hypothesis statement
  output$dynamic_hypothesis <- renderUI({
    if (!is.null(input$var1) && !is.null(input$var2)) {
      tagList(
        strong("Null Hypothesis "), "(H", tags$sub("0"), "): ", code(input$var1), " is independent of ", code(input$var2),
        br(), strong("Alternative Hypothesis "), "(H", tags$sub("1"), "): ", code(input$var1), " is not independent of ", code(input$var2)
      )
    } else {
      "Please select two variables."
    }
  })

output$conclusion <- renderUI({
  results <- test_results()
  if (!is.null(results)) {
    p_value <- results$test$p.value
    alpha <- input$alpha
    conclusion <- conclusion_function(p_value, alpha)
    HTML(conclusion)
  }
})

# Dynamic hypothesis for Goodness of Fit Test
output$gof_hypothesis <- renderUI({
  if (!is.null(input$var_gof)) {
    tagList(
      strong("Null Hypothesis "), "(H", tags$sub("0"), "): ", code(input$var_gof), " follows the specified distribution.",
      br(), strong("Alternative Hypothesis "), "(H", tags$sub("1"), "): ", code(input$var_gof), " does not follow the specified distribution."
    )
  } else {
    "Please select a variable and enter expected proportions."
  }
})

# Render observed and expected counts for Goodness of Fit Test
output$gof_table <- DT::renderDataTable({
  results <- gof_results()
  if (!is.null(results)) {
    df_gof <- data.frame(
      Category = names(results$observed),
      Observed = as.numeric(results$observed),
      Expected = as.numeric(results$expected)
    )
    datatable(df_gof, options = list(dom = 't', paging = FALSE, searching = FALSE), rownames = FALSE)
  }
})


# Test statistic for Goodness of Fit Test
output$gof_t_result <- renderUI({
  results <- gof_results()
  if (!is.null(results)) {
    HTML(paste0("<strong>Test Statistic:</strong> ", round(results$test$statistic, 5)))
  } else {
    HTML("Test statistic could not be calculated due to missing or invalid data.")
  }
})

# P-value for Goodness of Fit Test
output$gof_p_result <- renderUI({
  results <- gof_results()
  if (!is.null(results)) {
    HTML(paste0("<strong>P-value:</strong> ", formatC(results$test$p.value, digits = 5)))
  } else {
    HTML("P-value could not be calculated due to missing or invalid data.")
  }
})


# Conclusion for Goodness of Fit Test
output$gof_conclusion <- renderUI({
  results <- gof_results()
  if (!is.null(results)) {
    p_value <- results$test$p.value
    alpha <- input$alpha
    conclusion <- conclusion_function(p_value, alpha)
    HTML(conclusion)
  }
})

# Chi-Squared PDF Plot for Goodness of Fit Test with centered peak
output$chi_squared_plot <- renderPlot({
  # Use observed data and calculate df for the Chi-Squared distribution
  results <- gof_results()  # Assuming gof_results() provides observed values
  
  if (is.null(results)) {
    return(NULL)  # Return nothing if no results
  }
  
  observed <- results$observed  # Retrieve observed data
  df <- length(observed) - 1    # Degrees of freedom for Chi-Squared
  test_stat <- results$test$statistic  # The test statistic from the Chi-Squared test
  
  # Create a sequence for x-axis values, centering around df-2 (Chi-Squared distribution peak)
  x_max <- max(test_stat * 1.5, df * 2)  # Adjust the max x based on df and test statistic
  x_vals <- seq(0, x_max, length.out = 1000)  # Adjust the range based on df
  
  # Create a data frame for shading the tail
  tail_data <- data.frame(x = seq(test_stat, x_max, length.out = 500))
  
  # Plot the Chi-Squared distribution and the shaded tail
  ggplot(data.frame(x = x_vals), aes(x = x)) +
    stat_function(fun = dchisq, args = list(df = df), color = "blue", size = 1) +  # Plot Chi-Squared distribution
    geom_vline(aes(xintercept = test_stat), color = "red", linetype = "dashed", size = 1.2) +  # Vertical line for test statistic
    geom_area(data = tail_data, aes(x = x, y = dchisq(x, df = df)), fill = "red", alpha = 0.4) +  # Shading the tail
    labs(
      title = paste("Chi-Squared PDF (df =", df, ") with Test Statistic and P-Value Tail"),
      x = "Chi-Squared value",
      y = "Density"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 20, hjust = 0.5, face = "bold"),  # Title size and center alignment
      axis.title.x = element_text(size = 14),  # Adjust x-axis title size
      axis.title.y = element_text(size = 14)   # Adjust y-axis title size
    )
})

# Bucket list UI rendering
observeEvent(input$categorical_var, {
  req(input$categorical_var)

  categories <- na.omit(unique(df[[input$categorical_var]]))

  output$bucket_list_ui <- renderUI({
    column(
      width = 12,
      bucket_list(
        header = NULL,
        group_name = "bucket_list_group",
        orientation = "vertical",
        add_rank_list(text = "List of Levels", labels = categories),
        add_rank_list(text = "Sample 1", labels = NULL, input_id = "rank_list_1"),
        add_rank_list(text = "Sample 2", labels = NULL, input_id = "rank_list_2")
      )
    )
  })
})

# Reactive data extraction based on selected categories
selected_data <- reactive({
  req(input$quantitative_var, input$categorical_var)
  
  sample1 <- input$rank_list_1
  sample2 <- input$rank_list_2
  
  if (length(sample1) == 1 && length(sample2) == 1 && sample1 != sample2) {
    data_sample1 <- df[df[[input$categorical_var]] %in% sample1, ]
    data_sample2 <- df[df[[input$categorical_var]] %in% sample2, ]
    data_sample1 <- data_sample1[is.finite(data_sample1[[input$quantitative_var]]), ]
    data_sample2 <- data_sample2[is.finite(data_sample2[[input$quantitative_var]]), ]
    
    list(sample1 = data_sample1[[input$quantitative_var]], sample2 = data_sample2[[input$quantitative_var]])
  } else {
    NULL
  }
})

# Perform t-tests based on user inputs for Welch's test and/or permutation test
t_test_result <- reactive({
  data <- selected_data()
  
  if (!is.null(data) && length(data$sample1) > 1 && length(data$sample2) > 1) {
    test_result <- NULL
    
    # Run a normal t-test
    if (!isTRUE(input$welch) && !isTRUE(input$perm)) {
      test_result <- t.test(data$sample1, data$sample2, var.equal = TRUE, alternative = input$alternative)
    }
    
    # Perform both tests (Welch and Permutation)
    else if (isTRUE(input$welch) && isTRUE(input$perm)) {
      test_result <- perm_test(data$sample1, data$sample2, n_perm = 1000, welch = TRUE, alternative = input$alternative)
      return(list("t-stat" = as.numeric(test_result$statistic), "p-value" = as.numeric(test_result$p_value)))
    }
    
    # Welch's t-test
    else if (isTRUE(input$welch)) {
      test_result <- t.test(data$sample1, data$sample2, var.equal = FALSE, alternative = input$alternative)
    }
    
    # Permutation test
    else if (isTRUE(input$perm)) {
      test_result <- perm_test(data$sample1, data$sample2, n_perm = 1000, alternative = input$alternative)
      return(list("t-stat" = as.numeric(test_result$statistic), "p-value" = as.numeric(test_result$p_value)))
    }
    
    # Return the result from normal or Welch's t-test
    if (!is.null(test_result)) {
      return(list("t-stat" = as.numeric(test_result$statistic), "p-value" = as.numeric(test_result$p.value)))
    }
    
  } else {
    showNotification("Please select valid categories and ensure there is sufficient data for both samples.", type = "error")
    return(NULL)
  }
})

# Test statistic for t-Test
output$t_t_result <- renderUI({
  results <- t_test_result()
  if (!is.null(results)) {
    HTML(paste0("<strong>Test Statistic:</strong> ", round(results[["t-stat"]], 5)))
  } else {
    HTML("Test statistic could not be calculated due to missing or invalid data.")
  }
})

# P-value for t-Test
output$t_p_result <- renderUI({
  results <- t_test_result()
  if (!is.null(results)) {
    HTML(paste0("<strong>P-value:</strong> ", formatC(results[["p-value"]], digits = 5)))
  } else {
    HTML("P-value could not be calculated due to missing or invalid data.")
  }
})


output$dynamic_hypothesis_ttest <- renderUI({
    tagList(
      strong("Null Hypothesis "), "(H", tags$sub("0"), "): Mean of ", code(input$quantitative_var), " is the same across the two samples",
      br(), strong("Alternative Hypothesis "), "(H", tags$sub("1"), "): Mean of ", code(input$quantitative_var), " is not the same across two samples"
    )
})

output$qq_plot <- renderPlot({
  data <- selected_data()
  
  # Get selected categories for Sample 1 and Sample 2
  sample1 <- input$rank_list_1
  sample2 <- input$rank_list_2
  
  # Ensure that there is enough data in both samples
  if (!is.null(data) && length(data$sample1) > 1 && length(data$sample2) > 1) {
    
    # Set up side by side plotting
    par(mfrow = c(1, 2))  # Set up 1 row, 2 columns for side by side plotting
    
    # Q-Q Plot for Sample 1
    qqnorm(data$sample1, main = "Q-Q Plot: Sample 1", xlab = "Theoretical Quantiles", ylab = "Sample 1 Quantiles", col = "blue", pch = 16)
    qqline(data$sample1, col = "red", lwd = 2, lty = 2)
    
    # Q-Q Plot for Sample 2
    qqnorm(data$sample2, main = "Q-Q Plot: Sample 2", xlab = "Theoretical Quantiles", ylab = "Sample 2 Quantiles", col = "blue", pch = 16)
    qqline(data$sample2, col = "red", lwd = 2, lty = 2)
    
  } else {
    # If there is not enough data, show an error message
    plot.new()
    text(0.5, 0.5, "Not enough data in one or both samples for the Q-Q plot.", cex = 1.5)
  }

})

output$box_plot <- renderPlot({
  data <- selected_data()
  
  # Ensure that there is enough data in both samples
  if (!is.null(data) && length(data$sample1) > 1 && length(data$sample2) > 1) {
    # Combine the two samples into one data frame for plotting
    combined_data <- rbind(
      data.frame(Sample = "Sample 1", Value = data$sample1),
      data.frame(Sample = "Sample 2", Value = data$sample2)
    )
    
    # Create side-by-side box plots using ggplot2
    ggplot(combined_data, aes(x = Sample, y = Value, fill = Sample)) +
      geom_boxplot() +
      labs(title = "Box Plots for Sample 1 and Sample 2", x = "Sample", y = input$quantitative_var) +
      theme_minimal() +
      theme(axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14))
    
  } else {
    plot.new()
    text(0.5, 0.5, "Not enough data in one or both samples for box plots.", cex = 1.5)
  }

})

# Conclusion for t-Test
output$t_conclusion <- renderUI({
  results <- t_test_result()
  if (!is.null(results)) {
    p_value <- results[["p-value"]]
    alpha <- input$alpha
    conclusion <- conclusion_function(p_value, alpha)
    HTML(conclusion)
  }
})

# Generate bar plot based on selected categorical variable
output$barplot <- renderPlot({
    # Dynamically access the selected column from the data
    selected_data <- df[[input$categorical1]]
    # Create bar plot
    ggplot(data.frame(selected_data), aes(x = selected_data, fill = selected_data)) +
      geom_bar() +
      xlab(input$categorical1) +
      ylab("Count") +
      theme_minimal()+
      scale_fill_brewer(palette = "Set3") +
      labs(fill = "Legend")


})

# Render the histogram plot
output$histogram <- renderPlot({
  
  # Ensure that the selected input variable is numeric
  req(input$numerical)
  
  # Check if the selected column is numeric
  if (is.numeric(df[[input$numerical]])) {
    
    # Create the histogram using ggplot2
    ggplot(df, aes(x = .data[[input$numerical]])) +
      geom_histogram(binwidth = 5, fill = "grey", color = "black") +  # Define bin width and colors
      xlab(input$numerical) +  # Label the x-axis dynamically based on selected variable
      ylab("Frequency") +      # Label the y-axis
      theme_minimal()          # Use a minimal theme for a clean look
    
  } else {
    # If the variable is not numeric, display a notification
    showNotification("Please select a numeric variable.", type = "error")
  }
})

# Render the scatterplot with regression line and 95% confidence interval
output$scatterplot <- renderPlot({
  
  # Ensure that both selected variables are numeric
  req(input$numerical1, input$numerical2)
  
  # Check if selected variables are valid numeric columns
  if (is.numeric(df[[input$numerical1]]) && is.numeric(df[[input$numerical2]])) {
    
    # Create scatterplot with regression line and confidence interval using ggplot2
    ggplot(df, aes(x = .data[[input$numerical1]], y = .data[[input$numerical2]])) +
      geom_point(color = "black", size = 3) +  # Scatter points with custom color and size
      geom_smooth(method = "lm", se = TRUE, color = "red", fill = "lightblue", level = 0.95) +  # Add regression line and 95% CI
      xlab(input$numerical1) +  # Dynamic X-axis label
      ylab(input$numerical2) +  # Dynamic Y-axis label
      theme_minimal() +    # Minimal theme for a clean look
      ggtitle(paste("Scatterplot of", input$numerical1, "vs", input$numerical2))  # Dynamic title
  } else {
    # Show a notification if non-numeric columns are selected (though unlikely here)
    showNotification("Please select numeric variables for both axes.", type = "error")
  }
})

# Render the density plot
output$densityPlot <- renderPlot({
  
  # Ensure that the selected input variable is numeric and exists in the data
  req(input$numerical)
  
  # Check if the selected variable is numeric
  if (is.numeric(df[[input$numerical]])) {
    
    # Get the colors from the "Set3" palette
    set3_colors <- RColorBrewer::brewer.pal(12, "Set3")  # "Set3" palette has 12 colors
    random_color <- sample(set3_colors, 1)  # Randomly pick one color from the "Set3" palette
    
    # Create the density plot using ggplot2
    ggplot(df, aes(x = .data[[input$numerical]])) +
      geom_density(fill = random_color, alpha = 0.4) +  # Density plot with fill and transparency
      xlab(input$numerical) +  # Dynamic label for the x-axis
      ylab("Density") +  # Label for the y-axis
      ggtitle(paste("Density Plot of", input$numerical)) +  # Dynamic title based on the variable
      theme_minimal()  # Use a minimal theme for a clean look
    
  } else {
    # If the variable is not numeric, show a notification
    showNotification("Please select a numeric variable.", type = "error")
  }
})

# Render the interactive data table
output$data_table <- renderDT({
  datatable(df, options = list(pageLength = 10, searchHighlight = TRUE, search = list(regex = TRUE, smart = FALSE)), 
            filter = 'top',  # Add filter options on top of each column
            rownames = FALSE)
})

# Render the comparative boxplot
output$boxplot <- renderPlot({
  req(input$categorical, input$numerical)  # Ensure inputs are selected
  
  # Create the boxplot using ggplot2
  ggplot(df, aes_string(x = input$categorical, y = input$numerical, fill = input$categorical)) +
    geom_boxplot() +
    theme_minimal() +
    labs(title = "Comparative Boxplot",
         y = input$numerical,
         fill = input$categorical)
})

output$stacked_barplot <- renderPlot({
    # Dynamically access the selected columns from the data
    selected_data1 <- df[[input$categorical1]]
    selected_data2 <- df[[input$categorical2]]
    
      # Create a stacked bar plot
      ggplot(df, aes(x = selected_data1, fill = selected_data2)) +
        geom_bar(position = "stack") +
        xlab(input$categorical1) +
        ylab("Count") +
        theme_minimal() +
        scale_fill_brewer(palette = "Set3") +
        labs(fill = input$categorical2)  # Set label for the fill legend
})

# Render the mosaic plot based on Chi-Square test results
output$mosaicPlot <- renderPlot({
  results <- test_results()
  
  if (!is.null(results)) {
    observed_table <- results$observed_table
    vcd::mosaic(
      observed_table,
      shade = TRUE,
      legend = TRUE,
      labeling_args = list(
        set_varnames = c(input$var1, input$var2)
      ),
      main = "Mosaic Plot of Observed Frequencies"
    )
  } else {
    plot.new()
    text(0.5, 0.5, "Invalid or insufficient data for Mosaic Plot", cex = 1.5)
  }
})

}

