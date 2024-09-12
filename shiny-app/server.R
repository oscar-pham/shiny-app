library(shiny)
library(ggplot2)
library(DT)
#----------------------------------------------------------------------------

df <- read.csv("df_cleaned.csv", header = TRUE, sep = ",")
# Automatically convert all character columns to factors
df[] <- lapply(df, function(x) if(is.character(x)) as.factor(x) else x)

#----------------------------------------------------------------------------

simulate_test_statistics <- function(var1, var2, n = 1000) {
  test_statistics <- replicate(n, {
    # Create a contingency table from the sampled data
    contingency_table <- table(sample(var1, length(var1), replace = TRUE), 
                               sample(var2, length(var2), replace = TRUE))
    
    # Ensure that the table has no zero rows or columns before applying chisq.test
    if (all(rowSums(contingency_table) > 0) && all(colSums(contingency_table) > 0)) {
      chisq_test <- chisq.test(contingency_table, simulate.p.value = TRUE)
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
    return(paste("Since the p-value (", formatC(p_value, format = "e", digits = 5), ") is less than the significance level (α = ", alpha, "), we reject the null hypothesis."))
  } else {
    return(paste("Since the p-value (", formatC(p_value, format = "e", digits = 5), ") is greater than or equal to the significance level (α = ", alpha, "), we retain the null hypothesis."))
  }
}

#----------------------------------------------------------------------------

server <- function(input, output, session) {
  
  # UI elements for selecting variables
  output$var_select_1 <- renderUI({
    if (input$select == 1) {
      selectInput("var1", "Choose the first categorical variable", choices = names(df)[sapply(df, is.factor)], selected = names(df)[1])
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
        simulated <- any(chisq.test(contingency_table)$expected < 5)
        chisq_test <- chisq.test(contingency_table, simulate.p.value = simulated)
        
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
      HTML(paste0("<strong>P-value:</strong> ", formatC(results$test$p.value, format = "e", digits = 5)))
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
            geom_histogram(binwidth = 0.75, fill = "blue", color = "black") +
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
      # Plot normal Chi-Square p-value distribution
      p_value <- results$test$p.value
      
      ggplot(data.frame(x = c(0, 1)), aes(x = x)) +
        stat_function(fun = function(x) dchisq(x, df = results$test$parameter)) +
        geom_vline(aes(xintercept = p_value), color = "red", linetype = "dashed") +
        labs(title = "Chi-Square P-value Distribution", x = "P-value", y = "Density")
    }
  })
  
  output$simulation_flag <- reactive({
    results <- test_results()
    if (!is.null(results)) {
      return(results$simulated)
    }
    return(FALSE)  # Default to false if no results
  })
  
  outputOptions(output, "simulation_flag", suspendWhenHidden = FALSE)
  
  # Display warning if simulation was used
  output$warning_message <- renderUI({
    results <- test_results()
    if (!is.null(results) && results$simulated) {
      p("Warning: Simulated p-values used due to small expected counts. Assumptions have not been met.", style = "color: red; font-weight: bold;")
    } else {
      NULL
    }
  })
  
  # Mosaic plot for Chi-Square Test for Independence
  output$plot <- renderPlot({
    results <- test_results()
    if (!is.null(results) && !is.null(results$observed_table)) {
      mosaicplot(results$observed_table, main = paste("Mosaic Plot of", input$var1, "and", input$var2), color = TRUE)
    }
  })
  
  # Render expected contingency table
  output$expected_table <- DT::renderDataTable({
    results <- test_results()
    if (!is.null(results)) {
      datatable(as.data.frame.matrix(results$expected_table), options = list(dom = 't', paging = FALSE, searching = FALSE), rownames = TRUE)
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

}

