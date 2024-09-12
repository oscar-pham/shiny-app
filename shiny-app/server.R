#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
library(shiny)
library(gt)
library(DT)

df <- read.csv("df_cleaned.csv", header = TRUE, sep = ",")
# Automatically convert all character columns to factors
df[] <- lapply(df, function(x) if(is.character(x)) as.factor(x) else x)

server <- function(input, output, session) {
output$var_select_1 <- renderUI({
  if (input$select == 1) {
    selectInput(
      "var1",
      label = "Choose the first categorical variable",
      choices = names(df)[sapply(df, is.factor)],  # Only show categorical variables
      selected = names(df)[1]
    )
  }
})

output$var_select_2 <- renderUI({
  if (input$select == 1) {
    selectInput(
      "var2",
      label = "Choose the second categorical variable",
      choices = names(df)[sapply(df, is.factor)],  # Only show categorical variables
      selected = names(df)[2]
    )
  }
})

# Reactive expression to perform the appropriate test based on user input
test_results <- reactive({
  if (input$select == 1 && !is.null(input$var1) && !is.null(input$var2)) {
    var1 <- df[[input$var1]]
    var2 <- df[[input$var2]]
    contingency_table <- table(var1, var2)
    chisq_test <- chisq.test(contingency_table)
    list(
      test = chisq_test, 
      observed_table = contingency_table,  # Observed counts
      expected_table = chisq_test$expected  # Expected counts
    )
  } else {
    NULL
  }
})

# Reactive expression to generate the dynamic hypothesis statement
output$dynamic_hypothesis <- renderUI({
  if (!is.null(input$var1) && !is.null(input$var2)) {
    tagList(
      # Null Hypothesis with H0 subscript
      strong("Null Hypothesis "), 
      " (H", tags$sub("0"), "): The variable ", 
      code(input$var1), " is independent of the variable ", 
      code(input$var2),
      
      br(),  # Line break
      br(),
      
      # Alternative Hypothesis with H1 subscript
      strong("Alternative Hypothesis "), 
      " (H", tags$sub("1"), "): The variable ", 
      code(input$var1), " is not independent of the variable ", 
      code(input$var2)
    )
  } else {
    "Please select two variables."
  }
})


# Display the results of the test
output$result <- renderPrint({
  results <- test_results()
  if (!is.null(results)) {
    print(results$test)
    if (results$test$p.value < 0.05) {
      cat("Conclusion: Reject the null hypothesis (There is a significant relationship between variables)\n")
    } else {
      cat("Conclusion: Fail to reject the null hypothesis (No significant relationship)\n")
    }
  }
})

# Optionally, you can add a plot for the Chi-Squared Test for Independence
output$plot <- renderPlot({
  results <- test_results()
  if (input$select == 1 && !is.null(results) && !is.null(results$observed_table)) {
    if (nrow(results$observed_table) > 0 && ncol(results$observed_table) > 0) {
      mosaicplot(results$observed_table, 
                 main = paste("Mosaic Plot of", input$var1, "and", input$var2), 
                 color = TRUE)
    } else {
      # Display a message in the plot area if the table is empty or invalid
      plot.new()
      text(0.5, 0.5, "No data to plot", cex = 1.5)
    }
  }
})


# Render the expected contingency table using renderDataTable and bold the first column
output$expected_table <- DT::renderDataTable({
  results <- test_results()
  if (!is.null(results)) {
    datatable(as.data.frame.matrix(results$expected_table), 
              options = list(dom = 't', paging = FALSE, searching = FALSE), rownames = TRUE)
  }
})

} 

