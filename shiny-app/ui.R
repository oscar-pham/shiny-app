#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(ggplot2)
library(DT)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Chi-Square Hypothesis Testing App"),
  
  # Tab layout
  tabsetPanel(
    
    # First tab for data input
    tabsetPanel(
      
      # First tab for hypothesis testing
      tabPanel("Hypothesis Testing",
               sidebarLayout(
                 sidebarPanel(
                   selectInput(
                     "select",
                     label = "Choose a test to perform",
                     choices = list(
                       "Chi-Squared Test for Independence" = 1,
                       "Chi-Squared Goodness of Fit Test" = 2,
                       "One-sample t-test" = 3,
                       "Two-sample t-test" = 4
                     ),
                     selected = 1
                   ),
                   
                   # These UI elements will dynamically display when test 1 (Chi-Squared Test for Independence) is selected
                   uiOutput("var_select_1"),
                   uiOutput("var_select_2")
                 ),
                 
                 # Main panel to display results and plot
                 mainPanel(
                   conditionalPanel(
                     condition = "input.select == 1",
                     h1("Chi-Squared Test for Independence Hypothesis Testing"),
                     h3("Hypothesis:"),
                     p(uiOutput("dynamic_hypothesis")),
                     h3("Assumptions:"),
                     p("We assume that all observations are independent. To confirm this assumption, we will check whether all expected cell counts are greater than or equal to 5. This will be done by creating a contingency table for the expected cell counts of the two selected variables."),
                     DT::dataTableOutput("expected_table"),
                     verbatimTextOutput("result"),
                     plotOutput("plot")
                   ),
                 )
               )
      ),
    
    # Second tab for results
    tabPanel("Results",
             mainPanel(
             )
    ),
    
    # Third tab for test overview
    tabPanel("Overview",
             h4("About the Chi-Square Test"),
             p("The Chi-Square test is used to determine if there is a significant difference between the observed and expected frequencies in one or more categories."),
             p("It compares the observed frequencies from your data with the expected frequencies based on a specific hypothesis."),
             h4("Formula"),
             p("The formula for the Chi-Square statistic is:"),
             tags$ul(
               tags$li("X² = Σ((Observed - Expected)² / Expected)"),
               tags$li("Degrees of freedom (df) = number of categories - 1")
             ),
             h4("How to Interpret"),
             p("If the p-value is less than 0.05, we reject the null hypothesis, indicating that there is a statistically significant difference between the observed and expected data.")
    )
  )
)
)

