library(shiny)
library(ggplot2)
library(DT)
library(sortable)
library(shinyWidgets)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Chi-Square Hypothesis Testing App"),
  
  # Tab layout
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
                     "Two-sample t-test" = 3
                   ),
                   selected = 3
                 ),
                 
                 # These UI elements will dynamically display when test 1 (Chi-Squared Test for Independence) is selected
                 conditionalPanel(
                   condition = "input.select == 1",
                   uiOutput("var_select_1"),
                   uiOutput("var_select_2")
                 ),
                 
                 conditionalPanel(
                   condition = "input.select == 2",
                   selectInput("var_gof", "Choose the categorical variable", choices = NULL),
                   selectInput("distribution", "Choose distribution for expected values",
                               choices = list("Normal" = "normal", "Poisson" = "poisson", "Custom" = "custom"), selected = "normal"),
                   conditionalPanel(
                     condition = "input.distribution == 'custom'",
                     textInput("expected_values", "Enter expected proportions (comma-separated)", "0.25, 0.25, 0.25, 0.25")
                   )
                 ),
                 conditionalPanel(
                   condition = "input.select == 3",
                   # Dropdown to select the quantitative variable
                   selectInput(
                     inputId = "quantitative_var",
                     label = "Select a Quantitative Variable:",
                     choices = NULL
                   ),
                   
                   # Dropdown to select the categorical variable
                   selectInput(
                     inputId = "categorical_var",
                     label = "Select a Categorical Variable:",
                     choices = NULL
                   ),
                   
                   switchInput(inputId = "perm", label = "Permutation", value = FALSE, size = "small"),
                   switchInput(inputId = "welch", label = "Welch", value = FALSE, size = "small"),
                   
                   # Instructions for users
                   h4("Drag and drop categories into Sample 1 and Sample 2 boxes"),
                   
                   # Drag-and-drop containers for Sample 1 and Sample 2
                   fluidRow(
                     uiOutput("bucket_list_ui")
                   )
                 ),
                 
                 
                 
                 # Slider for selecting significance level (alpha)
                 sliderInput("alpha", "Significance Level (α)", min = 0.01, max = 0.1, value = 0.05, step = 0.01)
               ),
               
               # Main panel to display results and plot
               mainPanel(
                 conditionalPanel(
                   condition = "input.select == 1",
                   h1("Chi-Squared Test for Independence Hypothesis Testing"),
                   conditionalPanel(
                     condition = "input.var1 == input.var2",
                     p("Warning: Both selected variables are the same. Please select different variables.", style = "color: red; font-weight: bold;")
                   ),
                   
                   conditionalPanel(
                     condition = "input.var1 != input.var2",
                     h3("Hypothesis:"),
                     p(uiOutput("dynamic_hypothesis")),
                     h3("Assumptions:"),
                     p("We assume that all observations are independent. We will also assume that all expected cell counts are at least 5. This assumption can be checked by creating a contingency table for the expected cell counts of the selected variable."),
                     DT::dataTableOutput("expected_table"),
                     br(),
                     uiOutput("warning_message"),
                     h3("Test statistic:"),
                     uiOutput("t_result"),
                     br(),
                     conditionalPanel(
                       condition = "output.simulation_flag == true",
                       plotOutput("plot_pvalues")  # Show simulated p-values histogram
                     ),
                     
                     conditionalPanel(
                       condition = "output.simulation_flag == false",
                       plotOutput("plot_pvalue_graph")  # Show normal p-value graph
                     ),
                     h3("P-value:"),
                     uiOutput("p_result"),
                     h3("Conclusion:"),
                     uiOutput("conclusion"),
                     br(),
                     br(),
                     br()
                   )
                 ),
                 
                 conditionalPanel(
                   condition = "input.select == 2",
                   h1("Chi-Squared Goodness of Fit Test"),
                   h3("Hypothesis:"),
                   p(uiOutput("gof_hypothesis")),
                   h3("Assumptions:"),
                   p("We assume that all observations are independent. We will also assume that all expected cell counts are at least 5. This assumption can be checked by creating a contingency table for the expected cell counts of the selected variable."),
                   h4("Expected vs. Observed:"),
                   DT::dataTableOutput("gof_table"),
                   uiOutput("warning_message"),
                   h3("Test statistic:"),
                   uiOutput("gof_t_result"),
                   plotOutput("chi_squared_plot"),
                   h3("P-value:"),
                   uiOutput("gof_p_result"),
                   h3("Conclusion:"),
                   uiOutput("gof_conclusion")
                 ),
                 conditionalPanel(
                   condition = "input.select == 3",
                   h1("Two-sample t-Test"),
                   
                   # Check if the correct number of samples are selected
                   conditionalPanel(
                     condition = "input.rank_list_1 && input.rank_list_2 && (input.rank_list_1.length != 1 || input.rank_list_2.length != 1)",
                     br(),
                     h4("Please select exactly one sample for each group. No more, no less.")
                   ),
                   
                   # Render the rest only if exactly one sample is selected per group
                   conditionalPanel(
                     condition = "input.rank_list_1 && input.rank_list_2 && (input.rank_list_1.length == 1 && input.rank_list_2.length == 1)",
                   h3("Hypothesis:"),
                   p(uiOutput("dynamic_hypothesis_ttest")),
                   h3("Assumptions:"),
                   p("We assume that all observations are independent. We assume the data in each group is approximately normal and the groups have equal variance. To check normality, we will use Q-Q plots for each sample, and to check for equal variance, we will use side-by-side box plots."),
                   h4("Q-Q plot"),
                   p("In a Q-Q plot, if the data points closely follow the red dotted line, it indicates that the data is approximately normally distributed. Deviations from this line suggest that the data may not follow a normal distribution, with systematic patterns like curves indicating skewness and outliers appearing as points far from the line. If normality is violated, consider enabling the 'Permutation' option to run a permutation test."),
                   plotOutput("qq_plot"),
                   h4("Box plot"),
                   p(" In a box plot, if the boxes (interquartile ranges) for two groups are of similar length, it suggests that the groups may have equal variances. Significant differences in box height imply unequal variances, and outliers are shown as individual points beyond the whiskers. If unequal variances are detected, consider enabling the 'Welch' option to perform a Welch's t-test."),
                   plotOutput("box_plot"),
                   h3("Test statistic:"),
                   uiOutput("t_t_result"),
                   h3("P-value:"),
                   uiOutput("t_p_result"),
                   h3("Conclusion:"),
                   uiOutput("t_conclusion")
                   
                  )
                 )
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
