library(shiny)
library(ggplot2)
library(DT)
library(sortable)
library(shinyWidgets)
library(bslib)

ui <- fluidPage(
  page_navbar(
    id = "navbar",
    theme = bs_theme(version = 5, bootswatch = "sketchy"),
    title = "DATA2902: Shiny App",
    
    # First Tab Panel (Home)
    tabPanel(
      title = "Home", 
      icon = icon("home"),  # Add an icon to the tab
      h3("Welcome to the Hypothesis Testing Shiny App!"),
      p("In this Shiny App, we will analyze the DATA2X02 survey data using a range of powerful analysis tools!"),
      p("This Shiny App is designed with a focus on functionality, providing easy-to-use analysis tools to simplify the process and help users reach meaningful conclusions efficiently."),
      p("The Hypothesis Testing Shiny App is built entirely using RStudio and its packages. The data was pre-cleaned as part of Assignment 1 in DATA2x02."),
      img(),
      card(
        card_header("Packages"),  # Card Header
        card_body(
          tags$div("Shiny", 
                   style = "font-size: 20px; font-weight: bold;"),
          p("Shiny is the core package that powers this web app, enabling interactive data exploration and visualizations."),
          tags$div("ggplot2", 
                   style = "font-size: 20px; font-weight: bold;"),
          p("ggplot2 is used for creating the static plots in the app, offering a simple and effective way to visualize data."),
          tags$div("DT", 
                   style = "font-size: 20px; font-weight: bold;"),
          p("DT is responsible for the interactive data tables, allowing users to sort, filter, and search datasets easily."),
          tags$div("Sortable", 
                   style = "font-size: 20px; font-weight: bold;"),
          p("sortable enables the drag-and-drop functionality, allowing users to rearrange or group items intuitively."),
          tags$div("ShinyWidgets", 
                   style = "font-size: 20px; font-weight: bold;"),
          p("shinyWidgets adds extra UI components like sliders and switches, enhancing the app's interactivity."),
          tags$div("bslib", 
                   style = "font-size: 20px; font-weight: bold;"),
          p("bslib is used to apply custom themes, giving the app a polished and modern look.")
        )
      )
    ),
    
    tabPanel(title ="Hypothesis testing",
             icon = icon("balance-scale"),
             sidebarLayout(
               sidebarPanel(
                 class = "border-primary mb-3",  # Bootstrap classes for text color, background color, and margin-bottom
                 style = "max-width: 20rem;",
                 selectInput(
                   "select",
                   label = "Choose a test to perform",
                   choices = list(
                     "Chi-Squared Test for Independence" = 1,
                     "Chi-Squared Goodness of Fit Test" = 2,
                     "Two-sample t-test" = 3
                   ),
                   selected = 1,
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
                   tags$fieldset(
                     tags$legend("Options"),
                     
                   # First Switch: Permutation switch (unchecked by default)
                   div(
                     class = "form-check form-switch",
                     tags$input(
                       class = "form-check-input", 
                       type = "checkbox", 
                       id = "flexSwitchCheckDefault",
                       onchange = "Shiny.setInputValue('perm', this.checked);"  # Syncs with Shiny input for 'perm'
                     ),
                     tags$label(
                       class = "form-check-label", 
                       `for` = "flexSwitchCheckDefault",
                       "Permutation"  # Label for the first switch
                     )
                   ),
                   
                   # Second switch (checked by default)
                   div(
                     class = "form-check form-switch",
                     tags$input(
                       class = "form-check-input", 
                       type = "checkbox", 
                       id = "flexSwitchCheckChecked",
                       onchange = "Shiny.setInputValue('welch', this.checked);"  # Syncs with Shiny input for 'welch'
                     ),
                     tags$label(
                       class = "form-check-label", 
                       `for` = "flexSwitchCheckChecked",
                       "Welch"  # Label for the second switch
                     )
                   ),
                   br(),

                     # Instructions for users
                     tags$legend("Select levels for Sample 1 and Sample 2"),
                   p("Drag each element from the list of variable levels and drop one onto each sample box. This will divide the dataset into two samples."),
                   # Drag-and-drop containers for Sample 1 and Sample 2
                   div(
                   fluidRow(
                     uiOutput("bucket_list_ui")
                      )
                    )
                   ),
                   radioButtons(
                     inputId = "alternative", 
                     label = "Alternative Hypothesis:",
                     choices = list("Greater" = "greater", 
                                    "Lesser" = "less", 
                                    "Two-sided" = "two.sided"),
                     selected = "greater" 
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
                     tags$div("Hypothesis", 
                              style = "font-size: 30px; font-weight: bold;"),
                     p(uiOutput("dynamic_hypothesis")),
                     tags$div("Assumptions", 
                              style = "font-size: 30px; font-weight: bold;"),
                     p("We assume that all observations are independent. We will also assume that all expected cell counts are at least 5. This assumption can be checked by creating a contingency table for the expected cell counts of the selected variable."),
                     DT::dataTableOutput("expected_table"),
                     br(),
                     conditionalPanel(
                       condition = "output.simulation_flag == true",
                       p("Warning: Simulated p-values used due to small expected counts. Assumptions have not been met.", style = "color: red; font-weight: bold;")
                     ),
                     tags$div("Test statistic", 
                              style = "font-size: 30px; font-weight: bold;"),
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
                     tags$div("P-value", 
                              style = "font-size: 30px; font-weight: bold;"),
                     uiOutput("p_result"),
                     tags$div("Conclusion", 
                              style = "font-size: 30px; font-weight: bold;"),
                     uiOutput("conclusion"),
                     br(),
                     br(),
                     br()
                   )
                 ),
                 
                 conditionalPanel(
                   condition = "input.select == 2",
                   h1("Chi-Squared Goodness of Fit Test"),
                   tags$div("Hypothesis", 
                            style = "font-size: 30px; font-weight: bold;"),
                   p(uiOutput("gof_hypothesis")),
                   tags$div("Assumptions", 
                            style = "font-size: 30px; font-weight: bold;"),
                   p("We assume that all observations are independent. We will also assume that all expected cell counts are at least 5. This assumption can be checked by creating a contingency table for the expected cell counts of the selected variable."),
                   tags$div("Expected vs. Observed", 
                            style = "font-size: 25px; font-weight: bold;"),
                   DT::dataTableOutput("gof_table"),
                   br(),
                   conditionalPanel(
                     condition = "output.simulation_flag == true",
                     p("Warning: Simulated p-values used due to small expected counts. Assumptions have not been met.", style = "color: red; font-weight: bold;")
                   ),
                   tags$div("Test statistic", 
                            style = "font-size: 30px; font-weight: bold;"),
                   uiOutput("gof_t_result"),
                   plotOutput("chi_squared_plot"),
                   tags$div("P-value", 
                            style = "font-size: 30px; font-weight: bold;"),
                   uiOutput("gof_p_result"),
                   tags$div("Conclusion", 
                            style = "font-size: 30px; font-weight: bold;"),
                   uiOutput("gof_conclusion"),
                   br(),
                   br(),
                   br()
                 ),
                 conditionalPanel(
                   condition = "input.select == 3",
                   h1("Two-sample t-Test"),
                   
                   # Check if the correct number of samples are selected
                   conditionalPanel(
                     condition = "input.rank_list_1 && input.rank_list_2 && (input.rank_list_1.length != 1 || input.rank_list_2.length != 1)",
                     br(),
                     tags$div("Please select exactly one sample for each group. No more, no less.", 
                              style = "font-size: 30px; font-weight: bold;"),
                   ),
                   
                   # Render the rest only if exactly one sample is selected per group
                   conditionalPanel(
                     condition = "input.rank_list_1 && input.rank_list_2 && (input.rank_list_1.length == 1 && input.rank_list_2.length == 1)",
                     tags$div("Hypothesis", 
                              style = "font-size: 30px; font-weight: bold;"),
                     p(uiOutput("dynamic_hypothesis_ttest")),
                     tags$div("Assumptions", 
                              style = "font-size: 30px; font-weight: bold;"),
                     p("We assume that all observations are independent. We assume the data in each group is approximately normal and the groups have equal variance. To check normality, we will use Q-Q plots for each sample, and to check for equal variance, we will use side-by-side box plots."),
                     tags$div("Q-Q Plot", 
                              style = "font-size: 25px; font-weight: bold;"),
                     p("In a Q-Q plot, if the data points closely follow the red dotted line, it indicates that the data is approximately normally distributed. Deviations from this line suggest that the data may not follow a normal distribution, with systematic patterns like curves indicating skewness and outliers appearing as points far from the line. If normality is violated, consider enabling the 'Permutation' option to run a permutation test."),
                     plotOutput("qq_plot"),
                     tags$div("Box Plot", 
                              style = "font-size: 25px; font-weight: bold;"),
                     p(" In a box plot, if the boxes (interquartile ranges) for two groups are of similar length, it suggests that the groups may have equal variances. Significant differences in box height imply unequal variances, and outliers are shown as individual points beyond the whiskers. If unequal variances are detected, consider enabling the 'Welch' option to perform a Welch's t-test."),
                     plotOutput("box_plot"),
                     tags$div("Test statistic", 
                              style = "font-size: 30px; font-weight: bold;"),
                     uiOutput("t_t_result"),
                     tags$div("P-value", 
                              style = "font-size: 30px; font-weight: bold;"),
                     uiOutput("t_p_result"),
                     tags$div("Conclusion", 
                              style = "font-size: 30px; font-weight: bold;"),
                     uiOutput("t_conclusion"),
                     br(),
                     br(),
                     br()
                   )
                 )
               )
             )
             
    ),
    tabPanel(title = "Plotting",
             width = 3,
             icon = icon("chart-bar"),
             sidebarLayout(
               sidebarPanel(
                 class = "text-white bg-secondary mb-3",  # Bootstrap classes for text color, background color, and margin-bottom
                 style = "max-width: 20rem;",           # Inline style to limit the card's width
                 tags$div("Editor", 
                          style = "font-size: 30px; font-weight: bold;"), 
                 selectInput(
                   "plotting",
                   label = "Choose a plot",
                   choices = list(
                     "Bar Plot" = 1,
                     "Histogram" = 2,
                     "Scatterplot" = 3,
                     "Density Plot" = 4,
                     "Box Plot" = 5
                   ),
                   selected = 1
                 ),
                 conditionalPanel(
                   condition = "input.plotting == 1",
                   uiOutput("var_select_cat")
                 ),
                 conditionalPanel(
                   condition = "input.plotting == 2",
                   uiOutput("var_select_num3")  
                 ),
                 conditionalPanel(
                   condition = "input.plotting ==  3",
                   uiOutput("var_select_num1"),
                   uiOutput("var_select_num2")
                 ),
                 conditionalPanel(
                   condition = "input.plotting == 4",
                   uiOutput("var_select_num4")
                 ),
                 conditionalPanel(
                   condition = "input.plotting == 5",
                   uiOutput("var_select_num5"),
                   uiOutput("var_select_cat2")
                 )
                 
               ),
               mainPanel(
                 h2("Visualization output"),
                 conditionalPanel(
                   condition = "input.plotting == 1",
                   plotOutput("barplot")
                 ),
                 conditionalPanel(
                   condition = "input.plotting == 2",
                   plotOutput("histogram")
                 ),
                 conditionalPanel(
                   condition = "input.plotting == 3",
                   conditionalPanel(
                     condition = "input.numerical1 != input.numerical2",
                     plotOutput("scatterplot")
                   ),
                   conditionalPanel(
                     condition = "input.numerical1 == input.numerical2",
                     p("Warning: Both selected variables are the same. Please select different variables.", style = "color: red; font-weight: bold;")
                   )
                 ),
                 conditionalPanel(
                   condition = "input.plotting == 4",
                   plotOutput("densityPlot")
                 ),
                 conditionalPanel(
                   condition = "input.plotting == 5",
                   plotOutput("boxplot")
                 )
               )
             )
             
             
             ),
    tabPanel(
      title = "Data Table",
      icon = icon("table"),
      DTOutput("data_table")
    )
  )
)

