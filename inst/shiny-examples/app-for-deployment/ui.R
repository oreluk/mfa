# Define UI for application for mfa visualization
shinyUI(fluidPage(

  # Application title
  titlePanel("Multiple Factor Analysis Visualization"),

  # Sidebar with input widgets
  sidebarLayout(
    sidebarPanel(
      selectInput("plotchoice", label = h3("Select a plot to display:"),
                  choices = list("Factor Scores" = 1, "Partial Factor Scores" = 2,
                                 "Variable Loadings" = 3, 'Biplot (Partial Fac + Loadings)' = 4,
                                 "Eigenvalue Bar Plot" = 5,
                                 "Inertia Pie Chart" = 6),
                  selected = 1),
      conditionalPanel(
        h3("Additional Plotting Options:"),

        # if a X,Y plot, give dotcol, X,Y dims as possible user inputs
        condition = "input.plotchoice == 1 || input.plotchoice == 2 || input.plotchoice == 3 || input.plotchoice == 4",

        # Does the user want to see markers for where the exact numbers are?
        selectInput("dotcolor", label = h4("Show point markers?"),
                    choices = list("No"='white', 'Yes'='grey'),
                    selected = 'white'),

        # What dimension does the user want for the X-axis?
        numericInput("d1",
                     label = h4("X-axis Dimension:"),
                     value = 1,
                     min = 1, max = 12),

        # What dimension does the user want for the Y-axis?
        numericInput("d2",
                     label = h4("Y-axis Dimension:"),
                     value = 2,
                     min = 1, max = 12)
      ),
      conditionalPanel(
        # if loading, partial fac, or biplot, also show table number as option
        condition = "input.plotchoice == 2 || input.plotchoice == 3 || input.plotchoice == 4",
        # Which table does the user want to show?
        numericInput("t",
                     label = h4("Table Number:"),
                     value = 1,
                     min = 1, max = 10)
      )),

    # Plot
    mainPanel(
      plotOutput(outputId = "main_plot", width = '90%', height=600))

  )))
