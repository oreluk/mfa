
# Define UI for application that draws a histogram
shinyUI(fluidPage(

  # Application title
  titlePanel("Multiple Factor Analysis Visualization"),

  # Sidebar with input widgets
  sidebarLayout(
    sidebarPanel(
      radioButtons("radio", label = h3("Select a plot to display:"),
                   choices = list("Factor Scores" = 1, "Partial Factor Scores" = 2,
                                  "Variable Loadings" = 3, "Eigenvalue Bar" = 4),
                   selected = 1),
      numericInput("d1",
                   label = h3("X-axis Dimension:"),
                   value = 1),
      numericInput("d2",
                   label = h3("Y-axis Dimension:"),
                   value = 2),
      numericInput("t",
                   label = h3("Table Number (for partial factor or loadings):"),
                   value = 1)
      ),

    # Plot the thing
    mainPanel(
      plotOutput(outputId = "main_plot", width = '100%')
    )
  )
))
