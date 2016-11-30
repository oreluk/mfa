
# Define UI for application that draws a histogram
shinyUI(fluidPage(

  # Application title
  titlePanel("Multiple Factor Analysis Visualization"),

  # Sidebar with input widgets
  sidebarLayout(
    sidebarPanel(
      radioButtons("radio", label = h3("Select a plot to display:"),
                   choices = list("Factor Scores" = 1, "Partial Factor Scores" = 2,
                                  "Variable Loadings" = 3, 'Biplot (Partial Fac + Loadings)' = 4,
                                  "Eigenvalue Bar Plot" = 5,
                                  "Inertia Pie Chart" = 6),
                   selected = 1),
      numericInput("d1",
                   label = h3("X-axis Dimension:"),
                   value = 1,
                   min = 1, max = ncol(mfa_obj$factorScores)),
      numericInput("d2",
                   label = h3("Y-axis Dimension:"),
                   value = 2,
                   min = 1, max = ncol(mfa_obj$factorScores)),
      numericInput("t",
                   label = h3("Table Number (for partial factor or loadings):"),
                   value = 1,
                   min = 1, max = length(mfa_obj$partialFactorScores))
      ),

    # Plot the thing
    mainPanel(
      plotOutput(outputId = "main_plot", width = '90%', height=600)
    )
  )
))
