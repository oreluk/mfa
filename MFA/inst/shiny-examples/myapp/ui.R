shinyUI(fluidPage(
  titlePanel("Multiple Factor Analysis Visualization"),

  #selectInput(inputId = "zoom",
  #            label = "Zoom Level:",
  #            choices =  c(1, 2, 3, 4),
  #            selected = 1),

  #checkboxInput(inputId = "partial_facs",
  #              label = strong("Show partial factors"),
  #              value = FALSE),

  fluidRow(
    column(3,
           radioButtons("radio", label = h3("Select a plot to display:"),
                        choices = list("Factor Scores" = 1, "Partial Factor Scores" = 2,
                                       "Variable Loadings" = 3),selected = 1))
  ),

  fluidRow(
    column(3,
           numericInput("d1",
                        label = h3("X-axis Dimension:"),
                        value = 1)),
    column(3,
           numericInput("d2",
                        label = h3("Y-axis Dimension:"),
                        value = 2)),
    column(3,
           numericInput("t",
                        label = h3("Table Number:"),
                        value = 1))

  ),

  plotOutput(outputId = "main_plot", height = "300px")

  # Display this only if the density is shown
  #conditionalPanel(condition = "input.density == true",
  #                 sliderInput(inputId = "bw_adjust",
  #                             label = "Bandwidth adjustment:",
  #                             min = 0.2, max = 2, value = 1, step = 0.2)
  #)

)
)
