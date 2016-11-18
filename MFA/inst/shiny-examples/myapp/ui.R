shinyUI(bootstrapPage(

  #selectInput(inputId = "zoom",
  #            label = "Zoom Level:",
  #            choices =  c(1, 2, 3, 4),
  #            selected = 1),

  checkboxInput(inputId = "partial_facs",
                label = strong("Show partial factors"),
                value = FALSE),

  plotOutput(outputId = "main_plot", height = "300px")

  # Display this only if the density is shown
  #conditionalPanel(condition = "input.density == true",
  #                 sliderInput(inputId = "bw_adjust",
  #                             label = "Bandwidth adjustment:",
  #                             min = 0.2, max = 2, value = 1, step = 0.2)
  #)

))
