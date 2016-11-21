shinyServer(function(input, output) {

  output$main_plot <- renderPlot({

    #load the data
    d <- loadWineData()
    s = list(  seq(2,7), seq(8,13), seq(14,19), seq(20,24),
               seq(25,30), seq(31,35), seq(36,39), seq(40,45),
               seq(46,50), seq(51,54) )
    a = mfa(d, s)

    if (input$radio==1){
      plot_compromise(a,dim1=input$d1,dim2=input$d2)
    }

    if (input$radio==2){
      plot_partial_fac(a,dim1=input$d1,dim2=input$d2,table=input$t)
    }

    if (input$radio==3){
      plot_loading(a,dim1=input$d1,dim2=input$d2,table=input$t)
    }

  })
})
