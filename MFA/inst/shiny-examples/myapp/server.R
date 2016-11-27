shinyServer(function(input, output) {

  output$main_plot <- renderPlot({

    if (input$radio==1){
      plot_compromise(mfa_obj,dim1=input$d1,dim2=input$d2)
    }

    if (input$radio==2){
      plot_partial_fac(mfa_obj,dim1=input$d1,dim2=input$d2,
                       table=input$t)
    }

    if (input$radio==3){
      #check if active_var_names exists, if so use
      if (exists('active_var_names')){
      plot_loading(mfa_obj,dim1=input$d1,dim2=input$d2,
                   table=input$t,varnames=active_var_names)}
      else{
        plot_loading(mfa_obj,dim1=input$d1,dim2=input$d2,
                     table=input$t)}
    }

    if (input$radio==4){
        plot_ev(mfa_obj)}

    }

  )
  })
