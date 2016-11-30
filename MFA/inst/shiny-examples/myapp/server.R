shinyServer(function(input, output) {

  output$main_plot <- renderPlot({

    if (input$radio==1){
      #check that active_obs_names exists, if so use it
      if (exists('active_obs_names') & exists('active_col_vec')){
        plot_compromise(mfa_obj, dim1=input$d1, dim2=input$d2,
                         obsnames=active_obs_names,
                         textcolor=active_col_vec,
                         cexlab=1.7, cexmain=2,
                         sz=2,app=TRUE)}
      else if (exists('active_obs_names')) {
        plot_compromise(mfa_obj, dim1=input$d1, dim2=input$d2,
                         obsnames=active_obs_names,
                         cexlab=1.7, cexmain=2,
                        sz=2,app=TRUE)}
      else{
        plot_compromise(mfa_obj, dim1=input$d1, dim2=input$d2,
                        cexlab=1.7, cexmain=2,
                        sz=2,app=TRUE)}
    }

    if (input$radio==2){
      #check that active_obs_names exists, if so use it
      if (exists('active_obs_names') & exists('active_col_vec')){
        plot_partial_fac(mfa_obj, dim1=input$d1, dim2=input$d2,
                       table=input$t, obsnames=active_obs_names,
                       textcolor=active_col_vec)}
      else if (exists('active_obs_names')) {
        plot_partial_fac(mfa_obj, dim1=input$d1, dim2=input$d2,
                         table=input$t, obsnames=active_obs_names)}
      else{
        plot_partial_fac(mfa_obj, dim1=input$d1, dim2=input$d2,
                        table=input$t)}
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

    if (input$radio==5){
      plot_inertia_pie(mfa_obj)}

    }

  )
  })
