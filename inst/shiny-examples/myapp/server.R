shinyServer(function(input, output) {

  output$main_plot <- renderPlot({

    if (input$plotchoice == 1){
      #check that active_obs_names exists, if so use it
      if (exists('active_obs_names') & exists('active_col_vec')){
        plot_compromise(mfa_obj, dim1 = input$d1, dim2 = input$d2,
                         obsnames = active_obs_names,
                         textcolor = active_col_vec,
                         cexlab = 1.7, cexmain = 2,
                         sz = 2, app = TRUE,
                         dotcol=input$dotcolor)}
      else if (exists('active_obs_names')) {
        plot_compromise(mfa_obj, dim1 = input$d1, dim2 = input$d2,
                         obsnames = active_obs_names,
                         cexlab = 1.7, cexmain = 2,
                         sz = 2,app = TRUE,
                         dotcol = input$dotcolor)}
      else{
        plot_compromise(mfa_obj, dim1 = input$d1, dim2 = input$d2,
                        cexlab = 1.7, cexmain = 2,
                        sz = 2, app = TRUE,
                        dotcol = input$dotcolor)}
    }

    if (input$plotchoice == 2){
      #check that active_obs_names exists, if so use it
      if (exists('active_obs_names') & exists('active_col_vec')){
        plot_partial_fac(mfa_obj, dim1 = input$d1, dim2=input$d2,
                       table = input$t, obsnames = active_obs_names,
                       textcolor = active_col_vec,
                       cexlab = 1.7, cexmain = 2,
                       sz = 2, app = TRUE,
                       dotcol = input$dotcolor)}
      else if (exists('active_obs_names')) {
        plot_partial_fac(mfa_obj, dim1 = input$d1, dim2 = input$d2,
                         table = input$t, obsnames = active_obs_names,
                         cexlab = 1.7, cexmain = 2,
                         sz = 2, app = TRUE,
                         dotcol = input$dotcolor)}
      else{
        plot_partial_fac(mfa_obj, dim1=input$d1, dim2=input$d2,
                        table = input$t,
                        cexlab = 1.7, cexmain = 2,
                        sz = 2, app = TRUE,
                        dotcol = input$dotcolor)}
    }

    if (input$plotchoice == 3){
      #check if active_var_names exists, if so use
      if (exists('active_var_names')){
      plot_loading(mfa_obj,dim1 = input$d1,dim2 = input$d2,
                   table = input$t, varnames = active_var_names,
                   cexlab = 1.7, cexmain = 2,
                   sz = 2, app = TRUE,
                   dotcol = input$dotcolor)}
      else{
        plot_loading(mfa_obj, dim1 = input$d1, dim2 = input$d2,
                     table = input$t,
                     cexlab = 1.7, cexmain = 2,
                     sz = 2, app = TRUE,
                     dotcol = input$dotcolor)}
    }

    if (input$plotchoice == 4){
      #check if active_var_names exists, if so use
      if (exists('active_var_names') & exists('active_obs_names')){
        plot_biplot(mfa_obj, dim1 = input$d1, dim2 = input$d2,
                    table = input$t, varnames = active_var_names,
                    cexlab = 1.7, cexmain = 1.8,
                    sz = 2, app = TRUE,
                    obsnames = active_obs_names,
                    dotcol_vl = input$dotcolor,
                    dotcol_pf = input$dotcolor,
                    cexlegend = 1.2)}
      else{
        plot_biplot(mfa_obj, dim1=input$d1, dim2=input$d2,
                    table = input$t,
                    cexlab = 1.7, cexmain = 1.8,
                    sz = 2, app = TRUE,
                    dotcol_vl = input$dotcolor,
                    dotcol_pf = input$dotcolor,
                    cexlegend = 1.2)}
    }

    if (input$plotchoice == 5){
        plot_ev(mfa_obj, cexaxis = 2, cexnames = 2,
                cexlab = 2, cexmain = 2, app = TRUE)}

    if (input$plotchoice == 6){
      plot_inertia_pie(mfa_obj, cexmain = 2,
                       cexlab = 1.5, app = TRUE)}

    }

  )
  })
