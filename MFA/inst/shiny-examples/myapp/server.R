shinyServer(function(input, output) {

  output$main_plot <- renderPlot({

    #load the data
    d <- loadWineData()
    s = list(  seq(2,7), seq(8,13), seq(14,19), seq(20,24),
               seq(25,30), seq(31,35), seq(36,39), seq(40,45),
               seq(46,50), seq(51,54) )
    a = mfa(d, s)

    #should plot the first 2 factor scores
    X = a$factorScores[,1]
    Y = a$factorScores[,2]
    x = a$partialFactorScores[[1]][,1] #fac score first table, 1st comp
    y = a$partialFactorScores[[1]][,2] #fac score first table, 2nd comp
    plot(X, Y,
         type = "p", pch=19, cex=2,xlim=c(-1,1),ylim=c(-1,1))
    text(X,Y,labels=1:length(X),col='red',xlim=c(-1,1),ylim=c(-1,1))
    abline(v=0,h=0)
    title('Factor Scores for first 2 components')

    if (input$partial_facs) {
      plot(x, y,
           type = "p", pch=19, cex=2,xlim=c(-1,1),ylim=c(-1,1))
      text(x,y,labels=1:length(X),col='red',xlim=c(-1,1),ylim=c(-1,1))
      abline(v=0,h=0)
      title('Partial Factor Scores for Table 1: first 2 components')
    }

  })
})
