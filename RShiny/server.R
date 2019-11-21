server <- function(input, output) { #####
  
  #RenderPlot function talks with the plotOutput function and tells it what   to graph. This is where interaction can be built in.
  #Let's create a graph here comparing Revenue and Metascore.
  #PLOT 1
  output$plot1 <- renderPlot({
    ggplot(filter(kg, PC1 >= input$slide & PC1 <= input$slide2), aes(PC1, PC2, color=Population))  + geom_point() + xlim(x.low, x.high) + ylim(y.low, y.high) + scale_colour_manual(values=cols)
  })
  output$hover_info <- renderPrint({
    if(!is.null(input$plot_hover)){
      hover=input$plot_hover
      index <- kg$PC1==round(hover$x,4) & kg$PC2==round(hover$y,4)
      #cat("IID\n")
      if (!is.null(index))
        #print(hover$x)
      #kg[index,"Population"]
      paste0("x=", round(input$plot_hover$x,4), ";y=", round(input$plot_hover$y,4))
      
    }
  })

  
  
  #Let's create a graph here looking at Metascore by Year/Genre.
  #PLOT 2
  # facet_wrap(as.formula(paste("~", as.name(input$selector))))
  # cols[levels(as.factor(kg$Population))%in%input$Population])
  output$plot2 <- renderPlot({
    ggplot(filter(kg, Population==input$Population), aes(PC1, PC2)) + geom_point(color=cols[levels(as.factor(kg$Population))%in%input$Population]) + facet_wrap(~ Population) + xlim(x.low, x.high) + ylim(y.low, y.high)
  })
  #Let's create a graph here looking at different variables against metascore, facet wrapped by Year/Genre
  #PLOT 3
  output$plot3 <- renderPlot({
    ggplot(filter(kg, PC1 >= input$slide & PC1 <= input$slide2), aes(PC1, PC2, color=Population))  + geom_point() + facet_wrap(~ Population)
  })
  #The RenderDataTable function talks with the datatable output function and tells it what put in a table. n.
  #Let's create a table with our kg Data in it.
  #DataTable1
  
  # Let's summarize our data to look at key elements by Lead Actor
  #DataTable2
  output$dt2 <- renderDataTable({
    kg %>%
      group_by(Population, AFF) %>% 
      summarize(n()) %>% 
      datatable(rownames = FALSE)
  })
  
  #RenderText interacts with the textoutput function.
  #TEXT 1
  output$text1 <- renderText({paste("Rendered Text Goes Here" ,                              input$text_here) })
  
  ## For the Download Button, you need to create several things on the server side. You may also need to run the app through a browser instead of in an R Window.
  # Reactive value for selected dataset ----
  
  
  # Table of selected dataset ----

} 
