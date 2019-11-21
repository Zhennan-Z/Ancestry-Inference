# Introduction to Shiny
# Creating your first Shiny App
# 11/14/2019

library(shiny)
library(tidyverse)
library(DT)
library(shinyWidgets)
library(plotly)

#You can do things globally in Shiny. These will be set once as the app is opened.
kg <- read.table("kg_ex.txt", header = TRUE, stringsAsFactors = FALSE)

x.low <- min(kg$PC1)
x.high <- max(kg$PC1)
y.low <- min(kg$PC2)
y.high <- max(kg$PC2)

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
n <- length(unique(kg$Population))
cols = gg_color_hue(n)

# The ui provides the user interface for your app.
# UI
ui <- fluidPage( #####
                 fluidRow(
                   column(width = 5,
                          verbatimTextOutput("hover_info"))),
                 #If you have used CSS/HTML Before, you can customize your styles here
                 #HTML/CSS/TAGS
                 tags$head(tags$style(
                   HTML('
                        #sidebar {
                        background-color: #abcdef;
                        opacity: .85;
                        }
                        #sidebar:hover {
                        background-color: #EEEEEE;
                        opacity: 1;
                        }
                        #main:hover {
                        background-color: #f8f8ff;
                        font-family: cursive;
                        }
                        #tabs {
                        font-family: fantasy;
                        }
                        '
                   )
                   )
                   ),
                 #TITLE PANEL: Provides a place to put a title...
                 titlePanel(h4("1000 Genome Refenece Information")),
                 
                 #SidebarLayout/Sidebarpanel: Layout tells where to place the sidebar, and the panel holds various ui components.
                 #SIDEBAR
                 sidebarLayout(position = "left",
                               sidebarPanel(id = "sidebar", paste("Sidebar: Often reserved for Inputs"), 
                                            br(), #HTML can be used to modify your UI 
                                            p(em(strong("This is how you would put in extra information"))), 
                                            
                                            #sliderInput, provides the ID for reference in the server side.      
                                            #SLIDE
                                            sliderInput(inputId = "slide", label = "Please enter the Minimum PC1", min = -0.04, max = 0.04, value = -0.04),
                                            
                                            #SLIDE2
                                            sliderInput(inputId = "slide2", label = "Please enter the Maximum PC1", min = -0.04, max = 0.04, value = 0.04),
                                            
                                            #selectInput: Creates a selection box where one/more (if necessary)        choices can be made
                                            #SELECTOR
                                            selectInput(inputId = "Population", label = "Population", choices = unique(as.character(kg$Population))),
                                            
                                           
                                            
                                            #pickerInput: works like SelectInput, but provides select/deselect         all functionality
                                            #PICKER
                                            #pickerInput("picker", "Select Variable(s):", choices = names(kg),        options = list(`actions-box` = TRUE),multiple = TRUE),
                                            
                                            #radioButtons: Allows for a choice to be made among several options
                                            #RADIOS
                                            radioButtons("radios", "Add some color to your graph?", choices = c("Yes", "No"), selected = "No"),
                                            
                                            #textInput: Allows for the entering of text
                                            #TEXT_HERE
                                            textInput("text_here", "Enter your text here"),
                                            
                                            #The downloadButton provides functionality to download and save the contents of some output in the app
                                            #DOWNLOAD DATA
                                            downloadButton("downloadData", "Download Data")
                               ),
                               #The mainpanel is a place that generally holds your output, however, you can set it up however you wish. These outputs don't really do much, unless tied to server side
                               # MAIN
                               mainPanel(id = "main",
                                         paste("mainPanel: Often reserved for Outputs"),
                                         #Tabset and Tabpanel allow for the showing/hiding of various components.
                                         # TABS
                                         tabsetPanel(id = "tabs",
                                                     #INTRO
                                                     tabPanel("Intro", 
                                                              h3("Introduction to Shiny"),
                                                              br(), br(), br(),
                                                              p(em(strong("This is our opening Intro"))), 
                                                              br(), br(),
                                                              p(em(strong("We may want to describe the Shiny App")))),
                                                     #PLOTS
                                                     tabPanel("Plots", 
                                                              fluidRow(
                                                                splitLayout(style = "border: 1px solid silver:", 
                                                                            cellWidths = c("50%", "50%"),
                                                                            #plotoutput creates a space for a plot to be placed in the app
                                                                            #PLOT1
                                                                            plotOutput(outputId = "plot1", width = "100%",hover = hoverOpts(id ="plot_hover")),
                                                                            
                                                                            #PLOT2
                                                                            plotOutput(outputId = "plot2",width = "100%")
                                                                )),
                                                              #PLOT3
                                                              plotOutput(outputId = "plot3")
                                                     ),
                                                     #TABLES
                                                     tabPanel("Tables", 
                                                              #DT1
                                                              dataTableOutput(outputId = "dt1"),
                                                              #DT2
                                                              dataTableOutput(outputId = "dt2")),
                                                     
                                                     #TEXT    
                                                     tabPanel("Text",fluidRow(column(5, 
                                                                                     #TextOutput allows for text to be taken from the server and to be returned onto the ui. 
                                                                                     #TEXT1
                                                                                     textOutput(outputId = "text1")))
                                                     )
                                         )
                               )
                 ))

#####

# Provide instructions for the server side of things and for how to create the output
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
#####

# Run the application 
shinyApp(ui = ui, server = server)
