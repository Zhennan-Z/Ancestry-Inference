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
)
