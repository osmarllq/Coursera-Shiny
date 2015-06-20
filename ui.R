library(shiny);library(leaflet)

choices <- list(
  'Social Indicators' = c('Poverty'="NBI_TOT_2005",
               'Living Standards'="ICV_2005_DANE"),
  'Economic Indicators' = c('Aggregate Income'='va_tot',
                          'Per capita income'='va_pc',
                          'Market Potential'="Gravity"),  
  'Road Connectivity' = c('Closeness'="Closeness"                         
                          )  
  )


# Define UI for application
shinyUI(navbarPage('Exploratory Data Analysis',
                   tabPanel('LISA Cluster Map',
                            em('  In this map you will find a Cluster Map, which bases
  cluster identification on the', strong('Local Indicators of Spatial Association (LISA).'),
  'The LISA allows you to identify', strong('hotspots or coldspots'), 'using a permutations
  signficance test. For example, if we are analyzing the spatial distribution
  of', strong("living standards"), 'we can know which towns form a cluster of high living
  standards. It is also possible to identify clusters using two different variables.
  This is the bivariate LISA. If we relate the', strong('living standards'), 'index to the', strong('closeness'),
  'index, we can study if there is some sort of spatial association of living standards
  with road network conectivity. If there is a bunch of neighbouring towns
  with high living standards and a high road network conectivity (high closeness),
  this will result in a bivariate hotspot.'),br(),
  em('-- If you choose the same index in the fields', strong('variable 1 and variable 2'),
     'you will get the univariate LISA cluster map. If you choose two different indexes, 
      you will get the bivariate LISA cluster map. If you choose two different variables,
     you can also take a look at the "Scatterplot" tab.'),br(),
  em('-- It is important to note that cluster identification can be sensitive to the
     neighbourhood extent. Therefore, the App inclueds a slider which allows to
     change the neighbourhood size. If the slider is set to 1, the neighbours of 
     any given municipality will include all towns within an hour travel time'),br(),
  em('-- If you click on a polygon you can see the municipality it represents.'),br(),
  em('-- This Shiny App uses data about Antioquia, a region in Colombia.'),br()
                              ,
                            fluidRow(
                              column(3,
                                     div(class='well',             
                                         selectInput("var1", label=h6("Variable 1:"), choices = choices
                                                     ,  selected='ICV_2005_DANE'),
                                         
                                         selectInput("var2", label=h6("Variable 2:"),  choices = choices
                                                     , selected='Closeness'),
                                         br(),
                                         sliderInput("hdist", label = h6("Search Radius (hours):"),
                                                     min=0.5,max=4, value=1,step = 0.5,animate=TRUE),                    
                                         br(),
                                         p(em("Download Data")),
                                         downloadButton('downloadData', 'Download Clusters'),
                                         br(),hr(),br(),
                                         h5('Exploratory Spatial Data Analysis'),
                                         p('by ',a('Osmar Loaiza'),a('<olloaizaq@unal.edu.co>'))
                                     )                    
                              ),
                              column(9,                    
                                     fluidRow(
                                       column(12,                             
                                              leafletOutput("plot1", width="445px",height = "500px")
                                              
                                       )
                                     ),
                                     fluidRow( tagList(tags$div(
                                       plotOutput("legend", width="480px",height = "85px"),style="text-align: center;" ))
                                                     ),
                                     br(),
                                     fluidRow(                    
                                       column(3,
                                              radioButtons("signif", label=h6("Singificance Threshold:"),
                                                           c("1%" = 0.01,
                                                             "5%" = 0.05,
                                                             "10%"=0.1),selected=0.05)
                                              
                                       ),                      
                                       column(3,
                                              numericInput('perm',label=h6('Number of Permutations:'),
                                                           value=1000,min=100,max=10000,step=100)                                                     
                                       )                                  
                                     )                                     
                              )
                            )
                   ),
                   
                   tabPanel('Scatterplot',
                            em('-- The scatterplot in this tab will be displayed only
  if you choose two (2) different variables in the fields',strong('variable 1 and variable 2'),
  'which you can find in the',strong('LISA Cluster Map'), 'tab.'),br(),
  em("-- The scatterplot offers another way to look at the relationship between two
     variables. If you click on a point, the municipality's name it represents will be 
     displayed."),br(),
  em("-- Moreover, you will find a bootstrap correlation test to determine if the
     level of correlaion between two variables is stastically significant. The
     higher the number of simulations specified, the more reliable will be the test, 
     but the computation time will increase."),br(),
                            fluidRow(
                              column(5,
                                     fluidRow(               
                                       div(class='well',
                                           radioButtons("fit", label=h6("Trend Line:"),
                                                        c("Linear" = 'Linear',
                                                          "Lowess" = 'Lowess'),
                                                        selected='Lowess')                           
                                       )
                                     ),
                                     
                                     fluidRow(
                                       conditionalPanel("input.var1 != input.var2",
                                                        verbatimTextOutput('values')
                                       )
                                     ),
                                     
                                     fluidRow(                    
                                       div(class='well',p(em('Boostrap Test for linear correlation')),
                                           numericInput('nsim',label=h6('Number of Simulations:'),
                                                        value=1000,min=100,max=10000,step=100),
                                           numericInput('conf',label=h6('Confidence Level:'),
                                                        value=0.95,min=0.8,max=0.99,step=.01)
                                       )
                                     )                            
                              ),             
                              column(5,
                                     conditionalPanel("input.var1 != input.var2",
                                                      plotOutput("plot2", width="450px",height = "450px",
                                                                 clickId='Click'),
                                                      br(),                      
                                                      actionButton("clear", "Clean Names")
                                                      
                                     )
                              )
                            )
                   )
                   
                   
))