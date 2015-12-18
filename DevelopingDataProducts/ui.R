library(shiny)

# Define UI for application that draws a histogram
# shinyUI(fluidPage(
shinyUI(pageWithSidebar(
  

  headerPanel("London Rental Property Search"),
  
  
  sidebarPanel(
    conditionalPanel(condition="input.conditionedPanels==1",
                     helpText("Search For Properties"),
                     
                     textInput("location",
                               label = h6("Area Interested in e.g. Oxford, England or VALID UK Postcode e.g. G77 6HN"),
                               value = "Reading, England"),
                     numericInput("minPr", 
                                  label = h6("Min Price pcm (£)"), 
                                  min = 0, max = 5000, value = 100, step = 250), 
                     numericInput("maxPr", 
                                  label = h6("Max Price pcm (£)"), 
                                  min = 500, max = 5000, value = 2000, step = 250), 
                     radioButtons("type",label = h6("Property Type"),
                                  choices = list("houses","flats")),
                     numericInput("minBed", 
                                  label = h6("Min Beds"), 
                                  min = 0, max = 5, value = 1, step = 1), 
                     numericInput("maxBed", 
                                  label = h6("Max Beds"), 
                                  min = 1, max = 5, value = 2, step = 1), 
                     radioButtons("furnished",label = h6(""),
                                  choices = list("unfurnished","furnished")),
                     selectInput("searchR", 
                                 label = h6("Search Radius (miles)"), 
                                 choices = c('1/4'='0.25','1/2'='0.5','1'='1','3'='3','5'='5','10'='10','15'='15','20'='20','30'='30','40'='40')),
                     
                     actionButton("do", "Submit Query")
                     
    ),
    conditionalPanel(condition="input.conditionedPanels==2",
                     
                     # print(),
                     helpText("Let's work out commuting times"),
                     
                     textInput("commSt",
                               label = h6("Origin (taken from rental location)"),
                               value=""
                               ),
                     textInput("commEn",
                               label = h6("Destination (e.g. where your new job is)"),
                              value="Farringdon"),
                     selectInput("commSearchR", 
                                 label = h6("Search Radius (miles)"), 
                                 choices = c('1'='1','3'='3','5'='5')),

                      htmlOutput("selectTime"),
                     
                     actionButton("commDo", "Submit Query")
                     
                    
                     
    )
    
    
  ),
  
  
  mainPanel(
    tabsetPanel(
      tabPanel("Rent", value=1, 
               
               column(11,DT::dataTableOutput('tableProp')),
               column(11,plotOutput("histrent",width = "100%")),
               column(11,uiOutput("zoopla")),
               column(11,htmlOutput("propMap"))
              ),
      
      tabPanel("Commuting", value=2,
               column(11, DT::dataTableOutput('table')),
               column(11,htmlOutput("StationMap")),
               column(11, DT::dataTableOutput('tablesel'))
               ),
    
      id = "conditionedPanels"
    )
  )
  
  
  
))
