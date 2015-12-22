library(shiny)

priceChoices=c('250'='250','500'='500','750'='750','1000'='1000',
  '1250'='1250','1500'='1500','1750'='1750','2000'='2000',
  '2250'='2250','2500'='2500','2750'='2750','3000'='3000',
  '3250'='3250','3500'='3500','3750'='3750','4000'='4000')
bedNum=c('1'='1','2'='2','3'='3','4'='4','5'='5')
radNum=c('1/4'='0.25','1/2'='0.5','1'='1','3'='3','5'='5','10'='10','15'='15','20'='20','30'='30','40'='40')

shinyUI(pageWithSidebar(
  

  headerPanel("Should I Live In London?"),
  
  
  sidebarPanel(
    conditionalPanel(condition="input.conditionedPanels==1",
                     helpText("STEP 1: Search For Properties"),
                     
                     textInput("location",
                               label = h6("Area Interested in e.g. Oxford, England or a valid UK Postcode e.g. SW1H 9NH"),
                               value = "Watford, England"),
                     selectInput("searchR", 
                                 label = h6("Search Radius (miles)"), 
                                 choices = radNum,
                                 selected=radNum[3]),
                     selectInput("minPr", 
                                 label = h6("Min Price pcm (£)"), 
                                 choices = priceChoices,
                                 selected=priceChoices[2]),
                     selectInput("maxPr", 
                                 label = h6("Max Price pcm (£)"), 
                                 choices = priceChoices,
                                 selected=priceChoices[9]), 
                     radioButtons("type",label = h6("Property Type"),
                                  choices = list("houses","flats"),
                                  selected ="flats"),
                     selectInput("minBed", 
                                 label = h6("Min Beds"), 
                                 choices = bedNum
                                 ), 
                     selectInput("maxBed", 
                                 label = h6("Max Beds"), 
                                 choices = bedNum,
                                 selected=bedNum[2]), 
                     radioButtons("furnished",label = h6(""),
                                  choices = list("unfurnished","furnished")),
                     
                     
                     actionButton("do", "Fetch Properties")
    ),
    conditionalPanel(condition="input.conditionedPanels==2",
                     
                     # print(),
                     helpText("STEP 2: Let's work out commuting times"),
                     
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
    
    
  , width=3),
  
  
  mainPanel(
    tabsetPanel(
      tabPanel("Rent", value=1, 
               column(11,textOutput('searchTxt')),
               column(1,uiOutput("zoopla")),
               column(12,DT::dataTableOutput('tableProp')),
               column(12,plotOutput("histrent",width = "100%")),
               column(12,textOutput('mapTxt')),
               column(12,htmlOutput("propMap"))
              ),
      
      tabPanel("Commuting", value=2,
               column(12, DT::dataTableOutput('table')),
               column(12,htmlOutput("StationMap")),
               column(12, DT::dataTableOutput('tablesel'))
               ),
      tabPanel("Helpful Instructions", value=3,
               includeMarkdown("ReadMe.Rmd")
              ),
    
      id = "conditionedPanels"
    )
  )
  
  
  
))
