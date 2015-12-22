library(shiny)
library(httr)
library(XML)
library(ggplot2)
library(grid)
library(dplyr)
library(RCurl)
library(RJSONIO)
library(googleVis)
library(DT)

#CREATE RANGE OF TIMES FOR 24 HOUR PERIOD FOR SELECTING DEPARTURE TIME.
timesel<- seq(as.POSIXct("2015-12-10 00:00"), as.POSIXct("2015-12-10 24:00"), by="15 min")
timesel <- timesel  %>%
  gsub("[0-9]+-[0-9]+-[0-9]+ ","", .)  %>%
  gsub('.{3}$', '', .)
trainTimeChoices = setNames( timesel, timesel )


shinyServer(function(input, output, session) {
  
  source('functions.R', local=TRUE) # external functions to compute googleMap API properties
  
  
  #DEPARTURE TIME UI USES 'choices' from above
  output$selectTime <- renderUI({
    selectInput("commDt", 
                label = h6("Select Departure Time (hh:mm).  Date is defaulted to today"), 
                choices = trainTimeChoices,
                selected = trainTimeChoices[29])
  })
  
  observeEvent(input$do, {
    
    bedMin     <- reactive({as.numeric(input$minBed)})
    bedMax     <- reactive({as.numeric(input$maxBed)})
    furnished  <- reactive({input$furnished})
    priceMin   <- reactive({as.numeric(input$minPr)/4.})
    priceMax   <- reactive({as.numeric(input$maxPr)/4.})
    searchRad  <- reactive({input$searchR})
    loc        <- reactive({input$location})
    propType   <- reactive({input$type})
    
    googAdd <- geoCode(loc())
    gLat <- as.numeric(googAdd[1])
    gLng <- as.numeric(googAdd[2])
    print(googAdd[4])
    txt <- reactive({ input$location })
    output$selectedText <- renderText({txt()})
    observe({updateTextInput(session, "commSt",value = loc())})
    
    
    key <- "fzzfvsuf37rgy8k2vfruxcwd"
    
    listing_status="rent"
    url="http://api.zoopla.co.uk/api/v1/property_listings.xml"
    sample <- GET(url,  query = list(
      
      latitude=gLat,
      longitude=gLng,
      listing_status="rent",
      page_size=100,
      page_number=1,
      property_type=propType(),
      furnished=furnished(),
      minimum_beds=bedMin(),
      maximum_beds=bedMax(),
      minimum_price=priceMin(),
      maximum_price=priceMax(),
      radius=searchRad(),
      summarised="true",
      order_by="age",
      include_rented=1,
      api_key = key))
    result   <- content(sample)
    
    rootNode <- xmlRoot(result)
    resultcount <- as.numeric(xpathSApply(rootNode,"//result_count",xmlValue))
    price    <- data.frame(as.numeric(xpathSApply(rootNode,"//rental_prices/per_month",xmlValue)))
    colnames(price) = 'price'
    nbed     <- data.frame(as.integer(xpathSApply(rootNode,"//num_bedrooms",xmlValue)))
    
    colnames(nbed) = 'number_of_bedrooms'
    listUrl <- xpathSApply(rootNode,"//details_url",xmlValue)
    lat     <- xpathSApply(rootNode,"//listing/latitude",xmlValue)
    long    <- xpathSApply(rootNode,"//listing/longitude",xmlValue)
    
    
    
    histData <- reactive({
      validate(
        need(priceMax()-priceMin() >=0., "'Max Price' must be greater than or equal to 'Min Price' "),
        need(bedMax()-bedMin() >=0., "'Max Beds' must be greater than or equal to 'Min Beds' "),
        need(resultcount>0, "No properties found. Try increasing Max Price and/or search radius" ),
        need(googAdd[4]!='NA',"You have not entered an area")
      )
      df <- data.frame(price,nbed)
      df$number_of_bedrooms <- gsub('([0-9])',"\\1 bedroom", df$number_of_bedrooms)
      return(df)
    })
    
    tableData <- reactive({
      validate(
        need(priceMax()-priceMin() >=0., ""),
        need(bedMax()-bedMin() >=0., " "),
        need(resultcount>0, "" ),
        need(googAdd[4]!='NA',"")
      )
      df <- data.frame(price,nbed)
      countTot<-as.data.frame(table(nbed))
      colnames(countTot)=c('Number of Bedrooms','Total Number of Properties')
      meanPrice<-aggregate(df[, 1], list(df$number_of_bedrooms), mean)
      colnames(meanPrice)=c('Number of Bedrooms','Average Price (£)')
      medPrice<-aggregate(df[, 1], list(df$number_of_bedrooms), median)
      colnames(medPrice)=c('Number of Bedrooms','Median Price (£)')
      df2 <- merge(meanPrice,medPrice,by='Number of Bedrooms')
      df2 <- merge(df2,countTot,by='Number of Bedrooms')
      df2$`Average Price (£)`=as.integer(as.character(df2$`Average Price (£)`))
      df2$`Number of Bedrooms` <- gsub('([0-9])',"\\1 bedroom", df2$`Number of Bedrooms`)
      return(df2)
    })
    
    googPropData <- reactive({
      validate(
        need(priceMax()-priceMin() >=0., ""),
        need(bedMax()-bedMin() >=0., ""),
        need(resultcount>0, "" ),
        need(googAdd[4]!='NA',"")
        
      )
      nbedd  <- xpathSApply(rootNode,"//num_bedrooms",xmlValue)
      pprice <- (xpathSApply(rootNode,"//rental_prices/per_month",xmlValue))
      tip    <- paste("<a target='_blank' href='",listUrl,"'>",nbedd," bedroom"," £",pprice," pcm", "</a>",sep="")
      df  <- data.frame(tip,paste(lat,long,sep=":"))
      colnames(df) = c('propertylink','link')
      save(df,file="stations.Rda")
      return(df)
    })
    
     
    output$searchTxt <- renderText(paste("Showing",furnished(),propType(),"near",googAdd[4],sep=" "))
    output$zoopla  <- renderUI({ includeHTML("zoopla.html") })
    output$tableProp <- DT::renderDataTable(tableData(), server = FALSE,selection = 'single',options = list(dom = 't'))
    
    
    output$histrent <- renderPlot({
      g<- ggplot(histData(), aes(price, fill = number_of_bedrooms)) + geom_density(alpha = 0.5) 
      g<- ggplotGrob(g)  
      grid.draw(g)
    })
    
    #       
    
   
    
    
    output$mapTxt <- renderText("Click on any of the property locations to take a closer look on Zoopla.com")
    output$propMap <- renderGvis({gvisMap(googPropData(), "link" , "propertylink", 
                                                          options=list(showTip=TRUE, 
                                                                       showLine=TRUE, 
                                                                       enableScrollWheel=TRUE,
                                                                       mapType='normal', 
                                                                       useMapTypeControl=TRUE)) })
          
  })
  observeEvent(input$commDo, {
    
    
    commloc        <- reactive({input$commSt})
    commEnd        <- reactive({input$commEn})
    cSearchRad     <- reactive({input$commSearchR})
    commDept       <- reactive({input$commDt})
    
    deptime = paste(Sys.Date(),commDept(),"GMT", sep=" ")
    dTime=as.numeric(as.POSIXct(deptime))
    
    address <- geoCode(commloc())
    print(address[4])
    latlong <- c(address[1],address[2])
    rad=as.numeric(cSearchRad())
    radius=as.integer(rad*1609.3) #convert miles to meters: 
    
    
    stations <- reactive({
      df <-nearbyTrainsCode(latlong,radius)
      validate(
        need(nrow(df)>0,paste("No stations found in",address[4],".  Try Increasing your search radius.",sep=" ")),
        need(address[4]!='NA',"You have not entered an area")
      )
      return(df)
    })
    
    
    output$StationMap <- renderGvis({MyMap <- gvisMap(stations(), "latlng" , "stations", options=list(showTip=TRUE, showLine=TRUE, enableScrollWheel=TRUE,mapType='normal',useMapTypeControl=TRUE))})
    

    stations.df <- stations()
    gStations  <- paste(stations.df$stations,", UK", collapse = '|') 

    mode="transit"
    transit_mode="rail"
    from=gStations 
    to=commEnd()
    region="uk"
    key="AIzaSyDi5gaiVz3Eq3xMg6ndUC5X3bhQLxY_F4w" 
    url         <- "https://maps.googleapis.com/maps/api/distancematrix/xml"
    response    <- GET(url,query=list(origins=from,destinations=to,region=region,mode=mode,transit_mode=transit_mode,departure_time=dTime,key=key))
    doc         <- content(response,type="text/xml")
    
    status       <- sapply(doc["//row/element/status"],xmlValue)
    statcheck    <- data.frame(status,stations.df[1])
    statcheck    <- subset(statcheck, status=="OK")
    if(any(status!="OK")) warning("Google found things that weren't stations- will skip these.")
    durationText <- sapply(doc["//row/element/duration/text"],xmlValue)
    durationNum  <- as.numeric(sapply(doc["//row/element/duration/value"],xmlValue))/60
    distance     <- as.integer(as.numeric(sapply(doc["//row/element/distance/value"],xmlValue))/1000.)
    
    df_train<- data.frame(statcheck[2],distance,durationText,as.integer(durationNum))
    colnames(df_train) = c("Stations", "Distance (km)","Total Transit time", "(mins)") 
  
    output$table = DT::renderDataTable(df_train, server = FALSE,selection = 'single',options = list(dom = 'pt'))
  
  })
})
