library(shiny)
library(httr)
library(XML)
library(ggplot2)
library(grid)
library(gridExtra)
require(gtable)
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
choices = setNames( timesel, timesel )

shinyServer(function(input, output, session) {
  
  source('functions.R', local=TRUE) # external functions to compute googleMap API properties
  
  
  #DEPARTURE TIME UI USES 'choices' from above
  output$selectTime <- renderUI({
    selectInput("commDt", 
                label = h6("Select Departure Time (hh:mm).  Date is defaulted to today"), 
                choices = choices)
  })
  
  observeEvent(input$do, {
    
    # input$do
    
    
    bedMin     <- reactive({input$minBed})
    bedMax     <- reactive({input$maxBed})
    furnished  <- reactive({input$furnished})
    priceMin   <- reactive({input$minPr/4.})
    priceMax   <- reactive({input$maxPr/4.})
    searchRad  <- reactive({input$searchR})
    loc        <- reactive({input$location})
    propType   <- reactive({input$type})
    
    googAdd <- geoCode(loc())
    
    gLat <- as.numeric(googAdd[1])
    gLng <- as.numeric(googAdd[2])
    
    
    print(propType())
    
    print(loc())
    print(googAdd[4])
    
    txt <- reactive({ input$location })
    output$selectedText <- renderText({txt()})
    observe({updateTextInput(session, "commSt",value = loc())})
    
    
    key <- "fzzfvsuf37rgy8k2vfruxcwd"
  
    listing_status="rent"
    url="http://api.zoopla.co.uk/api/v1/property_listings.xml"
    # isolate({
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
      listUrl  <- xpathSApply(rootNode,"//details_url",xmlValue)
      lat    <- xpathSApply(rootNode,"//listing/latitude",xmlValue)
      long    <- xpathSApply(rootNode,"//listing/longitude",xmlValue)
      
#       print(nrow(price))
#       print(resultcount)

     
      validate(
        need(priceMax()-priceMin() >=0., "'Max Price' must be greater than or equal to 'Min Price' "),
        need(bedMax()-bedMin() >=0., "'Max Beds' must be greater than or equal to 'Min Beds' "),
        need(resultcount>2, "Less than 2 properties found in this price range. Try increasing Max Price" )
      )
      
      bed.df <- data.frame(price,nbed)
      countTot<-as.data.frame(table(nbed))
      colnames(countTot)=c('Number of Bedrooms','Total Number of Properties')
      meanPrice<-aggregate(bed.df[, 1], list(bed.df$number_of_bedrooms), mean)
      colnames(meanPrice)=c('Number of Bedrooms','Average Price (£)')
      medPrice<-aggregate(bed.df[, 1], list(bed.df$number_of_bedrooms), median)
      colnames(medPrice)=c('Number of Bedrooms','Median Price (£)')
      allPrice <- merge(meanPrice,medPrice,by='Number of Bedrooms')
      allPrice <- merge(allPrice,countTot,by='Number of Bedrooms')
      allPrice$`Average Price (£)`=as.integer(as.character(allPrice$`Average Price (£)`))
      allPrice$`Number of Bedrooms` <- gsub('([0-9])',"\\1 bedroom", allPrice$`Number of Bedrooms`)
      bed.df$number_of_bedrooms <- gsub('([0-9])',"\\1 bedroom", bed.df$number_of_bedrooms)
      
      
      
      output$histrent <- renderPlot({
        
        
        
        g<- ggplot(bed.df, aes(price, fill = number_of_bedrooms)) + geom_density(alpha = 0.5) 
        g<- ggplotGrob(g)  
        grid.draw(g)
      })
      
      output$tableProp = DT::renderDataTable(allPrice, server = FALSE,selection = 'single',options = list(dom = 't'))
      
      
      
     
      
      output$zoopla      <- renderUI({ includeHTML("zoopla.html") })
      
      #create links for each property to display on google maps
      nbedd <- xpathSApply(rootNode,"//num_bedrooms",xmlValue)
      pprice    <- (xpathSApply(rootNode,"//rental_prices/per_month",xmlValue))
      tip   <- paste("<a target='_blank' href='",listUrl,"'>",nbedd," bedroom"," £",pprice," pcm", "</a>",sep="")
      visdf <- data.frame(tip,paste(lat,long,sep=":"))
      colnames(visdf) = c('propertylink','link')
      
      # print(visdf)
      
      output$propMap <- renderGvis({MyMap1 <- gvisMap(visdf, "link" , "propertylink", 
                                                      options=list(showTip=TRUE, 
                                                                   showLine=TRUE, 
                                                                   enableScrollWheel=TRUE,
                                                                   mapType='normal', 
                                                                   useMapTypeControl=TRUE)) })
      
      
    # })
  })
  observeEvent(input$commDo, {
    
    
    
    
    commloc        <- reactive({input$commSt})
    commEnd        <- reactive({input$commEn})
    cSearchRad     <- reactive({input$commSearchR})
    commDept       <- reactive({input$commDt})
    
    print(commDept())
    
    
    deptime = paste(Sys.Date(),commDept(),"GMT", sep=" ")
    dTime=as.numeric(as.POSIXct(deptime))
  
    address <- geoCode(commloc())
    latlong <- c(address[1],address[2])
    rad=as.numeric(cSearchRad())
    radius=as.integer(rad*1609.3) #convert miles to meters: *1609.3435021075907
    
    stations_df <- nearbyTrainsCode(latlong,radius)
    gStations  <- paste(stations_df$stations,", UK", collapse = '|') 
    # print(gStations)
    
    mode="transit"
    transit_mode="rail"
    from=gStations 
    to=commEnd()
    region="uk"
    key="AIzaSyDi5gaiVz3Eq3xMg6ndUC5X3bhQLxY_F4w" 
    url    <- "https://maps.googleapis.com/maps/api/distancematrix/xml"
    response <- GET(url,query=list(origins=from,destinations=to,region=region,mode=mode,transit_mode=transit_mode,departure_time=dTime,key=key))
    doc      <- content(response,type="text/xml")
    
    status   <- sapply(doc["//row/element/status"],xmlValue)
    statcheck <- data.frame(status,stations_df[1])
    statcheck <- subset(statcheck, status=="OK")
    if(any(status!="OK")) warning("Google found things that weren't stations- will skip these.")
    durationText <- sapply(doc["//row/element/duration/text"],xmlValue)
    durationNum <- as.numeric(sapply(doc["//row/element/duration/value"],xmlValue))/60
    distance <-  as.integer(as.numeric(sapply(doc["//row/element/distance/value"],xmlValue))/1000.)
    
    
    
    output$StationMap <- renderGvis({MyMap <- gvisMap(stations_df, "latlng" , "stations", options=list(showTip=TRUE, showLine=TRUE, enableScrollWheel=TRUE,mapType='normal',useMapTypeControl=TRUE))})
    
    
    
    df_train<- data.frame(statcheck[2],distance,durationText,as.integer(durationNum))
    colnames(df_train) = c("Stations", "Distance (km)","Total Transit time", "(mins)") 
    
    
    
    output$table = DT::renderDataTable(df_train, server = FALSE,selection = 'single',options = list(dom = 't'))
    
   
    
  })
  
  
})
