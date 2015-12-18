geourl <- function(address, return.call = "json", sensor = "false") {
  root <- "http://maps.google.com/maps/api/geocode/"
  u <- paste(root, return.call, "?address=", address, "&region=uk", "&sensor=", sensor, sep = "")
  return(URLencode(u))
}

trainurl <- function(latlong, radius,return.call = "xml", sensor = "false") {
  key="AIzaSyDmBvrskrDqqfJxFo4uazEthtvhOFz95Oc"
  root <- "https://maps.googleapis.com/maps/api/place/nearbysearch/"
  u <- paste(root, return.call, "?location=", latlong[1],",",latlong[2], "&region=uk","&radius=",radius,"&types=train_station|subway_station","&sensor=", sensor,"&key=",key,sep="")
  return(URLencode(u))
}

geoCode <- function(address,verbose=FALSE) {
  if(verbose) cat(address,"\n")
  u <- geourl(address)
  doc <- getURL(u)
  x <- fromJSON(doc,simplify = FALSE)
  if(x$status=="OK") {
    lat <- x$results[[1]]$geometry$location$lat
    lng <- x$results[[1]]$geometry$location$lng
    location_type <- x$results[[1]]$geometry$location_type
    formatted_address <- x$results[[1]]$formatted_address
    return(c(lat, lng, location_type, formatted_address))
  } else {
    return(c(NA,NA,NA, NA))
  }
}

nearbyTrainsCode <- function(latlong,radius) {
  u <- trainurl(latlong,radius)
  doc <- GET(u)
  x <- content(doc,type="text/xml")
  status<-sapply(x["//PlaceSearchResponse/status"],xmlValue)
  if(status=="OK") {
    stations  = sapply(x["//result/name"],xmlValue)
    lat = sapply(x["//result/geometry/location/lat"],xmlValue)
    lng = sapply(x["//result/geometry/location/lng"],xmlValue)
    df <- data.frame(stations,paste(lat,lng,sep=":"))
    colnames(df)=c("stations","latlng")
    return(df)
  } else {
    return(NA)
  }
}

