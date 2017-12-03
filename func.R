rawNeighPoly <- readOGR("ShapeFiles/geo_export_fd218a17-865a-407c-bf9d-78cd14a72fea.shp")

evictionRawData <- read.socrata("https://data.sfgov.org/resource/93gi-sfd2.csv")
evictionRawData <- evictionRawData %>% select(eviction_id, address, file_date, 
                                owner_move_in, demolition, ellis_act_withdrawal,
                                neighborhood, client_location)

recentlyUpdatedString <- paste(month.name[month(max(evictionRawData$file_date))], year(max(evictionRawData$file_date)))
recentlyUpdatedDateAgg <- as.Date(paste(sep="", year(max(evictionRawData$file_date)), "-", month(max(evictionRawData$file_date)),"-01"))

# Exclude Treasure Island from shapefiles
neighPoly <- rawNeighPoly[rawNeighPoly$nhood != "Treasure Island", ]

evictionRawData <- evictionRawData[evictionRawData$client_location != "" & (
                                           evictionRawData$owner_move_in != "false" |
                                           evictionRawData$ellis_act_withdrawal != "false" |
                                           evictionRawData$demolition != "false"), ]


# Separating latitude and longitude values
evictionRawData$Location <- gsub("POINT \\(","", evictionRawData$client_location)
evictionRawData$Location <- gsub("\\)","", evictionRawData$Location)
evictionRawData$long <- as.numeric(do.call("rbind", strsplit(as.character(evictionRawData$Location), ' ', fixed = T))[,1])
evictionRawData$lat <- as.numeric(do.call("rbind", strsplit(as.character(evictionRawData$Location), ' ', fixed = T))[,2])

#Removing 'block of' patter from address
evictionRawData$address <- gsub("Block Of ", "", evictionRawData$address)

# Merging eviction reason columns
evictionRawData[,4:6] <- sapply(evictionRawData[,4:6], as.logical)
ReasonDF <- cbind(evictionRawData[c(1,4:6)][1], 
                  Reason = names(evictionRawData[c(1,4:6)])[max.col(evictionRawData[c(1,4:6)][-1], "first") + 1])
evictionRawData <- merge(evictionRawData[ ,c(1:3,7,10,11)], ReasonDF, by = "eviction_id")
evictionRawData$layerID <- as.character(1:length(evictionRawData$eviction_id))
evictionRawData$Reason <- as.character(evictionRawData$Reason)
evictionRawData$Reason[evictionRawData$Reason == "owner_move_in"] <- "Owner move in"
evictionRawData$Reason[evictionRawData$Reason == "ellis_act_withdrawal"] <- "Ellis Act withdrawal"
evictionRawData$Reason[evictionRawData$Reason == "demolition"] <- "Demolition"
evictionRawData$Reason <- as.factor(evictionRawData$Reason)

# Coerce individual dates to first day of each month
evictionRawData$agg_date <- as.Date(paste(year(evictionRawData$file_date),"-",month(evictionRawData$file_date),"-01", sep = ""))

evictionRawDataSP <- evictionRawData
coordinates(evictionRawDataSP) <- ~ long + lat
proj4string(evictionRawDataSP) <- proj4string(neighPoly)
evictionRawDataSP@data$District <- as.character(over(evictionRawDataSP, neighPoly)$nhood)
evictionRawData <- cbind(evictionRawDataSP@data, select(evictionRawData, long, lat))

# Color palette for eviction reason
circlePal <- colorFactor(c("green", "blue", "red"), evictionRawData$Reason)

unifyFunc <- function(D, R){
        if(length(D) == 2){
                subsetFuncRange(D[1], D[2], R)
        } else {
                subsetFuncSingle(D, R)
        }
}

subsetFuncSingle <- function(date, reason){
        setDate <- as.Date(paste(year(date),"-",month(date),"-01", sep = ""))
        reason <<- reason
        
        evictionData <<- evictionRawData %>% filter(agg_date == setDate)
        evictionSinceData <- evictionRawData %>% filter(agg_date <= setDate)
        legendMax <- evictionRawData
        if (reason != "All") {
                evictionData <<- evictionData %>% filter(Reason == reason)
                evictionSinceData <- evictionSinceData %>% filter(Reason == reason)
                legendMax <- legendMax %>% filter(Reason == reason)
        }
        legendStart <<- 0
        legendCount <- max(dplyr::count(legendMax, District)$n)
        polyPal <<- colorNumeric("Blues", c(legendStart,legendCount), na.color = "#f6fbff")
        evictionCount <- dplyr::count(evictionSinceData, District)
        legendCount <<- legendCount
        neighPoly.Final <<- merge(neighPoly, evictionCount, by.x = "nhood", by.y = "District")
}

subsetFuncRange <- function(startDate, endDate, reason){
        dates <- as.vector(sapply(c(startDate, endDate), function(i){return(paste(year(i),"-",month(i),"-01", sep = ""))}))
        reason <<- reason
        evictionData <<- evictionRawData %>% filter(agg_date >= dates[1] & agg_date <= dates[2])
        if (reason != "All") {
                evictionData <<- evictionData %>% filter(Reason == reason)
        }
        
        legendStart <<- min(dplyr::count(evictionData, District)$n)
        legendCount <- max(dplyr::count(evictionData, District)$n)
        legendCount <<- legendCount
        polyPal <<- colorNumeric("Blues", c(legendStart,legendCount), na.color = "#f6fbff")
        evictionCount <- dplyr::count(evictionData, District)
        
        neighPoly.Final <<- merge(neighPoly, evictionCount, by.x = "nhood", by.y = "District")
}
