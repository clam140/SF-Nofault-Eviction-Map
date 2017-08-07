evictionRawData <- read.csv("Data/Eviction_Notices.csv")
rawNeighPoly <- readOGR("ShapeFiles/geo_export_fd218a17-865a-407c-bf9d-78cd14a72fea.shp")

# Exclude Treasure Island from shapefiles
neighPoly <- rawNeighPoly[rawNeighPoly$nhood != "Treasure Island", ]



evictionRawData$File.Date <- as.Date(evictionRawData$File.Date, format = "%m/%d/%Y")
evictionRawData <- evictionRawData[evictionRawData$Location != "" , c(1:6,14,15,18,28,29)]


# Separating latitude and longitude values
evictionRawData$Location <- gsub("\\(","", evictionRawData$Location)
evictionRawData$Location <- gsub("\\)","", evictionRawData$Location)
evictionRawData$lat <- as.numeric(do.call("rbind", strsplit(as.character(evictionRawData$Location), ', ', fixed = T))[,1])
evictionRawData$long <- as.numeric(do.call("rbind", strsplit(as.character(evictionRawData$Location), ', ', fixed = T))[,2])

#Removing 'block of' patter from address
evictionRawData$Address <- gsub("Block Of ", "", evictionRawData$Address)

# Merging eviction reason columns
evictionRawData[,7:9] <- sapply(evictionRawData[,7:9], as.logical)
ReasonDF <- cbind(evictionRawData[c(1,7:9)][1], 
                  Reason = names(evictionRawData[c(1,7:9)])[max.col(evictionRawData[c(1,7:9)][-1], "first") + 1])
evictionRawData <- merge(evictionRawData[ ,c(1,2,5,6,10,12,13)], ReasonDF, by = "Eviction.ID")
evictionRawData$layerID <- as.character(1:length(evictionRawData$Eviction.ID))
evictionRawData$Reason <- as.character(evictionRawData$Reason)
evictionRawData$Reason[evictionRawData$Reason == "Owner.Move.In"] <- "Owner move in"
evictionRawData$Reason[evictionRawData$Reason == "Ellis.Act.WithDrawal"] <- "Ellis Act withdrawal"
evictionRawData$Reason <- as.factor(evictionRawData$Reason)

# Coerce individual dates to first day of each month
evictionRawData$File.Date <- as.Date(paste(year(evictionRawData$File.Date),"-",month(evictionRawData$File.Date),"-01", sep = ""))

evictionRawDataSP <- evictionRawData
coordinates(evictionRawDataSP) <- ~ long + lat
proj4string(evictionRawDataSP) <- proj4string(neighPoly)
evictionRawDataSP@data$District <- as.character(over(evictionRawDataSP, neighPoly)$nhood)
evictionRawData <- cbind(evictionRawDataSP@data, evictionRawData[,6:7])

subsetFunc <- function(date, reason){
        setDate <- as.Date(paste(year(date),"-",month(date),"-01", sep = ""))
        reason <<- reason
        if (reason == "All") {
                evictionData <<- evictionRawData %>% filter(File.Date == setDate)
                evictionSinceData <- evictionRawData %>% filter(File.Date <= setDate)
                legendData <- evictionRawData
                
        } else { 
                evictionData <<- evictionRawData %>% filter(File.Date == setDate & Reason == reason)
                evictionSinceData <- evictionRawData %>% filter(File.Date <= setDate & Reason == reason)
                legendData <- evictionRawData %>% filter(Reason == reason)
        }
        legendCount <- max(dplyr::count(legendData, District)$n)
        legendCount <<- legendCount
        polyPal <<- colorNumeric("Blues", c(0,legendCount), na.color = "#f6fbff")
        evictionCount <- dplyr::count(evictionSinceData, District)
        
        neighPoly.Final <<- merge(neighPoly, evictionCount, by.x = "nhood", by.y = "District")
}
