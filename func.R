evictionRawData <- read.csv("Data/Eviction_Notices.csv")
rawZIPpolygons  <- readOGR("ShapeFiles/geo_export_7755ffbc-fe52-4716-af88-06d8bc454dce.shp")

# Create Eviction Count Data
evictionRawData$Eviction.Notice.Source.Zipcode <- as.factor(as.character(evictionRawData$Eviction.Notice.Source.Zipcode))

# Exclude Treasure Island from shapefiles
ZIPpolygons <- rawZIPpolygons[rawZIPpolygons$zip != "94130", ]


# Subsetting evictions from 2012 onward
evictionRawData$File.Date <- as.Date(evictionRawData$File.Date, format = "%m/%d/%Y")
evictionRawData <- evictionRawData[evictionRawData$Location != "" & 
                                           evictionRawData$File.Date >= as.Date("2012-01-01"), c(1:6,14,15,18,28,29)]


# Separating latitude and longitude values
evictionRawData$Location <- gsub("\\(","", evictionRawData$Location)
evictionRawData$Location <- gsub("\\)","", evictionRawData$Location)
evictionRawData$lat <- as.numeric(do.call("rbind", strsplit(as.character(evictionRawData$Location), ', ', fixed = T))[,1])
evictionRawData$long <- as.numeric(do.call("rbind", strsplit(as.character(evictionRawData$Location), ', ', fixed = T))[,2])

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
evictionRawData$Neighborhoods...Analysis.Boundaries <- as.character(evictionRawData$Neighborhoods...Analysis.Boundaries)


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
        legendCount <- max(dplyr::count(legendData, Eviction.Notice.Source.Zipcode)$n)
        legendCount <<- legendCount
        polyPal <<- colorNumeric("Blues", c(0,legendCount), na.color = "#f6fbff")
        evictionCount <- dplyr::count(evictionSinceData, Eviction.Notice.Source.Zipcode)
        
        ZIPpolygons <<- merge(ZIPpolygons[1:12], evictionCount, by.x = "zip", by.y = "Eviction.Notice.Source.Zipcode")
}