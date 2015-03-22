library(R.utils)

#Set File URL
fileURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"

#Check For File Prior To Download
if (!"stormData.csv.bz2" %in% dir("./")) {
        download.file(fileURL, destfile = "stormData.csv.bz2", method="curl")
        bunzip2("stormData.csv.bz2", overwrite=T, remove=F)
} 

#Read StormData File
stormData <- read.csv("stormData.csv", sep = ",")

#Plot Record Count By Year
stormData$year <- as.numeric(format(as.Date(stormData$BGN_DATE, format = "%m/%d/%Y %H:%M:%S"), "%Y"))
hist(stormData$year, col = "gray", breaks = 30, main ="Records Per Year", xlab ="Years", ylab="Records")

eventData <- stormData[,c("EVTYPE", "FATALITIES", "INJURIES", "PROPDMG", "PROPDMGEXP", "CROPDMG", "CROPDMGEXP")]

#Property Damage Preparation
eventData$PROPEXP[eventData$PROPDMGEXP == "K"] <- 1000
eventData$PROPEXP[eventData$PROPDMGEXP == "M"] <- 1e+06
eventData$PROPEXP[eventData$PROPDMGEXP == ""]  <- 1
eventData$PROPEXP[eventData$PROPDMGEXP == "B"] <- 1e+09
eventData$PROPEXP[eventData$PROPDMGEXP == "m"] <- 1e+06
eventData$PROPEXP[eventData$PROPDMGEXP == "0"] <- 1
eventData$PROPEXP[eventData$PROPDMGEXP == "5"] <- 1e+05
eventData$PROPEXP[eventData$PROPDMGEXP == "6"] <- 1e+06
eventData$PROPEXP[eventData$PROPDMGEXP == "4"] <- 10000
eventData$PROPEXP[eventData$PROPDMGEXP == "2"] <- 100
eventData$PROPEXP[eventData$PROPDMGEXP == "3"] <- 1000
eventData$PROPEXP[eventData$PROPDMGEXP == "h"] <- 100
eventData$PROPEXP[eventData$PROPDMGEXP == "7"] <- 1e+07
eventData$PROPEXP[eventData$PROPDMGEXP == "H"] <- 100
eventData$PROPEXP[eventData$PROPDMGEXP == "1"] <- 10
eventData$PROPEXP[eventData$PROPDMGEXP == "8"] <- 1e+08

#Remove Bad Data Influence
eventData$PROPEXP[eventData$PROPDMGEXP == "+"] <- 0
eventData$PROPEXP[eventData$PROPDMGEXP == "-"] <- 0
eventData$PROPEXP[eventData$PROPDMGEXP == "?"] <- 0

#Calculate Total Property Damage
eventData$PROPDMGVAL <- eventData$PROPDMG * eventData$PROPEXP

#Crop Damage Preparation
eventData$CROPEXP[eventData$CROPDMGEXP == "M"] <- 1e+06
eventData$CROPEXP[eventData$CROPDMGEXP == "K"] <- 1000
eventData$CROPEXP[eventData$CROPDMGEXP == "m"] <- 1e+06
eventData$CROPEXP[eventData$CROPDMGEXP == "B"] <- 1e+09
eventData$CROPEXP[eventData$CROPDMGEXP == "0"] <- 1
eventData$CROPEXP[eventData$CROPDMGEXP == "k"] <- 1000
eventData$CROPEXP[eventData$CROPDMGEXP == "2"] <- 100
eventData$CROPEXP[eventData$CROPDMGEXP == ""]  <- 1

#Remove Bad Data Influence
eventData$CROPEXP[eventData$CROPDMGEXP == "?"] <- 0

#Calculate Total Crop Damage
eventData$CROPDMGVAL <- eventData$CROPDMG * eventData$CROPEXP

#Aggregate By Target Events
fatality <- aggregate(FATALITIES ~ EVTYPE, data = eventData, FUN = sum)
top_fatalities <- fatality[order(-fatality$FATALITIES), ][1:10, ]

injury <- aggregate(INJURIES ~ EVTYPE, data = eventData, FUN = sum)
top_injuries <- injury[order(-injury$INJURIES), ][1:10, ]

property <- aggregate(PROPDMGVAL ~ EVTYPE, data = eventData, FUN = sum)
top_property <- property[order(-property$PROPDMGVAL), ][1:10, ]

crop <- aggregate(CROPDMGVAL ~ EVTYPE, data = eventData, FUN = sum)
top_crop <- crop[order(-crop$CROPDMGVAL), ][1:10, ]

#Plots
par( mfrow=c( 2, 2 ) )
IV <- barplot(top_fatalities$FATALITIES, names = top_fatalities$EVTYPE, main = "Fatalities", xaxt="n", space=1 )
text( cex=1, x=IV+.25 , y=-4.25 , top_fatalities$EVTYPE , xpd=T , srt=60 , pos=2 )

I <- barplot(top_injuries$INJURIES, names = top_injuries$EVTYPE , main= "Injuries", xaxt="n", space=1 )
text( cex=1, x=I+.25 , y=-4.25 , top_injuries$EVTYPE , xpd=T , srt=60 , pos=2 )

II <- barplot(top_property$PROPDMG, names = top_property$EVTYPE, main="Property Damage", xaxt="n", space=1 )
text( cex=1, x=II+.25 , y=-4.25 , top_property$EVTYPE , xpd=T , srt=60 , pos=2 )

III <- barplot(top_crop$CROPDMG, names = top_crop$EVTYPE, main="Crop Damage", xaxt="n", space=1 )
text( cex=1, x=III+.25 , y=-4.25 , top_crop$EVTYPE , xpd=T , srt=60 , pos=2 )