#IST719 - FINAL PROJECT - VISUALIZING CITI BIKE PROGRAM

#Loading library 

library(lubridate)
library(ggplot2)
library(ggmap)
library(plyr)
library(RColorBrewer)
library(igraph)

#Load data each month, retrieved from https://www.citibikenyc.com/system-data
bike.Jan19 <- read.csv("C:/Users/manan/Desktop/Infoviz/JC-201811-citibike-tripdata.csv", stringsAsFactors = F)
bike.Feb19 <- read.csv("C:/Users/manan/Desktop/Infoviz/JC-201810-citibike-tripdata.csv", stringsAsFactors = F)
bike.Mar19 <- read.csv("C:/Users/manan/Desktop/Infoviz/JC-201812-citibike-tripdata.csv", stringsAsFactors = F)
bike.Apr19 <- read.csv("C:/Users/manan/Desktop/Infoviz/JC-201707-citibike-tripdata.csv", stringsAsFactors = F)
bike.Dec18 <- read.csv("C:/Users/manan/Desktop/Infoviz/JC-201809-citibike-tripdata.csv", stringsAsFactors = F)
bike.Nov18 <- read.csv("C:/Users/manan/Desktop/Infoviz/JC-201901-citibike-tripdata.csv", stringsAsFactors = F)
bike.Oct18 <- read.csv("C:/Users/manan/Desktop/Infoviz/JC-201902-citibike-tripdata.csv", stringsAsFactors = F)
bike.Sep18 <- read.csv("C:/Users/manan/Desktop/Infoviz/JC-201903-citibike-tripdata.csv", stringsAsFactors = F)
bike.Aug18 <- read.csv("C:/Users/manan/Desktop/Infoviz/JC-201805-citibike-tripdata.csv", stringsAsFactors = F)


#Merge data into 2 parts
#Each part has different time series format 
bike1 <- rbind(bike.Dec18,bike.Nov18,bike.Oct18,bike.Sep18,bike.Aug18,bike.Jan19,bike.Feb19,bike.Mar19,bike.Apr19)
#bike2 <- rbind(bike.Jan19,bike.Feb19,bike.Mar19,bike.Apr19)
#write.csv(bike1, file = "/Users/baskar/Documents/ml/Syracuse/IST-719-Info-Viz/Citibike_InfoViz/citibike-tripdata_Merged.csv")
#------------------------------------------------------------------
# PROCESS & TRANSFORM DATA 
#------------------------------------------------------------------
#Extract time series information 
startime.date1 <- strptime(bike1$starttime, "%Y-%m-%d %H:%M:%S", tz = "EST")
bike1$startime.weekday<- weekdays.POSIXt(startime.date1)
bike1$startime.day <-day(startime.date1)
bike1$startime.week<- week(startime.date1)
bike1$startime.weekinmon <- ceiling(bike1$startime.day/7)
bike1$startime.hour <- hour(startime.date1)
bike1$month <- month(startime.date1, label= T)

# startime.date2 <- strptime(bike2$starttime, "%m/%d/%Y %H:%M:%S", tz = "EST")
# bike2$startime.weekday<- weekdays.POSIXt(startime.date2)
# bike2$startime.day <-day(startime.date2)
# bike2$startime.week<- week(startime.date2)
# bike2$startime.weekinmon <- ceiling(bike2$startime.day/7)
# bike2$startime.hour <- hour(startime.date2)
# bike2$month <- month(startime.date2, label = T)

#merge data   
bike14 <- rbind(bike1, bike2) 
bike14<-bike1
bike14$birth.year <- as.numeric(bike14$birth.year)

#Categorize age ranges 
bike14$age <- 2018 - bike14$birth.year
bike14$ageCat[bike14$age <= 40] <- "Young"
bike14$ageCat[bike14$age >=60 ] <- "Old"
bike14$ageCat[bike14$age >40 & bike14$age < 60 ] <- "Middle"
bike14$ageCat <- factor(bike14$ageCat, levels = c("Young", "Middle", "Old"), labels = c("Young", "Middle", "Old"))
bike14$ageCat
aa<-as.data.frame(bike14$ageCat)
aa$`bike14$ageCat`
ggplot(aa,aes(x=bike14$ageCat, y=Use )) + 
  geom_line(size = 1, col = "blue") + theme_bw()

#Categorize time range for August 
startime.dateAug <- strptime(bike.Aug18$starttime, "%Y-%m-%d %H:%M:%S", tz = "EST")
bike.Aug18$startime.weekday<- weekdays.POSIXt(startime.dateAug)
bike.Aug18$startime.day <-day(startime.dateAug)
bike.Aug18$startime.week<- week(startime.dateAug)
bike.Aug18$startime.weekinmon <- ceiling(bike.Aug18$startime.day/7)
bike.Aug18$startime.hour <- hour(startime.dateAug)
bike.Aug18$month <- month(startime.dateAug, label= T)

bike.Aug18$timeCat[bike.Aug18$startime.hour < 5 & bike.Aug18$startime.hour >= 0] <- "Night"
bike.Aug18$timeCat[bike.Aug18$startime.hour < 12 & bike.Aug18$startime.hour >= 5 ] <- "Morning"
bike.Aug18$timeCat[bike.Aug18$startime.hour < 19 & bike.Aug18$startime.hour >= 12 ] <- "Afternoon"
bike.Aug18$timeCat[bike.Aug18$startime.hour <= 23 & bike.Aug18$startime.hour >= 19 ] <- "Evening"
bike.Aug18$timeCat <- factor(bike.Aug18$timeCat, levels = c("Morning", "Afternoon", "Evening", "Night"))


#------------------------------------------------------------------
# VISUALIZATION 
#------------------------------------------------------------------

#Populate time of use by hour and visualize 
bike14.use.hour <- ddply(bike14, c("startime.hour"), summarise, Use = length(startime.hour))
plot(bike14.use.hour, type = "l", col= "blue")
ggplot(bike14.use.hour, aes(x=startime.hour, y=Use )) + 
  geom_line(size = 1, col = "blue") + theme_bw()          #highest use: 8-9 am, 4-6pm 

#Extract data without NAs value values and unknown gender 
bike14.filter <- bike14[!is.na(bike14$birth.year) & bike14$gender != 0,]

# Populate data for plot  
bike14.use.demographics <- ddply(bike14.filter, c("ageCat", "gender"), summarise, Use = length(gender))
bike14.use.demographics$gender <- factor(bike14.use.demographics$gender, labels = c("Male", "Female"))

#Visualize with stacked bar charts  
ggplot(bike14.use.demographics, aes(x=ageCat, y=Use, fill = gender )) + 
  geom_bar(stat = "identity", position="dodge") + 
  geom_text(aes(label=Use), colour="black") + 
  theme_bw() 

#Visualize most pick up | drop off station 
top10.start.station <- head(sort(table(bike14$start.station.name), decreasing =  T),10)
top10.end.station <- head(sort(table(bike14$end.station.name), decreasing =  T),10)
mosaicplot(rbind(top10.start.station, top10.end.station), 
           color =c("#0052CC", "#005CE6", "#0066FF", "#1975FF", "#3385FF", "#4D94FF", "#66A3FF", "#80B2FF", "#99C2FF", "#B2D1FF"),las =1,
           main = "Top 10 Pickup | Dropoff bike location") 


#Populate data frame for calendar heat map 
bike14.use <- ddply(bike14, c("month", "startime.weekinmon", "startime.weekday"), summarise, Use = length(startime.weekday))
bike14.use$startime.weekday <- factor(bike14.use$startime.weekday,levels = c("Monday", "Tuesday", "Wednesday","Thursday","Friday","Saturday", "Sunday")
                                      ,labels = c("Mon", "Tues", "Weds", "Thurs", "Fri", "Sat", "Sun"))
#Generate calendar heat map 
p <- ggplot(bike14.use, aes(x=startime.weekday, y=startime.weekinmon, fill=Use))
p + geom_tile( color = "black") +
  scale_y_reverse() + 
  scale_fill_gradient2(low="red", high="green")+ 
  facet_wrap(~month) +  theme_bw() 


#Generate density map on different time in day for month August (CALENDAR HEAT MAP)
devtools::install_github("dkahle/ggmap")

library(ggmap)
register_google(key = "AIzaSyAvGt5o0VzVHzRk8hgaX6W2FcmnfR7p5QI")
nyc <- get_map(location = "East Village, New York City",zoom = 13, maptype = "roadmap")
ggmap(nyc)

ggmap(nyc) + stat_density2d (aes(x = start.station.longitude, y = start.station.latitude, fill = ..level.., alpha = ..level..)
                             , size = 0.1, bins = 5
                             , data = bike.Aug18
                             , geom = "polygon") + 
  scale_fill_continuous(space = "Lab") + 
  facet_wrap(~timeCat)

#------------------------------------------------------------------
# VISUALIZE BIKE STATION CONNECTIONS WITH NETWORK GRAPH 
#------------------------------------------------------------------

#Data frame of start station and end station 
station.conn <- bike14[c("start.station.name","end.station.name")]
#Calculate their frequency connections 
station <- data.frame(table(station.conn))
#Calculate  frequent connections between stations during the year -> visualize only 1% 
quantile(station$Freq, probs = c(0.5, 0.75, 0.99))  #99% has less than 677 connections 

#Order based on their frequent connection 
station.ordered <- station[order(station$Freq, decreasing = T),]
#Remove loop connections (pick-up station and end station are the same )
station.ordered <- station.ordered[station.ordered$start.station.name != station.ordered$end.station.name,]

#Extract only top 1% frequent connections 
station.top<- station.ordered[station.ordered$Freq > 677,]

#Create a data frame of Stations with their long/lat values and zipcode 
#Get the list of stations with their lat/long values 
station.df <- ddply(bike14, c("start.station.id", "start.station.name","start.station.longitude", "start.station.latitude")
                    , summarise, Freq = length(start.station.id))

#Reverse Geocode to get zipcode
for (i in 1:length(station.df$start.station.id)){
  res <- revgeocode(c(station.df$start.station.longitude[i], station.df$start.station.latitude[i]), output = "more")
  station.df$zip[i]<- levels(res$postal_code)
}

#Map zipcode into neighborhood 
#Read NYCity zipcode data from https://www.health.ny.gov/statistics/cancer/registry/appendix/neighborhoods.htm
nyc.zip <- read.csv(file.choose(), stringsAsFactors = F)
head(nyc.zip)
#Do a little process to extract zipcode in each neightboorhood
nyc.zip <- ddply(nyc.zip, c("borough","neighborhood"), transform, zipcode2 = unlist(strsplit(zipcode, ",")))
#remove white space 
nyc.zip$zipcode2 <- gsub(" ", "", nyc.zip$zipcode2)

#Based on the data above, map station's zipcode with borough and neighborhood 
station.df$neighborhood <- nyc.zip$neighborhood[match(station.df$zip, nyc.zip$zipcode2)]

#Convert name back to character 
station.top$start.station.name<- as.character(station.top$start.station.name)
station.top$end.station.name<- as.character(station.top$end.station.name)

g.df <- subset(station.top, select = c(start.station.name, end.station.name, Freq))
#Create network graph for top 1% connection 
g<- graph.data.frame(g.df, directed =  T)

#Analyze network 
summary(g)    #247 nodes, 1100 connection 

#Edit Vertices 
V(g)$neighborhood <- station.df$neighborhood[match(V(g)$name, station.df$start.station.name)]

#Color vertices by neighborhood groups 
#Create a data frame of colors for NYC neighborhood 
neightborhood.color <- data.frame(neighborhood = unique(station.df$neighborhood), color = I(brewer.pal(10, name = "Paired")))
#Match color into Vertices 
V(g)$color <- neightborhood.color$color[match(V(g)$neighborhood,neightborhood.color$neighborhood) ]

#Set size of Vertices based on their strength 
V(g)$size <- sqrt(graph.strength(g))
#remove Vertices labels 
V(g)$label = NA    

#Edit Edges  
E(g)$arrow.size = 0.1
E(g)$arrow.width = 0.1
#Color edges so that edges of the same neighborhood has idential color, edges of neighborhood interconnection has color "light grey"
E(g)$color <- "#d3d3d3"

for (i in 1:10)
{
  f <- V(g)[neighborhood == neightborhood.color$neighborhood[i]]
  E(g)[f %--% f]$color <- neightborhood.color$color[i]
}

#Plot the network graph with legend 
plot.igraph(g, layout = layout.fruchterman.reingold)
legend(x = 'topright', 
       legend = as.character(neightborhood.color$neighborhood),
       fill = neightborhood.color$color, bty = 'o', xjust = 0)

#---------------------------------------
#Group station network into Neighborhood network 
#Neighborhood names list 
neighborhood.names <- sort(unique(V(g)$neighborhood))
#Create mapping vector to aggregate edges between neighborhood groups 
neighborhood.nums <- as.numeric(as.factor(V(g)$neighborhood))
g.c <- contract.vertices(g, neighborhood.nums)
#Set weight and simplify network 
E(g.c)$weight <- 1
g.c <- simplify(g.c)

#Set Veticles size corresponding with neighborhood's connection 
neighborhood.sizes <- as.vector(table(V(g)$neighborhood))

#Edit verticles of new grouped network 
V(g.c)$label = neighborhood.names
V(g.c)$size = 5*sqrt(neighborhood.sizes)
V(g.c)$color = neightborhood.color$color[match(V(g.c)$label,neightborhood.color$neighborhood)]

#Edit Edges 
E(g.c)$arrow.size = 0.3
E(g.c)$arrow.width = 1
E(g.c)$width = 0.8 * sqrt(E(g.c)$weight)
E(g.c)$color = "#d3d3d3"
#Plot the neighborhood network graph 
plot.igraph(g.c,  layout = layout.fruchterman.reingold, vertex.label = NA)
legend(x = 'topright', 
       legend = neightborhood.color$neighborhood,
       fill = neightborhood.color$color, bty = 'o', xjust = 0)

