library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(ggrepel)
library(NLP)
library(dplyr)

airbnb_data <- read.csv("airbnb_map_data.csv", stringsAsFactors = F)


States_names <- tolower(unique(airbnb_data$State))

states <- map_data("state") # The map data

states$Price <- NA

###Option 1

states$Color[which(states$region == "north carolina")] <- "Yes"
states$Color[which(states$region == "texas")] <- "Yes"
states$Color[which(states$region == "massachusetts")] <- "Yes"
states$Color[which(states$region == "florida")] <- "Yes"
states$Color[which(states$region == "illinois")] <- "Yes"
states$Color[which(states$region == "nevada")] <- "Yes"
states$Color[which(states$region == "ohio")] <- "Yes"
states$Color[which(states$region == "colorado")] <- "Yes"
states$Color[which(states$region == "new jersey")] <- "Yes"
states$Color[which(states$region == "california")] <- "Yes"
states$Color[which(states$region == "tennessee")] <- "Yes"
states$Color[which(states$region == "louisiana")] <- "Yes"
states$Color[which(states$region == "new york")] <-  "Yes"
states$Color[which(states$region == "oregon")] <- "Yes"
states$Color[which(states$region == "rhode island")] <- "Yes"
states$Color[which(states$region == "washington")] <- "Yes"
states$Color[which(states$region == "minnesota")] <- "Yes"
states$Color[which(states$region == "maryland")] <- "Yes"

### Option 2 with a For loop
for(i in 1:length(States_names)){
  states$Color[which(states$region == States_names[i])] <- "Yes"
}


##Creating the Map
#rename column name
names(states)[names(states) == "long"] <- "Longitude"
names(states)[names(states) == "lat"] <- "Latitude"


p = ggplot(data = states) + 
  geom_polygon(aes(x = Longitude, y = Latitude, fill = Color, group = group), color = "white") + 
  coord_fixed(1.3) + ggtitle("Most Frequent Buzzword From Airbnb-Listing For Cities in U.S") +theme_bw()  +theme(plot.title = element_text(hjust = 0.5)) + geom_point(data =airbnb_data, aes(x= Lon, y = Lat)) + 
  geom_label_repel(data= airbnb_data,aes(label = Top_Word,x= Lon, y = Lat))
  

p +scale_fill_manual(values=c("light grey", "purple")) +labs(fill = "Does State Have Airbnb Data?")

