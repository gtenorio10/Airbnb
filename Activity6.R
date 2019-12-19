###Activity 6
setwd("C:/Users/Gaby/Desktop/BDSP")
library(tm)
library(qdap)

df_airbnb <- read.csv("listings.csv", stringsAsFactors = F)

#df_airbnb_sub <- subset(df_airbnb, select = c(id, name))



names(df_airbnb)[names(df_airbnb) == "id"] <- "doc_id"
names(df_airbnb)[names(df_airbnb) == "name"] <- "text"

airbnb_source <- DataframeSource(df_airbnb)
airbnb_corpus <- VCorpus(airbnb_source)


#### Clean function
#remove extra white space

stripWhitespace(df_airbnb$text)

#Remove Characters

#Change to lower case
tolower(df_airbnb$text)


#Remove remove numbers
removeNumbers(df_airbnb$text)

#Remove Punctuation
removePunctuation(df_airbnb$text)

#######################################

#without corpus

Clean_text <- function(text){
  text_clean <- stripWhitespace(text) # to remove extra white space
  text_clean <- tolower(text_clean) # change the words to be lowercase
  text_clean <- removeNumbers(text_clean) #remove number
  text_clean <- removePunctuation(text_clean) # remove punctuation
  text_clean <- stripWhitespace(text_clean)
 return(text_clean) 
}

name_of_listings <- df_airbnb$text

airbnb_clean <- Clean_text(name_of_listings)

#######################################
# clean with corpus

clean_corpus <- function(corpus, additional_stopwords) {
  # Remove punctuation
  corpus <- tm_map(corpus, removePunctuation)
  # Transform to lower case
  corpus <- tm_map(corpus, content_transformer(tolower))
  # Strip whitespace
  corpus <- tm_map(corpus, stripWhitespace)
  # remove Numbers
  corpus <- tm_map(corpus, removeNumbers)
  # remove stopwords
  corpus <- tm_map(corpus, removeWords, c(stopwords("en"), additional_stopwords))
  #
  #corpus <- tm_map(corpus, stemCompletion)
  
  return(corpus)
}

airbnb_corpus_clean <-clean_corpus(airbnb_corpus, additional_stopwords)

additional_stopwords <- c("room", "private", "bedroom", "home", "studio", "apartment", "near" , "san", "francisco", "suite",
                          "house", "bath", "flat", "location", "condo")

###################################################################
#########################################
### Document term matrix


# airbnb_corpus_clean_dtm <- DocumentTermMatrix(airbnb_corpus_clean) # transforming tdm from corpus
# print(airbnb_corpus_clean_dtm)
# 
# airbnb_corpus_clean_m <- as.matrix(airbnb_corpus_clean_dtm)
# dim(airbnb_corpus_clean_m)
##########################################
#I think we will be using this

### Term Document matrix 
airbnb_corpus_clean_tdm <- TermDocumentMatrix(airbnb_corpus_clean)

# Print out coffee_dtm data
print(airbnb_corpus_clean_tdm)

# Convert coffee_dtm to a matrix: coffee_m
airbnb_corpus_clean_m_tdm <- as.matrix(airbnb_corpus_clean_tdm)

# Print the dimensions of coffee_m
dim(airbnb_corpus_clean_m_tdm)

#########################################
#sorting the getting the row frequency

term_frequency <- rowSums(airbnb_corpus_clean_m_tdm)
sorted_term_freq <- sort(term_frequency, decreasing = T)
  
sorted_term_freq[1:30]

barplot(sorted_term_freq[1:10])

###################################################################
install.packages(c("ggplot2", "devtools", "dplyr", "stringr"))
install.packages(c("maps", "mapdata"))
#citation("ggmap")

devtools::install_github("dkahle/ggmap")


  library(ggplot2)
  library(ggmap)
  library(maps)
  library(mapdata)
  library(ggrepel)
  library(NLP)
  library(dplyr)

states <- map_data("state")
ggplot(data = states) + 
  geom_polygon(aes(x = long, y = lat, fill = region, group = group), color = "grey") + 
  coord_fixed(1.3) +
  guides(fill=FALSE) + # do this to leave off the color legend
  geom_point(data= map_data,aes(x= Lon, y = Lat)) +
  #geom_text(data= airbnb_df,aes(label = Word,x= longitude, y = Latitude))+
  #geom_text_repel(data= map_data,aes(label = Top_Word,x= Lon, y = Lat))
  geom_label(data= map_data,aes(label = Top_Word, x= Lon, y = Lat))

length(states) 




airbnb_df <- data.frame("City" = c("San Francisco"), "Word" = c("Mission"), "Latitude"= c(37.76516), "longitude" = c(-122.4319))

# #airbnb_states <- c("north carolina", "texas" , "massachusetts", "florida", "illinois", "nevada", "ohio",
#                    "colorado", "hawaii", "new jersey", "california",
#                    "tennessee", "louisiana", "new york",
#                    "oregon", "rhode island", "washington", "maryland")

states$color <- NA
text <- "mission"

lat_mean <- mean(lat) #37.76516
lon_mean <- mean(lon) #-122.4319

  