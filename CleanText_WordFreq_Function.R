setwd("C:/Users/Gaby/Desktop/BDSP")

df_RL <- read.csv("listings_RL.csv", stringsAsFactors = F)
df_SantaCruz <- read.csv("listings_santa_cruz.csv", stringsAsFactors = F)
df_SantaClara <-read.csv("listings_santa_clara.csv", stringsAsFactors = F)
df_Sea <- read.csv("listings_sea.csv", stringsAsFactors = F)
df_Mass <- read.csv("listings_mass.csv", stringsAsFactors = F)

  
  
library(tm)

additional_stopwords <- c("room", "private", "bedroom", "home", "studio", "apartment", "near" , "san", "francisco", "suite",
                          "house", "bath", "flat", "location", "condo", "apt", "los", "angeles", "bungalow", "double", "single", "bed", "cottage", "side", "guest", "bathroom", "jose", "santa", "palo", "alto", "clara", "cupertino", "walk"
                          ,"entrance", "shared", "large", "brba", "gym", "row", "pool", "santana", "hackerhome",
                          "tub", "cruz", "del", "steps", "point", "cabin", "mar", "close", "family", "seattle", "townhouse", "stay", "loft",  "one", "next", "parking", "executive", "two", "min", "monica", "master", "lax", "â~.", "nashville", "nash", "view", "views")



#####RL
#RL_wordfreq <- CleanText_WordFreq_fun(df_RL,additional_stopwords)
#Lat 41.57315
#Lon -71.41028
#Top3 beach, Newport, downtown
#$269.75

#####Santa Clara
#Santa_Clara_wordfreq <- CleanText_WordFreq_fun(df_SantaClara,additional_stopwords)
#Lat 37.5482
#Lon -121.9738
#Top3 downtown, Silicon Valley, cozy/view
#169.26

######Santa Cruz
#Santa_Cruz_wordfreq <- CleanText_WordFreq_fun(df_SantaCruz,additional_stopwords)
#Lat 36.98751
#Lon -121.9798
#Top3 beach ocean retreat
#$254.66

######Seattle
#Seattle_wordfreq <- CleanText_WordFreq_fun(df_Sea,additional_stopwords)
#Lat 47.62564
#Lon -122.3335
#Top3 capitol Hill, Queen Anne Hill, downtown, modern/Cozy
#There are many meanings for hill e.g outdoors, a place, capitol hill and queen anne hill
#$167.45

######Mass
#Mass_wordfreq <- CleanText_WordFreq_fun(df_Mass,additional_stopwords)
#Lat 42.37217
#Lon -71.10861
#Top3 Harvard, MIT, Cambridge  
#price $159.01


#LA_wordfreq <- CleanText_WordFreq_fun(df_LA, additional_stopwords)
df_LA <- read.csv("listings_LA.csv", stringsAsFactors = F)
mean(df_LA$price)

sf_wordfreq <- CleanText_WordFreq_fun(df_sf, additional_stopwords)


#######Creating Functions
#Cleaning function
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



#This cleans the text/get word freq

CleanText_WordFreq_fun <- function(df_airbnb, additional_stopwords){
  
   names(df_airbnb)[names(df_airbnb) == "id"] <- "doc_id"
   names(df_airbnb)[names(df_airbnb) == "name"] <- "text"
  
  airbnb_source <- DataframeSource(df_airbnb)
  airbnb_corpus <- VCorpus(airbnb_source)
  
  airbnb_corpus_clean <-clean_corpus(airbnb_corpus, additional_stopwords) 
  ### Term Document matrix 
  airbnb_corpus_clean_tdm <- TermDocumentMatrix(airbnb_corpus_clean)
  
  
  # Convert coffee_dtm to a matrix: 
  airbnb_corpus_clean_m_tdm <- as.matrix(airbnb_corpus_clean_tdm)
  
  term_frequency <- rowSums(airbnb_corpus_clean_m_tdm)
  sorted_term_freq <- sort(term_frequency, decreasing = T)
  
  Top30 <-sorted_term_freq[1:30]
  
  return(Top30)
  
}









