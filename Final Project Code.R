##Name dataframe
San.Diego <- read.csv("C:/Users/Gaby/Desktop/BDSP/Listing San Diego.csv", stringsAsFactors = FALSE)
library(tm)

tm#Mean Ohio
mean(San.Diego$latitude)
mean(San.Diego$longitude)
mean(San.Diego$price)

##change column names DC
names(San.Diego)[names(San.Diego) == "id"] <- "doc_id"
names(San.Diego)[names(San.Diego) == "name"] <- "text"


#Turning df into Vcorpus 
##create function
corpus <- function(df) {
  df_source <- DataframeSource(df)
  df_corpus <- VCorpus(df_source)
  return(df_corpus)
}
##Turn function into variable
df_corpus_city <- corpus(df)

##Input df
df_corpus_San.Diego <- corpus(San.Diego)


##Before cleaning data 
library(tm)
stopwords("en")
Airbnb <- function(corpus, additional_stopwords) {
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, c(stopwords("en"), additional_stopwords))
  corpus <- tm_map(corpus, stripWhitespace)
  return(corpus)
}
additional_stopwords <- c("near", "room", "apartment", "house", "home", "flat", "suite", "bedroom", "airbnb",
                          "minimum", "city", "location", "neighborhood","studio", "cottage", "street", "condo",
                          "private", "apt", "2br", "1br", "new", "san", "diego")

##Cleaning data (Made Function)
cleaning <- function(corpus, additional_stopwords){
  df_clean <- Airbnb(corpus, additional_stopwords)
  df_tdm <- TermDocumentMatrix(df_clean)
  df_m <- as.matrix((df_tdm))
  term_frequency <- rowSums(df_m)
  sorted_termfreq <- sort(term_frequency, decreasing = TRUE)
  return(sorted_termfreq)
}
##Made variable out of function
freq_df <- cleaning(df)
##Input specific corpus 
freq_SD <- cleaning(df_corpus_San.Diego, additional_stopwords)


# View the top 3 most common words
freq_SD[1:30]

