airbnb_map_df <- data.frame("City_name" = c("Asheville","Austin","Boston","Broward County","Chicago","Clark County",
                                         "Cambridge","Columbus","Denver","Hawaii","Jersey City", "Los Angeles",
                                         "Nashville","New Orleans","New York City","Oakland","Portland","Rhode Island",
                                         "Salem","San Diego","Santa Clara County","Santa Cruz County","San Francisco",
                                         "Seattle","Washington"),"State"= c("North Carolina","Texas",
                                         "Massachusetts","Florida","Illinois","Nevada","Massachusetts",
                                         "Ohio","Colorado","Hawaii","New Jersey","California","Tennessee",
                                         "Louisiana","New York","California","Oregon","Rhode Island","Oregon",
                                         "California","California","California","California","Washington"
                                          ,"D.C. District of Columbia"), "Top_Word"= " ", "Region" = c("S", 
                                          "S", "NE", "S", "MW", "W", "NE", "MW", "W", "P", "NE", "W", "S",
                                          "S", "NE", "W", "W", "NE", "W", "W", "W", "W", "W", "W", "NE"),
                                          "Lat" = " ", "Lon" = " ","Price" = NA, stringsAsFactors = F)


write.csv(Adj_WordFreq_df, file="Data/Adj_WordFreq_df.csv")

sorted_adj_wf_df <- sort(Adj_WordFreq_df$Freq_All_States, decreasing = T)

#df_nas <- read.csv("listings_Nas.csv", stringsAsFactors = F)

#nas_wordfreq <- CleanText_WordFreq_fun(df_nas, additional_stopwords)


Adj_WordFreq_df <- data.frame("Top_Word"= "", "Frequency" = rep(NA, 100), stringsAsFactors = F)
Adj_WordFreq_df$Top_Word[23] <- "Amazing"

Adj_WordFreq_df$Frequency[11] <- Adj_WordFreq_df$Frequency[11] + 119

Adj_WordFreq_df$Frequency[23] <- 634


Adj_WordFreq_df$Freq_All_States[11] <- Adj_WordFreq_df$Freq_All_States[11] + 1

which(Adj_WordFreq_df$Top_Word == "Great")

Adj_WordFreq_df <- read.csv("Adj_WordFreq_df.csv", stringsAsFactors = F)

install.packages("wordcloud")
library(wordcloud)

pdf()
wordcloud(Adj_WordFreq_df$Top_Word[1:27], 
          Adj_WordFreq_df$Frequency[1:27], 
          max.words = 27,
          min.freq = 1,
          colors = viridis(n = 100))

library(viridisLite)

barplot(sort(Adj_WordFreq_df$Frequency[1:10], decreasing = T), names.arg =Adj_WordFreq_df$Top_Word[1:10], las =2,main= "Top 5 Adjective Word Frequency", ylab= "Number of State with that word")





