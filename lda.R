##############################################
######## Topic Modeling ######################
##############################################

### Data Preparation ###

#Change your data set name here
data_name ="C:/Users/zhengkya/Desktop/Hackathon Project/Sample Input/sample_input.csv"

#Install pacakges
install.packages('Matrix')
install.packages('topicmodels')
install.packages('tm')
install.packages('slam')
install.packages('reshape2')
install.packages('sqldf')
install.packages('reshape')

#Load libraries
library(Matrix)
library(topicmodels)
library(tm)
library(slam)
library(reshape2)
library(sqldf)
library(reshape)

#Read the data.
#Note: Please change the input directory to your own version
data = read.csv(data_name,header=T, row.names=NULL)
data$song_id = as.factor(seq(1,nrow(data),by=1))

#Cleaning the text
#Remove '\','/' and add labels to each genre
data$genre = gsub("\\\\"," ",data$genre)
data$genre = gsub("/"," ",data$genre)
data$genre= gsub(" ","_",data$genre)
data$genre = paste("genre_",data$genre,sep="")
data$artist_name = paste("artist_", as.character(data$artist_name),sep='')
data$artist_name = gsub(' ','_',data$artist_name)
head(data)

#Create text to put into the model, by combining a selected set of strings together
#Note: Users can concatenate different groups of strings together. In this case,  I use track_name, artistname
#     genre. Users can input other kinds of strings such as lyrics
data$text = paste(as.character(data$artist_name),
                  as.character(data$genre),
                  as.character(data$lyrics),
                  as.character(data$song_name),
                  sep=" ")
text = tapply(data$text, data$song_id, paste, sep=" ", collapse= " ")



### Actual Modeling ###
corpus = Corpus(VectorSource(text))
corpus = tm_map(corpus, removePunctuation)
dtm = DocumentTermMatrix(corpus, control=list(weighting=weightTf, stopwords = TRUE))
lda = LDA(dtm, k=20, control=list(alpha=0.1))

#View the result data set
View(topics(lda,10)) # top 10 themes for each song_name
View(posterior(lda)$topics) # theme distribution for each song name 
View(terms(lda,10)) # top 10 words contained in each theme(topic)
View(posterior(lda)$terms) # word distribution in each theme


### Clean the result data set ###
theme_table <- data.frame(topics(lda,5))
theme_table_long <- reshape(theme_table,idvar = "rownum", 
                            ids = row.names(theme_table),times = names(theme_table),
                            timevar='song_id',v.names='theme_id',varying = list(names(theme_table)),direction='long')

theme_table_long$song_id <- gsub('X','',theme_table_long$song_id)


user_temp <- sqldf('select distinct song_name, artist_name, song_id from data order by song_name, artist_name')
user_with_theme <- sqldf('select l.song_name, l.artist_name, r.theme_id from user_temp as l inner join theme_table_long as r on l.song_id= r.song_id')

#Selecting the top words for each theme
theme_with_term <- data.frame(terms(lda,10))

theme_table_filtered <- c()
for(i in 1:20){
  row_binded <- rep("",6)
  t = 1
  for(j in 1:10){
    s = as.character(theme_with_term[j,i])
    if(t > 6){
      break
    }
    if(s!= "highbs" & s!= "lowbs" & s!="mediumbs" & s!="highbpm" & s!="lowbpm" & s!="mediumbpm"){
      row_binded[t] <- s
      t=t+1
    }
  }
  theme_table_filtered <- rbind(theme_table_filtered,row_binded)
}

theme_table_filtered <- data.frame(theme_table_filtered,row.names=NULL)
theme_table_filtered$theme_id <- seq(1,20,by=1)
names(theme_table_filtered)[1:6] <- c("word1","word2","word3","word4","word5","word6")
names(theme_table_filtered)[7] <- "theme_id"


final_table_for_viz <- sqldf("select l.song_name, l.artist_name, r.* from user_with_theme l inner join theme_table_filtered r on l.theme_id=r.theme_id")

#Selecting the most popular theme for each song
keep_list <- rep(0,48)
t=1
for(i in 1:nrow(final_table_for_viz)){
  if(i%%5 == 1){
    keep_list[t] <- i
    t = t+1
  }
}

final_table_for_viz <- final_table_for_viz[keep_list,]

#Get theme reference table
theme_reference=data.frame(terms(lda,10))
names(theme_reference) = gsub("Topic", "Theme", names(theme_reference))

### Change Your Directory and output data
#Change Your output directory here
setwd("C:/Users/zhengkya/Desktop/Hackathon Project/output/")
write.csv(final_table_for_viz, "song_with_theme_word.csv", row.names=F)
write.csv(theme_reference, "theme_reference.csv", row.names=F)
