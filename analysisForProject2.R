library(stringr)
library(tm)
library(SnowballC)
library(RColorBrewer)
library(wordcloud)
# R studio wants an absolute path for some reason
test = read.csv("/Users/bogdanabaev/College/R_code/Project 2/employee_reviews.csv")
#throw away things we dont need
test$link = NULL
test$helpful.count = NULL
test$dates = str_sub(test$dates,-4)
test$Status = word(test$job.title,1)
test$Position = word(test$job.title, 4,-1 )
dates = as.character(test$dates)
num_dates = as.numeric(dates)
num_dates[is.na(num_dates)] = 0
test$dates = num_dates

# put jobs into categories

#~ 27000 people
test[grepl("Anonymous",test$Position),]$Position = "Other"

#~4000 consultants, research people and specialists
test[grepl("Speci",test$Position),]$Position = "Specialist"
test[grepl("Scien",test$Position),]$Position = "Specialist"
test[grepl("Research",test$Position),]$Position = "Specialist"
test[grepl("Consult",test$Position),]$Position = "Specialist"


# ~9000 people those who make decisions
test[grepl("Seni",test$Position),]$Position = "Management"
test[grepl("Director",test$Position),]$Position = "Management"
test[grepl("Head",test$Position),]$Position = "Management"
test[grepl("Lead",test$Position),]$Position = "Management"
test[grepl("Manager",test$Position),]$Position = "Management"

#~17000 people tech people
test[grepl("Software",test$Position),]$Position = "Engineer"
test[grepl("Engineer",test$Position),]$Position = "Engineer"
test[grepl("Develop",test$Position),]$Position = "Engineer"
test[grepl("Prog",test$Position),]$Position = "Engineer"
test[grepl("Data",test$Position),]$Position = "Engineer"
test[grepl("Design",test$Position),]$Position = "Engineer"
test[grepl("Solut",test$Position),]$Position = "Engineer"
test[grepl("Tech",test$Position),]$Position = "Engineer"
#~4000 everyone on the business side of things
test[grepl("Account",test$Position),]$Position = "Office"
test[grepl("Business",test$Position),]$Position = "Office"
test[grepl("Mark",test$Position),]$Position = "Office"
test[grepl("Sale",test$Position),]$Position = "Office"
test[grepl("Analys",test$Position),]$Position = "Office"
test[grepl("Recruit",test$Position),]$Position = "Office"
test[grepl("Financ",test$Position),]$Position = "Office"
test[grepl("Exec",test$Position),]$Position = "Office"
test[grepl("Assis",test$Position),]$Position = "Office"
test[grepl("Assoc",test$Position),]$Position = "Office"

test[!grepl("Management",test$Position) & !grepl("Engineer",test$Position) & !grepl("Office", test$Position) & !grepl("Specialist",test$Position),]$Position = "Other"

write.csv(test,"/Users/bogdanabaev/College/R_code/Project 2/proj2_proprocessed.csv")
# first part ends over here, save it to a file


#part two of preprocessing

df = read.csv("/Users/bogdanabaev/College/R_code/Project 2/ proj2_proprocessed.csv")

# create wordclouds for pros,cons and summary of employees feedbacks
chars = as.character(df$summary)
docs <- Corpus(VectorSource(chars))
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)

#create image of wordcloud summary
png(file="./WordCloud_summarys.png",width=1000,height=700, bg="grey30", res=300)
# for some reason sometimes, this has to be run twice to make the image...
wordcloud(d$word, d$freq, col=terrain.colors(length(d$word), alpha=0.9),max.words = 50, random.order=FALSE, rot.per=0.3,scale=c(3,.25))




chars = as.character(df$pros)
docs <- Corpus(VectorSource(chars))
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)

#create image of wordcloud pros
png(file="./Wordcloud_good.png",width=1000,height=700, bg="grey30", res=300)
# for some reason sometimes, this has to be run twice to make the image...
wordcloud(d$word, d$freq, col=terrain.colors(length(d$word), alpha=0.9),max.words = 50, random.order=FALSE, rot.per=0.3,scale=c(3,.25))



chars = as.character(df$cons)
docs <- Corpus(VectorSource(chars))
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)

#create image of wordcloud cons
png(file="./Wordcloud_bad.png",width=1000,height=700, bg="grey30", res=300)
# for some reason sometimes, this has to be run twice to make the image...
wordcloud(d$word, d$freq, col=terrain.colors(length(d$word), alpha=0.9),max.words = 50, random.order=FALSE, rot.per=0.3,scale=c(3,.25))



