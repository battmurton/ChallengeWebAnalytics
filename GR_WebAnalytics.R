library(dplyr)
library(ggplot2)
library(tidyr)
library(tidytext)
library(wordcloud)
library(textclean)
library(tm)
library(reshape2)
library(topicmodels)
library(widyr)
library(NLP)
library(openNLP)
library(coreNLP) # stanfords coreNLP, try for better NER
library(textcat) # for language detection
library(translateR)# for translating spanish to english (not used here)
library(sentimentr)
library(quanteda)
#library(text2vec)

setwd('C:/Users/mburto67/Documents/CityChallenge/')
GR <- read.csv('content_phase_report_20180726_GR.csv')
GR_comments <- read.csv('phase_comments_report_20180731_GR.csv')

#remove posts from admins
GR <- subset(GR,Author...Platform.role != 'ADMIN')
GR_comments <- GR_comments %>% 
    filter(role != 'ADMIN') %>% 
    unite(Comments, c("Top.level.comment","Child.commen"), sep = '')


############################## cleaning ###########################################
# recode mission category since it's so long
str(GR$Missions)
GR$Missions <-  factor(GR$Missions)
levels(GR$Missions)
levels(GR$Missions) <-  list('Question' = "Ask a question // Formule una pregunta",'Inspire' = "Inspire Innovation // Inspirar la innovación",'Share' = "Share a recent journey // Comparta un viaje reciente")
levels(GR$Missions)

GR$Summary <- as.character(GR$Summary)
GR$Explore...Full.description <- as.character(GR$Explore...Full.description)
# replace empty explore posts with their summary.
for(i in 1:nrow(GR)){
    if(nchar(GR$Explore...Full.description[i]) == 0){
        GR$Explore...Full.description[i] = GR$Summary[i]
    }
}

# remove non ascii characters
GR$Explore...Full.description <-  replace_non_ascii(GR$Explore...Full.description)
GR_comments$Comments <- replace_non_ascii(GR_comments$Comments)

# remove weird apostrophe error
GR$Explore...Full.description <-  gsub(pattern = 'a cent ',replacement = "'",x = GR$Explore...Full.description)
GR_comments$Comments <-  gsub(pattern = 'a cent ',replacement = "'",x = GR_comments$Comments)
##################### activity ###############################
# number of authors?
length(unique(GR$Author...ID)) #47

# tally up ideas/experiences. Use Mission category
GR %>% 
    group_by(Missions) %>% 
    summarise(count = n())

# totals by category
GR %>% 
    select(Missions,Applause...Total.excluding.challenge.team, Comments...Total.excluding.challenge.team, Views...Total) %>% 
    group_by(Missions) %>% 
    summarise(applause = sum(Applause...Total.excluding.challenge.team), comment= sum(Comments...Total.excluding.challenge.team), view = sum(Views...Total)) 
# applause and comment rate by category
GR %>% 
    select(Missions,Applause...Total.excluding.challenge.team, Comments...Total.excluding.challenge.team, Views...Total) %>% 
    group_by(Missions) %>% 
    summarise(applause_rt = sum(Applause...Total.excluding.challenge.team)/n(), comment_rt = sum(Comments...Total.excluding.challenge.team)/n(), view_rt = sum(Views...Total)/n())


# Create impact field (combination of views, applause, comments)
GR$Views_Z <- (GR$Views...Total - mean(GR$Views...Total))/ sd(GR$Views...Total)
GR$CommentsZ <- (GR$Comments...Total.excluding.challenge.team - mean(GR$Comments...Total.excluding.challenge.team)) / sd(GR$Comments...Total.excluding.challenge.team)
GR$ApplauseZ <- (GR$Applause...Total.excluding.challenge.team - mean(GR$Applause...Total.excluding.challenge.team)) / sd(GR$Applause...Total.excluding.challenge.team)
GR$impact <- GR$Views_Z + GR$CommentsZ + GR$ApplauseZ
summary(GR$impact)

# sort by impact
GR %>% 
    group_by(ID) %>% 
    summarise(impact = impact) %>% 
    arrange(desc(impact))
print(GR[GR$ID==318,]$Title)


##################################### start of nlp #####################################################
#### this is where we need to join the content posts and the comments 
GR$Explore...Full.description <- tolower(GR$Explore...Full.description)
GR_comments$Comments <- tolower(GR_comments$Comments)

GR <- GR %>% 
    group_by(ID) %>% 
    mutate(Full.description = gsub('[0-9]+','',Explore...Full.description)) %>% 
    ungroup()

GR_comments <- GR_comments %>% 
    group_by(Contribution) %>% 
    transmute(Comments = gsub('[0-9]+','', Comments)) %>% 
    ungroup()

GR1 <- GR %>% 
    select(Text.ID,Explore...Full.description) %>% 
    `colnames<-`(c('ID','Content'))
GRC1 <- GR_comments %>% 
    select(Contribution,Comments) %>% 
    `colnames<-`(c('ID','Content'))

GR_full <- rbind(GR1,GRC1)
# I like this collection of stopwords the best, it seems to be more exhaustive than others
data("stop_words")

#GR_full$Content <- as.character(GR_full$Content)
# tokenize
mtk <- GR_full %>% 
    unnest_tokens(word,Content) %>% 
    anti_join(stop_words)
############### external word cloud txt file #########
write(as.String(mtk$word), file = 'example.txt')
####################################################
bigrams <- GR_full %>% 
    unnest_tokens(bigram, Content, token  = 'ngrams', n = 2)

bigrams_sep <- bigrams %>% 
    separate(bigram, c('word1','word2'), sep = ' ')

bigrams_sep <- bigrams_sep %>% 
    filter(!word1 %in% stop_words$word) %>% 
    filter(!word2 %in% stop_words$word)

# count of words, with ggplot bar chart
mtk %>% 
    count(word,sort = TRUE) %>% 
    top_n(20,n) %>% 
    ggplot(aes(x = reorder(word,n), y=n)) + geom_col() + coord_flip()

bigrams_count <- bigrams_sep %>% 
    count(word1,word2,sort = TRUE)

bigrams_count <- unite(bigrams_count, 'bigram', word1, word2, sep = ' ') %>% 
    filter(!grepl('grand',bigram))

bigrams_count %>% 
    top_n(20,n) %>% 
    ggplot(aes(x = reorder(bigram,n), y = n, fill = n)) + geom_col()+coord_flip()+labs(x = 'Bigram')

# wordcloud
mtk %>% 
    count(word) %>% 
    with(wordcloud(word, n, max.words = 100,colors = brewer.pal(8, "Dark2")))

bigrams_count %>% 
    with(wordcloud(bigram,n,max.words = 50, scale = c(2.5,.5), colors = brewer.pal(8, 'Dark2')))
#################################### sentiment ###############################################
grand = data.frame(c('grand'))
grand <- `colnames<-`(grand,'word')

word_sent <- mtk %>%
    anti_join(grand) %>% # get rid of grand from grand rapids for sentiment analysis
    inner_join(get_sentiments("bing")) %>%
    count(word, sentiment, sort = TRUE) %>%
    ungroup()

# top +/- words
word_sent %>% 
    group_by(sentiment) %>% 
    top_n(10) %>% 
    ungroup() %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(word, n, fill = sentiment)) + geom_col(show.legend = FALSE) +
    facet_wrap(~sentiment, scales = "free_y") + labs(y = "Contribution to sentiment", x = NULL) +
    coord_flip()+ theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14, face = "bold"),strip.text.x = element_text(size = 13)) + scale_fill_manual(values=c("#F8766D", "#00BA38"))


# word cloud shaded by sentiment
mtk %>%
    anti_join(grand) %>% 
    inner_join(get_sentiments("bing")) %>%
    count(word, sentiment, sort = TRUE) %>%
    acast(word ~ sentiment, value.var = "n", fill = 0) %>%
    comparison.cloud(colors = c("#F8766D", "#00BA38"),
                     max.words = 100)


####################### N.E.R. ####################################
# need this because there is no easy way to access people, loc, or org unlike with word and sentence annotator
entities <- function(doc, kind) {
    s <- doc$content
    a <- annotations(doc)[[1]]
    if(hasArg(kind)) {
        k <- sapply(a$features, `[[`, "kind")
        s[a[k == kind]]
    } else {
        s[a[a$type == "entity"]]
    }
}

# re run with capitals in place
long_s = as.String(GR_full)
long_s

word_ann <- Maxent_Word_Token_Annotator()
sent_ann <- Maxent_Sent_Token_Annotator()
person_ann <- Maxent_Entity_Annotator(kind = "person")
location_ann <- Maxent_Entity_Annotator(kind = "location")
organization_ann <- Maxent_Entity_Annotator(kind = "organization")
pipeline <- list(sent_ann,
                 word_ann,
                 person_ann,
                 location_ann,
                 organization_ann)

?annotate
long_annotations <- annotate(long_s, pipeline)
long_doc <- AnnotatedPlainTextDocument(long_s, long_annotations)

entities(long_doc, kind = 'organization')

person_NER <- as.data.frame(list(entities(long_doc, kind = "person")),col.names = c('person'))
pn <- person_NER %>% 
    count(person,sort = T)
location_NER <- as.data.frame(list(entities(long_doc, kind = "location")),col.names = c('location'))
blah <- location_NER %>% 
    count(location, sort=T) %>% 
    top_n(40)


org_NER <- as.data.frame(list(entities(long_doc, kind = "organization")), col.names = c('org'))
on <- org_NER %>% 
    count(org,sort = T) %>% 
    top_n(20)


any_NER <- as.data.frame(list(entities(long_doc)), col.names = c('name'))
length(unique(any_NER$name))
any_NER %>% 
    count(name) %>% 
    with(wordcloud(name,n,min.freq = 1, max.words = 50,colors = brewer.pal(8, "Dark2")))


entities(long_doc)

############################# semantic network #########################################
# created using quanteda
# quanteda objects are required
pus <- corpus(GR_full,docid_field = 'ID',text_field = 'Content')
summary(pus,5)
toks <- tokens(pus,remove_numbers = TRUE, remove_punct = TRUE)
head(toks,10)

no_stop_toks <- tokens_remove(toks, stop_words$word)
nstok.dfm <- dfm(no_stop_toks)
nstok.dfm <- dfm_trim(nstok.dfm)
topfeatures(nstok.dfm)
gr.fcm <- fcm(nstok.dfm)
dim(gr.fcm)

feat <- names(topfeatures(gr.fcm, 30))
gr.fcm <- fcm_select(gr.fcm, feat)
dim(gr.fcm)

# network diagram
size <- log(colSums(dfm_select(gr.fcm,feat)))
textplot_network(gr.fcm, min_freq = 0.4,omit_isolated = T,vertex_size = size / max(size) * 3)
