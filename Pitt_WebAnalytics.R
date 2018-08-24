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
Pitt <- read.csv('content_phase_report_20180726_PGH.csv')
Pitt_comments <- read.csv('phase_comments_report_20180731_PGH.csv')

#remove posts from admins (90)
Pitt <- subset(Pitt,Author...Platform.role != 'ADMIN')
Pitt_comments <- Pitt_comments %>% 
    filter(role != 'ADMIN') %>% 
    unite(Comments, c('Top.level.comment','Child.commen'), sep = '')

############################## cleaning ###########################################
# recode mission category since it's so long
str(Pitt$Missions)
Pitt$Missions <-  factor(Pitt$Missions)
levels(Pitt$Missions)
levels(Pitt$Missions) <-  list('Question' = "Ask a question",'Inspire' = "Inspire innovation",'Share' = "Share a recent journey")
levels(Pitt$Missions)

Pitt$Summary <- as.character(Pitt$Summary)
Pitt$Explore...Full.description <- as.character(Pitt$Explore...Full.description)
# replace empty explore posts with their summary.
for(i in 1:nrow(Pitt)){
    if(nchar(Pitt$Explore...Full.description[i]) == 0){
        Pitt$Explore...Full.description[i] = Pitt$Summary[i]
    }
}

# remove non ascii characters
Pitt$Explore...Full.description <-  replace_non_ascii(Pitt$Explore...Full.description)
Pitt_comments$Comments <- replace_non_ascii(Pitt_comments$Comments)

# remove weird apostrophe error
Pitt$Explore...Full.description <-  gsub(pattern = 'a cent ',replacement = "'",x = Pitt$Explore...Full.description)
Pitt_comments$Comments <- gsub('a cent ', "'", x = Pitt_comments$Comments)
##################### activity ###############################
# number of authors?
length(unique(Pitt$Author...ID)) #71

# tally up ideas/experiences. Use Mission category
Pitt %>% 
    group_by(Missions) %>% 
    summarise(count = n())

# totals by category
Pitt %>% 
    select(Missions,Applause...Total.excluding.challenge.team, Comments...Total.excluding.challenge.team, Views...Total) %>% 
    group_by(Missions) %>% 
    summarise(applause = sum(Applause...Total.excluding.challenge.team), comment= sum(Comments...Total.excluding.challenge.team), view = sum(Views...Total)) 
# applause and comment rate by category
Pitt %>% 
    select(Missions,Applause...Total.excluding.challenge.team, Comments...Total.excluding.challenge.team, Views...Total) %>% 
    group_by(Missions) %>% 
    summarise(applause_rt = sum(Applause...Total.excluding.challenge.team)/n(), comment_rt = sum(Comments...Total.excluding.challenge.team)/n(), view_rt = sum(Views...Total)/n())


# Create impact field (combination of views, applause, comments)
Pitt$Views_Z <- (Pitt$Views...Total - mean(Pitt$Views...Total))/ sd(Pitt$Views...Total)
Pitt$CommentsZ <- (Pitt$Comments...Total.excluding.challenge.team - mean(Pitt$Comments...Total.excluding.challenge.team)) / sd(Pitt$Comments...Total.excluding.challenge.team)
Pitt$ApplauseZ <- (Pitt$Applause...Total.excluding.challenge.team - mean(Pitt$Applause...Total.excluding.challenge.team)) / sd(Pitt$Applause...Total.excluding.challenge.team)
Pitt$impact <- Pitt$Views_Z + Pitt$CommentsZ + Pitt$ApplauseZ
summary(Pitt$impact)

# sort by impact
Pitt %>% 
    group_by(ID) %>% 
    summarise(impact = impact) %>% 
    arrange(desc(impact))
print(Pitt[Pitt$ID==37,]$Title)


##################################### start of nlp #####################################################
Pitt$Explore...Full.description <- tolower(Pitt$Explore...Full.description)
Pitt_comments$Comments <- tolower(Pitt_comments$Comments)
# remove numbers
Pitt <- Pitt %>% 
    group_by(ID) %>% 
    mutate(Full.description = gsub('[0-9]+', '', Explore...Full.description)) %>% 
    ungroup()
Pitt_comments <- Pitt_comments %>% 
    group_by(Contribution) %>% 
    transmute(Comments = gsub('[0-9]+','',Comments)) %>% 
    ungroup()

Pitt1 <-  Pitt %>% 
    select(Text.ID,Explore...Full.description) %>% 
    `colnames<-`(c('ID','Content'))
PittC1 <- Pitt_comments %>% 
    select(Contribution,Comments) %>% 
    `colnames<-`(c('ID','Content'))
Pitt_full <-  rbind(Pitt1,PittC1)
# I like this collection of stopwords the best, it seems to be more exhaustive than others
data("stop_words")

# tokenize
mtk <- Pitt_full %>% 
    select(Content) %>% 
    unnest_tokens(word, Content) %>% 
    anti_join(stop_words)

mtk 

bigrams <- Pitt_full %>% 
    unnest_tokens(bigram, Content, token = 'ngrams', n = 2)

bigrams_sep <- bigrams %>% 
    separate(bigram,c('word1','word2'),sep = ' ')

bigrams_sep <- bigrams_sep %>% 
    filter(!word1 %in% stop_words$word) %>% 
    filter(!word2 %in% stop_words$word)
# count of words, with ggplot bar chart
mtk %>% 
    count(word,sort = TRUE) %>% 
    top_n(20,n) %>% 
    ggplot(aes(x = reorder(word,n), y=n)) + geom_col() + coord_flip()

bigrams_count <- bigrams_sep %>% 
    count(word1,word2, sort = TRUE)

bigrams_count <- unite(bigrams_count,'bigram', word1, word2, sep = ' ')

bigrams_count %>% 
    top_n(20,n) %>% 
    ggplot(aes(x = reorder(bigram,n), y=n, fill = n)) + geom_col()+coord_flip()+labs(x = 'Bigram')
# wordcloud
mtk %>% 
    count(word) %>% 
    with(wordcloud(word, n, max.words = 100,colors = brewer.pal(8, "Dark2")))

bigrams_count %>% 
    with(wordcloud(bigram,n,max.words = 60, colors = brewer.pal(8, 'Dark2')))

#################################### sentiment ###############################################
word_sent <- mtk %>%
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
long_s = as.String(Pitt$Explore...Full.description)
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
pus <- corpus(Pitt_full,docid_field = 'ID',text_field = 'Content')
summary(pus,5)
toks <- tokens(pus,remove_numbers = TRUE, remove_punct = TRUE)
head(toks,10)

no_stop_toks <- tokens_remove(toks, stop_words$word)
nstok.dfm <- dfm(no_stop_toks)
nstok.dfm <- dfm_trim(nstok.dfm)
topfeatures(nstok.dfm)
Pitt.fcm <- fcm(nstok.dfm)
dim(Pitt.fcm)

feat <- names(topfeatures(Pitt.fcm, 30))
Pitt.fcm <- fcm_select(Pitt.fcm, feat)
dim(Pitt.fcm)

# network diagram
size <- log(colSums(dfm_select(Pitt.fcm,feat)))
textplot_network(Pitt.fcm, min_freq = 0.4,omit_isolated = T,vertex_size = size / max(size) * 3)




######################### Propose Phase Impact posts ###################
PITT_propose <- read.csv('proposal_phase_report_20180803_PGH.csv')
names(PITT_propose)

# Create impact field (combination of views, applause, comments)
PITT_propose$Views_Z <- (PITT_propose$Views...Total - mean(PITT_propose$Views...Total))/ sd(PITT_propose$Views...Total)
PITT_propose$CommentsZ <- (PITT_propose$Comments...Total.excluding.challenge.team - mean(PITT_propose$Comments...Total.excluding.challenge.team)) / sd(PITT_propose$Comments...Total.excluding.challenge.team)
PITT_propose$ApplauseZ <- (PITT_propose$Applause...Total.excluding.challenge.team - mean(PITT_propose$Applause...Total.excluding.challenge.team)) / sd(PITT_propose$Applause...Total.excluding.challenge.team)

PITT_propose$impact <- PITT_propose$Views_Z + PITT_propose$CommentsZ + PITT_propose$ApplauseZ
summary(PITT_propose$impact)

# sort by impact
PITT_propose %>% 
    group_by(ID) %>% 
    summarise(impact = impact) %>% 
    arrange(desc(impact))
print(PITT_propose[PITT_propose$ID==479,]$Title)
