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
MIA <- read.csv('content_phase_report_20180726_MIA.csv')
MIA_comments <- read.csv('phase_comments_report_20180731_MIA.csv')

#remove posts from admins (236 obs)
MIA <- subset(MIA, Author...Platform.role != 'ADMIN')
MIA_comments <- MIA_comments %>% 
    filter(role != 'ADMIN') %>% 
    unite(Comments, c("Top.level.comment","Child.commen"), sep = '')

############################## cleaning ###########################################
# recode mission category since it's so long
str(MIA$Missions)
MIA$Missions <-  factor(MIA$Missions)
levels(MIA$Missions)
levels(MIA$Missions) <-  list('Question' = "Ask a question // Formule una pregunta",'Inspire' = "Inspire Innovation //  Inspirar la innovación",'Share' = "Share a recent journey // Comparta un viaje reciente")

levels(MIA$Missions)

MIA$Summary <- as.character(MIA$Summary)
MIA$Explore...Full.description <- as.character(MIA$Explore...Full.description)
# replace empty explore posts with their summary.
for(i in 1:nrow(MIA)){
    if(nchar(MIA$Explore...Full.description[i]) == 0){
        MIA$Explore...Full.description[i] = MIA$Summary[i]
    }
}

# remove non ascii characters
MIA$`Explore...Full.description` <-  replace_non_ascii(MIA$`Explore...Full.description`)
MIA_comments$Comments <- replace_non_ascii(MIA_comments$Comments)


# remove weird apostrophe error
MIA$`Explore...Full.description` <-  gsub(pattern = 'a cent ',replacement = "'",x = MIA$`Explore...Full.description`)
MIA_comments$Comments <- gsub('a cent ',"'",MIA_comments$Comments)

##################### activity ###############################
# number of authors?
length(unique(MIA$`Author...ID`)) #210

# most active people (721 = Jineth, double posting in En/Es)
MIA %>% 
    group_by(`Author...ID`) %>% 
    count(sort = TRUE)

# tally up ideas/experiences. Use Mission category
MIA %>% 
    group_by(Missions) %>% 
    summarise(count = n())

# totals by category
MIA %>% 
    select(Missions,`Applause...Total.excluding.challenge.team`, `Comments...Total.excluding.challenge.team`, `Views...Total`) %>% 
    group_by(Missions) %>% 
    summarise(applause = sum(`Applause...Total.excluding.challenge.team`), comment= sum(`Comments...Total.excluding.challenge.team`), view = sum(`Views...Total`)) 

# applause and comment rate by category
MIA %>% 
    select(Missions,`Applause...Total.excluding.challenge.team`, `Comments...Total.excluding.challenge.team`, `Views...Total`) %>% 
    group_by(Missions) %>% 
    summarise(applause_rt = sum(`Applause...Total.excluding.challenge.team`)/n(), comment_rt = sum(`Comments...Total.excluding.challenge.team`)/n(), view_rt = sum(`Views...Total`)/n()) 

sum(MIA$`Views...Total`)

# Create impact field (combination of views, applause, comments)
MIA$Views_Z <- (MIA$`Views...Total` - mean(MIA$`Views...Total`))/ sd(MIA$`Views...Total`)
MIA$CommentsZ <- (MIA$`Comments...Total.excluding.challenge.team` - mean(MIA$`Comments...Total.excluding.challenge.team`)) / sd(MIA$`Comments...Total.excluding.challenge.team`)
MIA$ApplauseZ <- (MIA$`Applause...Total.excluding.challenge.team` - mean(MIA$`Applause...Total.excluding.challenge.team`)) / sd(MIA$`Applause...Total.excluding.challenge.team`)
MIA$impact <- MIA$Views_Z + MIA$CommentsZ + MIA$ApplauseZ
summary(MIA$impact)

MIA %>% 
    group_by(ID) %>% 
    summarise(impact = impact) %>% 
    arrange(desc(impact))
MIA[MIA$ID==207,]$Title

#################################### translation #############################################
# take care of spanish entries. translate to english and remove duplicates
# doing this here instead of before summary statistics because the double spanish posts actually accounted for a lot of views
# binary variable recode for spanish or not
### for regular content
spanish <-  numeric(nrow(MIA))
for(i in 1:nrow(MIA)){
    s = MIA$`Explore...Full.description`[i]
    if(textcat(s) == 'spanish'){
        spanish[i]=1
    }
}
sum(spanish) # only 8 posts
MIA$spanish = spanish 
span_authors <- MIA %>% 
    filter(spanish == 1) %>% 
    select(`Author...ID`)
span_authors

MIA %>% 
    filter(`Author...ID` %in% span_authors$`Author...ID`) %>%
    group_by(`Author...ID`) %>% 
    select(`Author...ID`,ID , `Explore...Full.description`)
print(MIA[MIA$ID == 330,]$`Explore...Full.description`)
MIA_eng <- MIA
MIA_eng[MIA_eng$ID == 133,]$`Explore...Full.description` <- 'They should take measures so that trucks, vans, as well as cars driven by people of the third age, do not transit along the left lane of the roads, this is what blocks the traffic in almost all the ways'

MIA_eng[MIA_eng$ID == 135,]$`Explore...Full.description` <-  'Subsequently, the divisions and subdivisions are made through scheludes in dAy-s and hours that vehicles are programmed according to the year and by letters and numbers the departures of the vehicles at peak times throughout the city, not to allow them to circulate trucks or trailers on the roads at peak times, except for emergency vehicles such as ambulances and / or firemen and police, measuring the timetables of the vehicles will allow them to substantially lower the journey of the same, those who do not comply with this will be subject and exposed to tickets for this will have several traffic regulations and to inform the city through the written and spoken media asA- as television changes and give a period of 30 days to apply This new law asA- the users could be organized for their future displacement To implement these measures to organize traffic in the city the government should give the following solutions: 1 Increase the bicycles stops at different points of the city for those short-haul, in this case should give free use of these bicycles both outward and return using cards prior registration in the county of your city. 2. This service should be free during the first 30 days. And then charge the service. 3. This encourages the user to improve their health with this practice'

MIA_eng[MIA_eng$ID == 138,]$`Explore...Full.description` <- 'One way to solve traffic problems in cities is to improve road infrastructure, create better forms of public transport and stimulate their use. Promotion of the use of the bicycle. This strategy manages to reduce dependence on the private vehicle, contributing also to the reduction of pollution within the city. In many cases it could be complemented with the promotion of small vehicles such as motorcycles. Citizens take the private car because we have to move to a remote place that we can not otherwise access more efficiently, and when we do all at once, the access roads are congested. Well, one way to avoid this is by promoting urban life on a human scale, that is, work, share and enjoy your leisure in areas of reduced scale. And for this, urban policies should be promoted that seek the diversity of the use of the city and that reward that workplaces are located close to the home. Create more roads, freeways that do not have access to the use of cargo vehicles. Use a shared vehicle like uber, lift. And another review is that sun passes are cheaper and that people have more access to this route. Thank you'

MIA_eng[MIA_eng$ID == 195,]$`Explore...Full.description` <- '
I think that if more companies put their employees to work from home, it would improve the traffic in Miami.'

MIA_eng[MIA_eng$ID == 215,]$`Explore...Full.description` <- 'Buses trains in China can have up to 1200 passengers I am sure that people and tourists will love it -All arrive on time and more quickly than everything in the express with different stops and parking lots as well as the ease of payment with cards instead of Change is easier The buses take up to 1 or 2 hours to take the trains should reach each stop for at least 10 minutes if less is a great expense but I think it will be a big gain and contribution for Miami thanks for reading my opinion yineth muAoz'

MIA_eng[MIA_eng$ID == 330,]$`Explore...Full.description` <- 'I like to use public transport to go to Brickell, because it is quick and cheaper than paying parking. however, the permanent problem is the schedule in which the buses say they pass, and the one they actually pass. They are supposed to spend every 20 minutes during the day, and less frequent at night. but the number of drivers and buses assigned make it impossible for this route to be carried out on schedule, since the traffic prevents the brickell - key biscayne from returning in time. A good solution is to make them less frequent. every hour in low hour and half hour in tip. That way people plan better, do other things while waiting, but know that at the indicated time the bus will pass. And it does not have to be eternally waiting for today to happen or not on time. yesterday I was lucky Transportation happened on time. I consider it a miracle.'
# removing the double posts (should have 234 posts now)
MIA_eng <- MIA_eng[-(MIA_eng$ID ==237),]
MIA_eng <- MIA_eng[-(MIA_eng$ID ==240),]


########## for comments
spanish <-  numeric(nrow(MIA_comments))
for(i in 1:nrow(MIA_comments)){
    s = MIA_comments$Comments[i]
    if(textcat(s) == 'spanish'){
        print(i)
        spanish[i]=1
    }
}

MIA_comments$Comments[35] <- 'Thank you very much.'
MIA_comments$Comments[38] <- 'Also taking into account transport by water and air as the use of the devices you use. In the cable cars. Thanks for your good ideas.'
MIA_comments$Comments[39] <- 'Good point. Thank you'

##################################### start of nlp #####################################################
MIA_eng$`Explore...Full.description` <- tolower(MIA_eng$`Explore...Full.description`)
MIA_comments$Comments <- tolower(MIA_comments$Comments)
# remove numbers
MIA_eng <- MIA_eng %>% 
    group_by(ID) %>% 
    mutate(Full.description = gsub('[0-9]+', '', `Explore...Full.description`)) %>% 
    ungroup()
MIA_comments <- MIA_comments %>% 
    group_by(Contribution) %>% 
    transmute(Comments = gsub('[0-9]+','',Comments)) %>% 
    ungroup()

MIA1 <- MIA_eng %>% 
    select(Text.ID,Full.description) %>% 
    `colnames<-`(c("ID",'Content'))
MIAC1 <- MIA_comments %>% 
    select(Contribution,Comments) %>% 
    `colnames<-`(c('ID','Content'))

MIA_full <- rbind(MIA1,MIAC1)

# I like this collection of stopwords the best, it seems to be more exhaustive than others
data("stop_words")
MIA_full <- as_tibble(MIA_full)
# tokenize
mtk <- MIA_full %>%
    unnest_tokens(word, Content) %>% 
    anti_join(stop_words)

mtk
bigrams <- MIA_full %>% 
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
# sort by impact
MIA_eng %>% 
    group_by(ID) %>% 
    summarise(impact = impact) %>% 
    arrange(desc(impact))

print(MIA_eng[MIA_eng$ID==434,]$'Explore...Full.description')
print(MIA_eng[MIA_eng$ID==434,]$Summary)
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

# look at posts as a whole, calculate total sentiment of post using sentimentr
# can sort by sentiment now to find most pos and neg posts
MIA_eng %>% 
    select(ID, `Explore...Full.description`) %>% 
    group_by(ID) %>% 
    mutate(s = sentiment_by(`Explore...Full.description`)$ave_sentiment) 

# 228 has a good mix of +/- sentences
text = MIA_eng[MIA_eng$ID == 228,]$`Explore...Full.description`
sentiment_by(text) %>% 
    highlight()   # cool highlighting feature from the sentimentr package 

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

long_s = as.String(MIA_full$Content)
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

unique(entities(long_doc))


############################# semantic network #########################################
# created using quanteda
# quanteda objects are required
pus <- corpus(MIA_full,docid_field = 'ID',text_field = 'Content')
summary(pus,5)
toks <- tokens(pus,remove_numbers = TRUE, remove_punct = TRUE)
head(toks,10)
stopp <- append(x = stop_words$word,c('amp'))
no_stop_toks <- tokens_remove(toks, stopp)
nstok.dfm <- dfm(no_stop_toks)
nstok.dfm <- dfm_trim(nstok.dfm, min_termfreq = 25)
topfeatures(nstok.dfm)
mia.fcm <- fcm(nstok.dfm)
dim(mia.fcm)

feat <- names(topfeatures(mia.fcm, 25))
mia.fcm <- fcm_select(mia.fcm, feat)
dim(mia.fcm)

# network diagram
size <- log(colSums(dfm_select(nstok.dfm,feat)))
textplot_network(mia.fcm, min_freq = 0.6,omit_isolated = T,edge_alpha = 0.8, edge_size = 3, vertex_size = 3, labelsize = 12)




###################################### coreNLP #########################################
# playing around with stanfords coreNLP for NER
downloadCoreNLP()
library(coreNLP)
initCoreNLP()
mystring = MIA_eng$`Explore...Full.description`
output = annotateString(mystring)
output$token$NER
output$sentiment
print.annotation(output)
getToken(output)
annotateString(mtk$word, format = "obj")



# more graphing of bigrams
library(igraph)
bigrams_count
bigram_graph <- bigrams_count %>% 
    filter(n>=5) %>% 
    graph_from_data_frame()

a <- grid::arrow(type = 'closed', length = unit(.15,'inches'))
library(ggraph)
ggraph(bigram_graph, layout = 'fr')+
    geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,arrow = a,end_cap = circle(0.07,'inches'))+
    geom_node_point(color = 'lightblue', size = 5)+
    geom_node_text(aes(label = name), vjust = 1, hjust = 1)+
    theme_void()
