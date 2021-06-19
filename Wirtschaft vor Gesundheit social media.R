rm(list=ls())

library(word2vec)
library(udpipe)
library(quanteda)
library(quanteda.textstats)
library(dplyr)
library(ggplot2)
library(uwot)
library(tm)
library(lubridate)
library(tidyr)
library(data.table)
library(stringr)
library(ggrepel)
library(readxl)


# data wrangling ----

twitter <- twitter %>% select(Status_id, Gender, Screen_name, Language, Datum, Party_Short, is_covid19, 
                               Name, Akteur, Akteur_Typ, Party, Is_retweet, text_lower) %>%
filter(Is_retweet == "FALSE" & Party_Short != "NA" & is_covid19 == 1 & Language == "de")

twitter$Ct_id <- twitter$Status_id
twitter$Datum <- twitter$Datum %>% as_date()
twitter$Text <- twitter$text_lower
twitter$Party_Short <- ifelse(twitter$Party_Short == "jf", "FDP", 
                              ifelse(twitter$Party_Short == "JCVP", "CVP", 
                                     ifelse(twitter$Party_Short == "JUSO", "SP",
                                            ifelse(twitter$Party_Short == "jglp", "glp", 
                                                   ifelse(twitter$Party_Short == "JBDP", "BDP", 
                                                          ifelse(twitter$Party_Short == "JG", "Grüne", 
                                                                 ifelse(twitter$Party_Short == "JSVP", "SVP",
                                                                        ifelse(twitter$Party_Short == "jevp", "EVP", twitter$Party_Short))))))))
twitter <- twitter %>% filter(Party_Short == "SVP"|Party_Short == "SP"| Party_Short == "FDP" |
                                Party_Short == "Grüne"| Party_Short == "glp"| Party_Short == "EVP" &
                                Datum >= "2020-01-01")

twitter <- twitter %>% select(Ct_id, Datum, Text, Party_Short, Language)

facebook <- readRDS("parties_on_facebook.RDS")
facebook$Datum <- as_date(facebook$Datum)
facebook <- facebook %>% filter(Datum > "2020-01-01" &
                                  Language == "de" &
                                  Party_Short == "SVP"|Party_Short == "SP"| Party_Short == "FDP" |
                                  Party_Short == "Grüne"| Party_Short == "glp"| Party_Short == "EVP") %>% 
  select(Ct_id, Datum,Text, Party_Short, Language)
facebook$Text <- facebook$Text %>% tolower()
facebook <- facebook %>% filter(Datum > "2020-01-01")


data <- rbind(twitter, facebook)
data$date <- data$Datum


# sentiment analysis ----

data$Text <- gsub(" ?(f|ht)tp(s?)://(.*)", " ", data$Text)
data$Text <- gsub("[[:punct:]]", " ", data$Text)
data$Text <- gsub("[[:digit:]]", " ", data$Text)
data$Party_Short <- gsub("glp", "GLP", data$Party_Short)

data <- data %>% filter(date > "2020-03-01")
sm_corpus <- corpus(data, text_field = "Text")
corpu <- corpus(data$Text)
summary(sm_corpus)
sm_toks <- tokens(sm_corpus) %>% tokens_remove(stopwords)
sm_dfm <- dfm(sm_toks)


nrc <- read_excel("NRC-Emotion-Lexicon-v0.92-In105Languages-Nov2017Translations.xlsx")
nrc <- nrc %>% select(word=`German (de)`,Positive:Negative)
nrc_long <- nrc %>% pivot_longer(-word,names_to="sentiment",values_to="val")
nrc_long <- nrc_long %>% filter(val==1)
dict_sent_nrc <- as.dictionary(nrc_long)

sm_sent_nrc <- tokens_lookup(sm_toks, dict_sent_nrc , valuetype = "glob") %>% dfm()

sm_sent_nrc %>% dfm_group(Party_Short)

sm_sent_nrc_df <- convert(sm_sent_nrc,"data.frame") %>%
  bind_cols(docvars(sm_sent_nrc))
sm_sent_nrc_df <- sm_sent_nrc_df %>%
  mutate(sentiment = log(( positive + 0.5)/(negative + 0.5)))

## pos-neg plot----
plot_data <- sm_sent_nrc_df %>% group_by(Party_Short) %>%
  select(positive,negative,sentiment) %>%
  summarize_all(mean) %>% as.data.frame()

plot_data <- plot_data %>% 
  pivot_longer(., cols = c(positive,negative), names_to = "Var", values_to = "Val")
plot_data$Var <- gsub("positive", "positiv", plot_data$Var)
plot_data$Var <- gsub("negative", "negativ", plot_data$Var)

ggplot(data = plot_data, (aes(x = Var, y = Val)))+
  theme_classic()+
  geom_col(aes(fill = Var), show.legend = F)+
  scale_fill_manual(values = c("black", "grey"))+
  facet_wrap(~Party_Short, ncol = 6)+
  theme(axis.text.x = element_text(angle = 45,size=12,colour="Black",vjust=1,hjust=1, family = "serif"),
        axis.title = element_blank(), panel.spacing = unit(0.2, "lines"))+
  ggtitle("Sentiment nach Häufigkeiten") +
  theme(plot.title = element_text(hjust = 0.5, size = 18, family = "serif"),
        panel.border = element_rect(colour = "black", fill = NA))



## positiver verlauf plot ----
sm_sent_nrc %>% dfm_group(Party_Short)
docvars(sm_sent_nrc)
sm_sent_nrc_df <- convert(sm_sent_nrc,"data.frame") %>%
  bind_cols(docvars(sm_sent_nrc))
sm_sent_nrc_df <- sm_sent_nrc_df %>%
  mutate(sentiment = log(( positive + 0.5)/(negative + 0.5)))

pos_plot_data <- sm_sent_nrc_df %>% group_by(Party_Short) %>%
  select(positive, date) %>% as.data.frame()
pos_plot_data <- pos_plot_data[seq(1, nrow(pos_plot_data), 20), ]
ggplot(data = pos_plot_data, aes(y = positive, x = date))+
  theme_classic()+
  stat_smooth(
    geom = 'area', method = 'loess', 
    alpha = 1, aes(fill = Party_Short), show.legend = F) +
  scale_fill_manual(values = c("orange", "blue", "lightgreen", "green", "red", "darkgreen" ))+
  ylim(0,1)+
  xlim(as.Date("2020-03-01"), as.Date("2020-12-15"))+
  facet_wrap(~Party_Short, nrow = 3, ncol = 2)+
  ggtitle("Häufigkeiten der positiven Wörter im Jahresverlauf") +
  theme(plot.title = element_text(hjust = 0.5, size = 18, family = "serif"), 
        axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        axis.text.x = element_text(family = "serif", angle = 45, size = 12),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(), 
        strip.text.x = element_text(size = 14, family = "serif"),
        panel.border = element_rect(colour = "black", fill = NA))





#model training ----
set.seed(1)
stopwords <- read.table("stopwords.txt")

dates <- c( "2020-03-01", "2020-06-01","2020-07-01", "2020-08-01",
            "2020-09-01", "2020-12-01")

t <- data %>% filter(date >= "2020-03-01"  & Language == "de") %>% as.data.frame() 
t$Text <- gsub(" ?(f|ht)tp(s?)://(.*)", " ", t$Text)
t$Text <- gsub("[[:punct:]]", " ", t$Text)

t <- t %>% na.omit()
df <- data.frame()
alif <- data.frame()

#for (k in dates_lag){
for (j in 2:length(dates)-1){
  for (i in unique(t$Party_Short)[1:6]){
    tryCatch({
    d <- t %>% filter(Party_Short == i & date >= dates[j] & date <= dates[j+1]) %>% na.omit()
    d$text <- ifelse(d$Text =="", NA, d$Text)
    d <- d %>% na.omit()
    x <- data.frame(doc_id = d$Ct_id, text = tolower(d$text), stringsAsFactors = F)
    anno <- udpipe(x, "german", trace = F, parallel.cores = 2)
    anno <-subset(anno,!is.na(lemma)&nchar(lemma)>2&!upos%in%"PUNCT"&
                     !lemma%in%stopwords) 
    anno$text <- sprintf("%s//%s", anno$lemma, anno$upos)
    x <- paste.data.frame(anno, term ="text",group ="doc_id",collapse =" ")
    model <- word2vec(x = d$text, type = "cbow", dim = 50,iter = 100, split =c(" ",".\n?!"),
                      stopwords = stopwords, window = 3, min_count = 1)
    corona <- if (grepl("\\bcorona\\b",toString(d$text))) as.data.frame(predict(model, "corona", type = "nearest")) else as.data.frame(matrix(NA, 1,4)) 
    pandemie <- if (grepl("\\bpandemie\\b", toString(d$text))) as.data.frame(predict(model, "pandemie", type = "nearest")) else as.data.frame(matrix(NA, 1,4))
    covid <- if (grepl("\\bcovid\\b", toString(d$text))) as.data.frame(predict(model, "covid", type = "nearest")) else as.data.frame(matrix(NA, 1,4))
    covid19 <- if (grepl("\\bcovid19\\b", toString(d$text))) as.data.frame(predict(model, "covid19", type = "nearest")) else as.data.frame(matrix(NA, 1,4))
    virus <- if (grepl("\\bvirus\\b",toString(d$text))) as.data.frame(predict(model, "virus", type = "nearest")) else as.data.frame(matrix(NA, 1,4))
    colnames(corona) <- c("term1","term2", "similarity", "rank")
    colnames(pandemie) <- c("term1","term2", "similarity", "rank")
    colnames(covid) <- c("term1","term2", "similarity", "rank")
    colnames(covid19) <- c("term1","term2", "similarity", "rank")
    colnames(virus) <- c("term1","term2", "similarity", "rank")
    alif <- rbind(corona, pandemie, covid, covid19, virus) %>% as.data.frame()
    alif$party <- i
    alif$date <- dates[j]
    cor <- rbind(cor, corona)
    embedded <- as.matrix(model)
    viz <-umap(embedded, n_neighbors = nrow(embedded)-1, n_threads =2)
    rownames(viz) <- rownames(embedded)
    df_i  <- data.frame(word =gsub("//.+","",rownames(viz)),
                 upos =gsub(".+//","",rownames(viz)),
                 x =viz[,1],y =viz[,2],
                 party = i, date = dates[j], stringsAsFactors =FALSE) 
   df <- rbind(df, alif)
    }, error=function(e){})
  }}
df <- df %>% na.omit()

# visualisierung ----

dfs  <-subset(df, upos%in%c("NOUN",  "ADJ"))
dfs <- subset(dfs, !word%in%nonsense)
#dfs <- subset(dfs, !"*?*"%in%dfs$word)
q <- as.character("?")
dfs$word <- gsub("\\?", "", dfs$word)

dfs$word <- ifelse(dfs$word == "pandemi", "pandemie", 
                   ifelse(dfs$word == "@alain_bersen", "@alain_berset", 
                          ifelse(dfs$word == "mask", "maske", 
                                 ifelse(dfs$word == "bev?lkerung", "bevölkerung", 
                                        ifelse(dfs$word == "massnahm", "massnahmen",
                                               ifelse(dfs$word == "schutzmassnah", "schutzmassnahmen",
                                                      ifelse(dfs$word == "initiativ", "initiative", 
                                                             ifelse(dfs$word == "#staythefhom", "#staythefhome", 
                                                                    ifelse(dfs$word == "risik", "risiko", 
                                                                           ifelse(dfs$word == "gripp", "grippe",
                                                                                  ifelse(dfs$word == "begrenzungsinitiativ", "begrenzungsinitiative",
                                                                                         ifelse(dfs$word == "grenzkontroll", "grenzkontrolle",
                                                                                                ifelse(dfs$word == "epidemi", "emidemie",
                                                                                                       ifelse(dfs$word == "unternehm", "unternehmen", tolower(dfs$word)))))))))))))))



dfs$monat <- ymd(dfs$date) %>% months() %>% as.character()

svp_data <- dfs %>% filter(party == "SVP") 
svp_data["initiativ//ADJ3",] <- NA
fdp_data <- dfs %>% filter(party == "FDP")
fdp_data["initiativ//NOUN1",] <- NA
fdp_data["pandemi//ADJ",] <- NA
fdp_data <- fdp_data %>% na.omit()


# plotting----

## SVP Plot ----
svp_data <- svp_data %>% filter(date == "2020-04-01"| date == "2020-06-01" | date == "2020-11-01")
svp_data$date <- ifelse(svp_data$date == "2020-04-01", "Frühjahr 2020", 
                        ifelse(svp_data$date == "2020-06-01", "Sommer 2020", 
                               ifelse(svp_data$date == "2020-11-01", "Herbst 2020", svp_data$date)))
svp <- ggplot(data =svp_data,aes(x =x,y =y,label =word))+
  theme_bw()+
  geom_point(aes(color = date), show.legend = F, size = 2)+
  geom_text_repel(aes(color = date),max.overlaps = 15, size = 5, show.legend = T)+
  scale_color_manual(values = c("black", "red", "blue"))+
  ggtitle("Word Embedding Modell der SVP") +
  theme(plot.title = element_text(hjust = 0.5, size = 18, family = "serif"))+
  labs(x = NULL, y = NULL, color = NULL)+
  theme(axis.ticks = element_blank(), axis.text = element_blank()) +
  theme(legend.position = c(0.87, 0.8),
        legend.background = element_rect(fill = "white", color = "black"))


## FDP Plot ----
fdp_data <- fdp_data %>% filter(date == "2020-03-01"| date == "2020-06-01" | date == "2020-11-01")
fdp_data$date <- ifelse(fdp_data$date == "2020-03-01", "Frühjahr 2020", 
                        ifelse(fdp_data$date == "2020-06-01", "Sommer 2020", 
                               ifelse(fdp_data$date == "2020-11-01", "Herbst 2020", fdp_data$date)))

ggplot(data =fdp_data,aes(x =x,y =y,label =word))+
  theme_bw()+
  geom_point(aes(color = date), show.legend = F, size =2)+
  geom_text_repel(aes(color = date), max.overlaps = 300, size = 4.5, show.legend = T)+
  scale_color_manual(values = c("black", "red", "blue"))+
  ggtitle("Word Embedding Modell der FDP") +
  theme(plot.title = element_text(hjust = 0.5, size = 18, family = "serif"))+
  labs( x = NULL, y = NULL, color = NULL)+
  theme(axis.ticks = element_blank(), axis.text = element_blank()) +
  theme(legend.position = c(0.87, 0.8),
        legend.background = element_rect(fill = "white", color = "black"))