# Step 5 Tokenize words add POS
# JDF 15 June 2019
# taken from test opennlp.R - in case something breaks


library(stringr)
library(purrr)
library(tidyr)
library(udpipe)
library(lattice)

# load models

udmodel <- udpipe_download_model(language = "english")
udmodel <- udpipe_load_model(file = udmodel$file_model)

# below is code the is assigning POS to strings
# edited line to move all Observations to X -- it worked
# each sentence is assigned its appropriate POS's

#unit_obs<-read.csv("Obs_srt.csv",stringsAsFactors = F)
setwd("C:/Users/jflowers6/Dropbox/R/Parts of Speech/Data Analysis/Data")
unit_obs<-read.csv("UGA.csv",stringsAsFactors = F)
#x<-udpipe_annotate(udmodel,unit_obs$Observation) ## from Obs_srt.csv
x<-udpipe_annotate(udmodel,unit_obs$A)
xdf<-as.data.frame(x)
xdf%>%select(token,upos)

# plot upos frequency

stats<-txt_freq(xdf$upos)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = stats, col = "yellow", 
         main = "UPOS (Universal Parts of Speech)\n frequency of occurrence", 
         xlab = "Freq")

# Nouns
stats <- subset(xdf, upos %in% c("NOUN")) 
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 20), col = "cadetblue", 
         main = "Most occurring nouns", xlab = "Freq")

# Adjectives
stats <- subset(xdf, upos %in% c("ADJ")) 
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 20), col = "purple", 
         main = "Most occurring adjectives", xlab = "Freq")

# Verbs
stats <- subset(xdf, upos %in% c("VERB")) 
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 20), col = "gold", 
         main = "Most occurring Verbs", xlab = "Freq")

# using Rake
stats <- keywords_rake(x = x, term = "lemma", group = "doc_id", 
                       relevant = x$upos %in% c("NOUN", "ADJ"))
stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
barchart(key ~ rake, data = head(subset(stats, freq > 3), 20), col = "red", 
         main = "Keywords identified by RAKE", 
         xlab = "Rake")
# Keywords identifiedUsing Rake

# create new output
# Obs_POS.csv

write.csv(x,file="UGA_POS.csv")

# search for wordphases

## Find exactly this sequence of POS tags


np <- keywords_phrases(x$xpos, pattern = c("DT", "NN", "VB", "RB", "JJ"), sep = "-")
head(np)
np <- keywords_phrases(x$xpos, pattern = c("DT", "NN", "VB", "RB", "JJ"), term = x$token)
head(np)

## Find noun phrases with the following regular expression: (A|N)+N(P+D*(A|N)*N)*
x$phrase_tag <- as_phrasemachine(x$xpos, type = "penn-treebank")
nounphrases <- keywords_phrases(x$phrase_tag, term = x$token, 
                                pattern = "(A|N)+N(P+D*(A|N)*N)*", is_regex = TRUE, 
                                ngram_max = 4, 
                                detailed = TRUE)
head(nounphrases, 15)
head(sort(table(nounphrases$keyword), decreasing=TRUE), 20)

# search for nounphrase with coordination conjunction
x$phrase_tag <- as_phrasemachine(x$xpos, type = "penn-treebank")
nounphrases <- keywords_phrases(x$phrase_tag, term = x$token, 
                                pattern =  "((A(CA)*|N)*N((P(CP)*)+(D(CD)*)*(A(CA)*|N)*N)*(C(D(CD)*)
                                *(A(CA)*|N)*N((P(CP)*)+(D(CD)*)*(A(CA)*|N)*N)*)*)", is_regex = TRUE, 
                                ngram_max = 8, 
                                detailed = TRUE)
head(nounphrases, 15)
head(sort(table(nounphrases$keyword), decreasing=TRUE), 20)


# searching for simple verbphrase

verbphrases <- keywords_phrases(x$phrase_tag, term = x$token, 
                                pattern = "((A|N)*N(P+D*(A|N)*N)*P*(M|V)*V(M|V)*|(M|V)*V(M|V)*D*(A|N)*N(P+D*(A|N)*N)
                                  *|(M|V)*V(M|V)*(P+D*(A|N)*N)+|(A|N)*N(P+D*(A|N)*N)*P*((M|V)*V(M|V)*D*(A|N)
                                  *N(P+D*(A|N)*N)*|(M|V)*V(M|V)*(P+D*(A|N)*N)+))", is_regex = TRUE, 
                                ngram_max = 12, 
                                detailed = TRUE)
head(verbphrases, 15)
head(sort(table(verbphrases$keyword), decreasing=TRUE), 20)

# searching for verbphrase with coordination conjuntion

verbphrases <- keywords_phrases(x$phrase_tag, term = x$token, 
                                pattern =  "(((A(CA)*|N)*N((P(CP)*)+(D(CD)*)*(A(CA)*|N)*N)*(C(D(CD)*)
                                *(A(CA)*|N)*N((P(CP)*)+(D(CD)*)*(A(CA)*|N)*N)*)*)(P(CP)*)*(M(CM)*|V)*V(M(CM)*|V)
                                *(C(M(CM)*|V)*V(M(CM)*|V)*)*|(M(CM)*|V)*V(M(CM)*|V)*(C(M(CM)*|V)*V(M(CM)*|V)*)
                                *(D(CD)*)*((A(CA)*|N)*N((P(CP)*)+(D(CD)*)*(A(CA)*|N)*N)*(C(D(CD)*)*(A(CA)*|N)
                                *N((P(CP)*)+(D(CD)*)*(A(CA)*|N)*N)*)*)|(M(CM)*|V)*V(M(CM)*|V)*(C(M(CM)*|V)
                                *V(M(CM)*|V)*)*((P(CP)*)+(D(CD)*)*(A(CA)*|N)*N)+|((A(CA)*|N)*N((P(CP)*)+(D(CD)*)
                                *(A(CA)*|N)*N)*(C(D(CD)*)*(A(CA)*|N)*N((P(CP)*)+(D(CD)*)*(A(CA)*|N)*N)*)*)(P(CP)*)
                                *((M(CM)*|V)*V(M(CM)*|V)*(C(M(CM)*|V)*V(M(CM)*|V)*)*(D(CD)*)*((A(CA)*|N)
                                *N((P(CP)*)+(D(CD)*)*(A(CA)*|N)*N)*(C(D(CD)*)*(A(CA)*|N)*N((P(CP)*)+(D(CD)*)
                                *(A(CA)*|N)*N)*)*)|(M(CM)*|V)*V(M(CM)*|V)*(C(M(CM)*|V)*V(M(CM)*|V)*)*((P(CP)*)+(D(CD)*)
                                *(A(CA)*|N)*N)+))", is_regex = TRUE, 
                                ngram_max = 4, 
                                detailed = TRUE)
head(verbphrases, 15)
head(sort(table(verbphrases$keyword), decreasing=TRUE), 20)
