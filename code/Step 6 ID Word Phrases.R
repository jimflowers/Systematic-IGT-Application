# Step 6 ID Word Phrases
# 19 Jun 2019
# JDF

library(tidyr)
library(udpipe)

# load models

udmodel <- udpipe_download_model(language = "english")
udmodel <- udpipe_load_model(file = udmodel$file_model)


# load data

x<-read.csv("Obs_POS.csv",stringsAsFactors = F)

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

