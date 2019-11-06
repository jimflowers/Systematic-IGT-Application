### Step 1 Convert PDF to Text
### Convert legis pdf to clean file of text lines -- yeah long title 
### quickly create a dataframe with pdf text data
### reading files  
### Strip sentences from pdfs
### Writes a simple file, clean lines from the pdf
### Edit for remote Nov 6
setwd("C:/Users/jflowers6/Dropbox/R/Parts of Speech")
## load libraries
library (pdftools)
library (tidyverse)
library (tokenizers)
library(stringr)
#library(tm)
library (dplyr)

files<-list.files(pattern="pdf$")
pdf.file <- getwd()
pdf.file<-files[5]
## R.home is a function that gives you the R home director
doc<-pdf_text(pdf.file)%>% strsplit(split = "\r\n")
#my_df<-as.data.frame(doc[[1]])
#my_df$Page<-"1"
my_df<-data.frame("Begin Text",1,stringsAsFactors = FALSE)
colnames(my_df)<-c("Text","Page")

Pages<-length(doc)
i<-1

while (i<=Pages) {
 
   page.data<-doc[[i]][2:(length(doc[[i]])-2)]
  
  if (i==1) {
   
     Pre.Line<-grep("AN ACT",page.data)
    page.data<-page.data[Pre.Line+1:length(page.data)]
  }
   #temp_df<-as.data.frame(doc[[i]])
  temp_df<-as.data.frame(page.data,stringsAsFactors = FALSE)
  temp_df$Page<-i
  colnames(temp_df)<-c("Text","Page")
  temp_df<-na.omit(temp_df)

  my_df<-rbind(my_df,temp_df,stringsasfactors=FALSE)
  i<-i+1
  
}

### My_df is a dataframe that contains the page no, line no, and the text from that line of a physical pdf page
View(my_df)
n2<-readline("Waiting....")

## remove NA

my_df<-my_df[-1,]
my_df<-filter(my_df,!my_df$Text=="FALSE")
#rownames(my_df)
#my_df<-my_df[2:length(my_df)]
my_df$Line.No<-substr(my_df$Text,1,2)
my_df$Text.Var<-substr(my_df$Text,4,(nchar(my_df$Text)))
View(my_df)
### Time to write the "clean data" to a file... we've converted the pdf to lines of readable text
write.csv(my_df, file="HB197_df_new.csv") # added _new to test this code on 10/30/19

