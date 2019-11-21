# Step 2 Identify Sections in the bill
# Author: JDF
# playing with Regex
# this code creates initial units of observations which have multiple sentences 
# within the sections of georgia code
# this script edited 25 Jun 2019
# this script edited 20 Nov 2019

# my_df2.csv is the file that has the strike thrus removed manually via excel
# my_df2.csv is the input file for Step 2
# my_df.csv is the output of Step 1

# i_file is the input file 
# o_file is the output file

library (tidyverse)
library (tokenizers)


# library(stringr) tidyverse attaches
#library(tm)
# library(dplyr) - since tidyverse attaches this package

i_file<-file.choose(new = FALSE) # uses a dialog box to let the user pick the file to process

test.frame<-read.csv(i_file,stringsAsFactors = F)
Obs_df<-setNames(data.frame(matrix(ncol = 4, nrow=0),stringsAsFactors = F), c("Doc ID","Obs.No","Observation","Section"))
t.frame<-setNames(data.frame(matrix(ncol=4, nrow=0)),c("Text","Page","Line.No","text.var"))
names(test.frame)[5]<-"text.var"
subsec<-"^\\([a-z]\\)"
paras<-"^\\([0-9]\\)"
subpara<-"^\\([A-Z]\\)"
#srch_str<-"^\\([a-z]\\)|^\\([0-9]\\)|^\\([A-Z]\\)"  # this string does not contain divisions and subdivisions
#srch_str<-"^\\([a-z]\\)|^\\([0-9]\\)|^\\([A-Z]\\)|^\\([a-z]\\)\\([0-9]\\)|^\\([0-9]\\)\\([A-Z]\\)" #this will catch (a)(1) or (1)(A)
srch_str<-"^\\(\\w\\)\\(\\w\\)\\s|^\\(\\w\\)\\s"
# tidy up the data -- eliminate those pesky quotes around major sections
# identify the lines with the word "SECTION"
section.array<-grep("SECTION",test.frame$text.var)
tot.sections<-length(section.array)-2  ### (last 2 sections do not matter -- has instructions for when law takes effect)
#i<-2  # section 1 is definitions so start at 2

i<-1 # grabbing the definitions - start with section 1
#num_sections<-2  # grab first 2 sections

# Basurto et al skipped definitions
# so we will remove those lines from the test.frame -- actually, I keep definitions now

while (i<=tot.sections) {
  t.frame<-rbind(t.frame,test.frame[(section.array[i]):(section.array[i+1]-1),,(factors=FALSE)])
  i<-i+1  
}


# eliminate those pesky quotes around major sections
t.frame$text.var<-gsub("\"","",t.frame$text.var)  # remove quotes
t.frame$text.var<-trimws(t.frame$text.var) # remove trailing whitespaces for each line

q<-nrow(t.frame)  # maximum number of lines
tx<-grep(srch_str,t.frame$text.var)  # builds a vector of section and subsection changes

# tx is not correct as it will find a section that is just part of the sentence
# that happens to start at the beginning of the line

numsec<-length(tx)  ## the number of sections that will create observations

s<-1


  while (s<=numsec)  {
  
    start<-tx[s]# where the section begins
 
    if (s==numsec) {
      nxt<-q
    }  else {
        nxt<-tx[s+1]-1  # where it ends   
      }

    # sec<-substr(trimws(t.frame$text.var[start]),1,3) #the section id  and this doesn not find (a)(1)
    sec<-str_extract(t.frame$text.var[start],srch_str)
   
    test<-""
    i<-start
      while (i<=nxt) {
      test<-paste(test,trimws(t.frame$text.var[i])) #builds the observation
      i<-i+1
      }
    b<-data.frame("Section"=sec,"Observation"=test,"Doc Id"=1,"Obs.No"=s,stringsAsFactors = F)  # a quick dataframe to be appended to the main data frame
    Obs_df<-rbind(Obs_df,b)
    s<-s+1  #off to the next section
  }
  

### Time to write the "clean data" to a file... we've converted the pdf to lines of readable text
o_file<-gsub(".csv","2.csv",i_file) #2 is the indicator that the file has been processed by Step 2
write.csv(Obs_df, file=o_file)  



