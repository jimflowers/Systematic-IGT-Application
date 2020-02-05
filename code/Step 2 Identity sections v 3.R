# Step 2 Identify Sections in the bill
# Author: JDF
# playing with Regex
# this code creates initial units of observations which have multiple sentences
# within the sections of georgia code
# this script originated  25 Jun 2019
# this script edited      27 Nov 2019
# this script edited      21 Jan 2020
#this script edited       5 Feb 2020  v 3.0 



# i_file is the input file
# o_file is the output file

library (tidyverse)
library (tokenizers)


# library(stringr) tidyverse attaches
#library(tm)
# library(dplyr) - since tidyverse attaches this package

# -------------- Load Files
i_file<-file.choose(new = FALSE) # uses a dialog box to let the user pick the file to process

test_frame<-read.csv(i_file,stringsAsFactors = F)
Obs_df<-setNames(data.frame(matrix(ncol = 6, nrow=0),stringsAsFactors = F), c("Doc ID","Obs.No","Observation","Section","OCGA","OCGA_SUB"))
t_frame<-setNames(data.frame(matrix(ncol=7, nrow=0),stringsAsFactors=F),c("Text","Page","Line.No","text.var","Section","OCGA","OCGA_SUB"))
names(test_frame)[5]<-"text.var"
outline_data<-setNames(data.frame(matrix(ncol=3,nrow=0),stringsAsFactors = F),c("outline_type","outline_level","location"))

# ------------ Set up key variables
subsec<-"\\([a-z]\\)"
paras<-"\\([0-9]\\)"
subpara<-"\\([A-Z]\\)"
division<-"\\([(ix|iv|v?i{0,3})]\\)"
subdivision<-"\\([(IX|IV|V?I{0,3})]\\)"
#srch_str<-"^\\([a-z]\\)|^\\([0-9]\\)|^\\([A-Z]\\)"  # this string does not contain divisions and subdivisions
#srch_str<-"^\\([a-z]\\)|^\\([0-9]\\)|^\\([A-Z]\\)|^\\([a-z]\\)\\([0-9]\\)|^\\([0-9]\\)\\([A-Z]\\)" #this will catch (a)(1) or (1)(A)

srch_str<-"\\(\\w\\)\\(\\w\\)\\s"
#code_sec_str<-"\\b\\d{1,3}\\-\\d{1,3}\\-\\d{1,3}\\.?\\d+\\b" #this finds Title_Ch_Sec references within a sentence
#code_sec_str<-"^[0-9]+\\-[0-9A-Z]+\\-\\d{1,3}\\.?\\d+\\b"
code_sec_str<-"\\b\\d{1,3}\\-\\w+\\-\\w+\\.?\\d?\\b"
#code_sec_str3<-"\\b\\d{1,3}\\-\\w+\\-\\w?\\.?\\d+\\b|^\\s+\"\\b\d{1,3}\\-\\w+\\-\\w?\\.?\\d+\\b|^\\s+\\b\d{1,3}\\-\\w+\\-\\w?\\.?\\d+\\b"
code_sec_str4<-"^\\s+\\w+\\-\\w+\\-\\w+\\.?\\d?"
# code_sec_str4<-"^\\s+\\w+\\-\\w+\\-\\w+\\.?\\d?|^\\s+|\"\\w+\\-\\w+\\-\\w+\\.w?\\w?"
code_sec_str2<-"[0-9]+\\-\\w+\\-\\w+\\.$"  # this finds Title_Ch_sec as section breaks like "31-9A-1."
#ocga_all<-"^(\\((.*?)\\)\\s)"
ocga_all<-"^\\s*(\\(.*?\\)\\s+)" # -- this regex was finding any match, even those within a sentence referencing a section
#ocga_all<-"(?<=(; or))\\s+(\\(.*?\\)\\s)" # this will look only for those at the beginning of the line with 2 or more spaces before the divisions id
# ocga_all<-"^\\s*(\\([[:alnum:]]\\)\\s)|^\\s+(\\([[:alnum:]]\\)\\s)" # looks for beginning quotes and ignores the "
# can't eliminate quote above using ''"
# Identify location of SECTION, double quotes, ocga changes --------------------------------------------


section_array<-grep("SECTION",test_frame$text.var) # finds outline indicator for bill section 1,2,...
period_present<-grepl("\\.|CHAPTER",test_frame$text.var)
OCGA_present<-grepl(code_sec_str4,test_frame$text.var)
OCGA_array<-grep(code_sec_str4,test_frame$text.var) #finds legal citations as outline indicators Ex: 31-9A-1.
OCGA_array<-keep(OCGA_array,period_present[OCGA_array-1])

tot.sections<-length(section_array)-2  ### (last 2 sections do not matter -- has instructions for when law takes effect)
tot_ocga_changes<-length(OCGA_array) # each time code_sec_str4  match found
section_array.end<-grep("\"",test_frame$text.var) # maps location of double quotes
beg.quote<-grep("^\\s+\"",test_frame$text.var)
titchsec<-grep(code_sec_str4,test_frame$text.var)
chi_title<-vector(mode="character", length=tot.sections)
eof_test_frame<-dim(test_frame)[1]

# find the title-chapter-section for each bill section


#i<-2  # section 1 is definitions so start at 2

i<-1 # grabbing the definitions - start with section 1
#num_sections<-2  # grab first 2 sections

# Basurto et al skipped definitions
# so we will remove those lines from the test_frame -- actually, I keep definitions now

# _____ this section much more elegant
# but, it does not pay attention to OCGA changes in Section 6, nor does it add parts, subparts to the equation
# and it leaves off .23  e.g. 39-1a-111.23

while (i<=tot.sections){
  if (grepl("CHAPTER",test_frame$text.var[beg.quote[i]])){
    ocga_tca<-gsub("\\.$","",trimws(test_frame$text.var[beg.quote[i]+1]))
  }else{
  # capture OCGA from the Scribner's instructions under "Section"
    ocga_tca<-discard(str_extract(test_frame$text.var[section_array[i]:beg.quote[i]-1],code_sec_str),is.na)
  }
  s_frame<-cbind(test_frame[(section_array[i]):(section_array[i+1]-1),],"Section"=i,"OCGA"=ocga_tca,stringsAsFactors=F) #modified 12/2/19 to capture Section
  # Find OCGA changes within Bill Sections
  period_present<-grepl("\\.|CHAPTER",s_frame$text.var)
  OCGA_array<-grep(code_sec_str4,s_frame$text.var)
  OCGA_array<-keep(OCGA_array,period_present[OCGA_array-1])
  
  j<-1
  sframe_end<-nrow(s_frame)
  j_array<-length(OCGA_array)
  while (j<=j_array) {

    if (j==j_array) {
      s_frame$OCGA[OCGA_array[j]:sframe_end]<-gsub("\"","",str_extract(s_frame$text.var[OCGA_array[j]],code_sec_str4))
    } else {
      s_frame$OCGA[OCGA_array[j]:OCGA_array[j+1]]<-gsub("\"","",str_extract(s_frame$text.var[OCGA_array[j]],code_sec_str4))
    }

    j<-j+1
  }

   t_frame<-rbind(t_frame,s_frame)
  i<-i+1
}

#----- end code above works... Section of bill as an action scenario is preserved
#--- but the OCGA section is not correct  5 Jan 9:24

# whatif the Code section changes within a bill section?


# Eliminate pesky quotes around inserted sections -------------------------
#temporarily commented out -- the spaces and quotes are good map signals
t_frame$text.var<-gsub("\"","",t_frame$text.var)  # remove quotes
t_frame$text.var<-trimws(t_frame$text.var) # remove trailing whitespaces for each line
t_frame$OCGA<-gsub("\\.$","",t_frame$OCGA) # removes . from line demarcing OCGA section
# -------------------------------------------------------------------------



# Now, we capture the OCGA indicators

ocga_new<-str_extract(t_frame$text.var,ocga_all)

t_frame$ocga_sub<-trimws(ocga_new)

tt<-nrow(t_frame)
i<-2
ocga_var<-t_frame$OCGA[i]
while (i<=tt){
  if (ocga_var==t_frame$OCGA[i]){
    if (is.na(t_frame$ocga_sub[i])) { t_frame$ocga_sub[i]<-t_frame$ocga_sub[i-1] }}
    else {ocga_var<-t_frame$OCGA[i]}
  i<-i+1
  print (i)
}

# 

#--- Flags the changes to subdivisions- as identified at the start of line -- but not 100% accurate
#--- happenstance may place a code reference at the beginning of the line
#--- will need to look behind to see if I have the conclusion of an observation before marking the next
#--- start as a legit subdivision

## the next part is not working



# Capture OCGA subsection and paragaph indicators -------------------------


q<-nrow(t_frame)  # maximum number of lines in the bill

#--- tx needs to find subdivision changes

tx<-grep(ocga_all,t_frame$text.var)  # builds a vector of subsection changes



# tx is not correct as it will find a section that is just part of the sentence
# that happens to start at the beginning of the line

numsec<-length(tx)  ## the number of sections that will create observations

s<-1

# now we build our output file
#-- the initial observation is all words within an ocga subdivsion
#--

while (s<=numsec)  {

  start<-tx[s]# where the section begins

  if (s==numsec) {
    nxt<-q
  }  else {
    nxt<-tx[s+1]-1  # where it ends  - the last line of that section
  }

  # sec<-substr(trimws(t_frame$text.var[start]),1,3) #the section id  and this doesn't not find (a)(1)
  sec<-t_frame$ocga_sub[start]  #make sure the srch_str is the right pattern
  ocga_txfr<-t_frame$OCGA[start]
  test<-""
  i<-start

  # if (is.na(ti_ch_sec)) {
  #   ti_ch_sec<-t_frame$OCGA
  # }
  while (i<=nxt) {
    test<-paste(test,trimws(t_frame$text.var[i])) #builds the observation
    i<-i+1
  }

  ti_ch_sec<-t_frame$OCGA[nxt]
  if (is.na(ti_ch_sec)) {
    ti_ch_sec<-t_frame$OCGA[start]
  }

 # ti_ch_sec<-str_extract(t_frame$text.var[i],code_sec_str4)
   # build the record and bind it to the dataframe
    b<-data.frame("Title_Chap"=ti_ch_sec ,"Section"=sec,"Observation"=test,"Doc Id"=1,"Obs.No"=s,"Start"=start,"End"=nxt,stringsAsFactors = F)  # a quick dataframe to be appended to the main data frame
    Obs_df<-rbind(Obs_df,b)
    s<-s+1  #off to the next section

  }


### Time to write the "clean data" to a file... we've converted the pdf to lines of readable text
o_file<-gsub(".csv","2.csv",i_file) #2 is the indicator that the file has been processed by Step 2
write.csv(Obs_df, file=o_file)
