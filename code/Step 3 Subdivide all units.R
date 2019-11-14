# Step 3 Subdivide all units
# Step 3 of Basurto method
# input file is Obs_df.csv created by Step 2
# this script creates a csv file titled - Obs_srt.csv
# edited and tested 15 Jun 2019


library (tidyverse)
library (tokenizers)

# Open files
Obs_df<-read.csv("Obs_df.csv",stringsAsFactors = F)
b<-setNames(data.frame(matrix(ncol = 5, nrow=0)), c("X","Doc ID","Obs.No","Observation","Section"))
# Initialize searchstrings
subsec<-"^\\([a-z]\\)"
paras<-"^\\([0-9]\\)"
subpara<-"^\\([A-Z]\\)"
srch_str<-"^\\([a-z]\\)|^\\([0-9]\\)|^\\([A-Z]\\)"

# initialize constants
# Number of Original Observations
Kobservations<-Obs_df$Obs.No
kobservations<-length(Kobservations)

# initialize counters
i<-1

while (i<=kobservations){
  
  ksentences<-count_sentences(Obs_df$Observation[i])
  j<-2
    print(cat("Observation #",i,"has",ksentences,"sentences"))
  t<-tokenize_sentences(Obs_df$Observation[i])                                                 
  while (j<=ksentences) {
    v.X<-Obs_df$X[i]
    v.Doc.Id<-Obs_df$Doc.Id[i]
    v.Obs.No<-Obs_df$Obs.No[i]+(.1*(j-1))
    v.obs<-t[[1]][j]
    V.Sec<-Obs_df$Section[i]
    b<-data.frame("X"=v.X,"Doc Id"=v.Doc.Id,"Obs.No"=v.Obs.No,"Observation"=v.obs,"Section"=V.Sec,stringsAsFactors = F)  # a quick dataframe to be
    Obs_df<-rbind(Obs_df,b)
    j=j+1  
    }
  i=i+1
}
Obs_df<-Obs_df[order(Obs_df$Obs.No),]
write.csv(Obs_df,file="Obs_srt.csv")




  