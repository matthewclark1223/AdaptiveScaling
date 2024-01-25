library(dplyr)



Clean<-function(FILE){
  df<-readxl::read_excel(paste0("./Data/Uncleaned/",FILE),sheet="MATLAB",col_names =F)
  
  if(ncol(df)>3){
    print("TOO many columns")
    print(FILE)}
  
  if(ncol(df)<3){
    print("NOT ENOUGH columns")
    print(FILE)}
  
  if(ncol(df)==3){
  colnames(df)<-c("Year","DELETE","Adopters")
  df<-df%>%select(Year,Adopters)}
  
}

for(i in 1:length(list.files("./Data/Uncleaned/"))){
  FILE<-list.files("./Data/Uncleaned/")[i]
  df<-Clean(FILE)
  x<-list.files("./Data/Uncleaned/")[i]
  x<-substr(x,34,100)
  name<-gsub(".xlsx","",x)
  write.csv(df,paste0("./Data/Cleaned/",name,".csv") )
}





