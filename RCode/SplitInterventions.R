library(dplyr)
files<-list.files("./Data/Cleaned/")
files<-files[grep(".csv",files)] # only incliude csvs. no folders
files<-files[-which(files[]%in%c("RAMSAR.csv","UNESCO.csv","WorldHeritage.csv","Brazil_RESEX.csv"))]
files<-files[-which(files[]%in%c("Chile_TURFS.csv"))]
FullDat<-data.frame(Year=NA,Adopters=NA,Intervention=NA)

for(i in 1:length(files)){
  d<-read.csv(paste0("./Data/Cleaned/",files[i]))
  d<-d%>%select(Year,Adopters)%>%
    mutate(Intervention=gsub(".csv","",files[i]))
  FullDat<-rbind(FullDat,d)
}
FullDat<-FullDat[-1,]#%>%mutate(Year=lubridate::ymd(Year, truncated = 2L))
#%>%group_by(Intervention)%>%mutate(min=min(Year),max=max(Year))%>%ungroup()


FullDat$Intervention<-factor(dplyr::recode(FullDat$Intervention,
                                           Phili = "Philippines",
                                           CoasPAs = "Coastal PAs",
                                           LMMA_Samoa = "Samoa",
                                           AusPPA_Q = "Queensland",
                                           AusPPA_SA = "S. Austraila",
                                           AusPPA_Tas = "Tasmania",
                                           AusPPA_Vic = "Victoria",
                                           AusPPA_WA = "W. Australia",
                                           Kenya_Cons = "Kenya",
                                           LMMA_Fiji = "Fiji",
                                           LMMA_Sols = "Solomon Isl.",
                                           MarPAs2 = "Marine PAs",
                                           Mexico_UMAs = "Mexico",
                                           NamibiaRegConservancies = "Namibia",
                                           TerPAs = "Terrestrial PAs",
                                           yTanzania_CBFM = "Tanzania CBFM",
                                           yTanzania_JFM = "Tanzania JFM",
                                           yTanzania_WMA = "Tanzania WMA"),
                             levels=c("Philippines","Coastal PAs","Samoa","Queensland","S. Austraila","Tasmania", "Victoria",
                                      "W. Australia","Brazil", "Kenya","Fiji", 
                                      "Solomon Isl.","Marine PAs","Mexico","Namibia",
                                      "Terrestrial PAs" ,"Tanzania CBFM","Tanzania JFM","Tanzania WMA"))

FullDat$Adopters<-ifelse(FullDat$Intervention=="Namibia",round(FullDat$Adopters/1000),FullDat$Adopters)
FullDat$Adopters<-ifelse(FullDat$Intervention=="Kenya",round(FullDat$Adopters),FullDat$Adopters)
# Now I need to add the estimate of the total number of potential adopters 

Adopter_Pool<-data.frame(Intervention=unique(FullDat$Intervention),
                         Pool = c(190599, 73768,24323,205217,71485,205,
                                  round(1709175/1000),850,330,4038,206,29554,round(1195156/1000),
                                  869,249,2918,2918,2918)) #

FullDat<-merge(FullDat,Adopter_Pool,by="Intervention",all.x=TRUE)


#Now clip leading numbers of initially seeded projects
dat_split<-(FullDat%>%group_by(Intervention)%>%group_split())


ClipLead<-function(data){
  seeded<-min(data$Adopters)
  leading<-which(data$Adopters==seeded)
  if( length(leading)>2){
  cut<-length(leading)-2
  data<-data[-(1:cut),]
  }
  
  data<-dplyr::filter(data,Adopters>0)
  return(data)  
  
}


dat_split<-lapply(dat_split,ClipLead)



