library(tidyverse)
files<-list.files("./Data/Cleaned/")
files<-files[grep(".csv",files)] # only incliude csvs. no folders
files<-files[-which(files[]%in%c("RAMSAR.csv","UNESCO.csv","WorldHeritage.csv","Brazil_RESEX.csv"))]
FullDat<-data.frame(Year=NA,Adopters=NA,Intervention=NA)

for(i in 1:length(files)){
  d<-read.csv(paste0("./Data/Cleaned/",files[i]))
  d<-d%>%select(Year,Adopters)%>%
    mutate(Intervention=gsub(".csv","",files[i]))
  FullDat<-rbind(FullDat,d)
}
FullDat<-FullDat[-1,]#%>%mutate(Year=lubridate::ymd(Year, truncated = 2L))
#%>%group_by(Intervention)%>%mutate(min=min(Year),max=max(Year))%>%ungroup()


equal_breaks <- function(n = 3, s = 0.05, ...){
  function(x){
    # rescaling
    d <- s * diff(range(x)) / (1+2*s)
    as.integer(seq(min(x)+d, max(x)-d, length=n))
  }
}

FullDat$Intervention<-factor(dplyr::recode(FullDat$Intervention,
                        AusPPA_Q = "Queensland",
                        AusPPA_SA = "S. Austraila",
                        AusPPA_Tas = "Tasmania",
                        AusPPA_Vic = "Victoria",
                        AusPPA_WA = "W. Australia",
                        Chile_TURFS = "Chile",
                        CoasPAs = "Coastal PAs",
                        Kenya_Cons = "Kenya",
                        LMMA_Fiji = "Fiji",
                        LMMA_Samoa = "Samoa",
                        LMMA_Sols = "Solomon Isl.",
                        MarPAs2 = "Marine PAs",
                        Mexico_UMAs = "Mexico",
                        NamibiaRegConservancies = "Namibia",
                        Phili = "Philippines",
                        TerPAs = "Terrestrial PAs",
                        yTanzania_CBFM = "Tanzania CBFM",
                        yTanzania_JFM = "Tanzania JFM",
                        yTanzania_WMA = "Tanzania WMA"),
                        levels=c("Philippines","Coastal PAs","Samoa","Chile",
                                 "Queensland","S. Austraila","Tasmania", "Victoria",
                                 "W. Australia", "Kenya","Fiji", 
                                 "Solomon Isl.","Marine PAs","Mexico","Namibia",
                                 "Terrestrial PAs" ,"Tanzania CBFM","Tanzania JFM","Tanzania WMA"))
                        
FullDat$Adopters<-ifelse(FullDat$Intervention=="Kenya",FullDat$Adopters*1000,FullDat$Adopters)
FullDat$Adopters<-ifelse(FullDat$Intervention=="Chile",
                         FullDat$Adopters-c(0,0,0,0,1,2,5,20,30,50,59,77,100,107,125,145,171,171),
                         FullDat$Adopters)

ggplot(FullDat,aes(x=Year,y=Adopters))+geom_point()+
  facet_wrap(~Intervention,scales="free",ncol=4)+
  ggthemes::theme_clean()+
  geom_rect(data = data.frame(Intervention = factor(c("Chile","Samoa","Philippines","Coastal PAs"))), 
            aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), 
            alpha = 0.5, fill="grey", inherit.aes = FALSE)+
  scale_x_continuous(breaks=equal_breaks(n=3, s=0.05), 
                     expand = c(0.03, 0))+
  scale_y_continuous(breaks=equal_breaks(n=3, s=0.05), 
                     expand = c(0.05, 0),labels=scales::comma)+
  theme(axis.title = element_text(colour = "black",size=18),
        axis.text=element_text(color="black",size=11),
        strip.text = element_text(face="bold"))+
 theme(plot.margin = margin(1,1,1,1, "cm"))


###########
FullDat$Intervention<-factor(FullDat$Intervention,levels=c("Fiji","Philippines","Samoa","Solomon Isl.","Kenya",
                                                           "Mexico","Namibia","Tanzania CBFM","Tanzania JFM",
                                                           "Tanzania WMA", "Queensland","S. Austraila","Tasmania", "Victoria",
                                                           "W. Australia","Marine PAs",
                                                           "Coastal PAs","Terrestrial PAs","Chile"))


#No shading
ggplot(FullDat,aes(x=Year,y=Adopters))+geom_point()+
  facet_wrap(~Intervention,scales="free",ncol=5)+
  ggthemes::theme_clean()+
  scale_x_continuous(breaks=equal_breaks(n=3, s=0.05), 
                     expand = c(0.03, 0))+
  scale_y_continuous(breaks=equal_breaks(n=3, s=0.05), 
                     expand = c(0.05, 0),labels=scales::comma)+
  theme(axis.title = element_text(colour = "black",size=18),
        axis.text=element_text(color="black",size=9),
        strip.text = element_text(face="bold"))+
  theme(plot.margin = margin(1,1,1,1, "cm"))



