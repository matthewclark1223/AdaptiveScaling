source("./RCode/SplitInterventions.R")




drawsAll<-data.frame(draw=NA,mod_time=NA,value=NA,Intervention=NA,MAE=NA,Pool=NA)



for( i in c(9:18)){ #
df<-dat_split[[i]]
df$X<-1:nrow(df)
sample_n = df$Pool[1] # total population estimate keep at 1!

##

final<-tail(df,n=1)$Adopters

if((final/sample_n) <0.05 & (final/sample_n) >0.005){ # if between 0.5% and 5% divide sample/20
  sample_n<-round(sample_n/20)
  
}

if((final/sample_n) <0.005 ){ #if less that 0.5%, divide sample by 500
  sample_n<-round(sample_n/200)
  
}

sample_days = ceiling(nrow(df)*0.5) #round up

# Choose which days the samples were taken. 
# Ideally this would be daily, but we all know that is difficult.
#sample_time = sort(sample(1:t_max, sample_days, replace=F))   #run this code if you want random sampling throughout time series

sample_time = 1:sample_days #just sample the beginning!

t_max<-nrow(df)
sample_y<-df$Adopters[1:sample_days]

stan_d = list(n_obs = sample_days,
              n_params = 2,
              n_difeq = 3,
              n_sample = sample_n,
              n_fake = length(1:t_max),
              y = sample_y,
              t0 = 0,
              ts = sample_time,
              fake_ts = c(1:t_max))

# Which parameters to monitor in the model:
params_monitor = c("y_hat", "y0", "params", "fake_I") #fake_I from generated quantities


#RUN THESE THE FIRST TIME YOU RUN THE SCRIPT EACH SESSION
#library(rstan)
#rstan_options(auto_write = TRUE)
#options(mc.cores = parallel::detectCores())




#mid
if(df$Intervention[1] %in%c("Queensland","S. Austraila", "Tasmania","Kenya","Fiji","Solomon Isl.",
                            "Mexico","Philippines"
                            )){
# Fit and sample from the posterior
mod = stan("./StanCode/SIaM.stan",
           data = stan_d,
           pars = params_monitor,seed = 123,
           chains = 3,
           warmup = 3000,
           iter = 5000)}


#low
if(df$Intervention[1] %in%c("Victoria", "W. Australia","Namibia","Samoa","Tanzania CBFM",
                            "Tanzania JFM","Tanzania WMA")){
# Fit and sample from the posterior
mod = stan("./StanCode/SIaM_Low.stan",
           data = stan_d,
           pars = params_monitor,seed = 123,
           chains = 3,
           warmup = 3000,
           iter = 5000)}


#high
if(df$Intervention[1] %in%c("Marine PAs","Terrestrial PAs","Coastal PAs")){
# Fit and sample from the posterior
mod = stan("./StanCode/SIaM_High.stan",
           data = stan_d,
           pars = params_monitor,seed = 123,
           chains = 3,
           warmup = 3000,
           iter = 5000)}


# You should do some MCMC diagnostics, including:
#traceplot(mod, pars="lp__")

#traceplot(mod, pars=c("params", "y0"),inc_warmup=T )
#summary(mod)$summary[,"Rhat"]

# These all check out for my model, so I'll move on.

# Extract the posterior samples to a structured list:

name<-paste0(df$Intervention[1],".rds" )
saveRDS(mod, name )

posts <- rstan::extract(mod)



draws<-as_tibble(posts$fake_I[,,2])%>%add_column(draw=1:6000)
names(draws)[1:nrow(df)]<-1:nrow(df)
draws <-  pivot_longer(draws, c(1:nrow(df)) , names_to = "mod_time")

draws$Intervention<-df$Intervention[1]


#Do the MAE in the loop
Observed<-df[sample_days+1:nrow(df),]$Adopters/sample_n
Observed<-na.omit(Observed)

mod_median = na.omit(apply(posts$fake_I[,,2], 2, median)[sample_days+1:nrow(df)])
length(Observed) == length(mod_median)

MAE<-mean(abs(mod_median-Observed))

draws$MAE<-MAE
draws$Pool<-sample_n

drawsAll<-rbind(drawsAll,draws)





}



drawsAll<-drawsAll[-1,]
write.csv(drawsAll,"MultiInterventionPosterior.csv")

#drawsAll<-read.csv("MultiInterventionPosterior.csv")

#

addX<-function(dat){
  dat$X<-1:nrow(dat)
  return(dat)
}



FullDat<-lapply(dat_split,addX)%>%bind_rows()

#head(FullDat)
#head(drawsAll)


PoolFix<-FullDat%>%mutate(PoolFix=Pool)%>%select(Intervention,PoolFix)%>%distinct()
drawsAll<-merge(drawsAll,PoolFix,by="Intervention")
drawsAll<-drawsAll%>%mutate(value2=(Pool/PoolFix)*value, MAE2=(Pool/PoolFix)*MAE)

Sample_Days<-FullDat%>%group_by(Intervention)%>%count()%>%mutate(sample_days=ceiling(0.5*n))%>%filter(Intervention != "Brazil")
drawsAll<-merge(drawsAll,Sample_Days,by="Intervention")

#won't quite work. merge fulldat and drawsAll, also , need to transform the interventions with sparse adoption back to correct scale. 
drawsAll<-drawsAll%>%filter(Intervention != "Brazil")
FullDat<-FullDat%>%filter(Intervention != "Brazil")

drawsAll$Intervention<-factor(drawsAll$Intervention,levels=c("Philippines","Samoa","Coastal PAs","Queensland","S. Austraila","Tasmania", "Victoria",
                                                             "W. Australia","Brazil", "Kenya","Fiji", 
                                                             "Solomon Isl.","Marine PAs","Mexico","Namibia",
                                                             "Terrestrial PAs" ,"Tanzania CBFM","Tanzania JFM","Tanzania WMA"))

library(scales)



ggplot(drawsAll, aes(x=as.integer(mod_time), y=value2)) +
  tidybayes::stat_lineribbon(data=filter(drawsAll,as.integer(mod_time)<=drawsAll$sample_days+1),
                             aes(fill_ramp = after_stat(.width)), 
                             .width = c(0.5,0.9), fill = "#969696",
                             color=alpha("gray",0.01),linetype="blank",
                             alpha=0.75) +
  tidybayes::stat_lineribbon(data=filter(drawsAll,as.integer(mod_time)>drawsAll$sample_days),
                             aes(fill_ramp = after_stat(.width)), 
                             .width = c(0.25,0.5,0.9), fill = "#252525",
                             color=alpha("gray",0.01),linetype="blank",
                             alpha=0.75) +
  geom_line(data=FullDat,aes(x=as.integer(X),y=Adopters/Pool),color="red",size=1.0)+
  geom_vline(data=Sample_Days,aes(xintercept = sample_days+1),linetype="longdash")+
  facet_wrap(~Intervention,scales="free")+
  #annotate("text", x = 2.5, y = .15, label = "True rate",color="red")+
 # annotate("text", x = 8.5, y = .26, label = "Prediction (MAE: xx%)",color="#252525")+
  ggthemes::theme_clean()+
  xlab("Project year")+#scale_x_continuous(breaks=c(2,6,10))+
  scale_y_continuous(labels=scales::percent, name="Adoption")+
  scale_x_continuous(breaks=equal_breaks(n=3, s=0.05), 
                     expand = c(0.02, 0))+
  theme(axis.title = element_text(colour = "black",size=16),
        axis.text=element_text(color="black",size=10),
        strip.text = element_text(size=9))
  




ggplot(drawsAll, aes(x=as.integer(mod_time), y=value)) +
  tidybayes::stat_lineribbon(data=filter(drawsAll,as.integer(mod_time)<=drawsAll$sample_days+1),
                             aes(fill_ramp = after_stat(.width)), 
                             .width = c(0.5,0.9), fill = "#969696",
                             color=alpha("gray",0.01),linetype="blank",
                             alpha=0.75) +
  tidybayes::stat_lineribbon(data=filter(drawsAll,as.integer(mod_time)>drawsAll$sample_days),
                             aes(fill_ramp = after_stat(.width)), 
                             .width = c(0.25,0.5,0.9), fill = "#252525",
                             color=alpha("gray",0.01),linetype="blank",
                             alpha=0.75) +
  geom_line(data=FullDat,aes(x=X,y=Adopters/Pool),color="red",size=1.0)+
  geom_vline(data=Sample_Days,aes(xintercept = sample_days+1),linetype="longdash")+
  facet_wrap(~Intervention,scales="free")+
  #annotate("text", x = 2.5, y = .15, label = "True rate",color="red")+
  # annotate("text", x = 8.5, y = .26, label = "Prediction (MAE: xx%)",color="#252525")+
  ggthemes::theme_clean()+
  xlab("Project year")+ylab("Adoption")+#scale_x_continuous(breaks=c(2,6,10))+
  scale_y_continuous(labels=scales::percent)+
  theme(axis.title = element_text(colour = "black",size=16),
        axis.text=element_text(color="black",size=10))


Tab<-drawsAll%>%group_by(Intervention)%>%summarise(MAE=median(MAE2))

Chile<-data.frame(Intervention="Chile",MAE=0.024)
Tab<-rbind(Tab,Chile)

pop<-FullDat%>%group_by(Intervention)%>%summarise(Population=median(Pool), Final=max(Adopters))
pop$Population <-ifelse(pop$Intervention %in%c("Namibia","Kenya"),pop$Population*1000,pop$Population)
pop$Final <-ifelse(pop$Intervention %in%c("Namibia","Kenya"),pop$Final*1000,pop$Final)
pop$FinalPerc<-scales::percent(pop$Final/pop$Population)
pop$Final<-scales::comma(pop$Final)
pop$Population<-scales::comma(pop$Population)

Tab<-merge(pop,Tab,by="Intervention")
names(Tab)[3:4]<-c("Adopters (#)", "Adopters (%)")

Tab<-Tab[order(Tab$Intervention),]
rownames(Tab) <- NULL
Tab %>%mutate(MAE=paste0(round(MAE*100,digits=2),"%"))%>%
 kableExtra::kbl() %>%
  kableExtra::kable_classic_2(full_width = F)

