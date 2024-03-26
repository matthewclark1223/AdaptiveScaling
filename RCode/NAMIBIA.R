library(tidyverse)

df2<-read.csv("./Data/Cleaned/NamibiaRegConservancies.csv")


df2$Adopters <-round(df2$Adopters/1000)

sample_n2 = round(1195156/1000)
# total population estimate from Namibia data

ggplot(df2,aes(x=Year,y=Adopters/sample_n))+geom_point()




sample_days2 = ceiling(nrow(df2)*0.5) #round up

# Choose which days the samples were taken. 
# Ideally this would be daily, but we all know that is difficult.
#sample_time = sort(sample(1:t_max, sample_days, replace=F))   #run this code if you want random sampling throughout time series

sample_time2 = 1:sample_days2 #just sample the beginning!

t_max2<-nrow(df2)+8
sample_y2<-df2$Adopters[1:sample_days2]

stan_d2 = list(n_obs = sample_days2,
              n_params = 2,
              n_difeq = 3,
              n_sample = sample_n2,
              n_fake = length(1:t_max2),
              y = sample_y2,
              t0 = 0,
              ts = sample_time2,
              fake_ts = c(1:t_max2))

# Which parameters to monitor in the model:
params_monitor = c("y_hat", "y0", "params", "fake_I") #fake_I from generated quantities


#RUN THESE THE FIRST TIME YOU RUN THE SCRIPT EACH SESSION
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())


# Fit and sample from the posterior
mod2 = stan("./StanCode/SIaM.stan",
           data = stan_d2,
           pars = params_monitor,seed = 123,
           chains = 3,
           warmup = 2000,#8000
           iter = 4000) #10000


posts2 <- rstan::extract(mod2)


draws2<-as_tibble(posts2$fake_I[,,2])%>%add_column(draw=1:6000)
names(draws2)[1:t_max2]<-1:t_max2
draws2 <-  pivot_longer(draws2, c(1:t_max2) , names_to = "mod_time")

ggplot(draws2, aes(x=as.integer(mod_time), y=value)) +
  
  tidybayes::stat_lineribbon(data=filter(draws,as.integer(mod_time)<=sample_days+1),
                             aes(fill_ramp = after_stat(.width)), 
                             .width = c(0.5,0.9), fill = "#969696",
                             color=alpha("gray",0.01),linetype="blank",
                             alpha=0.75) +
  tidybayes::stat_lineribbon(data=filter(draws,as.integer(mod_time)>sample_days),
                             aes(fill_ramp = after_stat(.width)), 
                             .width = c(0.25,0.5,0.9), fill = "#252525",
                             color=alpha("gray",0.01),linetype="blank",
                             alpha=0.75) +
  tidybayes::stat_lineribbon(data=filter(draws2,as.integer(mod_time)<=sample_days2+1), ##early
                             aes(fill_ramp = after_stat(.width)), 
                             .width = c(0.5,0.9), fill = "#74a9cf",
                             color=alpha("gray",0.01),linetype="blank",
                             alpha=0.65) +
  tidybayes::stat_lineribbon(data=filter(draws2,as.integer(mod_time)>sample_days2), ##early
                             aes(fill_ramp = after_stat(.width)), 
                             .width = c(0.25,0.5,0.9), fill = "#0570b0",
                             color=alpha("gray",0.01),linetype="blank",
                             alpha=0.65) +
  geom_vline(xintercept = sample_days2+1,linetype="dotted", color="#0570b0",size=1.2,alpha=0.65)+ ##early
  geom_line(data=df,aes(x=X,y=Adopters/sample_n),color="red",size=1.0)+
  geom_vline(xintercept = sample_days+1,linetype="longdash",size=1.2,color="#252525")+
  annotate("text", x = 5.5, y = .12, label = "True rate",color="red",fontface="bold" ,size=6)+
  annotate("text", x = 23.5, y = .12, label = "Prediction\n(updated data)",color="#252525",fontface="bold",size=5)+
  annotate("text", x = 23.5, y = .06, label = "MAE: 10.6%",color="#252525",size=6)+
  annotate("text", x = 15, y = .51, label = "Prediction\n(original data)",color="#0570b0",fontface="bold",size=5)+
  annotate("text", x = 15, y = .45, label = "MAE: 43.1%",color="#0570b0",size=6)+
  ggthemes::theme_clean()+
  xlab("Project year")+ylab("Adoption")+
  scale_y_continuous(labels=scales::percent,limits=c(0,0.65))+
  theme(axis.title = element_text(colour = "black",size=20),
        axis.text=element_text(color="black",size=16))










#####
df<-read.csv("./Data/Cleaned/NamibiaRegConservancies.csv")

Cumm_2017<-13328+ 189230
Cumm_2018<-210+2173+3752 +13328+ 189230


NEW<-data.frame(X=20:27,Year=c(2017:2024),Adopters=c(Cumm_2017,rep(Cumm_2018,7)))
df<-rbind(df,NEW)

df$Adopters <-round(df$Adopters/1000)

sample_n = round(1195156/1000)
 # total population estimate from Philippines data

ggplot(df,aes(x=Year,y=Adopters/sample_n))+geom_point()




sample_days = ceiling(nrow(df)*0.67) #round up

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
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())


# Fit and sample from the posterior
mod = stan("./StanCode/SIaM_Namibia_Update.stan",
           data = stan_d,
           pars = params_monitor,seed = 123,
           chains = 3,
           warmup = 2000,#8000
           iter = 4000) #10000

# You should do some MCMC diagnostics, including:
traceplot(mod, pars="lp__")

traceplot(mod, pars=c("params", "y0"),inc_warmup=T )
#summary(mod)$summary[,"Rhat"]

# These all check out for my model, so I'll move on.

# Extract the posterior samples to a structured list:
posts <- rstan::extract(mod)
hist(posts$params[,1])
hist(posts$params[,2])




draws<-as_tibble(posts$fake_I[,,2])%>%add_column(draw=1:6000)
names(draws)[1:t_max]<-1:t_max
draws <-  pivot_longer(draws, c(1:t_max) , names_to = "mod_time")

ggplot(draws, aes(x=as.integer(mod_time), y=value)) +
  tidybayes::stat_lineribbon(data=filter(draws,as.integer(mod_time)<=sample_days+1),
                             aes(fill_ramp = after_stat(.width)), 
                             .width = c(0.5,0.9), fill = "#969696",
                             color=alpha("gray",0.01),linetype="blank",
                             alpha=0.75) +
  tidybayes::stat_lineribbon(data=filter(draws,as.integer(mod_time)>sample_days),
                             aes(fill_ramp = after_stat(.width)), 
                             .width = c(0.25,0.5,0.9), fill = "#252525",
                             color=alpha("gray",0.01),linetype="blank",
                             alpha=0.75) +
  geom_line(data=df,aes(x=X,y=Adopters/sample_n),color="red",size=1.0)+
  geom_vline(xintercept = sample_days+1,linetype="longdash")+
  annotate("text", x = 9, y = .14, label = "True rate",color="red",fontface="bold" ,size=6)+
  annotate("text", x = 23.5, y = .275, label = "Prediction",color="#252525",fontface="bold",size=6)+
  annotate("text", x = 23.5, y = .245, label = "MAE: 10.6%",color="#252525",size=6)+
  ggthemes::theme_clean()+
  xlab("Project year")+ylab("Adoption")+
  scale_y_continuous(labels=scales::percent,limits=c(0,0.5))+
  theme(axis.title = element_text(colour = "black",size=20),
        axis.text=element_text(color="black",size=16))





#mae

Observed<-df[sample_days+1:nrow(df),]$Adopters/sample_n
Observed<-na.omit(Observed)

mod_median = na.omit(apply(posts$fake_I[,,2], 2, median)[sample_days+1:nrow(df)])
length(Observed) == length(mod_median)

mod_median<-mod_median * sample_n
Observed<-Observed* sample_n




mean(abs(mod_median-Observed)) / Observed[length(Observed)]
