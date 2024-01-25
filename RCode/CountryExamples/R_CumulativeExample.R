df<-read.csv("./Data/Cleaned/LMMA_Samoa.csv")


sample_n = 330 # total population estimate from Samoa data

ggplot(df,aes(x=Year,y=Adopters/sample_n))+geom_point()




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



# Fit and sample from the posterior
mod = stan("./StanCode/SIaM.stan",
           data = stan_d,
           pars = params_monitor,
           chains = 3,
           warmup = 8000,seed=123,
           iter = 10000)

# You should do some MCMC diagnostics, including:
traceplot(mod, pars="lp__")

traceplot(mod, pars=c("params", "y0"),inc_warmup=T )
#summary(mod)$summary[,"Rhat"]

# These all check out for my model, so I'll move on.

# Extract the posterior samples to a structured list:
posts <- extract(mod)
hist(posts$params[,1])
hist(posts$params[,2])




draws<-as_tibble(posts$fake_I[,,2])%>%add_column(draw=1:6000)
names(draws)[1:nrow(df)]<-1:nrow(df)
draws <-  pivot_longer(draws, c(1:nrow(df)) , names_to = "mod_time")

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
  annotate("text", x = 2.5, y = .15, label = "True rate",color="red")+
  annotate("text", x = 8.5, y = .26, label = "Prediction (MAE: 1.5%)",color="#252525")+
  ggthemes::theme_clean()+
  xlab("Project year")+ylab("Adoption")+scale_x_continuous(breaks=c(2,6,10))+
  scale_y_continuous(labels=scales::percent)+
  theme(axis.title = element_text(colour = "black",size=16),
        axis.text=element_text(color="black",size=14))


#simple ribbon
draws2<-draws%>%group_by(mod_time)%>%summarise(high=quantile(value,0.95),low=quantile(value,0.05))
ggplot(draws2,aes(x=as.integer(mod_time)) ) +
  geom_ribbon(data=filter(draws2,as.integer(mod_time)>sample_days),
              aes(x=as.integer(mod_time),ymin = low, ymax = high), 
              fill = "#525252", alpha = 0.5)+
  geom_ribbon(data=draws2,
              aes(x=as.integer(mod_time),ymin = low, ymax = high), 
              fill = "#525252", alpha = 0.5)+
  #geom_point(data=df,aes(x=X,y=Adopters/sample_n),color="red")+
  geom_line(data=df,aes(x=X,y=Adopters/sample_n),color="red",size=1.0)+
  geom_vline(xintercept = sample_days+1,linetype="longdash")+
  # annotate("text", x = 5, y = .08, label = "True rate",color="red")+
  #annotate("text", x = 9.5, y = .02, label = "Prediction (MAE: 0.54%)",color="#525252")+
  ggthemes::theme_clean()+
  xlab("Project year")+ylab("Adoption")+
  theme(axis.title = element_text(colour = "black",size=16),
        axis.text=element_text(color="black",size=14))#+ylim(0,0.25)




#Colorful plot


ggplot(draws, aes(x=as.integer(mod_time), y=value)) +
  tidybayes::stat_lineribbon(data=draws,aes(fill_ramp = after_stat(.width)), 
                             .width = c(0.05,0.1,0.2,0.3,0.4,0.5,0.7,0.8,0.9), fill = "#2297E6",alpha=0.75) +
  #samples
  #geom_line(data=draws,mapping = aes(x = as.integer(mod_time), 
  #                                  y=value, group = draw), color="#252525",alpha = 0.05, size=0.1) +
  
  #points
  geom_point(data=df[1:sample_days,],aes(x=X,y=Adopters/sample_n,fill="Training"),
             col="#525252" ,shape = 21, size = 5,stroke=2) +
  geom_point(data=df[sample_days+1:nrow(df),],aes(x=X,y=Adopters/sample_n,fill="Test"),
             col="#525252" ,shape = 21, size = 5,stroke=2)+
  theme_classic()+
  scale_fill_manual(name="",values=c("Training"="#28E2E5","Test"="#CD0BBC"),
                    guide = guide_legend(reverse = TRUE))+
  xlab("Time")+ylab("Adoption")+
  theme(axis.title = element_text(colour = "black",size=16),
        axis.text=element_text(color="black",size=14))


#mae

Observed<-df[sample_days+1:nrow(df),]$Adopters/sample_n
Observed<-na.omit(Observed)

mod_median = na.omit(apply(posts$fake_I[,,2], 2, median)[sample_days+1:nrow(df)])
length(Observed) == length(mod_median)

mean(abs(mod_median-Observed))
