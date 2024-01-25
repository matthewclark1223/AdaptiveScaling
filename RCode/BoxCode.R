
library(deSolve)
I0 = 0.02    # initial fraction adopted, seed
M0 = 0.0 #Proportion Immune
S0 = 1 - (I0+M0) # initial fraction available to adopt


# Assign rates of spread, drop-out, and independent uptake:
beta = 0.15 #rate of spread 0.4
gamma = 0.03 #rate of drop-out 0.15
alpha = 0.01 #independent rate of adoption 0.002

# We will use the package deSolve to integrate, which requires certain data structures.
# Store parameters and initial values
# Parameters must be stored in a named list.
params <- list(beta = beta,
               gamma = gamma,
               alpha=alpha)

# Initial conditions are stored in a vector
inits <- c(S0, I0,M0) #0 denotes that it is an initial condition

# Create a time series over which to integrate.

t_min = 0
t_max = 50
times = t_min:t_max

# We must create a function for the system of ODEs.
# See the 'ode' function documentation for further insights.
SISa <- function(t, y, params) {
  with(as.list(c(params, y)), {
    
    dS = - beta * y[1] * y[2] +gamma * y[2] - alpha*y[1]#y[1] non-adopters (S) and y[2] is adopters (I)
    
    dI = beta * y[1] * y[2] - gamma * y[2] + alpha*y[1]
    
    dM = 0
    
    res <- c(dS,dI,dM)
    list(res)
  })
}

# Run the integration:
out <- ode(inits, times, SISa, params, method="rk")
#
# Store the output in a data frame:
out <- data.frame(out)
colnames(out) <- c("time", "S", "I")

# quick plot of the simulated dynamic
plot(NA,NA, xlim = c(t_min, t_max), ylim=c(0, 1), xlab = "Time", ylab="Proportion of population adopted")
#lines(out$S ~ out$time, col="black")
lines(out$I ~ out$time, col="red")
#legend(x = 30, y = 0.8, legend = c("Non-adopted", "Adopted"), 
#      col = c("black", "red"), lty = c(1, 1), bty="n")



sample_days = 25 # number of days sampled before fitting
sample_n = 1000 # total population estimate

# Choose which days the samples were taken. 
# Ideally this would be daily, but we all know that is difficult.
#sample_time = sort(sample(1:t_max, sample_days, replace=F))   #run this code if you want random sampling throughout time series

sample_time = 1:sample_days #just sample the beginning!

# Extract the "true" fraction of the population that is adopted on each of the sampled days:
sample_propinf = out[out$time %in% sample_time, 3]

# Generate binomially distributed data.
# So, on each day we count the number of adopters in the population.
# We expect binomially distributed error in this estimate, hence the random number generation.
sample_y = rbinom(sample_days, sample_n, sample_propinf)
points(x=1:sample_days,y=sample_y/sample_n,col="blue")



###fitting Stan data

stan_d = list(n_obs = sample_days,
              n_params = length(params),
              n_difeq = length(inits),
              n_sample = sample_n,
              n_fake = length(1:t_max),
              y = sample_y,
              t0 = 0,
              ts = sample_time,
              fake_ts = c(1:t_max))

# Which parameters to monitor in the model:
params_monitor = c("y_hat", "y0", "params", "fake_I") #fake_I from generated quantities

library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())



# Fit and sample from the posterior
mod = stan("./StanCode/SISaM.stan",
           data = stan_d,
           pars = params_monitor,
           chains = 3,
           warmup = 500,seed=123,
           iter = 1500)

# You should do some MCMC diagnostics, including:
traceplot(mod, pars="lp__")

traceplot(mod, pars=c("params", "y0"))
#summary(mod)$summary[,"Rhat"]


posts <- rstan::extract(mod)
hist(posts$params[,1])
hist(posts$params[,2])
median(posts$params[,2])



draws<-as_tibble(posts$fake_I[,,2])%>%add_column(draw=1:3000)
names(draws)[1:50]<-1:50
draws <-  pivot_longer(draws, c(1:50) , names_to = "mod_time")

out<-out[-4]

out<-out[seq(1,nrow(out),2),]

ggplot(draws, aes(x=as.integer(mod_time), y=value)) +
  tidybayes::stat_lineribbon(data=filter(draws,as.integer(mod_time)<=sample_days),
                             aes(fill_ramp = after_stat(.width)), 
                             .width = c(0.5,0.9), fill = "#969696",
                             color=alpha("gray",0.01),linetype="blank",
                             alpha=0.75) +
  tidybayes::stat_lineribbon(data=filter(draws,as.integer(mod_time)>sample_days-1),
                             aes(fill_ramp = after_stat(.width)), 
                             .width = c(0.25,0.5,0.9), fill = "#252525",
                             color=alpha("gray",0.01),linetype="blank",
                             alpha=0.75) +
  geom_point(data=filter(out, time<sample_days),aes(x=time,y=I),stroke=2,
             color="#67000d",size=3,shape=21,fill="#cb181d",alpha=0.8)+
  ggstar::geom_star(data=filter(out, time==50),aes(x=time,y=I),
                    color="black",size=7,fill="black")+
  #annotate("text", x = 45, y = .81, label = "Target",color="black",fontface="bold",size=7)+
  #annotate("text", x = 40, y = .60, label = "Prediction",color="#252525",fontface="bold",size=7)+
  geom_vline(xintercept = sample_days,linetype="longdash")+
  #annotate("text", x = 6, y = .22, label = "True rate",color="red",fontface="bold",size=7)+
  #annotate("text", x = 14, y = .22, label = "Prediction (MAE: 5.2%)",color="#252525",fontface="bold",size=7)+
  ggthemes::theme_clean()+
  xlab("Project timeline")+ylab("Adoption")+
  scale_x_continuous(breaks=c(0,10,25,40,50),labels=c("Start","20%","50%","80%","End"))+
  scale_y_continuous(labels=scales::percent,limits=c(0,1))+
  theme(axis.title = element_text(colour = "black",size=16),
        axis.text=element_text(color="black",size=14))

######################################################################
I0 = 0.02    # initial fraction adopted, seed
M0 = 0.0 #Proportion Immune
S0 = 1 - (I0+M0) # initial fraction available to adopt


# Assign rates of spread, drop-out, and independent uptake:
beta = 0.15 #rate of spread 0.4
gamma = 0.03 #rate of drop-out 0.15
alpha = 0.01 #independent rate of adoption 0.002

# We will use the package deSolve to integrate, which requires certain data structures.
# Store parameters and initial values
# Parameters must be stored in a named list.
params <- list(beta = beta,
               gamma = gamma,
               alpha=alpha)

# Initial conditions are stored in a vector
inits <- c(S0, I0,M0) #0 denotes that it is an initial condition

# Create a time series over which to integrate.

t_min = 0
t_max = 50
times = t_min:t_max

# We must create a function for the system of ODEs.
# See the 'ode' function documentation for further insights.
SISa <- function(t, y, params) {
  with(as.list(c(params, y)), {
    
    dS = - beta * y[1] * y[2] +gamma * y[2] - alpha*y[1]#y[1] non-adopters (S) and y[2] is adopters (I)
    
    dI = beta * y[1] * y[2] - gamma * y[2] + alpha*y[1]
    
    dM = 0
    
    res <- c(dS,dI,dM)
    list(res)
  })
}

# Run the integration:
out <- ode(inits, times, SISa, params, method="rk")
#
# Store the output in a data frame:
out <- data.frame(out)
colnames(out) <- c("time", "S", "I")

# quick plot of the simulated dynamic
plot(NA,NA, xlim = c(t_min, t_max), ylim=c(0, 1), xlab = "Time", ylab="Proportion of population adopted")
#lines(out$S ~ out$time, col="black")
lines(out$I ~ out$time, col="red")
#legend(x = 30, y = 0.8, legend = c("Non-adopted", "Adopted"), 
#      col = c("black", "red"), lty = c(1, 1), bty="n")



sample_days = 10 # number of days sampled before fitting
sample_n = 1000 # total population estimate

# Choose which days the samples were taken. 
# Ideally this would be daily, but we all know that is difficult.
#sample_time = sort(sample(1:t_max, sample_days, replace=F))   #run this code if you want random sampling throughout time series

sample_time = 1:sample_days #just sample the beginning!

# Extract the "true" fraction of the population that is adopted on each of the sampled days:
sample_propinf = out[out$time %in% sample_time, 3]

# Generate binomially distributed data.
# So, on each day we count the number of adopters in the population.
# We expect binomially distributed error in this estimate, hence the random number generation.
sample_y = rbinom(sample_days, sample_n, sample_propinf)
points(x=1:sample_days,y=sample_y/sample_n,col="blue")



###fitting Stan data

stan_d = list(n_obs = sample_days,
              n_params = length(params),
              n_difeq = length(inits),
              n_sample = sample_n,
              n_fake = length(1:t_max),
              y = sample_y,
              t0 = 0,
              ts = sample_time,
              fake_ts = c(1:t_max))

# Which parameters to monitor in the model:
params_monitor = c("y_hat", "y0", "params", "fake_I") #fake_I from generated quantities

#library(rstan)
#rstan_options(auto_write = TRUE)
#options(mc.cores = parallel::detectCores())



# Fit and sample from the posterior
mod = stan("./StanCode/SISaM.stan",
           data = stan_d,
           pars = params_monitor,
           chains = 3,
           warmup = 500,seed=123,
           iter = 1500)

# You should do some MCMC diagnostics, including:
traceplot(mod, pars="lp__")

traceplot(mod, pars=c("params", "y0"))
#summary(mod)$summary[,"Rhat"]


posts <- rstan::extract(mod)
hist(posts$params[,1])
hist(posts$params[,2])
median(posts$params[,2])



draws<-as_tibble(posts$fake_I[,,2])%>%add_column(draw=1:3000)
names(draws)[1:50]<-1:50
draws <-  pivot_longer(draws, c(1:50) , names_to = "mod_time")

out<-out[-4]

out<-out[seq(1,nrow(out),2),]

ggplot(draws, aes(x=as.integer(mod_time), y=value)) +
  tidybayes::stat_lineribbon(data=filter(draws,as.integer(mod_time)<=sample_days),
                             aes(fill_ramp = after_stat(.width)), 
                             .width = c(0.5,0.9), fill = "#969696",
                             color=alpha("gray",0.01),linetype="blank",
                             alpha=0.75) +
  tidybayes::stat_lineribbon(data=filter(draws,as.integer(mod_time)>sample_days-1),
                             aes(fill_ramp = after_stat(.width)), 
                             .width = c(0.25,0.5,0.9), fill = "#252525",
                             color=alpha("gray",0.01),linetype="blank",
                             alpha=0.75) +
  geom_point(data=filter(out, time<sample_days+1),aes(x=time,y=I),stroke=2,
             color="#67000d",size=3,shape=21,fill="#cb181d",alpha=0.8)+
  ggstar::geom_star(data=filter(out, time==50),aes(x=time,y=I),
                    color="black",size=7,fill="black")+
  annotate("text", x = 45, y = .81, label = "Target",color="black",fontface="bold",size=7)+
  annotate("text", x = 30, y = .53, label = "Prediction",color="#252525",fontface="bold",size=7)+
  geom_vline(xintercept = sample_days,linetype="longdash")+
  #annotate("text", x = 6, y = .22, label = "True rate",color="red",fontface="bold",size=7)+
  #annotate("text", x = 14, y = .22, label = "Prediction (MAE: 5.2%)",color="#252525",fontface="bold",size=7)+
  ggthemes::theme_clean()+
  xlab("Project timeline")+ylab("Adoption")+
  scale_x_continuous(breaks=c(0,10,25,40,50),labels=c("Start","20%","50%","80%","End"))+
  scale_y_continuous(labels=scales::percent,limits=c(0,1))+
  theme(axis.title = element_text(colour = "black",size=16),
        axis.text=element_text(color="black",size=14))


#################
I0 = 0.02    # initial fraction adopted, seed
M0 = 0.0 #Proportion Immune
S0 = 1 - (I0+M0) # initial fraction available to adopt


# Assign rates of spread, drop-out, and independent uptake:
beta = 0.15 #rate of spread 0.4
gamma = 0.03 #rate of drop-out 0.15
alpha = 0.01 #independent rate of adoption 0.002

# We will use the package deSolve to integrate, which requires certain data structures.
# Store parameters and initial values
# Parameters must be stored in a named list.
params <- list(beta = beta,
               gamma = gamma,
               alpha=alpha)

# Initial conditions are stored in a vector
inits <- c(S0, I0,M0) #0 denotes that it is an initial condition

# Create a time series over which to integrate.

t_min = 0
t_max = 50
times = t_min:t_max

# We must create a function for the system of ODEs.
# See the 'ode' function documentation for further insights.
SISa <- function(t, y, params) {
  with(as.list(c(params, y)), {
    
    dS = - beta * y[1] * y[2] +gamma * y[2] - alpha*y[1]#y[1] non-adopters (S) and y[2] is adopters (I)
    
    dI = beta * y[1] * y[2] - gamma * y[2] + alpha*y[1]
    
    dM = 0
    
    res <- c(dS,dI,dM)
    list(res)
  })
}

# Run the integration:
out <- ode(inits, times, SISa, params, method="rk")
#
# Store the output in a data frame:
out <- data.frame(out)
colnames(out) <- c("time", "S", "I")

# quick plot of the simulated dynamic
plot(NA,NA, xlim = c(t_min, t_max), ylim=c(0, 1), xlab = "Time", ylab="Proportion of population adopted")
#lines(out$S ~ out$time, col="black")
lines(out$I ~ out$time, col="red")
#legend(x = 30, y = 0.8, legend = c("Non-adopted", "Adopted"), 
#      col = c("black", "red"), lty = c(1, 1), bty="n")



sample_days = 40 # number of days sampled before fitting
sample_n = 1000 # total population estimate

# Choose which days the samples were taken. 
# Ideally this would be daily, but we all know that is difficult.
#sample_time = sort(sample(1:t_max, sample_days, replace=F))   #run this code if you want random sampling throughout time series

sample_time = 1:sample_days #just sample the beginning!

# Extract the "true" fraction of the population that is adopted on each of the sampled days:
sample_propinf = out[out$time %in% sample_time, 3]

# Generate binomially distributed data.
# So, on each day we count the number of adopters in the population.
# We expect binomially distributed error in this estimate, hence the random number generation.
sample_y = rbinom(sample_days, sample_n, sample_propinf)
points(x=1:sample_days,y=sample_y/sample_n,col="blue")



###fitting Stan data

stan_d = list(n_obs = sample_days,
              n_params = length(params),
              n_difeq = length(inits),
              n_sample = sample_n,
              n_fake = length(1:t_max),
              y = sample_y,
              t0 = 0,
              ts = sample_time,
              fake_ts = c(1:t_max))

# Which parameters to monitor in the model:
params_monitor = c("y_hat", "y0", "params", "fake_I") #fake_I from generated quantities

#library(rstan)
#rstan_options(auto_write = TRUE)
#options(mc.cores = parallel::detectCores())



# Fit and sample from the posterior
mod = stan("./StanCode/SISaM.stan",
           data = stan_d,
           pars = params_monitor,
           chains = 3,
           warmup = 500,seed=123,
           iter = 1500)

# You should do some MCMC diagnostics, including:
traceplot(mod, pars="lp__")

traceplot(mod, pars=c("params", "y0"))
#summary(mod)$summary[,"Rhat"]


posts <- rstan::extract(mod)
hist(posts$params[,1])
hist(posts$params[,2])
median(posts$params[,2])



draws<-as_tibble(posts$fake_I[,,2])%>%add_column(draw=1:3000)
names(draws)[1:50]<-1:50
draws <-  pivot_longer(draws, c(1:50) , names_to = "mod_time")

out<-out[-4]

out<-out[seq(1,nrow(out),2),]

ggplot(draws, aes(x=as.integer(mod_time), y=value)) +
  tidybayes::stat_lineribbon(data=filter(draws,as.integer(mod_time)<=sample_days),
                             aes(fill_ramp = after_stat(.width)), 
                             .width = c(0.5,0.9), fill = "#969696",
                             color=alpha("gray",0.01),linetype="blank",
                             alpha=0.75) +
  tidybayes::stat_lineribbon(data=filter(draws,as.integer(mod_time)>sample_days-1),
                             aes(fill_ramp = after_stat(.width)), 
                             .width = c(0.25,0.5,0.9), fill = "#252525",
                             color=alpha("gray",0.01),linetype="blank",
                             alpha=0.75) +
  geom_point(data=filter(out, time<sample_days+1),aes(x=time,y=I),stroke=2,
             color="#67000d",size=3,shape=21,fill="#cb181d",alpha=0.8)+
  ggstar::geom_star(data=filter(out, time==50),aes(x=time,y=I),
                    color="black",size=7,fill="black")+
  #annotate("text", x = 45, y = .81, label = "Target",color="black",fontface="bold",size=7)+
  # annotate("text", x = 40, y = .54, label = "Prediction",color="#252525",fontface="bold",size=7)+
  geom_vline(xintercept = sample_days,linetype="longdash")+
  #annotate("text", x = 6, y = .22, label = "True rate",color="red",fontface="bold",size=7)+
  #annotate("text", x = 14, y = .22, label = "Prediction (MAE: 5.2%)",color="#252525",fontface="bold",size=7)+
  ggthemes::theme_clean()+
  xlab("Project timeline")+ylab("Adoption")+
  scale_x_continuous(breaks=c(0,10,25,40,50),labels=c("Start","20%","50%","80%","End"))+
  scale_y_continuous(labels=scales::percent,limits=c(0,1))+
  theme(axis.title = element_text(colour = "black",size=16),
        axis.text=element_text(color="black",size=14))










