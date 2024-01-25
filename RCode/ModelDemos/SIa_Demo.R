library(deSolve)
I0 = 0.02    # initial fraction adopted, seed
S0 = 1 - I0 # initial fraction available to adopt


# Assign rates of spread, drop-out, and independent uptake:
beta = 0.06 #rate of spread 0.4
#gamma = 0.15 #rate of drop-out 0.15
alpha = 0.02 #independent rate of adoption 0.002

# We will use the package deSolve to integrate, which requires certain data structures.
# Store parameters and initial values
# Parameters must be stored in a named list.
params <- list(beta = beta,
               #gamma = gamma,
               alpha=alpha)

# Initial conditions are stored in a vector
inits <- c(S0, I0) #0 denotes that it is an initial condition

# Create a time series over which to integrate.

t_min = 0
t_max = 50
times = t_min:t_max

# We must create a function for the system of ODEs.
# See the 'ode' function documentation for further insights.
SIa <- function(t, y, params) {
  with(as.list(c(params, y)), {
    
    dS = - beta * y[1] * y[2]  - alpha*y[1]#y[1] non-adopters (S) and y[2] is adopters (I)
    
    dI = beta * y[1] * y[2]  + alpha*y[1]
    
    res <- c(dS,dI)
    list(res)
  })
}

# Run the integration:
out <- ode(inits, times, SIa, params, method="rk")
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

library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())



# Fit and sample from the posterior
mod = stan("./StanCode/SIa.stan",
           data = stan_d,
           pars = params_monitor,
           chains = 3,
           warmup = 500,
           iter = 1500)

# You should do some MCMC diagnostics, including:
traceplot(mod, pars="lp__")

traceplot(mod, pars=c("params", "y0"))
#summary(mod)$summary[,"Rhat"]

# These all check out for my model, so I'll move on.

# Extract the posterior samples to a structured list:
posts <- extract(mod)
hist(posts$params[,1])
hist(posts$params[,2])

#plot it
# Proportion adopted from the synthetic data:
sample_prop = sample_y / sample_n

# Model predictions across the sampling time period.
# These were generated with the "fake" data and time series.
mod_median = apply(posts$fake_I[,,2], 2, median)
mod_low = apply(posts$fake_I[,,2], 2, quantile, probs=c(0.025))
mod_high = apply(posts$fake_I[,,2], 2, quantile, probs=c(0.975))
mod_time = stan_d$fake_ts

# Combine into two data frames for plotting
df_sample = data.frame(sample_prop, sample_time)
df_fit = data.frame(mod_median, mod_low, mod_high, mod_time)

# Plot the synthetic data with the model predictions
# Median and 95% Credible Interval

ggplot(df_sample, aes(x=sample_time, y=sample_prop)) +
  geom_point(col="black", shape = 19, size = 1.5) +
  # Error in integration:
  geom_line(data = df_fit, aes(x=mod_time, y=mod_median), color = "red",size=1) + 
  geom_line(data = df_fit, aes(x=mod_time, y=mod_high), color = "red", linetype=3,size=1) + 
  geom_line(data = df_fit, aes(x=mod_time, y=mod_low), color = "red", linetype=3,size=1) + 
  # Aesthetics
  scale_x_continuous(limits=c(0, 50), breaks=c(0,25,50)) +
  scale_y_continuous(limits=c(0,1), breaks=c(0,.5,1)) +
  theme_classic() + 
  theme_classic()+
  xlab("Time")+ylab("Predicted adoption")+
  theme(axis.title = element_text(colour = "black",size=16),
        axis.text=element_text(color="black",size=14))
