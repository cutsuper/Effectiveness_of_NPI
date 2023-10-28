set.seed(1234)
library(here)
library(epidemia)
library(tidyverse)
options(mc.cores = parallel::detectCores())
library(ggplot2)
library(scales)
library(EnvStats)
library(gridExtra)
source("plot-with-eta.r")
levels(EuropeCovid$pops[,1])[c(3,12,14)] = c("No_Mask","Mask","No_Mask_2")
EuropeCovid$pops$pop[c(1,3,4)] = c(104782373, 216846488, 104782373)

o2d<-function(){
  i2o <- EuropeCovid$obs$deaths$i2o
  shape1 <- 5.807; scale1 <- 0.948; # infection to onset https://www.acpjournals.org/doi/10.7326/M20-0504
  shape2 <- 1.454 ; scale2 <- 10.434 # using estimated of onset to death from chess data
  x1 <- rgamma(1e6,shape=shape1,scale=scale1) # infection-to-onset distribution
  x2 <- rgamma(1e6,shape=shape2,scale=scale2) # infection-to-onset distribution
  f_cached <- ecdf(x1+x2) # empirical cumulative distribtion function
  convolution = function(u) f_cached(u)
  f = rep(NA,length(EuropeCovid$obs$deaths$i2o)) # f is the probability of dying on day i given infection
  f[1] = (convolution(1.5) - convolution(0)) # first entry
  for(i in 2:length(EuropeCovid$obs$deaths$i2o)) { # all other entries
    f[i] = (convolution(i+.5) - convolution(i-.5))
  }
  return(f)
}

dat <- read.csv("data/deathnumber_bymaskmandate_bydate.csv")
dat = dat[,c(2,3,4,4)]
colnames(dat) = c("Date", "Mask", "No_Mask", "No_Mask_2")
# Mask - 29 mask
# No_Mask - 13 no mask
# No_Mask_2 - 13 no mask
dat <- pivot_longer(dat, cols = -Date, names_to = 'country', values_to = 'deaths')
dat$deaths[is.na(dat$deaths)]=0 # to have all time series starting on the same day 30th Jan 2020
dat <- drop_na(dat) # to drop NAs just incase
dat <- mutate(dat, Date = as.Date(Date, format='%m/%d/%y'))
#dat <- mutate(dat, Date = as.Date(Date, format='%d/%m/%Y'))
dat <- rename(dat, date=Date)
dat <- filter(dat, date <= as.Date("2020-07-01")) %>% arrange(country, date) # only use data to July


# 14 states
dat <- read.csv("data/deathnumber_bymaskmandate_bydate_14.csv")
dat = dat[,c(2,4,3,3)]
colnames(dat) = c("Date", "Mask", "No_Mask", "No_Mask_2")
# Mask - 29 mask
# No_Mask - 13 no mask
# No_Mask_2 - 13 no mask
dat <- pivot_longer(dat, cols = -Date, names_to = 'country', values_to = 'deaths')
dat$deaths[is.na(dat$deaths)]=0 # to have all time series starting on the same day 30th Jan 2020
dat <- drop_na(dat) # to drop NAs just incase
dat <- mutate(dat, Date = as.Date(Date, format='%Y-%m-%d'))
#dat <- mutate(dat, Date = as.Date(Date, format='%d/%m/%Y'))
dat <- rename(dat, date=Date)
dat <- filter(dat, date <= as.Date("2020-07-01")) %>% arrange(country, date)



# formatting data to be digested by epidemia
args <- EuropeCovid
args$data <- dat
i2o <- o2d() # note choice here

# deaths are related to infections by using infection to death distribution and ifr
deaths <- epiobs(
  formula = deaths(country, date) ~ 1,
  prior_intercept = rstanarm::normal(location=1,scale = 0.1),
  prior_aux = rstanarm::normal(location=10, scale=2),
  link="identity",
  i2o=i2o * 0.01,
)

# sampling parameters of chains, iterations, seed and samling related parameters
args$algorithm <- "sampling"
args$obs <- list(deaths=deaths)
args$sampling_args <- list(iter=1000, seed=12345, control=list(adapt_delta=0.95,max_treedepth=15))
# for rt just using a weekly random walk for each country with a separate R0
args$rt <- epirt(
  formula = R(country, date) ~ 0 + country + rw(time = week, gr=country, prior_scale = 0.1),
  prior = rstanarm::normal(log(3.5), 0.1)
)

args$prior_tau = rstanarm::exponential(rate = 1)
# make sure for peridod before week of 13th March has same weekly index (same Rt)
args$data$week <- format(args$data$date+3, "%V")
# start random walk on a given date
args$data <- mutate(
  args$data,
  week = replace(week, which(week <= 11), NA)
)
args$pop_adjust <- F


fit <- do.call(epim, args)
save(fit, file = "fit_mask_rename.Rdata")

# original fit
fit$pop_adjust <- TRUE
fit_orig <- fit

# date from which we will change Rt
changeDate <- as.Date('2020-04-03') # 3-13
# get index for random walks
nms <- colnames(as.matrix(fit_orig))
idx_mask <- grep("^R\\|rw.*Mask", nms)
idx_nomask2 <- grep("^R\\|rw.*No_Mask_2", nms)
idx_nomask <- grep("^R\\|rw.*No_Mask", nms)

idx_mask = idx_mask[!idx_mask %in% idx_nomask] #唉
idx_nomask = idx_nomask[!idx_nomask %in% idx_nomask2]

#get index for R0
idx_mask_R0 <- grep("^R\\|countryMask", nms)
idx_nomask2_R0 <- grep("^R\\|countryNo_Mask_2", nms)
idx_nomask_R0 <- grep("^R\\|countryNo_Mask", nms)
idx_nomask_R0 = idx_nomask_R0[1]
#get index for seeds
idx_mask_seeds <- grep("seeds\\[Mask\\]", nms)
idx_nomask2_seeds <- grep("seeds\\[No_Mask_2\\]", nms)
idx_nomask_seeds <- grep("seeds\\[No_Mask\\]", nms)
nchains <- length(fit_orig$stanfit@sim$samples)

mat <- as.matrix(fit)

# compute orderings for the draws
order_mask <- order(mat[, idx_mask_R0])
order_nomask2 <- order(mat[, idx_mask_R0])
order_nomask <- order(mat[, idx_nomask_R0])

nomaskSeeds = mat[,idx_nomask_seeds]
nomask2Seeds = mat[,idx_nomask2_seeds]
maskSeeds = mat[,idx_mask_seeds]

nomaskR=exp(mat[,idx_nomask_R0])
maskR=exp(mat[,idx_mask_R0])
nomask2R=exp(mat[,idx_nomask2_R0])


# generate counterfactuals
e_orig <- posterior_linpred(fit)

## absolute
e1 <- e_orig
w <- e_orig$time >= changeDate

e1$draws[order_nomask, w & (e1$group == "No_Mask")] <- e_orig$draws[order_mask, w & (e_orig$group == "Mask")]
e1$draws[order_mask, w & (e1$group == "Mask")] <- e_orig$draws[order_nomask2, w & (e_orig$group == "No_Mask_2")]
e1$draws[order_nomask2, w & (e1$group == "No_Mask_2")] <- e_orig$draws[order_mask, w & (e_orig$group == "Mask")]

## absolute
e2 <- e_orig
w <- e_orig$time >= changeDate

e2$draws[order_nomask, w & (e2$group == "No_Mask")] <- e_orig$draws[order_nomask2, w & (e_orig$group == "No_Mask_2")]
e2$draws[order_mask, w & (e2$group == "Mask")] <- e_orig$draws[order_nomask, w & (e_orig$group == "No_Mask")]
e2$draws[order_nomask2, w & (e2$group == "No_Mask_2")] <- e_orig$draws[order_nomask, w & (e_orig$group == "No_Mask")]

## relative
fit3=fit_orig
warmup <- args$sampling_args$iter/2
for (chain in 1:nchains) {
  order_mask <- warmup + order(fit_orig$stanfit@sim$samples[[chain]][[idx_mask_R0]][-(1:warmup)])
  order_nomask2 <- warmup + order(fit_orig$stanfit@sim$samples[[chain]][[idx_nomask2_R0]][-(1:warmup)])
  order_nomask <- warmup + order(fit_orig$stanfit@sim$samples[[chain]][[idx_nomask_R0]][-(1:warmup)])
  for (i in 1:length(idx_mask)) {
    fit3$stanfit@sim$samples[[chain]][[idx_mask[i]]][order_mask] <- fit_orig$stanfit@sim$samples[[chain]][[idx_nomask2[i]]][order_nomask2]
    fit3$stanfit@sim$samples[[chain]][[idx_nomask2[i]]][order_nomask2] <- fit_orig$stanfit@sim$samples[[chain]][[idx_mask[i]]][order_mask]
    fit3$stanfit@sim$samples[[chain]][[idx_nomask[i]]][order_nomask] <- fit_orig$stanfit@sim$samples[[chain]][[idx_mask[i]]][order_mask]
  }
}

fit_corr_plot <- fit_orig
for (chain in 1:nchains) {
  for (i in 1:length(idx_mask)) {
    fit_corr_plot$stanfit@sim$samples[[chain]][[idx_mask[i]]] <- fit_orig$stanfit@sim$samples[[chain]][[idx_nomask2[i]]] 
    fit_corr_plot$stanfit@sim$samples[[chain]][[idx_nomask2[i]]] <- fit_orig$stanfit@sim$samples[[chain]][[idx_mask[i]]]
    fit_corr_plot$stanfit@sim$samples[[chain]][[idx_nomask[i]]] <- fit_orig$stanfit@sim$samples[[chain]][[idx_mask[i]]] 
  }
}



#relative
fit4=fit_orig
warmup <- args$sampling_args$iter/2
for (chain in 1:nchains) {
  order_mask <- warmup + order(fit_orig$stanfit@sim$samples[[chain]][[idx_mask_R0]][-(1:warmup)])
  order_nomask2 <- warmup + order(fit_orig$stanfit@sim$samples[[chain]][[idx_nomask2_R0]][-(1:warmup)])
  order_nomask <- warmup + order(fit_orig$stanfit@sim$samples[[chain]][[idx_nomask_R0]][-(1:warmup)])
  for (i in 1:length(idx_mask)) {
    fit4$stanfit@sim$samples[[chain]][[idx_mask[i]]][order_mask] <- fit_orig$stanfit@sim$samples[[chain]][[idx_nomask[i]]][order_nomask]
    fit4$stanfit@sim$samples[[chain]][[idx_nomask2[i]]][order_nomask2] <- fit_orig$stanfit@sim$samples[[chain]][[idx_nomask[i]]][order_nomask]
    fit4$stanfit@sim$samples[[chain]][[idx_nomask[i]]][order_nomask] <- fit_orig$stanfit@sim$samples[[chain]][[idx_nomask2[i]]][order_nomask2]
  }
}




## plotting rt
# get posterior infections
seed = 1234
rt <- posterior_rt(fit_orig, seed=seed)
rt_cf1 <- posterior_rt_(fit_orig, eta=e1$draws, seed=seed)
rt_cf2 <- posterior_rt_(fit_orig, eta=e2$draws, seed=seed)
rt_cf3 <- posterior_rt(fit3, seed=seed)
rt_cf4 <- posterior_rt(fit4, seed=seed)
deaths_obs <-args$data$deaths
deaths_fit <- posterior_predict(fit_orig, type="deaths",posterior_mean=TRUE, seed=seed)
deaths_cf1 <- posterior_predict_(fit_orig, eta=e1$draws, type="deaths",posterior_mean=TRUE, seed=seed)
deaths_cf2 <- posterior_predict_(fit_orig, eta=e2$draws, type="deaths",posterior_mean=TRUE, seed=seed)
deaths_cf3 <- posterior_predict(fit3, type="deaths",posterior_mean=TRUE, seed=seed)
deaths_cf4 <- posterior_predict(fit4, type="deaths",posterior_mean=TRUE, seed=seed)
infections <- posterior_infections(fit_orig,poster_mean=TRUE, seed=seed)
infections_cf1 <- posterior_infections_(fit_orig, eta=e1$draws, posterior_mean=TRUE, seed=seed)
infections_cf2 <- posterior_infections_(fit_orig, eta=e2$draws, posterior_mean=TRUE, seed=seed)
infections_cf3 <- posterior_infections(fit3, posterior_mean=TRUE, seed=seed)
infections_cf4 <- posterior_infections(fit4,posterior_mean=TRUE, seed=seed)

rt$group <- as.character(rt$group)
rt_cf1$group <- as.character(rt_cf1$group)
rt_cf2$group <- as.character(rt_cf2$group)
rt_cf3$group <- as.character(rt_cf3$group)
rt_cf4$group <- as.character(rt_cf4$group)

df <- data.frame(
  date = rt$time, 
  median = apply(rt$draws, 2, function(x) quantile(x, 0.5)),
  median_li = apply(rt$draws, 2, function(x) quantile(x, 0.025)),
  median_ui = apply(rt$draws, 2, function(x) quantile(x, 0.975)),
  deaths_median = apply(deaths_fit$draws, 2, function(x) quantile(x, 0.5)),
  deaths_li = apply(deaths_fit$draws, 2, function(x) quantile(x, 0.025)),
  deaths_ui = apply(deaths_fit$draws, 2, function(x) quantile(x, 0.975)),
  deaths=deaths_obs,
  infections_median = apply(infections$draws, 2, function(x) quantile(x, 0.5)),
  infections_li= apply(infections$draws, 2, function(x) quantile(x, 0.025)),
  infections_ui = apply(infections$draws, 2, function(x) quantile(x, 0.975)),
  group = rt$group
)
df_cf1 <- data.frame(
  date = rt_cf1$time, 
  median = apply(rt_cf1$draws, 2, function(x) quantile(x, 0.5)),
  median_li = apply(rt_cf1$draws, 2, function(x) quantile(x, 0.025)),
  median_ui = apply(rt_cf1$draws, 2, function(x) quantile(x, 0.975)),
  deaths_median = apply(deaths_cf1$draws, 2, function(x) quantile(x, 0.5)),
  deaths_li = apply(deaths_cf1$draws, 2, function(x) quantile(x, 0.025)),
  deaths_ui = apply(deaths_cf1$draws, 2, function(x) quantile(x, 0.975)),
  deaths=deaths_obs,
  infections_median = apply(infections_cf1$draws, 2, function(x) quantile(x, 0.5)),
  infections_li= apply(infections_cf1$draws, 2, function(x) quantile(x, 0.025)),
  infections_ui = apply(infections_cf1$draws, 2, function(x) quantile(x, 0.975)),  
  group = rt_cf1$group
)
df_cf2 <- data.frame(
  date = rt_cf2$time, 
  median = apply(rt_cf2$draws, 2, function(x) quantile(x, 0.5)),
  median_li = apply(rt_cf2$draws, 2, function(x) quantile(x, 0.025)),
  median_ui = apply(rt_cf2$draws, 2, function(x) quantile(x, 0.975)),
  deaths_median = apply(deaths_cf2$draws, 2, function(x) quantile(x, 0.5)),
  deaths_li = apply(deaths_cf2$draws, 2, function(x) quantile(x, 0.025)),
  deaths_ui = apply(deaths_cf2$draws, 2, function(x) quantile(x, 0.975)),  
  deaths=deaths_obs,  
  infections_median = apply(infections_cf2$draws, 2, function(x) quantile(x, 0.5)),
  infections_li= apply(infections_cf2$draws, 2, function(x) quantile(x, 0.025)),
  infections_ui = apply(infections_cf2$draws, 2, function(x) quantile(x, 0.975)),  
  group = rt_cf2$group
)
df_cf3 <- data.frame(
  date = rt_cf3$time, 
  median = apply(rt_cf3$draws, 2, function(x) quantile(x, 0.5)),
  median_li = apply(rt_cf3$draws, 2, function(x) quantile(x, 0.025)),
  median_ui = apply(rt_cf3$draws, 2, function(x) quantile(x, 0.975)),  
  deaths_median = apply(deaths_cf3$draws, 2, function(x) quantile(x, 0.5)),
  deaths_li = apply(deaths_cf3$draws, 2, function(x) quantile(x, 0.025)),
  deaths_ui = apply(deaths_cf3$draws, 2, function(x) quantile(x, 0.975)),  
  deaths=deaths_obs,
  infections_median = apply(infections_cf3$draws, 2, function(x) quantile(x, 0.5)),
  infections_li= apply(infections_cf3$draws, 2, function(x) quantile(x, 0.025)),
  infections_ui = apply(infections_cf3$draws, 2, function(x) quantile(x, 0.975)),    
  group = rt_cf3$group
)
df_cf4 <- data.frame(
  date = rt_cf4$time, 
  median = apply(rt_cf4$draws, 2, function(x) quantile(x, 0.5)),
  median_li = apply(rt_cf4$draws, 2, function(x) quantile(x, 0.025)),
  median_ui = apply(rt_cf4$draws, 2, function(x) quantile(x, 0.975)),    
  deaths_median = apply(deaths_cf4$draws, 2, function(x) quantile(x, 0.5)),
  deaths_li = apply(deaths_cf4$draws, 2, function(x) quantile(x, 0.025)),
  deaths_ui = apply(deaths_cf4$draws, 2, function(x) quantile(x, 0.975)),  
  deaths=deaths_obs,
  infections_median = apply(infections_cf4$draws, 2, function(x) quantile(x, 0.5)),
  infections_li= apply(infections_cf4$draws, 2, function(x) quantile(x, 0.025)),
  infections_ui = apply(infections_cf4$draws, 2, function(x) quantile(x, 0.975)),    
  group = rt_cf4$group
)



df$colour <- "Original"
df_cf1$colour <- "Scenario 1"
df_cf2$colour <- "Scenario 2"
df_cf3$colour <- "Scenario 3"
df_cf4$colour <- "Scenario 4"
df <- rbind(df, df_cf1, df_cf2, df_cf3, df_cf4)


source("transform_plot.r")

######################################################################################################################################################
# Rt
######################################################################################################################################################
date_breaks <- "1 month"
base <- ggplot2::ggplot() +
  ggplot2::xlab(" ") +
  ggplot2::scale_x_date(
    date_breaks = date_breaks,
    labels = scales::date_format("%b 2020")
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(
      #angle = 45,
      hjust = 1
    ),
    axis.text = ggplot2::element_text(size = 10),
    axis.title = ggplot2::element_text(size = 10)
  ) +
  ggplot2::theme(legend.position = "right")
p1 <- base + 
  ggplot2::geom_line(linetype = "dashed",
    mapping = ggplot2::aes(x = date, y = median, color = colour), 
    data = df[df$group%in%c("Abs: Mask -> No_Mask"),], 
    size = .5,color="seagreen",
  ) + 
  ggplot2::geom_line(
    mapping = ggplot2::aes(x = date, y = median), 
    #data = df_originals[df$group%in%c("Abs: Mask -> No_Mask"),], 
    data = df[df$group%in%c("Fit No_Mask"),], 
    size = .5,color='red',
  )+ geom_hline(yintercept = 1)
  #xlim(as.Date('2020-02-01'), as.Date('2020-07-01'))+ geom_hline(yintercept = 1)
p1 <- p1 + ggplot2::labs(y = "Median Rt") + ggtitle("Abs: Mask -> No_Mask")



p2 <- base + 
  ggplot2::geom_line(linetype = "dashed",
    mapping = ggplot2::aes(x = date, y = median, color = colour), 
    data = df[df$group%in%c("Abs: No_Mask -> Mask"),], 
    size = .5,color="red",
  ) + 
  ggplot2::geom_line(
    mapping = ggplot2::aes(x = date, y = median), 
    data = df_originals[df$group%in%c("Abs: No_Mask -> Mask"),], 
    size = .5,color='seagreen',
  )+ geom_hline(yintercept = 1)
  #xlim(as.Date('2020-02-01'), as.Date('2020-07-01'))+ geom_hline(yintercept = 1)
p2 <- p2 + ggplot2::labs(y = "Median Rt") + ggtitle("Abs: No_Mask -> Mask")


######################################################################################################################################################
# Deaths
######################################################################################################################################################
# colours thanks for Michael Betancourts aesthetics
ci <- c("#C79999")
mn <- c("#7C0000")


date_breaks <- "1 month"
base <- ggplot2::ggplot() +
  ggplot2::xlab(" ") +
  ggplot2::scale_x_date(
    date_breaks = date_breaks,
    labels = scales::date_format("%b 2020")
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(
     # angle = 45,
      hjust = 1
    ),
    axis.text = ggplot2::element_text(size = 10),
    axis.title = ggplot2::element_text(size = 10)
  )# +
  #ggplot2::theme(legend.position = "right") 

p1 <- base + 
  ggplot2::geom_line(linetype = "dashed",
    mapping = ggplot2::aes(x = date, y = deaths_median, color = colour), 
    data = df[df$group%in%c("Abs: Mask -> No_Mask"),], 
    size = 1,color='seagreen',
  )+ 
  geom_ribbon(
    mapping = ggplot2::aes(x = date, ymin = deaths_li,ymax=deaths_ui), 
    data = df[df$group%in%c("Abs: Mask -> No_Mask"),], 
    size = 1,fill="seagreen",alpha=0.4,
  )+ 
  geom_line(
    mapping = ggplot2::aes(x = date, y = deaths_median, color = colour), 
    data = df[df$group%in%c('Fit No_Mask'),], 
    size = 1,color='red',
  )
# +
#  ggplot2::geom_bar(
#    mapping = ggplot2::aes(x = date, y=deaths), stat="identity",
#    data = df[df$country%in%c("No_Mask"),], 
#    width = 0.5,fill='steelblue',alpha=0.3,
#  ) +
#  ggplot2::geom_point(
#    mapping = ggplot2::aes(x = date, y = max),
#    data = df_aux[df_aux$country%in%c("No_Mask"),], 
#    colour = "white") +
#  xlim(as.Date('2020-02-01'), as.Date('2020-07-01'))
p1 <- p1 + ggplot2::labs(y = "Deaths") + ggtitle("Mask -> No_Mask")




p2 <- base + 
  ggplot2::geom_line(linetype = "dashed",
    mapping = ggplot2::aes(x = date, y = deaths_median, color = colour), 
    data = df[df$group%in%c("Abs: No_Mask -> Mask"),], 
    size = 1,color='red',
  )+ 
  geom_ribbon(
    mapping = ggplot2::aes(x = date, ymin = deaths_li,ymax=deaths_ui), 
    data = df[df$group%in%c("Abs: No_Mask -> Mask"),], 
    size = 1,fill="red",alpha=0.4,
  )+
  geom_line(
    mapping = ggplot2::aes(x = date, y = deaths_median, color = colour), 
    data = df[df$group%in%c('Fit Mask'),], 
    size = 1,color='seagreen',
  )
p2 <- p2 + ggplot2::labs(y = "Deaths") + ggtitle("No_Mask -> Mask")




######################################################################################################################################################
# Infections
######################################################################################################################################################
# colours thanks for Michael Betancourts aesthetics
ci <- c("#C79999")
mn <- c("#7C0000")


date_breaks <- "1 month"
base <- ggplot2::ggplot() +
  ggplot2::xlab("") +
  ggplot2::scale_x_date(
    date_breaks = date_breaks,
    labels = scales::date_format("%b 2020")
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(
      #angle = 45,
      hjust = 1
    ),
    axis.text = ggplot2::element_text(size = 10),
    axis.title = ggplot2::element_text(size = 10)
  )# +
  #ggplot2::theme(legend.position = "right") 

p1 <- base + 
  ggplot2::geom_line(linetype = "dashed",
    mapping = ggplot2::aes(x = date, y = infections_median, color = colour), 
    data = df[df$group%in%c("Abs: Mask -> No_Mask"),], 
    size = 1,color='seagreen',
  ) + 
  ggplot2::geom_ribbon(
    mapping = ggplot2::aes(x = date, ymin = infections_li,ymax=infections_ui), 
    data = df[df$group%in%c("Abs: Mask -> No_Mask"),], 
    size = 1,fill='seagreen',alpha=0.4,
  ) +
  ggplot2::geom_line(
    mapping = ggplot2::aes(x = date, y = infections_median, color = colour), 
    data = df[df$group%in%c("Fit No_Mask"),], 
    size = 1,color='red',
  )
p1 <- p1 + ggplot2::labs(y = "Infections") + ggtitle("Mask -> No_Mask")



p2 <- base + 
  ggplot2::geom_line(linetype = "dashed",
    mapping = ggplot2::aes(x = date, y = infections_median, color = colour), 
    data = df[df$group%in%c("Abs: No_Mask -> Mask"),], 
    size = 1,color='red',
  ) + 
  ggplot2::geom_ribbon(
    mapping = ggplot2::aes(x = date, ymin = infections_li,ymax=infections_ui), 
    data = df[df$group%in%c("Abs: No_Mask -> Mask"),], 
    size = 1,fill='red',alpha=0.4,
  )+
  ggplot2::geom_line(
    mapping = ggplot2::aes(x = date, y = infections_median, color = colour), 
    data = df[df$group%in%c("Fit Mask"),], 
    size = 1,color='seagreen',
  )
p2 <- p2 + ggplot2::labs(y = "Infections") + ggtitle("No_Mask -> Mask")








date_breaks <- "1 month"
base <- ggplot2::ggplot() +
  ggplot2::xlab(" ") +
  ggplot2::scale_x_date(
    date_breaks = date_breaks,
    labels = scales::date_format("%b 2020")
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(
      #angle = 45,
      hjust = 1
    ),
    axis.text = ggplot2::element_text(size = 8),
    axis.title = ggplot2::element_text(size = 10)
  ) +
  ggplot2::theme(legend.position = "right")
p1 <- base + 
  ggplot2::geom_line(
    mapping = ggplot2::aes(x = date, y = median, color = colour), 
    data = df_originals[df$group%in%c("Fit No_Mask"),], 
    size = .5,color="red",
  ) + 
  ggplot2::geom_line(
    mapping = ggplot2::aes(x = date, y = median), 
    data = df_originals[df$group%in%c("Fit Mask"),],
    size = .5,color='seagreen',
  )+ geom_hline(yintercept = 1)
p1 <- p1 + ggplot2::labs(y = "Median Rt")

view(df[df$group%in%c("Fit Mask"),][,1:11])
view(df[df$group%in%c("Abs: No_Mask -> Mask"),][,1:11])

write.csv(df[df$group%in%c("Fit Mask"),][,1:11], 'mask.csv')
write.csv(df[df$group%in%c("Fit No_Mask"),][,1:11], 'nomask.csv')
write.csv(df[df$group%in%c("Abs: No_Mask -> Mask"),][,1:11], 'nomask-mask.csv')
write.csv(df[df$group%in%c("Abs: Mask -> No_Mask"),][,1:11], 'mask-nomask.csv')
