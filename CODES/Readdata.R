library(tidyverse)
library(dplyr)
state = data.frame(cbind(c(state.abb[1:8], "DC", state.abb[9:50]), c(state.name[1:8], "District of Columbia", state.name[9:50])))
state = arrange(state, X2) # order in 51 state name
stateabb = state$X1
statename = state$X2

mask = read.csv('data/mask.csv')
mask = arrange(mask, state)[,4]
mask = as.Date(mask)

d = read.csv('data/jhu3.csv')[,c(2,5,6,7)]
d = as_tibble(d)
colnames(d) = c("DateRep", "Cases", "Deaths", "Country")
d$DateRep = as.Date(d$DateRep, "%m/%d/%y")
pop = unique(read.csv('data/death_data_upto613.csv')[,c(8,10)])
# pop = arrange(pop, state_name)
pop = arrange(pop, state_name)[,2]
X = 1:51
x = arrange(read.csv('data/ifr_by_state.csv')[,c(2,3)], state)
ifr.by.country = cbind(X,x,pop)
colnames(ifr.by.country) = c("X", "country", "ifr","popt")
countries = data.frame(unique(ifr.by.country$country))
colnames(countries) = "Regions"
interventions = read.csv("data/int.csv")
interventions = interventions[match(stateabb, interventions$StatePostal),]
interventions$StatePostal = countries$Regions
colnames(interventions)[1] = "Country"
interventions[is.na(interventions)] = "2020/7/15"
for(i in 2:6){
  interventions[,i] = as.Date(interventions[,i], "%Y/%m/%d")
}
d$Cases = abs(d$Cases)
d$Deaths = abs(d$Deaths)
outlier = c(2,12,13,27,35,42,46,49,51)
countries = data.frame(countries$Regions[-outlier])
colnames(countries) = "Regions"
ifr.by.country = ifr.by.country[-outlier,]
interventions = interventions[-outlier,]
mask = mask[-outlier]

na = is.na(mask)
countries = data.frame(countries$Regions[!na])
colnames(countries) = "Regions"
ifr.by.country = ifr.by.country[!na,]
interventions = interventions[!na,]
mask = mask[!na]
interventions$GathRestrictAny = mask

d1 = arrange(d, Country)
na.d = rep(na, each = 136)
d = d1[!na.d,]

# Significant: Schoolclose; Stayathome
str(d)
str(countries)
str(ifr.by.country)
str(interventions)
save(d,countries,ifr.by.country,interventions,file = "data/mask.Rdata") #2/1 - 6/15 29
