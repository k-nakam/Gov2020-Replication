###################################
# Code for Potential Extension
###################################
rm(list = ls())
library(tidyverse)
library(plm)
library(stargazer)
library(sensemakr)

path <- "/Users/nakamurakentarou/Library/CloudStorage/Dropbox/Harvard/Classes/4. 2024Fall/GOV2020/Others_Replication/Trauma Replication Files"
setwd(path) # set working directory

load('cps.Rda') # individual-level CPS data
load('cps_lag.Rda') # individual-level CPS data w/ one lag
load('cps_2lag.Rda') # individual-level CPS data w/ two lags
load('discop.Rda') # county-level data
load('stanrob.Rda') # Stanford Mass Shootings data for robustness check

# Replication of Main Result -> I could not replicate the main findings (where is it?)
# Given the number of observation, I think discop is the data I need to use
# But the coefficient is different. Do I miss anything?
# All the replication code uses "turnout" but your table is about DV: Incumbent Presidential Party Vote Share.
# Where is the variable for this? I try to create it
discop <- discop %>%
  mutate(incum_share = ifelse(year %in% c(1980, 1992, 1996, 2000, 2012, 2016), dem_share, 1 - dem_share))

head(discop)
p1 <- plm(incum_share ~ tr_ar + perc_bl + pop_tot + income, 
     data = discop,
     index = c('fips', 'year'),
     model = 'within',
     effect = 'twoways')
p2 <- plm(incum_share ~ tr_sh + perc_bl + pop_tot + income + fatalities + injured, 
          data = discop,
          index = c('fips', 'year'),
          model = 'within',
          effect = 'twoways')
p3 <- plm(incum_share ~ tr_dis + perc_bl + pop_tot + income, 
          data = discop,
          index = c('fips', 'year'),
          model = 'within',
          effect = 'twoways')
stargazer(list(p1,p2,p3), type = "text")
#I got something different

# Sensitivity Analyses of Your Finding:
m1 <- lm(incum_share ~ tr_ar + perc_bl + pop_tot + income + as.factor(state) + as.factor(year), data = discop)
s1 <- sensemakr(m1, treatment = "tr_ar", benchmark_covariates = c("perc_bl"), kd = 1:3, ky = 1:3)
plot(s1)

m2 <- lm(incum_share ~ tr_sh + perc_bl + pop_tot + income + as.factor(state) + as.factor(year), data = discop)
s2 <- sensemakr(m2, treatment = "tr_sh", benchmark_covariates = c("perc_bl"), kd = 1:3, ky = 1:3)
plot(s2)

# I try everything with different DV: turnout (in the original code) and dem_share
## turnout
p1 <- plm(turnout ~ tr_ar + perc_bl + pop_tot + income, 
          data = discop,
          index = c('fips', 'year'),
          model = 'within',
          effect = 'twoways')
p2 <- plm(turnout ~ tr_sh + perc_bl + pop_tot + income + fatalities + injured, 
          data = discop,
          index = c('fips', 'year'),
          model = 'within',
          effect = 'twoways')
p3 <- plm(turnout ~ tr_dis + perc_bl + pop_tot + income, 
          data = discop,
          index = c('fips', 'year'),
          model = 'within',
          effect = 'twoways')
stargazer(list(p1,p2,p3), type = "text")
#I got something different

# Sensitivity Analyses of Your Finding:
m1 <- lm(turnout ~ tr_ar + perc_bl + pop_tot + income + as.factor(state) + as.factor(year), data = discop)
s1 <- sensemakr(m1, treatment = "tr_ar", benchmark_covariates = c("perc_bl"), kd = 1:3, ky = 1:3)
plot(s1)

m2 <- lm(turnout ~ tr_sh + perc_bl + pop_tot + income + as.factor(state) + as.factor(year), data = discop)
s2 <- sensemakr(m2, treatment = "tr_sh", benchmark_covariates = c("perc_bl"), kd = 1:3, ky = 1:3)
plot(s2)

## dem_share
p1 <- plm(dem_share ~ tr_ar + perc_bl + pop_tot + income, 
          data = discop,
          index = c('fips', 'year'),
          model = 'within',
          effect = 'twoways')
p2 <- plm(dem_share ~ tr_sh + perc_bl + pop_tot + income + fatalities + injured, 
          data = discop,
          index = c('fips', 'year'),
          model = 'within',
          effect = 'twoways')
p3 <- plm(dem_share ~ tr_dis + perc_bl + pop_tot + income, 
          data = discop,
          index = c('fips', 'year'),
          model = 'within',
          effect = 'twoways')
stargazer(list(p1,p2,p3), type = "text")
#I got something different

# Sensitivity Analyses of Your Finding:
m1 <- lm(dem_share ~ tr_ar + perc_bl + pop_tot + income + as.factor(state) + as.factor(year), data = discop)
s1 <- sensemakr(m1, treatment = "tr_ar", benchmark_covariates = c("perc_bl"), kd = 1:3, ky = 1:3)
plot(s1)

m2 <- lm(dem_share ~ tr_sh + perc_bl + pop_tot + income + as.factor(state) + as.factor(year), data = discop)
s2 <- sensemakr(m2, treatment = "tr_sh", benchmark_covariates = c("perc_bl"), kd = 1:3, ky = 1:3)
plot(s2)


