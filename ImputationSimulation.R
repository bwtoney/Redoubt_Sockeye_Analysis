# Drafted by Blake Toney on May7th, 2026
# Intended use: sensitivity analysis of imputation approach to missing data
# WORKING DRAFT: Last updated 5/19/26 
# Current status: Simulations based on modeling of run timing using glm with a 4th order polynomial of Day of Year plus a Year effect
# What is it good for: mostly just exploring how the imputation process works
# What did I find?: DOesn't seem like any more than 10 iterations is impactful to final imputation estimate


# library
# install.packages("devtools")
# install.packages("extrafont")
library(extrafont)
# devtools::install_github("justinpriest/JTPfunc")
library(JTPfunc)
library(dplyr)
library(MASS)
library(ggplot2)
library(readr)
library(tidyverse)
# ?impute_global()

# Step one: Simulate dataset to test out the imputation function...==============

simdat <- expand.grid(
  DOY = 164:281,
  Year = 1:5)

# Simulate count data across years
# Bring in model coefficients from nb.glm created in another script to describe the "shape" of run
# Model formula = glm(Count ~ Year + poly(doy, 4)))
set.seed(111)

Year.effect <- rnorm(5, 0, 0.25)
names(Year.effect) <- 1:5

b1 <- -114.17681    
b2 <- -174.92503    
b3 <-  - 0.03102    
b4 <- - 23.73688    
theta <- 0.9214
sigma_err <- 0.5   

sim.mod <- poly(simdat$DOY, 4)

simdat <- simdat %>%
  mutate(
    eta = b1 * sim.mod[,1] +
      b2 * sim.mod[,2] +
      b3 * sim.mod[,3]+
      b4 * sim.mod[,4]+
      Year.effect[as.character(Year)],
    eta_err = eta + rnorm(n(), mean = 0, sd = sigma_err),
    Count = exp(eta_err))

# Visualize simulated count data:
ggplot(simdat, aes(x=DOY, y=Count, color = as.factor(Year)))+
  geom_line()

range(simdat$Count)
#looks pretty mediocre, doesn't start to show fish until July... Anyways, lets try and simulate some missing data


# identify some random days as "unobserved" or NA's within your dataframe
remove_days <- sample(seq_len(nrow(simdat)), size = 7, replace = FALSE)
simdat2 <- simdat
simdat2$Count[remove_days] <- NA

#And store removed values for later use
Original_est <- simdat$Count[remove_days]


# Step 1: Set up dataframe to impute
simdat2 <- simdat2 %>% mutate(imputed = is.na(Count))

# Step 2: Create list to store imputed values
sim <- list()

# Step 3: Use multiplicative imputation as per Blick, in an iterative procedure (adapted from Priest)

# j=1
# i=125
for(j in 1:100){
  
  for(i in 1:nrow(simdat2)){
    if(simdat2$imputed[i]){
      
      sumyr  <- sum(simdat2$Count[simdat2$Year == simdat2$Year[i]], na.rm = TRUE)
      sumdoy <- sum(simdat2$Count[simdat2$DOY  == simdat2$DOY[i]],  na.rm = TRUE)
      sumall <- sum(simdat2$Count, na.rm = TRUE)
      
      simdat2$Count[i] <- sumyr * sumdoy / sumall
    }
  }
  sim[[j]] <- simdat2$Count[remove_days] # save each round of the for loop value
}


#Alright, lets set up a plot for graphing convergence
sim_df <- data.frame(do.call(rbind, sim))

#label "iteration" for x-axis
sim_df$iteration <- seq_len(nrow(sim_df))

#Pivot longer
sim_df <- sim_df %>%
  pivot_longer(cols = -iteration, names_to = "value_index", values_to = "imputed_value") %>%
  mutate(value_index = readr::parse_number(value_index),
         Original_est = simdat$Count[ remove_days[value_index]])

# And plot
ggplot(sim_df, aes(x = iteration, y = imputed_value, color = factor(value_index))) +
  geom_point(alpha = 0.7, size = 2) +
  geom_hline(aes(yintercept = Original_est, color = factor(value_index)), linetype = "dashed", linewidth = 0.8) +
  labs(x = "Iteration", y = "Imputed Count", color = "Missing Index", title = "Convergence of Imputed Values Across Iterations") +
  theme_minimal(base_size = 14)

### Looks like my estimation is only as good as the surrounding values (Can be pretty bad)
rmse1 <- sqrt(mean((sim_df$imputed_value - sim_df$Original_est)^2))


# Look at comparing the more chaotic years (regime change) and static years

# Stable================ intercept and coefficients pulled from nb.glm(Count = Year+poly(DOY, 4) for the years 2009-2013)
simdat3 <- expand.grid(
  DOY = 164:281,
  Year = 2009:2013)

Year.effect <- c(
  "2009" = 0.2148,
  "2010" = 0.6071,
  "2011" = 0.7079,
  "2012" = 1.2962,
  "2013" = 2.2635)

b1 <- -22.0200
b2 <- -43.2220
b3 <- 12.4886
b4 <- 0.7973
theta <- 0.7881
sigma_err <- 0.5

sim.mod <- poly(simdat3$DOY, 4)
intercept <- 3.2607

simdat_stable <- simdat3 %>%
  mutate(
    eta = intercept +
      b1 * sim.mod[,1] +
      b2 * sim.mod[,2] +
      b3 * sim.mod[,3] +
      b4 * sim.mod[,4] +
      Year.effect[as.character(Year)],
    eta_err = eta + rnorm(n(), mean = 0, sd = sigma_err),
    Count = exp(eta_err)) %>%
  dplyr::select(Year, DOY, Count)


# Chaotic years ===================intercept and coefficients pulled from nb.glm(Count = Year+poly(DOY, 4) for the years 2020-2024)
simdat4 <- expand.grid(
  DOY = 164:281,
  Year = 2020:2024)

Year.effect <- c(
  "2020" = -0.62769,
  "2021" = -0.02348,
  "2022" = 0.08673,
  "2023" = 0.99317,
  "2024" = 1.18662)

b1 <- -44.73905
b2 <- -70.70412
b3 <- 0.49029
b4 <- -12.93828
theta <- 0.8173
sigma_err <- 0.5

sim.mod <- poly(simdat4$DOY, 4)

intercept <- 4.76124
simdat_chaotic <- simdat4 %>%
  mutate(
    eta = intercept +
      b1 * sim.mod[,1] +
      b2 * sim.mod[,2] +
      b3 * sim.mod[,3] +
      b4 * sim.mod[,4] +
      Year.effect[as.character(Year)],
    eta_err = eta + rnorm(n(), mean = 0, sd = sigma_err),
    Count = exp(eta_err))%>%
  dplyr::select(Year, DOY, Count)

ggplot(simdat_stable, aes(x=DOY, y=Count, color = as.factor(Year)))+geom_line()+labs(title = "Stable")
ggplot(simdat_chaotic, aes(x=DOY, y=Count, color = as.factor(Year)))+geom_line()+labs(title = "Chaotic")
# Need to figure out why some days are so insanely extreme!
glimpse(simdat)
glimpse(simdat_stable)
glimpse(simdat_chaotic)


# Imputation Approach to chaoctic vs stable years =============================
set.seed(222)
remove_days2 <- sample(seq_len(nrow(simdat_stable)), size = 7, replace = FALSE)
remove_days3 <- sample(seq_len(nrow(simdat_chaotic)), size = 7, replace = FALSE)

simdat3b <- simdat_stable
simdat4b <- simdat_chaotic

simdat3b$Count[remove_days2] <- NA
simdat4b$Count[remove_days3] <- NA

#And store removed values for later use
Original_est_stable <- simdat_stable$Count[remove_days2]
Original_est_chaotic <- simdat_chaotic$Count[remove_days3]


# Step 1: Set up dataframe to impute
simdat3b <- simdat3b %>% mutate(imputed = is.na(Count))
simdat4b <- simdat4b %>% mutate(imputed = is.na(Count))

# Step 2: Create list to store imputed values
sim2 <- list()
sim3 <- list()
# Step 3: Use multiplicative imputation as per Blick, in an iterative procedure (adapted from Priest)

# j=1
# i=125
for(j in 1:40){
  for(i in 1:nrow(simdat3b)){
    if(simdat3b$imputed[i]){
      sumyr  <- sum(simdat3b$Count[simdat3b$Year == simdat3b$Year[i]], na.rm = TRUE)
      sumdoy <- sum(simdat3b$Count[simdat3b$DOY  == simdat3b$DOY[i]],  na.rm = TRUE)
      sumall <- sum(simdat3b$Count, na.rm = TRUE)
      simdat3b$Count[i] <- sumyr * sumdoy / sumall
    }}
  sim2[[j]] <- simdat3b$Count[remove_days2] # save each round of the for loop value
}
for(j in 1:40){
  for(i in 1:nrow(simdat4b)){
    if(simdat4b$imputed[i]){
      sumyr  <- sum(simdat4b$Count[simdat4b$Year == simdat4b$Year[i]], na.rm = TRUE)
      sumdoy <- sum(simdat4b$Count[simdat4b$DOY  == simdat4b$DOY[i]],  na.rm = TRUE)
      sumall <- sum(simdat4b$Count, na.rm = TRUE)
      simdat4b$Count[i] <- sumyr * sumdoy / sumall
    }}
  sim3[[j]] <- simdat4b$Count[remove_days3] # save each round of the for loop value
}

#Alright, lets set up a plot for graphing convergence
sim_df2 <- data.frame(do.call(rbind, sim2))
sim_df3 <- data.frame(do.call(rbind, sim3))

#label "iteration" for x-axis
sim_df2$iteration <- seq_len(nrow(sim_df2))
sim_df3$iteration <- seq_len(nrow(sim_df3))

#Pivot longer
sim_df2 <- sim_df2 %>%
  pivot_longer(cols = -iteration, names_to = "value_index", values_to = "imputed_value") %>%
  mutate(value_index = readr::parse_number(value_index),
         Original_est = simdat3b$Count[ remove_days[value_index]])
sim_df3 <- sim_df3 %>%
  pivot_longer(cols = -iteration, names_to = "value_index", values_to = "imputed_value") %>%
  mutate(value_index = readr::parse_number(value_index),
         Original_est = simdat4b$Count[ remove_days[value_index]])

# And plot
ggplot(sim_df2, aes(x = iteration, y = imputed_value, color = factor(value_index))) +
  geom_point(alpha = 0.7, size = 2) +
  geom_hline(aes(yintercept = Original_est, color = factor(value_index)), linetype = "dashed", linewidth = 0.8) +
  labs(x = "Iteration", y = "Imputed Count", color = "Missing Index", title = "Stable Years - Convergence of Imputed Values Across Iterations") +
  theme_bw(base_size = 14)

ggplot(sim_df3, aes(x = iteration, y = imputed_value, color = factor(value_index))) +
  geom_point(alpha = 0.7, size = 2) +
  geom_hline(aes(yintercept = Original_est, color = factor(value_index)), linetype = "dashed", linewidth = 0.8) +
  labs(x = "Iteration", y = "Imputed Count", color = "Missing Index", title = "Chaotic Years - Convergence of Imputed Values Across Iterations") +
  theme_bw(base_size = 14)

### Looks like my estimation is only as good as the surrounding values (Can be pretty bad)
rmse2 <- sqrt(mean((sim_df2$imputed_value - sim_df2$Original_est)^2))
rmse3 <- sqrt(mean((sim_df3$imputed_value - sim_df3$Original_est)^2))

rmse1
rmse2
rmse3
# Seems like it's worth improving this count simulation...
# Check RMSE values to compare simulations, unsurprisingly, chaotic years yielded the highes error in imputing values.
# Anyways, next steps are to test out how the imputation performs when you increase the number of missing values... or multiple values in a row
# Alternatively, going to try and leverage a Bayesian model that uses 10-year cumsum proportion as a run shape prior and this years daily counts to update


# Try and source the other code
source("RunModel.R")
