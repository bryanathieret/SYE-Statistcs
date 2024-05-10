

library(tidyverse)
library(readxl)
library(rjags)
library(runjags)
library(R2jags)
library(lattice)
library(mcmcplots)
library(strex)
library(loo)

# Data sets: 
###########################
C2015 = tibble::tibble(
  "Week" = c(1, 2, 3, 4, 5, 6, 1, 2, 3, 4, 5, 6, 1, 2, 3, 4, 5, 6),
  "Group" = c("AFR", "AFR", "AFR", "AFR", "AFR", "AFR", "MS15", "MS15", "MS15", "MS15", "MS15", "MS15", "MS360", "MS360", "MS360", "MS360", "MS360", "MS360"),
  "Ethanol_Percentage" = c(5, 5, 20, 20, 20, 20, 5, 5, 20, 20, 20, 20, 5, 5, 20, 20, 20, 20),
  "Access_Hours" = c(24, 2, 2, 2, 2, 2, 24, 2, 2, 2, 2, 2, 24, 2, 2, 2, 2, 2),
  "Median" = c(1.61, 0.40, 1.10, 1.21, 1.37, 1.18, 1.89, 0.67, 1.12, 1.49, 1.23, 1.32, 1.53, 0.57, 1.20, 1.11, 1.28, 1.32),
  "a" = c(0.29, 0.18, 0.50, 0.59, 0.77, 0.62, 0.87, 0.32, 0.75, 0.89, 1.05, 0.39, 0.21, 0.09, 0.29, 0.56, 0.38, 0.60),
  "b" = c(4.16, 0.75, 2.11, 1.42, 2.08, 1.66, 3.40, 0.99, 1.61, 2.13, 1.74, 1.77, 3.28, 1.14, 1.98, 2.15, 2.52, 2.05),
  "n" = c(11, 11, 11, 11, 11, 11, 10, 10, 10, 10, 10, 10, 20, 20, 20, 20, 20, 20),
  "Sessions" = c(3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3))
C2015 = C2015 |>
  mutate(
    Estimated_Mean = (a+2*Median+b)/(4),
    Estimated_SD = ((b - a)/(2 * qnorm((n-0.375)/(n+0.25)))),
    Estimated_Variance = (Estimated_SD)^2
  )


C2015 |> filter(Group != "AFR") |> mutate(article = "C2015") |> mutate(Group = factor(Group)) |> select(article, Week, Group, Ethanol_Percentage, Estimated_Mean, Estimated_Variance, n)

control = C2015 |> filter(Group != "AFR") |> slice(1:6) |> select(Week, n, Estimated_Mean, Estimated_SD) |> rename("Estimated_Mean_2" = Estimated_Mean,
                                                                                                                   "n_2" = n, "Estimated_SD_2" = Estimated_SD)
ms = C2015 |> filter(Group != "AFR") |> slice(7:12) |> select(Week, n, Estimated_Mean, Estimated_SD) |> rename("Estimated_Mean_1" = Estimated_Mean,
                                                                                                               "n_1" = n, "Estimated_SD_1" = Estimated_SD)

C15 = left_join(ms, control) |> 
  mutate(article = "C2015") |> 
  mutate(mean_diff = Estimated_Mean_1 - Estimated_Mean_2,
         sd_diff = sqrt(((n_1 - 1)*Estimated_SD_1^2 + (n_2 - 1)*Estimated_SD_2^2) / (n_1+n_2 - 2))) |> 
  select(article, Week, mean_diff, sd_diff) |>
  rename("timepoint" = Week)


GN2006 <- read_excel("G&N2006.xlsx")
GN2006 = GN2006 |> group_by(Phase, Group) |> summarise(mean = mean(Ethanol_Intake),
                                                       sd = sd(Ethanol_Intake),
                                                       variance = var(Ethanol_Intake), 
                                                       n = n()) 

control = GN2006 |> filter(Group == "MS15") |> rename("mean_2" = mean,
                                                      "sd_2" = sd, "n_2" = n) |> 
  select(-variance, -Group)

ms = GN2006 |> filter(Group == "MS360") |> rename("mean_1" = mean,
                                                  "sd_1" = sd, "n_1" = n) |> 
  select(-variance, -Group)

GN06 = left_join(ms, control, by = "Phase") |>
  mutate(article = "GN2006") |> 
  mutate(mean_diff = mean_1 - mean_2,
         sd_diff = sqrt(((n_1 - 1)*sd_1^2 + (n_2 - 1)*sd_2^2) / (n_1+n_2 - 2))) |>
  mutate(timepoint = case_when(
    Phase == "Acquisition" ~ 2,
    Phase == "Maintenance" ~ 4,
    Phase == "Stabilization" ~ 6
  )) |> select(article, mean_diff, timepoint, sd_diff)


P2003 <- read_excel("P2003.xlsx")

Sessions = tibble::tibble(Ethanol_Percentage = c(2, 4, 6, 8),
                          Sessions = c(4, 4, 7, 11),
                          n = c(8, 8, 8, 8))

P2003 = left_join(P2003, Sessions, by = "Ethanol_Percentage")

P2003 = P2003 |> mutate(SD = SEM*sqrt(n),
                        variance = SD^2)

control = P2003 |> filter(Group == "MS15") |> rename("mean_2" = Mean,
                                                     "sd_2" = SD, "n_2" = n) |> 
  select(-variance, -Group, -Upper_Point, -SEM, -Sessions)

ms = P2003 |> filter(Group == "MS360") |> rename("mean_1" = Mean,
                                                 "sd_1" = SD, "n_1" = n) |> 
  select(-variance, -Group, -Upper_Point, -SEM, -Sessions)

P03 = left_join(ms, control) |>
  mutate(article = "P2003") |> 
  mutate(mean_diff = mean_1 - mean_2,
         sd_diff = sqrt(((n_1 - 1)*sd_1^2 + (n_2 - 1)*sd_2^2) / (n_1+n_2 - 2))) |> 
  select(article, mean_diff, sd_diff) |>
  mutate(timepoint = c(1, 2, 3, 4))

O2011 <- read_excel("O2011.xlsx")
O2011 = O2011 |> pivot_wider(names_from = Summary_Statistics, values_from = Values)

# estimated mean and SD from Wan et al., 2014
O2011 = O2011 |> mutate(
  Estimated_Mean = ((Minimum + 2*IQR1 + 2*Median + 2*IQR3 + Maximum)/8),
  Estimated_SD = (((Maximum - Minimum)/(4*qnorm((n-0.375)/(n+0.25)))) + ((IQR3-IQR1)/(4*qnorm((0.75*n-0.125)/(n+0.25))))),
  Estimated_Variance = (Estimated_SD)^2
) 

control = O2011 |> filter(Group == "MS15") |> rename("mean_2" = Estimated_Mean,
                                                     "sd_2" = Estimated_SD, "n_2" = n) |> 
  select(-Estimated_Variance, -Group, -Sessions, -Maximum, -IQR3, -IQR1, -Ethanol_Percentage, -Minimum, -Median)

ms = O2011 |> filter(Group == "MS360") |> rename("mean_1" = Estimated_Mean,
                                                 "sd_1" = Estimated_SD, "n_1" = n) |> 
  select(-Estimated_Variance, -Group, -Sessions, -Maximum, -IQR3, -IQR1, -Ethanol_Percentage, -Minimum, -Median) 

O11 = left_join(ms, control) |>
  mutate(article = "O2011") |> 
  mutate(mean_diff = mean_1 - mean_2,
         sd_diff = sqrt(((n_1 - 1)*sd_1^2 + (n_2 - 1)*sd_2^2) / (n_1+n_2 - 2))) |> 
  select(article, mean_diff, Week, sd_diff) |>
  rename("timepoint" = Week)

G2007 <- read_excel("G2007.xlsx")
G2007 = G2007 |> group_by(Group, Days) |> summarise(Mean = mean(Mean_Ethanol_Intake),
                                                    SD = sd(Mean_Ethanol_Intake), 
                                                    Variance = var(Mean_Ethanol_Intake),
                                                    n = n())

control = G2007 |> filter(Group == "MS15") |> rename("mean_2" = Mean,
                                                     "sd_2" = SD, "n_2" = n) |> select(-Variance, -Group)

ms = G2007 |> filter(Group == "MS360") |> rename("mean_1" = Mean,
                                                 "sd_1" = SD, "n_1" = n) |> select(-Variance, -Group)



G07 = left_join(ms, control, by = "Days") |>
  mutate(article = "G2007") |> 
  mutate(mean_diff = mean_1 - mean_2,
         sd_diff = sqrt(((n_1 - 1)*sd_1^2 + (n_2 - 1)*sd_2^2) / (n_1+n_2 - 2))) |> 
  select(article, mean_diff, Days, sd_diff) |>
  mutate(timepoint = case_when(
    Days == "1-18" ~ 2,
    Days == "19-36" ~ 4,
    Days == "37-54" ~ 6
  )) |> select(-Days)



Daoura2011 = tibble::tibble(
  "Group" = c("MS0", "MS0", "MS0", "MS0", "MS0",
              "MS15", "MS15", "MS15", "MS15", "MS15",
              "MS360", "MS360", "MS360", "MS360", "MS360"),
  "Week" = c(1, 2, 3, 4, 5, 1, 2, 3, 4, 5, 1, 2, 3, 4, 5),
  "Median" = c(1.97, 1.71, 2.08, 2.58, 1.74, 
               1.64, 1.75, 1.73, 1.84, 1.83,
               1.69, 1.90, 1.62, 2.40, 2.72),
  "IQR" = c(1.14, 2.27, 1.83, 2.35, 1.79,
            1.11, 2.42, 1.59, 1.85, 1.49,
            0.58, 1.63, 2.36, 2.14, 1.67),
  "n" = c(15, 15, 15, 15, 15, 
          15, 15, 15, 15, 15,
          14, 14, 14, 14, 14)
)

Daoura2011 = Daoura2011 |> mutate(
  "IQR1" = Median - (IQR/2),
  "IQR3" = Median + (IQR/2)
)

# C3 method 
Daoura2011 = Daoura2011 |>
  mutate(
    Estimated_Mean = ((IQR1+Median+IQR3)/3),
    Estimated_SD = ((IQR3-IQR1)/(2*qnorm((0.75*n-0.125)/(n+0.25)))),
    Estimated_Variance = Estimated_SD^2
  )

control = Daoura2011 |> filter(Group == "MS15") |> rename("mean_2" = Estimated_Mean,
                                                          "sd_2" = Estimated_SD, "n_2" = n) |> select(Week, n_2, mean_2, sd_2)

ms = Daoura2011 |> filter(Group == "MS360") |> rename("mean_1" = Estimated_Mean,
                                                      "sd_1" = Estimated_SD, "n_1" = n) |> select(Week, n_1, mean_1, sd_1)


D11 = left_join(ms, control) |>
  mutate(article = "D11") |> 
  mutate(mean_diff = mean_1 - mean_2,
         sd_diff = sqrt(((n_1 - 1)*sd_1^2 + (n_2 - 1)*sd_2^2) / (n_1+n_2 - 2))) |> 
  select(article, Week, mean_diff, sd_diff) |>
  rename("timepoint" = Week)

L2017 <- read_excel("L2017.xlsx") 

L2017 |> select(c(1:22)) |>
  pivot_longer(c(5:22), names_to = "Week", values_to = "Amount") |>
  mutate(Sessions = 3,
         Intake_Session = Amount/Sessions) |>
  group_by(Rearing, Supplier, Week) |>
  summarise(mean = mean(Intake_Session),
            sd = sd(Intake_Session))

diff_supplier = L2017 |> select(c(1:22)) |>
  pivot_longer(c(5:22), names_to = "Week", values_to = "Amount") |>
  mutate(Sessions = 3,
         Intake_Session = Amount/Sessions)

dif = lm(Intake_Session ~ Week*Supplier*Rearing, data = diff_supplier)
summary(dif) # no difference between suppliers and intake of ethanol (I can combine these)


L2017 = L2017 |> select(c(1:22)) |>
  pivot_longer(c(5:22), names_to = "Week", values_to = "Amount") |>
  filter(!is.na(Amount)) |>
  mutate(Sessions = 3,
         Intake_Session = Amount/Sessions) |>
  group_by(Rearing, Week) |>
  summarise(mean = mean(Intake_Session),
            sd = sd(Intake_Session),
            variance = var(Intake_Session))

control = L2017 |> filter(Rearing == "MS15") |> 
  slice(1:12) |> mutate(n = 10) |>
  rename("mean_2" = mean, "sd_2" = sd, "n_2" = n) |> select(Week, mean_2, sd_2, n_2)


ms = L2017 |> filter(Rearing == "MS360") |> 
  slice(1:12) |> mutate(n = 10) |>
  rename("mean_1" = mean, "sd_1" = sd, "n_1" = n) |> select(Week, mean_1, sd_1, n_1)


L17 = left_join(ms, control, by = "Week") |>
  mutate(article = "L2017") |> 
  mutate(mean_diff = mean_1 - mean_2,
         sd_diff = sqrt(((n_1 - 1)*sd_1^2 + (n_2 - 1)*sd_2^2) / (n_1+n_2 - 2))) |> 
  select(article, mean_diff, Week, sd_diff) |>
  mutate(timepoint = case_when(
    Week == "Intake W1" ~ 1,
    Week == "Intake W2" ~ 2,
    Week == "Intake W3" ~ 3, 
    Week == "Intake W4" ~ 4,
    Week == "Intake W5" ~ 5,
    Week == "Intake W6" ~ 6,
    Week == "Intake W7" ~ 7,
    Week == "Intake W8" ~ 8,
    Week == "Intake W9" ~ 9,
    Week == "Intake W10" ~ 10, 
    Week == "Intake W11" ~ 11,
    Week == "Intake W12" ~ 12,
    Week == "Intake W13" ~ 13,
    Week == "Intake W14" ~ 14
    
  )) |> select(-Week)

full = full_join(full_join(full_join(full_join(full_join(full_join(L17, O11), P03), G07), GN06), D11), C15) |> select(-Phase) |> filter(timepoint <= 7)

mean.diff = full |> pivot_wider(names_from = timepoint, values_from = mean_diff) |> select(-article) 

##########################
# Model data formation and parameters: 
################################################################################################################
N = 7
t = c(6, 5, 4, 3, 3, 5, 6)
nChains = 4
burnInSteps = 1000
thinSteps = 50
numSavedSteps = 5000
nIter = ceiling((numSavedSteps * thinSteps)/nChains)
mean.diff = full |> mutate(timepoint = c(1, 2, 3, 4, 5, 6, 
                                         1, 2, 3, 4, 5, 
                                         1, 2, 3, 4, 
                                         1, 2, 3,
                                         1, 2, 3,
                                         1, 2, 3, 4, 5,
                                         1, 2, 3, 4, 5, 6)) |>
  select( -sd_diff) |> pivot_wider(names_from = timepoint, values_from = mean_diff) |> select(-article) 
mean.diff = as.matrix(mean.diff)
mean.diff[is.na(mean.diff)] <- 0


sd_diff = full |> mutate(timepoint = c(1, 2, 3, 4, 5, 6, 
                                       1, 2, 3, 4, 5, 
                                       1, 2, 3, 4, 
                                       1, 2, 3,
                                       1, 2, 3,
                                       1, 2, 3, 4, 5,
                                       1, 2, 3, 4, 5, 6)) |>
  select(-mean_diff) |> pivot_wider(names_from = timepoint, values_from = sd_diff) |> select(-article)


sd_diff = cbind(sd_diff)
sd_diff = as.matrix(sd_diff)
sd_diff[ is.na( sd_diff ) ] <- 0



time_period = full |> 
  select(-mean_diff, -sd_diff) |>
  mutate(timeperiod = c(1, 2, 3, 4, 5, 6, 
                        1, 2, 3, 4, 5, 
                        1, 2, 3, 4, 
                        1, 2, 3,
                        1, 2, 3,
                        1, 2, 3, 4, 5,
                        1, 2, 3, 4, 5, 6)) |>
  pivot_wider(names_from = timeperiod, values_from = timepoint) |> select(-article) 
time_period = as.matrix(time_period)
time_period[is.na(time_period)] <- 0

increasing = c(0, 1, 1, 0, 0, 0, 1)
steady = c(1, 0, 0, 1, 1, 1, 0)

################################################################################################################

articleTimepoint = tibble(as.data.frame(time_period)) |> 
  mutate(article = c(1:7)) |> 
  pivot_longer(c(1:6), values_to = "timepoint") |> 
  select(-name) |> filter(timepoint != 0) 
timepoint = articleTimepoint |> select(timepoint)
article = articleTimepoint |> select(article)

-------------------------------------------------------------------------
  # Model 5: theta ~ time_period + time_period^2 + increasing + increasing*time_period
  #########
  
  modelstring = "
    model {
        for(i in 1:N) {
            for(j in 1:t[i]) {
                mean.diff[i, j] ~ dnorm(theta[i, j], prec[i, j])
                prec[i, j] <- 1/(sd_diff[i, j]* sd_diff[i, j])
                theta[i, j] <- beta1[i]*time_period[i, j]+beta2[i]*(time_period[i, j])^2+beta3[i]*(increasing[i])+beta4[i]*(time_period[i, j]*increasing[i])
                loglik[i, j] <- logdensity.norm(mean.diff[i, j], theta[i, j], prec[i, j])
                }
            beta1[i] ~ dnorm(beta.p, tau)  
            beta2[i] ~ dnorm(beta.p, tau) 
            beta3[i] ~ dnorm(beta.p, tau) 
            beta4[i] ~ dnorm(beta.p, tau)
        }
      beta.p ~ dnorm(0, 0.00001)
      tau <- 1/vari
      vari <- pow(sd, 2)
      sd ~ dnorm(0, 0.01)I(0,)
      pr.gr.zero <- step(beta.p) - equals(0, beta.p)
      }"
  
  writeLines(modelstring, con="model5.txt")
  
  jagsmod5 = jags(model.file = 'model5.txt', 
                  data = list('t' = t, 'N' = N, 'mean.diff' = mean.diff, 'sd_diff' = sd_diff, 'time_period' = time_period, 'increasing' = increasing), 
                  parameters.to.save =  c("beta1", "beta2", "beta3", "beta4", "tau", "theta", "loglik"), 
                  n.chains = nChains, 
                  n.iter = nIter, 
                  n.burnin = burnInSteps, 
                  n.thin = thinSteps)
  print(jagsmod5)
  
  jags5 = tibble(as.data.frame(as.matrix(jagsmod5$BUGSoutput$summary)))
  
  jag5.beta = jags5 |> slice(1:28) 
  jag5.loglik = jags5 |> slice(30:61) 
  jag5.tau = jags5 |> slice(62)
  jags5.theta = jags5 |> slice(63:94)
  
  
  # loglikelihood
  loglik = jags5 |> slice(30:61) |> summarise(loglik = sum(mean))
  loglik
  
  # AIC
  (2*3 - 2*loglik$loglik)
  
  # BIC
  (-2*loglik$loglik + 4*log(32))
  
  
  t = c(4, 6, 5, 7, 4, 5, 1)
  
  y = {}
  y[1] = jags5.theta |> filter(timepoint == 1) |> select(mean)
  y[2] = jags5.theta |> filter(timepoint == 2) |> select(mean)
  y[3] = jags5.theta |> filter(timepoint == 3) |> select(mean)
  y[4] = jags5.theta |> filter(timepoint == 4) |> select(mean)
  y[5] = jags5.theta |> filter(timepoint == 5) |> select(mean)
  y[6] = jags5.theta |> filter(timepoint == 6) |> select(mean)
  y[7] = jags5.theta |> filter(timepoint == 7) |> select(mean)
  
  w = {}
  w[1] = jags5.theta |> filter(timepoint == 1) |> select(sd) |> summarise(var = 1/(sd^2))
  w[2] = jags5.theta |> filter(timepoint == 2) |> select(sd) |> summarise(var = 1/(sd^2))
  w[3] = jags5.theta |> filter(timepoint == 3) |> select(sd) |> summarise(var = 1/(sd^2))
  w[4] = jags5.theta |> filter(timepoint == 4) |> select(sd) |> summarise(var = 1/(sd^2))
  w[5] = jags5.theta |> filter(timepoint == 5) |> select(sd) |> summarise(var = 1/(sd^2))
  w[6] = jags5.theta |> filter(timepoint == 6) |> select(sd) |> summarise(var = 1/(sd^2))
  w[7] = jags5.theta |> filter(timepoint == 7) |> select(sd) |> summarise(var = 1/(sd^2))
  
  
  
  Q = {}
  mu = {}
  for (i in 1:7) {
    y[i] 
    w[i]
    
    # weighted average of study estimates: 
    mu[i] = (sum(w[[i]]*y[[i]])/sum(w[[i]]))
    
    for (j in 1:t[i]) {
      Q = sum( w[[j]]* (y[[j]] - mu[i])^2 )
    }
  }
  
  # H^2
  H = (Q/31)^2
  
  # I^2
  I = ((H - 1)/ H)^2
  I ## 0.5472764
  
    
    
    

    
    
-------------------------------------------------------------------------
  # Model 6: theta ~ time_period + time_period^2 + increasing + steady*time_period + increasing*time_period
  ############
  
  modelstring = "
    model {
        for(i in 1:N) {
            for(j in 1:t[i]) {
                mean.diff[i, j] ~ dnorm(theta[i, j], prec[i, j])
                prec[i, j] <- 1/(sd_diff[i, j]* sd_diff[i, j])
                theta[i, j] <- beta1[i]*time_period[i, j]+beta2[i]*(time_period[i, j])^2+beta3[i]*(increasing[i])+beta4[i]*(time_period[i, j]*steady[i])+beta5[i]*(time_period[i, j]*increasing[i])
                loglik[i, j] <- logdensity.norm(mean.diff[i, j], theta[i, j], prec[i, j])
                }
            beta1[i] ~ dnorm(beta.p, tau)  
            beta2[i] ~ dnorm(beta.p, tau) 
            beta3[i] ~ dnorm(beta.p, tau) 
            beta4[i] ~ dnorm(beta.p, tau)
            beta5[i] ~ dnorm(beta.p, tau)
        }
      beta.p ~ dnorm(0, 0.00001)
      tau <- 1/vari
      vari <- pow(sd, 2)
      sd ~ dnorm(0, 0.01)I(0,)
      pr.gr.zero <- step(beta.p) - equals(0, beta.p)
      }"
  
  writeLines(modelstring, con="model6.txt")
  
  jagsmod6 = jags(model.file = 'model6.txt', 
                  data = list('t' = t, 'N' = N, 'mean.diff' = mean.diff, 'sd_diff' = sd_diff, 'time_period' = time_period, 'increasing' = increasing, 'steady' = steady), 
                  parameters.to.save =  c("beta1", "beta2", "beta3", "beta4", "tau", "theta", "loglik"), 
                  n.chains = nChains, 
                  n.iter = nIter, 
                  n.burnin = burnInSteps, 
                  n.thin = thinSteps)
  print(jagsmod6)
  
  jags6 = tibble(as.data.frame(as.matrix(jagsmod6$BUGSoutput$summary)))
  
  jags6.beta = jags6 |> slice(1:32) 
  jags6.loglik = jags6 |> slice(30:61) 
  jags6.tau = jags6 |> slice(62)
  jags6.theta = jags6 |> slice(63:94)
  
  
  # loglikelihood
  loglik = jags6 |> slice(30:61) |> summarise(loglik = sum(mean))
  loglik
  
  # AIC
  (2*3 - 2*loglik$loglik)
  
  # BIC
  (-2*loglik$loglik + 4*log(32))
  
  
  t = c(4, 6, 5, 7, 4, 5, 1)
  
  y = {}
  y[1] = jags6.theta |> filter(timepoint == 1) |> select(mean)
  y[2] = jags6.theta |> filter(timepoint == 2) |> select(mean)
  y[3] = jags6.theta |> filter(timepoint == 3) |> select(mean)
  y[4] = jags6.theta |> filter(timepoint == 4) |> select(mean)
  y[5] = jags6.theta |> filter(timepoint == 5) |> select(mean)
  y[6] = jags6.theta |> filter(timepoint == 6) |> select(mean)
  y[7] = jags6.theta |> filter(timepoint == 7) |> select(mean)
  
  w = {}
  w[1] = jags6.theta |> filter(timepoint == 1) |> select(sd) |> summarise(var = 1/(sd^2))
  w[2] = jags6.theta |> filter(timepoint == 2) |> select(sd) |> summarise(var = 1/(sd^2))
  w[3] = jags6.theta |> filter(timepoint == 3) |> select(sd) |> summarise(var = 1/(sd^2))
  w[4] = jags6.theta |> filter(timepoint == 4) |> select(sd) |> summarise(var = 1/(sd^2))
  w[5] = jags6.theta |> filter(timepoint == 5) |> select(sd) |> summarise(var = 1/(sd^2))
  w[6] = jags6.theta |> filter(timepoint == 6) |> select(sd) |> summarise(var = 1/(sd^2))
  w[7] = jags6.theta |> filter(timepoint == 7) |> select(sd) |> summarise(var = 1/(sd^2))
  
  
  
  Q = {}
  mu = {}
  for (i in 1:7) {
    y[i] 
    w[i]
    
    # weighted average of study estimates: 
    mu[i] = (sum(w[[i]]*y[[i]])/sum(w[[i]]))
    
    for (j in 1:t[i]) {
      Q = sum( w[[j]]* (y[[j]] - mu[i])^2 )
    }
  }
  
  # H^2
  H = (Q/31)^2
  
  # I^2
  I = ((H - 1)/ H)^2
  I ## 0.5991446
  
  
  -----------------------------------------
    # Model 7: theta ~ time_period + time_period^2 + increasing + steady*time_period + increasing*time_period + time_period*increasing*steady
    ############
    modelstring = "
    model {
        for(i in 1:N) {
            for(j in 1:t[i]) {
                mean.diff[i, j] ~ dnorm(theta[i, j], prec[i, j])
                prec[i, j] <- 1/(sd_diff[i, j]* sd_diff[i, j])
                theta[i, j] <- beta1[i]*time_period[i, j]+beta2[i]*(time_period[i, j])^2+beta3[i]*(increasing[i])+beta4[i]*(time_period[i, j]*steady[i])+beta5[i]*(time_period[i, j]*increasing[i])+beta6[i]*(time_period[i, j]*increasing[i]*steady[i])
                loglik[i, j] <- logdensity.norm(mean.diff[i, j], theta[i, j], prec[i, j])
                }
            beta1[i] ~ dnorm(beta.p, tau)  
            beta2[i] ~ dnorm(beta.p, tau) 
            beta3[i] ~ dnorm(beta.p, tau) 
            beta4[i] ~ dnorm(beta.p, tau)
            beta5[i] ~ dnorm(beta.p, tau)
            beta6[i] ~ dnorm(beta.p, tau)
        }
      beta.p ~ dnorm(0, 0.00001)
      tau <- 1/vari
      vari <- pow(sd, 2)
      sd ~ dnorm(0, 0.01)I(0,)
      pr.gr.zero <- step(beta.p) - equals(0, beta.p)
      }"
  
  writeLines(modelstring, con="model7.txt")
  
  jagsmod7 = jags(model.file = 'model7.txt', 
                  data = list('t' = t, 'N' = N, 'mean.diff' = mean.diff, 'sd_diff' = sd_diff, 'time_period' = time_period, 'increasing' = increasing, 'steady' = steady), 
                  parameters.to.save =  c("beta1", "beta2", "beta3", "beta4", "tau", "theta", "loglik"), 
                  n.chains = nChains, 
                  n.iter = nIter, 
                  n.burnin = burnInSteps, 
                  n.thin = thinSteps)
  print(jagsmod7)
  
  jags7 = tibble(as.data.frame(as.matrix(jagsmod7$BUGSoutput$summary)))
  
  jags7.beta = jags7 |> slice(1:32) 
  jags7.loglik = jags7 |> slice(30:61) 
  jags7.tau = jags7 |> slice(62)
  jags7.theta = jags7 |> slice(63:94)
  
  
  # loglikelihood
  loglik = jags7 |> slice(30:61) |> summarise(loglik = sum(mean))
  loglik
  
  # AIC
  (2*3 - 2*loglik$loglik)
  
  # BIC
  (-2*loglik$loglik + 4*log(32))
  
  
  t = c(4, 6, 5, 7, 4, 5, 1)
  
  y = {}
  y[1] = jags7.theta |> filter(timepoint == 1) |> select(mean)
  y[2] = jags7.theta |> filter(timepoint == 2) |> select(mean)
  y[3] = jags7.theta |> filter(timepoint == 3) |> select(mean)
  y[4] = jags7.theta |> filter(timepoint == 4) |> select(mean)
  y[5] = jags7.theta |> filter(timepoint == 5) |> select(mean)
  y[6] = jags7.theta |> filter(timepoint == 6) |> select(mean)
  y[7] = jags7.theta |> filter(timepoint == 7) |> select(mean)
  
  w = {}
  w[1] = jags7.theta |> filter(timepoint == 1) |> select(sd) |> summarise(var = 1/(sd^2))
  w[2] = jags7.theta |> filter(timepoint == 2) |> select(sd) |> summarise(var = 1/(sd^2))
  w[3] = jags7.theta |> filter(timepoint == 3) |> select(sd) |> summarise(var = 1/(sd^2))
  w[4] = jags7.theta |> filter(timepoint == 4) |> select(sd) |> summarise(var = 1/(sd^2))
  w[5] = jags7.theta |> filter(timepoint == 5) |> select(sd) |> summarise(var = 1/(sd^2))
  w[6] = jags7.theta |> filter(timepoint == 6) |> select(sd) |> summarise(var = 1/(sd^2))
  w[7] = jags7.theta |> filter(timepoint == 7) |> select(sd) |> summarise(var = 1/(sd^2))
  
  
  
  Q = {}
  mu = {}
  for (i in 1:7) {
    y[i] 
    w[i]
    
    # weighted average of study estimates: 
    mu[i] = (sum(w[[i]]*y[[i]])/sum(w[[i]]))
    
    for (j in 1:t[i]) {
      Q = sum( w[[j]]* (y[[j]] - mu[i])^2 )
    }
  }
  
  # H^2
  H = (Q/31)^2
  
  # I^2
  I = ((H - 1)/ H)^2
  I # 0.5530638
  
-------------------------------------
# Model 8: theta ~ time_period + time_period^2 + increasing + steady*time_period + increasing*time_period + time_period*increasing*steady + oneBottle
    ############
  
  
  modelstring = "
    model {
        for(i in 1:N) {
            for(j in 1:t[i]) {
                mean.diff[i, j] ~ dnorm(theta[i, j], prec[i, j])
                prec[i, j] <- 1/(sd_diff[i, j]* sd_diff[i, j])
                theta[i, j] <- beta1[i]*time_period[i, j]+beta2[i]*(time_period[i, j])^2+beta3[i]*(increasing[i])+beta4[i]*(oneBottle[i])+beta5[i]*(time_period[i, j]*steady[i])+beta6[i]*(time_period[i, j]*increasing[i])+beta7[i]*(time_period[i, j]*increasing[i]*steady[i])
                loglik[i, j] <- logdensity.norm(mean.diff[i, j], theta[i, j], prec[i, j])
                }
            beta1[i] ~ dnorm(beta.p, tau)  
            beta2[i] ~ dnorm(beta.p, tau) 
            beta3[i] ~ dnorm(beta.p, tau) 
            beta4[i] ~ dnorm(beta.p, tau)
            beta5[i] ~ dnorm(beta.p, tau)
            beta6[i] ~ dnorm(beta.p, tau)
            beta7[i] ~ dnorm(beta.p, tau)
        }
      beta.p ~ dnorm(0, 0.00001)
      tau <- 1/vari
      vari <- pow(sd, 2)
      sd ~ dnorm(0, 0.01)I(0,)
      pr.gr.zero <- step(beta.p) - equals(0, beta.p)
      }"
  
  writeLines(modelstring, con="model8.txt")
  
  jagsmod8 = jags(model.file = 'model8.txt', 
                  data = list('t' = t, 'N' = N, 'mean.diff' = mean.diff, 'sd_diff' = sd_diff, 'time_period' = time_period, 'increasing' = increasing, 'steady' = steady, 'oneBottle' = oneBottle), 
                  parameters.to.save =  c("beta1", "beta2", "beta3", "beta4", "tau", "theta", "loglik"), 
                  n.chains = nChains, 
                  n.iter = nIter, 
                  n.burnin = burnInSteps, 
                  n.thin = thinSteps)
  print(jagsmod8)
  
  jags8 = tibble(as.data.frame(as.matrix(jagsmod8$BUGSoutput$summary)))
  
  jags8.beta = jags8 |> slice(1:32) 
  jags8.loglik = jags8 |> slice(30:61) 
  jags8.tau = jags8 |> slice(62)
  jags8.theta = jags8 |> slice(63:94)
  
  
  # loglikelihood
  loglik = jags8 |> slice(30:61) |> summarise(loglik = sum(mean))
  loglik
  
  # AIC
  (2*3 - 2*loglik$loglik)
  
  # BIC
  (-2*loglik$loglik + 4*log(32))
  
  
  t = c(4, 6, 5, 7, 4, 5, 1)
  
  y = {}
  y[1] = jags8.theta |> filter(timepoint == 1) |> select(mean)
  y[2] = jags8.theta |> filter(timepoint == 2) |> select(mean)
  y[3] = jags8.theta |> filter(timepoint == 3) |> select(mean)
  y[4] = jags8.theta |> filter(timepoint == 4) |> select(mean)
  y[5] = jags8.theta |> filter(timepoint == 5) |> select(mean)
  y[6] = jags8.theta |> filter(timepoint == 6) |> select(mean)
  y[7] = jags8.theta |> filter(timepoint == 7) |> select(mean)
  
  w = {}
  w[1] = jags8.theta |> filter(timepoint == 1) |> select(sd) |> summarise(var = 1/(sd^2))
  w[2] = jags8.theta |> filter(timepoint == 2) |> select(sd) |> summarise(var = 1/(sd^2))
  w[3] = jags8.theta |> filter(timepoint == 3) |> select(sd) |> summarise(var = 1/(sd^2))
  w[4] = jags8.theta |> filter(timepoint == 4) |> select(sd) |> summarise(var = 1/(sd^2))
  w[5] = jags8.theta |> filter(timepoint == 5) |> select(sd) |> summarise(var = 1/(sd^2))
  w[6] = jags8.theta |> filter(timepoint == 6) |> select(sd) |> summarise(var = 1/(sd^2))
  w[7] = jags8.theta |> filter(timepoint == 7) |> select(sd) |> summarise(var = 1/(sd^2))
  
  
  
  Q = {}
  mu = {}
  for (i in 1:7) {
    y[i] 
    w[i]
    
    # weighted average of study estimates: 
    mu[i] = (sum(w[[i]]*y[[i]])/sum(w[[i]]))
    
    for (j in 1:t[i]) {
      Q = sum( w[[j]]* (y[[j]] - mu[i])^2 )
    }
  }
  
  # H^2
  H = (Q/31)^2
  
  # I^2
  I = ((H - 1)/ H)^2
  I # 0.6022571

  
  
  
  
  
  
  
  
  
  