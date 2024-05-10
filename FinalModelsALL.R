


library(tidyverse)
library(readxl)
library(rjags)
library(runjags)
library(R2jags)
library(lattice)
library(mcmcplots)
library(strex)
library(loo)
library(emojifont)

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
         pooled_SD = sqrt(((n_1 - 1)*Estimated_SD_1^2 + (n_2 - 1)*Estimated_SD_2^2) / (n_1+n_2 - 2)),
         sd_diff = Estimated_SD_1 - Estimated_SD_2, 
         yij = mean_diff/pooled_SD) |> select(article, Week, mean_diff, pooled_SD, yij, sd_diff) |>
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
         pooled_SD = sqrt(((n_1 - 1)*sd_1^2 + (n_2 - 1)*sd_2^2) / (n_1+n_2 - 2)),
         sd_diff = sd_1 - sd_2,
         yij = mean_diff/pooled_SD) |>
  mutate(timepoint = case_when(
    Phase == "Acquisition" ~ 2,
    Phase == "Maintenance" ~ 4,
    Phase == "Stabilization" ~ 6
  )) |> select(article, mean_diff, pooled_SD, yij, timepoint, sd_diff)


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
         pooled_SD = sqrt(((n_1 - 1)*sd_1^2 + (n_2 - 1)*sd_2^2) / (n_1+n_2 - 2)),
         sd_diff = sd_1 - sd_2, 
         yij = mean_diff/pooled_SD) |> select(article, mean_diff, pooled_SD, yij, sd_diff) |>
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
         pooled_SD = sqrt(((n_1 - 1)*sd_1^2 + (n_2 - 1)*sd_2^2) / (n_1+n_2 - 2)),
         sd_diff = sd_1 - sd_2, 
         yij = mean_diff/pooled_SD) |> select(article, mean_diff, pooled_SD, yij, Week, sd_diff) |>
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
         pooled_SD = sqrt(((n_1 - 1)*sd_1^2 + (n_2 - 1)*sd_2^2) / (n_1+n_2 - 2)),
         sd_diff = sd_1 - sd_2, 
         yij = mean_diff/pooled_SD) |> select(article, mean_diff, pooled_SD, yij, Days, sd_diff) |>
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
         pooled_SD = sqrt(((n_1 - 1)*sd_1^2 + (n_2 - 1)*sd_2^2) / (n_1+n_2 - 2)),
         sd_diff = sd_1 - sd_2, 
         yij = mean_diff/pooled_SD) |> select(article, Week, mean_diff, pooled_SD, yij, sd_diff) |>
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
         pooled_SD = sqrt(((n_1 - 1)*sd_1^2 + (n_2 - 1)*sd_2^2) / (n_1+n_2 - 2)),
         sd_diff = sd_1 - sd_2, 
         yij = mean_diff/pooled_SD) |> select(article, mean_diff, pooled_SD, yij, Week, sd_diff) |>
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

mean.diff = full |> select(-pooled_SD, -yij) |> pivot_wider(names_from = timepoint, values_from = mean_diff) |> select(-article) 

##########################


# Model data formation and parameters: 
################################################################################################################
N = 7
t = c(6, 5, 4, 3, 3, 5, 6)
nChains = 4
burnInSteps = 1000
thinSteps = 50
numSavedSteps = 5000
nIter = 150000
mean.diff = full |> mutate(timepoint = c(1, 2, 3, 4, 5, 6, 
                                         1, 2, 3, 4, 5, 
                                         1, 2, 3, 4, 
                                         1, 2, 3,
                                         1, 2, 3,
                                         1, 2, 3, 4, 5,
                                         1, 2, 3, 4, 5, 6)) |>
  select(-pooled_SD, -yij, -sd_diff) |> pivot_wider(names_from = timepoint, values_from = mean_diff) |> select(-article) 
mean.diff = as.matrix(mean.diff)
mean.diff[is.na(mean.diff)] <- 0


sd_diff = full |> mutate(timepoint = c(1, 2, 3, 4, 5, 6, 
                                       1, 2, 3, 4, 5, 
                                       1, 2, 3, 4, 
                                       1, 2, 3,
                                       1, 2, 3,
                                       1, 2, 3, 4, 5,
                                       1, 2, 3, 4, 5, 6)) |>
  select(-mean_diff, -yij, -pooled_SD) |> pivot_wider(names_from = timepoint, values_from = sd_diff) |> select(-article)


sd_diff = cbind(sd_diff)
sd_diff = as.matrix(sd_diff)
sd_diff[ is.na( sd_diff ) ] <- 0



time_period = full |> 
  select(-pooled_SD, -yij, -mean_diff, -sd_diff) |>
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

increasing = c(0, 1, 1, 0, 0, 0, 0)
steady = c(1, 0, 0, 1, 1, 1, 1)

################################################################################################################


articleTimepoint = tibble(as.data.frame(time_period)) |> 
  mutate(article = c(1:7)) |> 
  pivot_longer(c(1:6), values_to = "timepoint") |> 
  select(-name) |> filter(timepoint != 0) 
timepoint = articleTimepoint |> select(timepoint)
article = articleTimepoint |> select(article)


## Model 1:
### theta ~ time + time^2 + increasing + steady  

modelstring = "
    model {
        for(i in 1:N) {
            for(j in 1:t[i]) {
                mean.diff[i, j] ~ dnorm(theta[i, j], prec[i, j])
                prec[i, j] <- 1/(sd_diff[i, j]* sd_diff[i, j])
                theta[i, j] <- beta1[i]*time_period[i, j]+beta2[i]*(time_period[i, j])^2+beta3[i]*(increasing[i])+beta4[i]*(steady[i])
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
      sd ~ dgamma(0.01, 0.01)
      pr.gr.zero <- step(beta.p) - equals(0, beta.p)
      }"

writeLines(modelstring, con="modelf.txt")


jagsmodf = jags(model.file = 'modelf.txt', 
                data = list('t' = t, 'N' = N, 'mean.diff' = mean.diff, 'sd_diff' = sd_diff, 'time_period' = time_period, 'increasing' = increasing, 'steady' = steady), 
                parameters.to.save =  c("beta1", "beta2", "beta3", "beta4", "tau", "theta", "loglik"), 
                n.chains = nChains, 
                n.iter = nIter, 
                n.burnin = burnInSteps, 
                n.thin = thinSteps)
print(jagsmodf)


jagsf = tibble(as.data.frame(as.matrix(jagsmodf$BUGSoutput$summary)))

jagsf.beta = jagsf |> slice(1:28) |>
  mutate(Model = 1)
jagsf.theta = jagsf |> slice(63:94)
jagsf.theta = jagsf.theta |> mutate(timepoint = timepoint$timepoint,
                                    article = articleTimepoint$article) |> mutate(increasing = case_when(
                                      article == 1 ~ "Increasing",
                                      article == 2 ~ "Steady",
                                      article == 3 ~ "Increasing", 
                                      article == 4 ~ "Increasing",
                                      article == 5 ~ "Steady", 
                                      article == 6 ~ "Steady",
                                      article == 7 ~ "Steady"
                                    )) |> mutate(increasing = as.factor(increasing),
                                                 article = as.factor(article)) |>
  mutate(Model = 1)

## Model 2: 
### theta ~ time + time^2 + increasing + steady + increasing*oneBottle + steady*oneBottle  

modelstring = "
    model {
        for(i in 1:N) {
            for(j in 1:t[i]) {
                mean.diff[i, j] ~ dnorm(theta[i, j], prec[i, j])
                prec[i, j] <- 1/(sd_diff[i, j]* sd_diff[i, j])
                theta[i, j] <- beta1[i]*time_period[i, j]+beta2[i]*(time_period[i, j])^2+beta3[i]*(increasing[i])+beta4[i]*(steady[i])+beta5[i]*(increasing[i]*oneBottle[i])+beta6[i]*(steady[i]*oneBottle[i])
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

writeLines(modelstring, con="modelf2.txt")

jagsmodf2 = jags(model.file = 'modelf2.txt', 
                data = list('t' = t, 'N' = N, 'mean.diff' = mean.diff, 'sd_diff' = sd_diff, 'time_period' = time_period, 'increasing' = increasing, 'steady' = steady, 'oneBottle'= oneBottle), 
                parameters.to.save =  c("beta1", "beta2", "beta3", "beta4", "beta5", "beta6", "tau", "theta", "loglik"), 
                n.chains = nChains, 
                n.iter = nIter, 
                n.burnin = burnInSteps, 
                n.thin = thinSteps)
print(jagsmodf2)

jagsf2 = tibble(as.data.frame(as.matrix(jagsmodf2$BUGSoutput$summary)))

jagsf2.beta = jagsf2 |> slice(1:42) |>
  mutate(Model = 2)
jagsf2.loglik = jagsf2 |> slice(44:75) 
jagsf2.tau = jagsf2 |> slice(76)
jagsf2.theta = jagsf2 |> slice(77:108) |> mutate(timepoint = timepoint$timepoint,
                                                 article = articleTimepoint$article) |> mutate(increasing = case_when(
                                                   article == 1 ~ "Increasing",
                                                   article == 2 ~ "Steady",
                                                   article == 3 ~ "Increasing", 
                                                   article == 4 ~ "Increasing",
                                                   article == 5 ~ "Steady", 
                                                   article == 6 ~ "Steady",
                                                   article == 7 ~ "Steady"
                                                 )) |> mutate(increasing = as.factor(increasing),
                                                              article = as.factor(article)) |>
  mutate(Model = 2)


## Model 3:  
### theta ~ time + time^2 + increasing*time + steady*time + oneBottle*time 

modelstring = "
    model {
        for(i in 1:N) {
            for(j in 1:t[i]) {
                mean.diff[i, j] ~ dnorm(theta[i, j], prec[i, j])
                prec[i, j] <- 1/(sd_diff[i, j]* sd_diff[i, j])
                theta[i, j] <- beta1[i]*time_period[i, j]+beta2[i]*(time_period[i, j])^2+beta3[i]*(increasing[i]*time_period[i, j])+beta4[i]*(steady[i]*time_period[i, j])+beta5[i]*(oneBottle[i]*time_period[i, j])
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

writeLines(modelstring, con="modelf3.txt")

jagsmodf3 = jags(model.file = 'modelf3.txt', 
                 data = list('t' = t, 'N' = N, 'mean.diff' = mean.diff, 'sd_diff' = sd_diff, 'time_period' = time_period, 'increasing' = increasing, 'steady' = steady, 'oneBottle'= oneBottle), 
                 parameters.to.save =  c("beta1", "beta2", "beta3", "beta4", "beta5", "tau", "theta", "loglik"), 
                 n.chains = nChains, 
                 n.iter = nIter, 
                 n.burnin = burnInSteps, 
                 n.thin = thinSteps)
print(jagsmodf3)


jagsf3 = tibble(as.data.frame(as.matrix(jagsmodf3$BUGSoutput$summary)))

jagsf3.beta = jagsf3 |> slice(1:35) |>
  mutate(Model = 3)
jagsf3.loglik = jagsf3 |> slice(37:68) 
jagsf3.tau = jagsf3 |> slice(69)
jagsf3.theta = jagsf3 |> slice(70:108) |> mutate(timepoint = timepoint$timepoint,
                                                 article = articleTimepoint$article) |> mutate(increasing = case_when(
                                                   article == 1 ~ "Increasing",
                                                   article == 2 ~ "Steady",
                                                   article == 3 ~ "Increasing", 
                                                   article == 4 ~ "Increasing",
                                                   article == 5 ~ "Steady", 
                                                   article == 6 ~ "Steady",
                                                   article == 7 ~ "Steady"
                                                 ))|> mutate(increasing = as.factor(increasing),
                                                              article = as.factor(article)) |>
  mutate(Model = 3)



##############################################################
# Combining the 3 models:  

all3beta = full_join(jagsf.beta, full_join(jagsf2.beta, jagsf3.beta))

all3beta = all3beta |> mutate(beta = 
                     c(paste0("\u03B2", "\u2081", " (time-point)"), paste0("\u03B2", "\u2081", " (time-point)"), paste0("\u03B2", "\u2081", " (time-point)"), paste0("\u03B2", "\u2081", " (time-point)"), paste0("\u03B2", "\u2081", " (time-point)"), paste0("\u03B2", "\u2081", " (time-point)"), paste0("\u03B2", "\u2081", " (time-point)"),
                       paste0("\u03B2", "\u2082", " (time-point)", "\u00B2"), paste0("\u03B2", "\u2082", " (time-point)", "\u00B2"), paste0("\u03B2", "\u2082", " (time-point)", "\u00B2"), paste0("\u03B2", "\u2082", " (time-point)", "\u00B2"), paste0("\u03B2", "\u2082", " (time-point)", "\u00B2"), paste0("\u03B2", "\u2082", " (time-point)", "\u00B2"), paste0("\u03B2", "\u2082", " (time-point)", "\u00B2"),
                       paste0("\u03B2", "\u2083", "(increasing)"), paste0("\u03B2", "\u2083", "(increasing)"), paste0("\u03B2", "\u2083", "(increasing)"), paste0("\u03B2", "\u2083", "(increasing)"), paste0("\u03B2", "\u2083", "(increasing)"), paste0("\u03B2", "\u2083", "(increasing)"), paste0("\u03B2", "\u2083", "(increasing)"),
                       paste0("\u03B2", "\u2084", "(steady)"), paste0("\u03B2", "\u2084", "(steady)"), paste0("\u03B2", "\u2084", "(steady)"), paste0("\u03B2", "\u2084", "(steady)"), paste0("\u03B2", "\u2084", "(steady)"), paste0("\u03B2", "\u2084", "(steady)"), paste0("\u03B2", "\u2084", "(steady)"),
                       
                       paste0("\u03B2", "\u2081", " (time-point)"), paste0("\u03B2", "\u2081", " (time-point)"), paste0("\u03B2", "\u2081", " (time-point)"), paste0("\u03B2", "\u2081", " (time-point)"), paste0("\u03B2", "\u2081", " (time-point)"), paste0("\u03B2", "\u2081", " (time-point)"), paste0("\u03B2", "\u2081", " (time-point)"),
                       paste0("\u03B2", "\u2082", " (time-point)", "\u00B2"), paste0("\u03B2", "\u2082", " (time-point)", "\u00B2"), paste0("\u03B2", "\u2082", " (time-point)", "\u00B2"), paste0("\u03B2", "\u2082", " (time-point)", "\u00B2"), paste0("\u03B2", "\u2082", " (time-point)", "\u00B2"), paste0("\u03B2", "\u2082", " (time-point)", "\u00B2"),paste0("\u03B2", "\u2082", " (time-point)", "\u00B2"),
                       paste0("\u03B2", "\u2083", "(increasing)"), paste0("\u03B2", "\u2083", "(increasing)"), paste0("\u03B2", "\u2083", "(increasing)"), paste0("\u03B2", "\u2083", "(increasing)"), paste0("\u03B2", "\u2083", "(increasing)"), paste0("\u03B2", "\u2083", "(increasing)"), paste0("\u03B2", "\u2083", "(increasing)"),
                       paste0("\u03B2", "\u2084", "(steady)"), paste0("\u03B2", "\u2084", "(steady)"), paste0("\u03B2", "\u2084", "(steady)"), paste0("\u03B2", "\u2084", "(steady)"), paste0("\u03B2", "\u2084", "(steady)"), paste0("\u03B2", "\u2084", "(steady)"), paste0("\u03B2", "\u2084", "(steady)"),
                       paste0("\u03B2", "\u2085", "(increasing*oneBottle)"), paste0("\u03B2", "\u2085", "(increasing*oneBottle)"), paste0("\u03B2", "\u2085", "(increasing*oneBottle)"), paste0("\u03B2", "\u2085", "(increasing*oneBottle)"), paste0("\u03B2", "\u2085", "(increasing*oneBottle)"), paste0("\u03B2", "\u2085", "(increasing*oneBottle)"), paste0("\u03B2", "\u2085", "(increasing*oneBottle)"),
                       paste0("\u03B2\u2086", "(steady*oneBottle)"), paste0("\u03B2\u2086", "(steady*oneBottle)"), paste0("\u03B2\u2086", "(steady*oneBottle)"), paste0("\u03B2\u2086", "(steady*oneBottle)"), paste0("\u03B2\u2086", "(steady*oneBottle)"), paste0("\u03B2\u2086", "(steady*oneBottle)"), paste0("\u03B2\u2086", "(steady*oneBottle)"),
                       
                       paste0("\u03B2", "\u2081", " (time-point)"), paste0("\u03B2", "\u2081", " (time-point)"), paste0("\u03B2", "\u2081", " (time-point)"), paste0("\u03B2", "\u2081", " (time-point)"), paste0("\u03B2", "\u2081", " (time-point)"), paste0("\u03B2", "\u2081", " (time-point)"), paste0("\u03B2", "\u2081", " (time-point)"),
                       paste0("\u03B2", "\u2082", " (time-point)", "\u00B2"), paste0("\u03B2", "\u2082", " (time-point)", "\u00B2"), paste0("\u03B2", "\u2082", " (time-point)", "\u00B2"), paste0("\u03B2", "\u2082", " (time-point)", "\u00B2"), paste0("\u03B2", "\u2082", " (time-point)", "\u00B2"), paste0("\u03B2", "\u2082", " (time-point)", "\u00B2"), paste0("\u03B2", "\u2082", " (time-point)", "\u00B2"), 
                       paste0("\u03B2", "\u2083", "(increasing*time-point)"), paste0("\u03B2", "\u2083", "(increasing*time-point)"), paste0("\u03B2", "\u2083", "(increasing*time-point)"), paste0("\u03B2", "\u2083", "(increasing*time-point)"), paste0("\u03B2", "\u2083", "(increasing*time-point)"), paste0("\u03B2", "\u2083", "(increasing*time-point)"), paste0("\u03B2", "\u2083", "(increasing*time-point)"),
                       paste0("\u03B2", "\u2084", " (steady*time-point)"), paste0("\u03B2", "\u2084", " (steady*time-point)"), paste0("\u03B2", "\u2084", " (steady*time-point)"), paste0("\u03B2", "\u2084", " (steady*time-point)"), paste0("\u03B2", "\u2084", " (steady*time-point)"), paste0("\u03B2", "\u2084", " (steady*time-point)"), paste0("\u03B2", "\u2084", " (steady*time-point)"),
                       paste0("\u03B2", "\u2085", " (oneBottle*time-point)"), paste0("\u03B2", "\u2085", " (oneBottle*time-point)"), paste0("\u03B2", "\u2085", " (oneBottle*time-point)"), paste0("\u03B2", "\u2085", " (oneBottle*time-point)"), paste0("\u03B2", "\u2085", " (oneBottle*time-point)"), paste0("\u03B2", "\u2085", " (oneBottle*time-point)"), paste0("\u03B2", "\u2085", " (oneBottle*time-point)")
                            )) |>
  mutate(article = c(1, 2, 3, 4, 5, 6, 7, 
                     1, 2, 3, 4, 5, 6, 7, 
                     1, 2, 3, 4, 5, 6, 7, 
                     1, 2, 3, 4, 5, 6, 7, 
                     
                     1, 2, 3, 4, 5, 6, 7, 
                     1, 2, 3, 4, 5, 6, 7, 
                     1, 2, 3, 4, 5, 6, 7, 
                     1, 2, 3, 4, 5, 6, 7, 
                     1, 2, 3, 4, 5, 6, 7, 
                     1, 2, 3, 4, 5, 6, 7, 
                     
                     1, 2, 3, 4, 5, 6, 7, 
                     1, 2, 3, 4, 5, 6, 7, 
                     1, 2, 3, 4, 5, 6, 7, 
                     1, 2, 3, 4, 5, 6, 7, 
                     1, 2, 3, 4, 5, 6, 7)) |>
  mutate(beta_n = c(1, 1, 1, 1, 1, 1, 1, 
                    2, 2, 2, 2, 2, 2, 2,
                    3, 3, 3, 3, 3, 3, 3,
                    4, 4, 4, 4, 4, 4, 4,
                    
                    1, 1, 1, 1, 1, 1, 1, 
                    2, 2, 2, 2, 2, 2, 2,
                    3, 3, 3, 3, 3, 3, 3, 
                    4, 4, 4, 4, 4, 4, 4, 
                    5, 5, 5, 5, 5, 5, 5, 
                    6, 6, 6, 6, 6, 6, 6,
                    
                    1, 1, 1, 1, 1, 1, 1, 
                    2, 2, 2, 2, 2, 2, 2,
                    3, 3, 3, 3, 3, 3, 3, 
                    4, 4, 4, 4, 4, 4, 4, 
                    5, 5, 5, 5, 5, 5, 5
                    ))
  


hey = all3beta |> filter(Model == 3) |> filter(beta_n == 5) |>
  summarise(
            low.b = quantile(mean, c(0.025)),
            upper.b = quantile(mean, c(0.975)),
            mean = mean(mean))

pt(hey$mean, df = 6, lower.tail = FALSE)
pt(hey$mean, df = 6, lower.tail = TRUE)


pooledm1 = all3beta |> filter(Model == 1) |> 
  group_by(beta_n) |>
  mutate(pooled_mean = mean(mean),
         low.b = quantile(mean, c(0.025)),
         upper.b = quantile(mean, c(0.975)))


all3beta  |>
  mutate(beta = as.factor(beta)) |>
  ggplot(aes(y = mean, x = article, col = beta)) +
  geom_point(aes(col = beta), shape = 21, size = 4) +
  scale_color_grey(labels = c("\u03B2\u2081(time-point)", 
                                "\u03B2\u2082(time-point)\u00B2",
                                "\u03B2\u2083(increasing)",
                                "\u03B2\u2083(increasing*time-point)",
                                "\u03B2\u2084(steady*time-point)",
                                "\u03B2\u2084(steady)",
                                "\u03B2\u2086(oneBottle*time-point)",
                                bquote("\u03B2""[\u2085]""(increasing*oneBottle)"),
                                paste("\u03B2\u2086(steady*onebottle)"))) +
  #geom_smooth(aes(group = beta, col = beta), se = FALSE)
  geom_line(data = pooledm1, aes(y = pooled_mean, x = article, col = beta, group = beta))
  



all3beta |> 
  mutate(Model = as.factor(Model),
         article = as.factor(article)) |>
  ggplot(aes(x = article, y = mean, col = Model)) +
  geom_hline(yintercept = 0, color = "grey") +
  geom_point(shape = 21, stroke = 1, position = position_dodge(0.9), size = 2) +
  geom_errorbar(aes(ymax = mean+sd, ymin = mean-sd), position = position_dodge(0.9), width = 0.4) +
  facet_wrap(~beta) +
  theme_classic()


all3beta |> 
  mutate(Model = as.factor(Model),
         article = as.factor(article)) |>
  ggplot(aes(x = article, y = mean, col = beta)) +
  geom_hline(yintercept = 0, color = "grey") +
  geom_point(shape = 21, stroke = 1, position = position_dodge(0.9), size = 2) +
  geom_line(aes(group = beta), position = position_dodge(0.9)) +
  geom_errorbar(aes(ymax = mean+sd, ymin = mean-sd), position = position_dodge(0.9), width = 0.4) +
  facet_wrap(~Model) +
  theme_classic()


all3beta |> 
  mutate(Model = as.factor(Model),
         article = as.factor(article)) |>
  ggplot(aes(x = article, y = mean, col = beta)) +
  geom_hline(yintercept = 0, color = "grey") +
  geom_point(shape = 21, stroke = 1, position = position_dodge(0.9), size = 3) +
  #geom_line(aes(group = article)) +
  geom_errorbar(aes(ymax = mean+sd, ymin = mean-sd), position = position_dodge(0.9), width = 0.4) +
  coord_flip() +
  facet_wrap(~Model) +
  scale_color_grey() +
  theme_classic()



all3beta |> mutate(Model = as.factor(Model),
                   article = as.factor(article)) |>
  ggplot(aes(x = mean, y = article, col = Model)) +
  geom_vline(xintercept = 0, color = "grey") +
  geom_point(shape = 21, stroke = 1, size = 2, position = position_dodge(0.9)) +
  #geom_line(aes(group = beta), position = position_dodge(0.9)) +
  geom_errorbar(aes(xmax = mean+sd, xmin = mean-sd), position = position_dodge(0.9), width = 0.4) +
  facet_wrap(~beta_n) +
  theme_classic()


scale_color_grey() +
  scale_fill_grey() 






all3theta = full_join(jagsf.theta, full_join(jagsf2.theta, jagsf3.theta))

all3theta |> filter(Model == 1) |>
  ggplot(aes(x = timepoint, y = mean)) +
  geom_point() + 
  geom_line(aes(group = article))


all3theta |> filter(Model == 1) |>
  group_by(timepoint, increasing) |>
  summarise(median = median(mean),
            low.b = quantile(mean, c(0.025)),
            upper.b = quantile(mean, c(0.975))) |>
  ggplot(aes(x = timepoint, y = median)) +
  geom_point() +
  geom_line() + 
  geom_errorbar(aes(ymin = median+low.b, ymax = median+upper.b)) +
  facet_wrap(~increasing)


all3theta |> 
  mutate(Model = as.factor(Model),
         article = as.factor(article),
         timepoint = as.factor(timepoint)) |>
  ggplot(aes(x = timepoint, y = mean)) +
  geom_hline(yintercept = 0, color = "grey", linewidth = 1) +
  geom_jitter(stroke = 1.5, size = 6, aes( col = Model, shape = Model), width = 0.2) +
  theme_classic() +
  geom_smooth(aes(group = Model, fill = Model, color = Model), method = "loess") +
  facet_wrap(~increasing) +
  labs(x = "Time-Point", y = "Estimated Mean Difference") +
  scale_color_viridis_d(option = "plasma", direction = 1, begin = 0.85, end = 0) +
  scale_fill_viridis_d(option = "plasma", direction = 1, begin = 0.85, end = 0) +
  scale_shape_manual(values = c(0, 1, 2)) +
  theme(
    legend.position = "top",
    legend.text = element_text(color = "black", family = "Avenir", size = 16),
    legend.title = element_text(color = "black", family = "Avenir", size = 17),
    strip.text = element_text(color = "black", family = "Avenir", size = 20),
    axis.title = element_text(color = "black", family = "Avenir", size = 24),
    axis.text = element_text(color = "black", family = "Avenir", size = 19)
    
  )

all3theta |> filter(Model == 3) |>
  rename("twofive" = '2.5%',
         "nineseven" = '97.5%') |>
  mutate(Model = as.factor(Model),
         article = as.factor(article),
         timepoint = as.factor(timepoint)) |>
  ggplot(aes(x = timepoint, y = mean)) +
  geom_hline(yintercept = 0, color = "grey", linewidth = 1) +
  geom_point(stroke = 1.5, size = 6, aes( col = article), shape = 21) +
  theme_classic() +
  geom_line(aes(group = article, color = article), linewidth = 1) +
  geom_ribbon(aes(ymin = mean-sd, ymax = mean+sd, fill = article, x = timepoint, group = article), alpha = 0.3) +
  #geom_ribbon(aes(ymin = mean-abs(twofive), ymax = mean+abs(nineseven), fill = article, x = timepoint, group = article), alpha = 0.3) +
  facet_wrap(~increasing) +
  labs(x = "Time-Point", y = "Estimated Mean Difference") +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") + 
  theme(
    legend.position = "top",
    legend.direction = "horizontal",
    legend.box = "horizontal",
    legend.text = element_text(color = "black", family = "Avenir", size = 16),
    legend.title = element_blank(),
    strip.text = element_text(color = "black", family = "Avenir", size = 20),
    axis.title = element_text(color = "black", family = "Avenir", size = 24),
    axis.text = element_text(color = "black", family = "Avenir", size = 19)
  )



all3theta |> 
  mutate(Model = as.factor(Model),
         article = as.factor(article),
         timepoint = as.factor(timepoint)) |>
  ggplot(aes(x = timepoint, y = mean)) +
  geom_hline(yintercept = 0, color = "grey", linewidth = 1) +
  geom_jitter(stroke = 1.5, size = 4, aes( col = Model, shape = Model), width = 0.3) +
  theme_classic() +
  geom_smooth(aes(group = Model, fill = Model, color = Model), method = "loess") +
  facet_wrap(vars(increasing, Model)) +
  labs(x = "Time-Point", y = "Estimated Mean Difference") +
  scale_color_viridis_d(option = "plasma", direction = 1, begin = 0.85, end = 0) +
  scale_fill_viridis_d(option = "plasma", direction = 1, begin = 0.85, end = 0) +
  scale_shape_manual(values = c(0, 1, 2)) +
  theme(
    legend.position = "top",
    legend.text = element_text(color = "black", family = "Avenir", size = 16),
    legend.title = element_text(color = "black", family = "Avenir", size = 17),
    strip.text = element_text(color = "black", family = "Avenir", size = 20),
    axis.title = element_text(color = "black", family = "Avenir", size = 24),
    axis.text = element_text(color = "black", family = "Avenir", size = 19)
    
  )


all3theta |> 
  mutate(Model = as.factor(Model),
         article = as.factor(article),
         timepoint = as.factor(timepoint)) |>
  ggplot(aes(x = timepoint, y = mean)) +
  geom_hline(yintercept = 0, color = "grey", linewidth = 1) +
  geom_jitter(stroke = 1.5, size = 6, aes( col = Model, shape = Model), width = 0.2) +
  theme_classic() +
  geom_smooth(aes(group = Model, fill = Model, color = Model), method = "loess") +
  labs(x = "Time-Point", y = "Estimated Mean Difference") +
  scale_color_viridis_d(option = "plasma", direction = 1, begin = 0.85, end = 0) +
  scale_fill_viridis_d(option = "plasma", direction = 1, begin = 0.85, end = 0) +
  scale_shape_manual(values = c(0, 1, 2)) +
  theme(
    legend.position = "top",
    legend.text = element_text(color = "black", family = "Avenir", size = 16),
    legend.title = element_text(color = "black", family = "Avenir", size = 17),
    strip.text = element_text(color = "black", family = "Avenir", size = 20),
    axis.title = element_text(color = "black", family = "Avenir", size = 24),
    axis.text = element_text(color = "black", family = "Avenir", size = 19)
    
  )






facet.names = c("1 (Increasing)", 
                   "2 (Steady)", 
                   "3 (Increasing)", 
                   "4 (Increasing)",
                   "5 (Steady)",
                   "6 (Steady)",
                  "7 (Steady)")
names(facet.names) = c(1, 2, 3, 4, 5, 6, 7)

all3theta |> 
  mutate(Model = as.factor(Model),
         article = as.factor(article),
         timepoint = as.factor(timepoint)) |>
  ggplot(aes(x = timepoint, y = mean)) +
  geom_hline(yintercept = 0, color = "grey", linewidth = 0.7) +
  geom_jitter(stroke = 1.5, size = 4, aes( col = Model, shape = Model), width = 0.3) +
  theme_classic() +
  geom_line(aes(group = Model, color = Model), linewidth = 0.8) +
  geom_ribbon(aes(ymin = mean-sd, ymax = mean+sd, fill = Model, group = Model), alpha = 0.35) + 
  facet_wrap(~article, labeller = labeller(article = facet.names)) +
  labs(x = "Time-Point", y = "Estimated Mean Difference") +
  scale_color_viridis_d(option = "plasma", direction = 1, begin = 0.85, end = 0) +
  scale_fill_viridis_d(option = "plasma", direction = 1, begin = 0.85, end = 0) +
  scale_shape_manual(values = c(0, 1, 2)) +
  theme(
    legend.position = "top",
    legend.text = element_text(color = "black", family = "Avenir", size = 16),
    legend.title = element_text(color = "black", family = "Avenir", size = 17),
    strip.text = element_text(color = "black", family = "Avenir", size = 20),
    axis.title = element_text(color = "black", family = "Avenir", size = 24),
    axis.text = element_text(color = "black", family = "Avenir", size = 19)
    
  )







all3theta |> group_by(Model, timepoint, increasing) |>
  summarise(minimum = min(mean),
            maximum = max(mean),
            median = median(mean)) |>
  kable()



