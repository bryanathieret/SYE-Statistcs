

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

increasing = c(0, 1, 1, 0, 0, 0, 1)
steady = c(1, 0, 0, 1, 1, 1, 0)

################################################################################################################


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

plot(jagsmod7)
denplot(jagsmod7, parms = c("beta1"))
denplot(jagsmod7, parms = c("beta2"))
denplot(jagsmod7, parms = c("beta3"))
denplot(jagsmod7, parms = c("beta4"))
traplot(jagsmod7, parms = c("beta1"))
traplot(jagsmod7, parms = c("beta2"))
traplot(jagsmod7, parms = c("beta3"))
traplot(jagsmod7, parms = c("beta4"))


posterior_sample <- coda.samples(jagsmod7$model,
                                 variable.names = c("beta1", "beta2", "beta3", "beta4"),
                                 n.iter = 10000,
                                 n.chains = 5)
summary(posterior_sample)
plot(posterior_sample)





jags7 = tibble(as.data.frame(as.matrix(jagsmod7$BUGSoutput$summary)))

jags7.beta = jags7 |> slice(1:28) 
jags7.theta = jags7 |> slice(63:94)
jags7.theta = jags7.theta |> mutate(timepoint = timepoint$timepoint,
                                    article = articleTimepoint$article) |> mutate(increasing = case_when(
                                      article == 1 ~ "Steady",
                                      article == 2 ~ "Increasing",
                                      article == 3 ~ "Increasing", 
                                      article == 4 ~ "Steady",
                                      article == 5 ~ "Steady", 
                                      article == 6 ~ "Steady",
                                      article == 7 ~ "Increasing"
                                    )) |> mutate(increasing = as.factor(increasing),
                                                 article = as.factor(article))

  # beta estimates: 

  
  b1 = jags7.beta |> slice(1:7) |> mutate(timepoint = c(1:7)) |> mutate(concentration = "All")
  b1 |> ggplot(aes(x = timepoint, y = mean)) + 
    geom_point() +
    geom_line() +
    geom_ribbon(aes(ymax = mean+sd, ymin = mean-sd), alpha = 0.3)
  
  b2 = jags7.beta |> slice(8:14) |> mutate(timepoint = c(1:7),
                                           concentration = "two")
  b2 |> ggplot(aes(x = timepoint, y = mean)) + 
    geom_point() +
    geom_line() +
    geom_ribbon(aes(ymax = mean+sd, ymin = mean-sd), alpha = 0.3)
  
  b3 = jags7.beta |> slice(15:21) |> mutate(timepoint = c(1:7), 
                                            concentration = "increasing")
  
  b4 = jags7.beta |> slice(22:28) |> mutate(timepoint = c(1:7),
                                            concentration = "steady")
  
  b3b4 = full_join(b3, b4)
  
  b3b4 |> ggplot(aes(x = timepoint, y = mean)) + 
    geom_point(aes(col = concentration)) +
    geom_line(aes(col = concentration)) 
    #geom_ribbon(aes(ymax = mean+sd, ymin = mean-sd), alpha = 0.3)
  
  b3b4 |> ggplot(aes(x = timepoint, y = mean)) + 
    geom_point(aes(col = concentration), size = 3) +
    geom_smooth(aes(col = concentration), se = FALSE) 
  
  b3b4 |> ggplot(aes(x = timepoint, y = mean)) + 
    geom_point(aes(col = concentration), size = 3) +
    geom_smooth(method = "lm", aes(col = concentration), se = FALSE) 
  
  betas = full_join(b3b4, b1) |> rename("upper" = "97.5%",
                                        "lower" = "2.5%")
  betas |> 
    ggplot(aes(x = timepoint, y = mean, group = concentration)) +
    geom_point(aes(col = concentration)) + 
    geom_line(aes(col = concentration)) +
    geom_ribbon(aes(ymax = upper, ymin = lower, fill = concentration), alpha = 0.3) + 
    facet_wrap(~concentration)
  

  # thetas: 
  
  jags7.theta |> ggplot(aes(x = timepoint, y = mean)) +
    geom_point(aes(col = increasing)) + 
    geom_line(aes(col = increasing, group = article)) +
    geom_ribbon(aes(ymax = mean+sd, ymin = mean-sd, group = article), alpha = 0.3) +
    facet_wrap(~article)
  
  jags7.theta |> ggplot(aes(x = timepoint, y = mean)) +
    geom_point(aes(col = increasing)) + 
    geom_line(aes(col = increasing, group = article)) +
    theme_minimal()
  
  jags7.theta |> ggplot(aes(x = timepoint, y = mean)) +
    geom_point(aes(col = increasing)) + 
    geom_line(aes(col = increasing, group = article)) +
    theme_minimal() +
    facet_wrap(~increasing)
  
  jags7.theta |> ggplot(aes(x = timepoint, y = mean)) +
    geom_point(aes(col = increasing)) + 
    geom_smooth(aes(col = increasing, group = article)) +
    theme_minimal()
  
  jags7.theta |> ggplot(aes(x = timepoint, y = mean)) +
    geom_point(aes(col = increasing)) + 
    geom_smooth(aes(col = increasing, group = article)) +
    theme_minimal() + 
    facet_wrap(~increasing)
  
  jags7.theta |> ggplot(aes(x = timepoint, y = mean)) +
    geom_point(aes(col = increasing, size = sd)) + 
    geom_smooth(aes(col = increasing, group = article)) +
    theme_minimal() +
    facet_wrap(~increasing)
  
  jags7.theta |> ggplot(aes(x = timepoint, y = mean)) +
    geom_point(aes(col = increasing)) + 
    geom_smooth(aes(col = increasing, group = article), method = "lm", se = FALSE) +
    theme_minimal() +
    facet_wrap(~increasing)
  
  # article 2 is an outlier (O2011)
  jags7.theta |> ggplot(aes(x = timepoint, y = mean)) +
    geom_point(aes(col = article, size = sd)) + 
    geom_smooth(aes(col = article, group = article), method = "lm", se = FALSE) +
    theme_minimal() +
    facet_wrap(~increasing)
  
  
  jags7.theta |> ggplot(aes(x = timepoint, y = mean)) +
    geom_jitter(aes(col = increasing), width = 0.1) + 
    geom_smooth(aes(col = increasing, group = increasing), se = FALSE) 
    
  
  jags7.theta |> ggplot(aes(x = timepoint, y = mean)) +
    geom_jitter(aes(col = increasing), width = 0.1, alpha = 0.3) + 
    geom_line(aes(group = article, col = increasing), alpha = 0.3, position = position_dodge(0.1), linewidth = 1) +
    geom_smooth(aes(col = increasing, group = increasing), se = FALSE) +
    theme_minimal()
  
  jags7.theta |> ggplot(aes(x = timepoint, y = mean)) +
    geom_jitter(aes(col = increasing), width = 0.1, alpha = 0.3) + 
    geom_line(aes(group = article, col = increasing), alpha = 0.3, position = position_dodge(0.1), linewidth = 1) +
    geom_smooth(aes(col = increasing, group = increasing), se = FALSE, method = "lm") +
    theme_minimal()
    
  
  jags7.theta |> group_by(timepoint) |> summarise(mean = mean(mean), 
                                                  sd = mean(sd)) |>
    ggplot(aes(y = mean, x = timepoint)) + 
    geom_point() +
    geom_line() +
    geom_ribbon(aes(ymax= mean+sd, ymin = mean-sd), alpha = 0.3)
  
  
  
  jags7.theta |> group_by(timepoint, increasing) |> summarise(mean = mean(mean), 
                                                  sd = mean(sd)) |>
    ggplot(aes(y = mean, x = timepoint, group = increasing)) + 
    geom_point(aes(col = increasing)) +
    geom_line(aes(col = increasing)) +
    geom_ribbon(aes(ymax= mean+sd, ymin = mean-sd, group = increasing), alpha = 0.3)
  
  
  jags7.theta |> group_by(timepoint, increasing) |> summarise(mean = mean(mean), 
                                                              sd = mean(sd)) |>
    ggplot(aes(y = mean, x = timepoint, group = increasing)) + 
    geom_point(aes(col = increasing)) +
    geom_smooth(aes(col = increasing), method = "loess", se = FALSE) +
    geom_ribbon(aes(ymax= mean+sd, ymin = mean-sd, group = increasing), alpha = 0.3)

  
  jags7.theta |> group_by(timepoint, increasing) |> summarise(mean = mean(mean), 
                                                              sd = mean(sd)) |>
    ggplot(aes(y = mean, x = timepoint, group = increasing)) + 
    geom_point(aes(col = increasing)) +
    geom_smooth(aes(col = increasing), method = "lm", se = FALSE) 
    #geom_ribbon(aes(ymax= mean+sd, ymin = mean-sd, group = increasing), alpha = 0.3)
  
  jags7.theta |> ggplot(aes(x = mean, col = article, y = timepoint)) + 
    geom_boxplot(aes(group = timepoint)) +
    geom_jitter(size = 3, width = 0.00000001) 
  
  jags7.theta |> ggplot(aes(x = mean, col = article, y = timepoint)) + 
    geom_violin(aes(group = timepoint)) +
    geom_jitter(size = 3, width = 0.00000001) 
  

  
  
  
  
  
  jags7.theta |> ggplot(aes(x = mean, col = article, fill = article)) + geom_density(alpha = 0.3)
  jags7.theta |> ggplot(aes(x = mean, col = increasing, fill = increasing)) + geom_density(alpha = 0.3)
  jags7.theta |> ggplot(aes(x = mean, col = increasing, fill = increasing)) + geom_density(alpha = 0.3) + facet_wrap(~increasing)
  jags7.theta |> ggplot(aes(x = mean, col = increasing, fill = increasing)) + geom_histogram(alpha = 0.3) + facet_wrap(~increasing)
  jags7.theta |> ggplot(aes(x = mean, col = article, fill = article)) + geom_density(alpha = 0.3) + facet_wrap(~increasing)
  jags7.theta |> ggplot(aes(x = mean, col = article, fill = article)) + geom_density(alpha = 0.3) + facet_wrap(~article)
  jags7.theta |> ggplot(aes(x = mean, col = article, fill = article)) + geom_histogram(alpha = 0.3) + facet_wrap(~article)
  jags7.theta |> ggplot(aes(x = mean, col = increasing, fill = increasing)) + geom_density(alpha = 0.3) + facet_wrap(~timepoint)
  jags7.theta |> ggplot(aes(x = mean, col = increasing, fill = increasing)) + geom_histogram(alpha = 0.3) + facet_wrap(~timepoint)
  

  
#############################################################
# Good graphs 
    
  
  # article   `1`   `2`   `3`   `4`   `5`   `6`
  # <chr>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
  # 1 L2017       1     2     3     4     5     6
  # 2 O2011       3     4     5     6     7    NA
  # 3 P2003       1     2     3     4    NA    NA
  # 4 G2007       2     4     6    NA    NA    NA
  # 5 GN2006      2     4     6    NA    NA    NA
  # 6 D11         1     2     3     4     5    NA
  # 7 C2015       1     2     3     4     5     6
  
  
    
    jags7.theta |> ggplot(aes(x = mean, y = article, col = increasing)) + 
    geom_vline(xintercept = 0, color = "grey") +
    geom_point(size = 4) + 
    geom_errorbar(aes(xmax = mean+sd, xmin = mean-sd), width = 0, color = "black", linewidth = 0.8) +
    facet_wrap(~timepoint) +
    labs(y = "Article", x = "Estimated Mean Difference") +
    theme_classic() +
    theme(
      axis.text = element_text(color = "black", size = 18, family = "Avenir"),
      axis.title = element_text(color = "black", family = "Avenir", size = 22),
      strip.text = element_text(color = "black", family = "Avenir", size = 16),
      legend.position = "top",
      legend.title = element_blank(),
      legend.text = element_text(color = "black", size = 16, family = "Avenir")
      
    ) +
    scale_color_manual(values = c("#832681", "#e75263"))
  
  
  
  jags7.theta = jags7.theta |> rename("upper" = "97.5%",
                                      "lower" = "2.5%")
  jags7.theta |> ggplot(aes(x = timepoint, y = mean)) +
    geom_hline(yintercept = 0, color = "grey") +
    geom_point(aes(col = increasing)) + 
    geom_line(aes(col = increasing, group = article)) +
    geom_ribbon(aes(ymax = mean+sd, ymin = mean-sd, group = article, fill = increasing), alpha = 0.3) +
    facet_wrap(~article) +
    theme_classic() +
    labs(y = "Estimated Mean Difference", x = "Time-Point") +
    theme(
      axis.text = element_text(color = "black", size = 18, family = "Avenir"),
      axis.title = element_text(color = "black", family = "Avenir", size = 22),
      strip.text = element_text(color = "black", family = "Avenir", size = 16),
      legend.position = "top",
      legend.title = element_blank(),
      legend.text = element_text(color = "black", size = 16, family = "Avenir")
      
    ) +
    scale_color_manual(values = c("#832681", "#e75263")) +
    scale_fill_manual(values = c("#832681", "#e75263"))
  
  
  
  jags7.theta |> ggplot(aes(y = mean, x = timepoint, group = article)) + 
    geom_hline(yintercept = 0, color = "grey", linewidth = 1) +
    geom_point(size = 3, aes(col = increasing), shape = 21, stroke = 1) + 
    geom_line(aes(col = increasing), linewidth = 0.8) +
    #geom_smooth(aes(group = increasing, col = increasing), se = FALSE) +
    scale_color_manual(values = c("#832681", "#e75263")) +
    theme_classic() + facet_wrap(~increasing) +
    geom_ribbon(aes(ymax = mean+sd, ymin = mean-sd, fill = increasing, group = article), alpha = 0.3) +
    scale_fill_manual(values = c("#832681", "#e75263")) +
    labs(y = "Estimated Mean Difference", x = "Time-Point") +
    theme(
      axis.text = element_text(color = "black", size = 18, family = "Avenir"),
      axis.title = element_text(color = "black", family = "Avenir", size = 22),
      strip.text = element_text(color = "black", family = "Avenir", size = 16),
      legend.position = "top",
      legend.title = element_blank(),
      legend.text = element_text(color = "black", size = 16, family = "Avenir")
      
    )
  
  jags7.theta |> mutate(timepoint = as.factor(timepoint)) |>
    ggplot(aes(x = timepoint, y = mean, group = increasing)) +
    geom_hline(yintercept = 0, color = "grey", linewidth = 1) +
    geom_smooth(method = "loess", aes(fill = increasing, col = increasing), linewidth = 1.2) +
    geom_jitter(aes(group = article, col = increasing, shape = increasing), size = 5, stroke = 1, width = 0.15) +
    theme_classic() +
    scale_shape_manual(values = c(0, 5)) +
    scale_color_manual(values = c("#832681", "#e75263")) +
    scale_fill_manual(values = c("#832681", "#e75263")) +
    labs(y = "Estimated Mean Difference", x = "Time-Point") +
    theme(
      axis.text = element_text(color = "black", size = 18, family = "Avenir"),
      axis.title = element_text(color = "black", family = "Avenir", size = 22),
      strip.text = element_text(color = "black", family = "Avenir", size = 16),
      legend.position = "top",
      legend.title = element_blank(),
      legend.text = element_text(color = "black", size = 16, family = "Avenir"))
  
  
  
  allBetas = full_join(b3b4, full_join(b1, b2))
  
  allBetas |> mutate(concentration = case_when(
    concentration == "All" ~ !! paste0("\u03B2", "\u2081", " (time-point)"),
    concentration == "two" ~ !! paste0("\u03B2", "\u2082", " (time-point)"),
    concentration == "increasing" ~ !! paste0("\u03B2", "\u2083", " (increasing)"),
    concentration == "steady" ~ !! paste0("\u03B2", "\u2084", " (steady)")
  )) |> mutate(timepoint = as.factor(timepoint)) |>
    ggplot(aes(x = timepoint, y = mean, group = concentration)) +
    geom_hline(yintercept = 0, color = "grey", linewidth = 1) +
    geom_point(aes(col = concentration), shape = 21, size = 4, stroke = 1) + 
    geom_smooth(aes(col = concentration, fill = concentration), method = "loess") +
    #geom_ribbon(aes(ymax = upper, ymin = lower, fill = concentration), alpha = 0.3) + 
    facet_wrap(~concentration, scales = "free") + 
    theme_classic() +
    scale_color_brewer(palette = "Dark2") + 
    scale_fill_brewer(palette = "Dark2") +
    #scale_color_manual(values = c("#fde725", "#35b779", "#31688e", "#440154")) +
    #cale_fill_manual(values = c("#fde725", "#35b779", "#31688e", "#440154")) +
    labs(y = "Estimated Mean Difference", x = "Time-Point") +
    theme(
      axis.text = element_text(color = "black", size = 18, family = "Avenir"),
      axis.title = element_text(color = "black", family = "Avenir", size = 22),
      strip.text = element_text(color = "black", family = "Avenir", size = 16),
      legend.position = "top",
      legend.title = element_blank(),
      legend.text = element_text(color = "black", size = 12, family = "Avenir"))
  

  

    


