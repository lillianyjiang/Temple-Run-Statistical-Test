
library(dplyr)
library(ggplot2)
library(DescTools)
TempleRun <- read.csv("/Users/lillianjiang/Downloads/Temple run stats - Data.csv")

#turn these columns from the dataset into integers
CPU_C <- pull(TempleRun, CoinPU_Coins)
RPU_C <- pull(TempleRun, RunPU_Coins)

#Make a side by side boxplot comparing the two powerups 
Powerups <- data.frame(
  coins = c(RPU_C, CPU_C),
  type = c(
    rep("Run Powerup", length(RPU_C)),
    rep("Coin Powerup",length(CPU_C))
  )
)

boxplot_powerup<- ggplot(Powerups, aes(x = type, y = coins, color = type)) +
  geom_boxplot() +
  geom_jitter() +
  scale_color_brewer(type = "qual", palette = 2) +
  theme_minimal() +
  theme(legend.position = "none")+
  labs(title = "Boxplot of Coins collected by Powerup", x = "Powerup Type", y = "Coins")

print(boxplot_powerup)

#conduct statistical test 
delta_0 <- 0

sigma_sq_R <- var(TempleRun$RunPU_Coins)
sigma_sq_C <- var(TempleRun$CoinPU_Coins)

n_1 <- length(CPU_C)
n_2 <- length(RPU_C)

z_stat <- (mean(RPU_C) - mean(CPU_C) - delta_0) / 
  sqrt(sigma_sq_R / n_1 + sigma_sq_C / n_2)

z_stat

#print the conclusion of the data
inequality <- ifelse(z_stat <1.96, "less", "greater")
BLAH <- ifelse(inequality == "less", "fail to reject", "reject")
hmmm <- ifelse(BLAH == "fail to reject", "cannot", "there is sufficient evidence to")
sprintf("Since the z statistic is %s than 1.96, we %s the null hypothesis. We %s conclude that the mean number of coins collected during the run powerup is greater than the mean number of coins collected during the coin powerup", inequality, BLAH, hmmm)


