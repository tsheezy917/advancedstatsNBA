install.packages("readr")
install.packages("dplyr")
install.packages("readxl")
install.packages("ggplot2")
install.packages("purrr")

library(ggplot2)
library(readr)
library(dplyr)
library(readxl)
library(purrr)

player_stats2 <- read_csv("~/Downloads/player advanced stats - Sheet4.csv") %>% filter(GP > 52 & MIN > 24) %>% select(-X1)

team_stats <- read_csv("~/Downloads/player advanced stats - Sheet5.csv")
team_stats <- team_stats[-1,-1]
team_cat <- names(team_stats)
team_cat <- paste("T", team_cat, sep = "_")
team_cat[[1]] <- "TEAM"
names(team_stats) <- team_cat
team_stats2 <- tbl_df(map(team_stats[,2:8], as.numeric))
team_stats1 <- select(team_stats, TEAM, T_PIE)

top3 <- cbind(team_stats1, team_stats2) %>% left_join(player_stats2, by = "TEAM") %>% 
tbl_df %>% group_by(TEAM) %>% arrange(desc(NETRTG)) %>% select(TEAM, PLAYER, GP:W, OFFRTG:NETRTG, PIE, T_PIE:T_W, T_OFFRTG:T_NETRTG) %>% 
top_n(3, wt = NETRTG) %>% top_n(3, wt = OFFRTG) 

playoffs <- c("GSW", "UTA", "SAS", "LAC", "HOU", "POR", "OKC", "MEM", "BOS", "CLE", "TOR", "CHI", "IND", "ATL", "MIL", "WAS")

top3 <- top3 %>% mutate(PIEr = round(PIE/T_PIE, 3),  NET_ABV = round((NETRTG - T_NETRTG)/NETRTG, 3), POT = ifelse(TEAM %in% playoffs, 1, 0), 
                        PIE_index = round((PIE * PIEr * ((T_W/T_GP)/.82) * (W/GP))/4.9, 3)) %>% select(TEAM:PIE_index) %>% arrange(PLAYER)

top3 <- top3 %>% mutate(POT = factor(POT, levels = c(1, 0), labels = c("Playoff", "Non-Playoff")))

#examine the spread of teams above and below certain win totals.  
top3 %>% mutate(abv500 = ifelse(T_W > 41, 1, 0)) %>% group_by(abv500) %>% summarize(N_ABV = mean(NET_ABV), spread = sd(NET_ABV))
top3 %>% mutate(abv500 = ifelse(T_W > 50, 1, 0)) %>% group_by(abv500) %>% summarize(N_ABV = mean(NET_ABV), spread = sd(NET_ABV))
top3 %>% mutate(abv500 = ifelse(T_W > 54, 1, 0)) %>% group_by(abv500) %>% summarize(N_ABV = mean(NET_ABV), spread = sd(NET_ABV))


#random plots
ggplot(top3, aes(NET_ABV, T_W/T_GP, col = factor(POT), size = T_NETRTG, alpha = .5)) + geom_point()
ggplot(top3, aes(T_PIE, T_W, col = factor(POT), size = T_NETRTG, alpha = .5)) + geom_point()
ggplot(top3, aes(PIE, NETRTG, col = factor(POT), size = T_NETRTG, alpha = .5)) + geom_point()
ggplot(top3_index, aes(ave_NRTG, Win_perc, col = POT, size = ave_PIE, alpha = .5)) + geom_point()
ggplot(top3_index, aes(PIE, Win_perc, col = POT, size = ave_PIE, alpha = .5)) + geom_point()
ggplot(top3_index, aes(PIE_gap, Win_perc, col = POT, size = ave_PIE, alpha = .5)) + geom_point()
ggplot(top3_index, aes(hi_impact, Win_perc, col = POT, size = ave_NRTG, alpha = .5)) + geom_point()

