# Tanay Mukherjee
# Dec 1, 2019

# Basketball League


#1
rnorm_round <- function(n, mean, sd) {
  return(round(rnorm(n, mean=mean,sd=sd)))
}
rnorm_round(100,500,80)

#2
BL <- read.csv("C:\\Users\\its_t\\Documents\\CUNY Fall 2019\\9750 - Software Tools and Techniques_Data Science\\HW3\\Basketball League.csv")
BL

nsim = 1e6

for (i in nsim) {
  BL_simualtion <- function(h,a) {
    
    # Argument h denotes home and argument a denotes away matches
    home <- filter(BL, Team == h)
    away <- filter(BL, Team == a)
    
    # Given that the expctation can be defined as (0.6*A1+0.4*D1)-(0.4*A2+0.6*D2)+H1
    expectation <- (0.6*home$A + 0.4*home$D) - (0.4*away$A + 0.6*away$D) + home$H
    expectation
    # Gievn that standard deviation is 1/R1+1/R2
    sd <- ((1/home$R) + (1/away$R))
    
    spread <- 0
    
    # Initiating spread for 0 in the series to initiate simulation
    while (spread == 0){
      spread <- rnorm_round(1, expectation, sd)
    }
    
    # return(c(ifelse(spread > 0, h, a), spread))
    return(c(ifelse(spread > 0, paste("Winner is", h,"and loser is",a) , paste("Winner is", a,"and loser is",h)), paste("and the points spread is", spread)))
  }
  
}
BL_simualtion("Jersey Lions", "Manhattan Ducks")


# 3


library(gtools)

BL <- read.csv("C:\\Users\\its_t\\Documents\\CUNY Fall 2019\\9750 - Software Tools and Techniques_Data Science\\HW3\\Basketball League.csv")
BL

teams_groupings <- as.data.frame(permutations(8, 2, as.vector(BL$Team), repeats.allowed = FALSE)) 
teams_groupings <- teams_groupings[rep(row.names(teams_groupings), 2), 1:2]
rownames(teams_groupings) <- 1:nrow(teams_groupings)

colnames(teams_groupings) <- c("h","a")
teams_groupings %>% data.table()

i <- sapply(teams_groupings, is.factor)
teams_groupings[i] <- lapply(teams_groupings[i], as.character)

str(teams_groupings)
class(teams_groupings)

dummy <- data.frame(Wins=numeric(8), Losses=numeric(8), Spread=numeric(8), Ranking = numeric(8), Season_top =numeric(8))
BL <- cbind(BL,dummy)

BL <- BL %>% mutate(Seq = c(1,2,3,4,5,6,7,8))
temp <- BL[,-1]
rownames(BL) <- temp[,1]
rownames(BL) <- BL[,1]
# BL[,1] <- NULL

i <- sapply(BL, is.factor)
BL[i] <- lapply(BL[i], as.character)

str(BL)
class(BL)


duplicated <- BL
results <- BL

h <- 0
a <- 0




nsim = 5000
temp <- 0

for(i in 1:nsim){
  BL <- duplicated
  for (j in 1:112){
    
    h <- teams_groupings[j,1]
    a <- teams_groupings[j,2]
    
    # Argument h denotes home and argument a denotes away team
    home <- BL[h,]
    away <- BL[a,]
    
    # Given that the expctation can be defined as (0.6*A1+0.4*D1)-(0.4*A2+0.6*D2)+H1
    expectation <- (0.6*home$A + 0.4*home$D) - (0.4*away$A + 0.6*away$D) + home$H
    
    # Gievn that standard deviation is 1/R1+1/R2
    sd <- ((1/home$R) + (1/away$R))
    
    temp <- temp + 1
    spread <- 0
    
    while (spread == 0) {
      spread <- rnorm_round(1, expectation, sd)
    }
    
    if(spread > 0){
      BL[h,6] <- BL[h,6] + 1
      BL[h,8] <- BL[h,8] + spread
      BL[a,7] <- BL[a,7] + 1
    }else{
      BL[a,6] <- BL[a,6] + 1
      BL[a,8] <- BL[a,8] + abs(spread)
      BL[h,7] <- BL[h,7] + 1
      
    }
    
  }
  
  BL <- BL %>% arrange(desc(Wins), desc(Spread), Team)
  
  BL[1,10] = BL[1,10] +1
  BL[,9] = c(1,2,3,4,5,6,7,8)
  
  BL <- BL %>% arrange(Seq)
  results$Spread <- results$Spread + BL$Spread
  results$Ranking <- results$Ranking + BL$Ranking
  results$Season_top <- results$Season_top + BL$Season_top
  
}
results <- results %>% mutate(Win_percent = (results$Season_top/nsim)*100)
results <- results %>% mutate(Avg_rank = results$Ranking/nsim)



results  %>% ggplot(aes(x= reorder(Team, Win_percent*100), y = Win_percent, fill=Team)) +
  geom_bar(position = "dodge", stat = "identity", width = .4) +
  coord_flip() + scale_color_fivethirtyeight() + theme_fivethirtyeight() +
  ggtitle("All participating teams by Win Percentage")+ 
  theme(plot.title = element_text(hjust = 0.5))  +
  theme(legend.position = "none") +
  guides(fill = guide_legend(reverse=TRUE)) +
  scale_y_continuous(breaks = seq(0, 100, by = 10),labels = function(x) paste0(x,"%")) +
  geom_text(aes(label = sprintf("%0.2f", round(Win_percent, digits = 2)), Win_percent = Win_percent + 0.05), position = position_dodge(0.9),hjust = 0)

results  %>% ggplot(aes(x= reorder(Team, Avg_rank), y = Avg_rank, fill=Team)) +
  geom_col()+ coord_flip() + scale_color_fivethirtyeight() + theme_fivethirtyeight() +
  ggtitle("All participating teams by Average Rank")+ 
  theme(plot.title = element_text(hjust = 0.5))  +
  theme(legend.position = "none") +
  geom_text(aes(label = paste(round(Avg_rank,2)), hjust = 0))



# 4

Tourney <- results %>% arrange(desc(Avg_rank))
Tourney$Seq <- c(1,2,3,4,5,6,7,8)
Tourney$Wins <- c(0,0,0,0,0,0,0,0)
Tourney$Losses  <- c(0,0,0,0,0,0,0,0)
Tourney$Spread <- c(0,0,0,0,0,0,0,0)
Tourney$Season_top <- c(0,0,0,0,0,0,0,0)
Tourney$Ranking <- c(0,0,0,0,0,0,0,0)
Tourney$Win_percent <- c(0,0,0,0,0,0,0,0)
Tourney$Avg_rank <- c(0,0,0,0,0,0,0,0)

temp <- Tourney[,-1]
rownames(Tourney) <- temp[,1]
rownames(Tourney) <- Tourney[,1]

dummy <- Tourney
duplicated <- Tourney
T_results <- Tourney
semi1 <- Tourney
semi2 <- Tourney
final <- Tourney
check <- Tourney

nsim = 5000

for(i in 1:nsim){
  
  # For 1 vs 8
  x <- 1
  y <- 8
  for (j in 1:3) {
    
    # Argument h denotes home and argument a denotes away team
    home <- Tourney[x,]
    away <- Tourney[y,]
    
    # Given that the expctation can be defined as (0.6*A1+0.4*D1)-(0.4*A2+0.6*D2)+H1
    expectation <- (0.6*home$A + 0.4*home$D) - (0.4*away$A + 0.6*away$D) + home$H
    
    # Gievn that standard deviation is 1/R1+1/R2
    sd <- ((1/home$R) + (1/away$R))
    
    
    spread <- 0
    
    while (spread == 0) {
      spread <- rnorm_round(1, expectation, sd)
    }
    
    if(!(Tourney[x,6] == 2 & Tourney[y,6] == 2)) {
      if(spread > 0){
        Tourney[x,6] <- Tourney[x,6] + 1
        Tourney[x,8] <- Tourney[x,8] + spread
        Tourney[y,7] <- Tourney[y,7] + 1
      }else{
        Tourney[y,6] <- Tourney[y,6] + 1
        Tourney[y,8] <- Tourney[y,8] + abs(spread)
        Tourney[x,7] <- Tourney[x,7] + 1
      }
      z <- 0
      z <- x
      x <- y
      y <- z
      
    }else {
      {break}
    }
  }
  
  # For 2 vs 7
  x <- 2
  y <- 7
  
  
  for (j in 1:3) {
    
    # Argument h denotes home and argument a denotes away team
    home <- Tourney[x,]
    away <- Tourney[y,]
    
    # Given that the expctation can be defined as (0.6*A1+0.4*D1)-(0.4*A2+0.6*D2)+H1
    expectation <- (0.6*home$A + 0.4*home$D) - (0.4*away$A + 0.6*away$D) + home$H
    
    # Gievn that standard deviation is 1/R1+1/R2
    sd <- ((1/home$R) + (1/away$R))
    
    
    spread <- 0
    
    while (spread == 0) {
      spread <- rnorm_round(1, expectation, sd)
    }
    
    if(!(Tourney[x,6] == 2 & Tourney[y,6] == 2)) {
      if(spread > 0){
        Tourney[x,6] <- Tourney[x,6] + 1
        Tourney[x,8] <- Tourney[x,8] + spread
        Tourney[y,7] <- Tourney[y,7] + 1
      }else{
        Tourney[y,6] <- Tourney[y,6] + 1
        Tourney[y,8] <- Tourney[y,8] + abs(spread)
        Tourney[x,7] <- Tourney[x,7] + 1
      }
      z <- 0
      z <- x
      x <- y
      y <- z
      
    }else {
      {break}
    }
  }
  
  # For 3 vs 6
  x <- 3
  y <- 6
  
  for (j in 1:3) {
    
    # Argument h denotes home and argument a denotes away team
    home <- Tourney[x,]
    away <- Tourney[y,]
    
    # Given that the expctation can be defined as (0.6*A1+0.4*D1)-(0.4*A2+0.6*D2)+H1
    expectation <- (0.6*home$A + 0.4*home$D) - (0.4*away$A + 0.6*away$D) + home$H
    
    # Gievn that standard deviation is 1/R1+1/R2
    sd <- ((1/home$R) + (1/away$R))
    
    
    spread <- 0
    
    while (spread == 0) {
      spread <- rnorm_round(1, expectation, sd)
    }
    
    if(!(Tourney[x,6] == 2 & Tourney[y,6] == 2)) {
      if(spread > 0){
        Tourney[x,6] <- Tourney[x,6] + 1
        Tourney[x,8] <- Tourney[x,8] + spread
        Tourney[y,7] <- Tourney[y,7] + 1
      }else{
        Tourney[y,6] <- Tourney[y,6] + 1
        Tourney[y,8] <- Tourney[y,8] + abs(spread)
        Tourney[x,7] <- Tourney[x,7] + 1
      }
      z <- 0
      z <- x
      x <- y
      y <- z
      
    }else {
      {break}
    }
  }
  
  # For 4 vs 5
  x <- 4
  y <- 5
  
  for (j in 1:3) {
    
    # Argument h denotes home and argument a denotes away team
    home <- Tourney[x,]
    away <- Tourney[y,]
    
    # Given that the expctation can be defined as (0.6*A1+0.4*D1)-(0.4*A2+0.6*D2)+H1
    expectation <- (0.6*home$A + 0.4*home$D) - (0.4*away$A + 0.6*away$D) + home$H
    
    # Gievn that standard deviation is 1/R1+1/R2
    sd <- ((1/home$R) + (1/away$R))
    
    
    spread <- 0
    
    while (spread == 0) {
      spread <- rnorm_round(1, expectation, sd)
    }
    
    if(!(Tourney[x,6] == 2 & Tourney[y,6] == 2)) {
      if(spread > 0){
        Tourney[x,6] <- Tourney[x,6] + 1
        Tourney[x,8] <- Tourney[x,8] + spread
        Tourney[y,7] <- Tourney[y,7] + 1
      }else{
        Tourney[y,6] <- Tourney[y,6] + 1
        Tourney[y,8] <- Tourney[y,8] + abs(spread)
        Tourney[x,7] <- Tourney[x,7] + 1
      }
      z <- 0
      z <- x
      x <- y
      y <- z
      
    }else {
      {break}
    }
  }
  
  
  Tourney <- Tourney %>% arrange(desc(Wins))
  Tourney$Season_top <- c(1,1,1,1,0,0,0,0)
  Tourney <- Tourney %>% arrange(Seq)
  T_results$Wins <- T_results$Wins + Tourney$Wins
  dummy <- Tourney
  
  
  # 1 and 8 winner vs 4 and 5 winner
  if (Tourney[1,10] > Tourney[8,10]) {
    x <- 1
  }else {x <- 8}
  
  
  if (Tourney[4,10] > Tourney[5,10]) {
    y <- 4
  }else {y <- 5}
  
  if(x < y){
    x <- 1
  }else{
    z <- 0
    z <- y
    y <- x
    x <- z
  }
  
  Tourney <- duplicated
  
  for (j in 1:3) {
    
    # Argument h denotes home and argument a denotes away team
    home <- Tourney[x,]
    away <- Tourney[y,]
    
    # Given that the expctation can be defined as (0.6*A1+0.4*D1)-(0.4*A2+0.6*D2)+H1
    expectation <- (0.6*home$A + 0.4*home$D) - (0.4*away$A + 0.6*away$D) + home$H
    
    # Gievn that standard deviation is 1/R1+1/R2
    sd <- ((1/home$R) + (1/away$R))
    
    
    spread <- 0
    
    while (spread == 0) {
      spread <- rnorm_round(1, expectation, sd)
    }
    
    if(!(Tourney[x,6] == 2 & Tourney[y,6] == 2)) {
      if(spread > 0){
        Tourney[x,6] <- Tourney[x,6] + 1
        Tourney[x,8] <- Tourney[x,8] + spread
        Tourney[y,7] <- Tourney[y,7] + 1
      }else{
        Tourney[y,6] <- Tourney[y,6] + 1
        Tourney[y,8] <- Tourney[y,8] + abs(spread)
        Tourney[x,7] <- Tourney[x,7] + 1
      }
      z <- 0
      z <- x
      x <- y
      y <- z
      
    }else {
      {break}
    }
  }
  
  semi1 <- Tourney
  Tourney <- dummy
  
  # 2 and 7 winner vs 3 and 6 winner
  if (Tourney[2,10] > Tourney[7,10]) {
    x <- 2
  }else {x <- 7}
  
  
  if (Tourney[3,10] > Tourney[6,10]) {
    y <- 3
  }else {y <- 6}
  
  if(x < y){
    x <- 2
  }else{
    z <- 0
    z <- y
    y <- x
    x <- z
  }
  
  Tourney <- duplicated
  
  for (j in 1:3) {
    
    # Argument h denotes home and argument a denotes away team
    home <- Tourney[x,]
    away <- Tourney[y,]
    
    # Given that the expctation can be defined as (0.6*A1+0.4*D1)-(0.4*A2+0.6*D2)+H1
    expectation <- (0.6*home$A + 0.4*home$D) - (0.4*away$A + 0.6*away$D) + home$H
    
    # Gievn that standard deviation is 1/R1+1/R2
    sd <- ((1/home$R) + (1/away$R))
    
    
    spread <- 0
    
    while (spread == 0) {
      spread <- rnorm_round(1, expectation, sd)
    }
    
    if(!(Tourney[x,6] == 2 & Tourney[y,6] == 2)) {
      if(spread > 0){
        Tourney[x,6] <- Tourney[x,6] + 1
        Tourney[x,8] <- Tourney[x,8] + spread
        Tourney[y,7] <- Tourney[y,7] + 1
      }else{
        Tourney[y,6] <- Tourney[y,6] + 1
        Tourney[y,8] <- Tourney[y,8] + abs(spread)
        Tourney[x,7] <- Tourney[x,7] + 1
      }
      z <- 0
      z <- x
      x <- y
      y <- z
      
    }else {
      {break}
    }
  }
  
  semi2 <- Tourney
  
  semi1 <- semi1 %>% arrange(desc(Wins))
  semi1$Season_top <- c(1,0,0,0,0,0,0,0)
  x <- semi1[1,11]
  semi1 <- semi1 %>% arrange(Seq)
  
  semi2 <- semi2 %>% arrange(desc(Wins))
  semi2$Season_top <- c(1,0,0,0,0,0,0,0)
  y <- semi2[1,11]
  semi2 <- semi2 %>% arrange(Seq)
  
  T_results$Wins <- T_results$Wins + semi1$Wins + semi2$Wins
  
  Tourney <- duplicated
  
  
  # The grand final
  if (x > y) {
    z <- 0
    z <- y
    y <- x
    x <- z
  }else {
    x <- 0 + x
    y <- 0 + y
  }
  
  for (j in 1:3) {
    
    # Argument h denotes home and argument a denotes away team
    home <- Tourney[x,]
    away <- Tourney[y,]
    
    # Given that the expctation can be defined as (0.6*A1+0.4*D1)-(0.4*A2+0.6*D2)+H1
    expectation <- (0.6*home$A + 0.4*home$D) - (0.4*away$A + 0.6*away$D) + home$H
    
    # Gievn that standard deviation is 1/R1+1/R2
    sd <- ((1/home$R) + (1/away$R))
    
    
    spread <- 0
    
    while (spread == 0) {
      spread <- rnorm_round(1, expectation, sd)
    }
    
    if(!(Tourney[x,6] == 2 & Tourney[y,6] == 2)) {
      if(spread > 0){
        Tourney[x,6] <- Tourney[x,6] + 1
        Tourney[x,8] <- Tourney[x,8] + spread
        Tourney[y,7] <- Tourney[y,7] + 1
      }else{
        Tourney[y,6] <- Tourney[y,6] + 1
        Tourney[y,8] <- Tourney[y,8] + abs(spread)
        Tourney[x,7] <- Tourney[x,7] + 1
      }
      z <- 0
      z <- x
      x <- y
      y <- z
      
    }else {
      {break}
    }
    
  }
  
  
  
  final <- Tourney
  
  final <- final %>% arrange(desc(Wins))
  final$Season_top <- c(1,0,0,0,0,0,0,0)
  final <- final %>% arrange(Seq)
  
  T_results$Wins <- T_results$Wins + final$Wins
  check$Season_top <- check$Season_top + final$Season_top
  
}

T_results <- check %>% mutate(Win_percent = (check$Season_top/nsim*100))

T_results  %>% ggplot(aes(x= reorder(Team, Win_percent), y = Win_percent, fill=Team)) +
  geom_bar(position = "dodge", stat = "identity", width = .4) +
  coord_flip() + scale_color_fivethirtyeight() + theme_fivethirtyeight() +
  ggtitle("All participating teams by Win Percentage")+ 
  theme(plot.title = element_text(hjust = 0.5))  +
  theme(legend.position = "none") +
  guides(fill = guide_legend(reverse=TRUE)) +
  scale_y_continuous(breaks = seq(0, 100, by = 10),labels = function(x) paste0(x,"%")) +
  geom_text(aes(label = sprintf("%0.1f", round(Win_percent, digits = 2)), Win_percent = Win_percent + 0.05), position = position_dodge(0.9),hjust = 0)



