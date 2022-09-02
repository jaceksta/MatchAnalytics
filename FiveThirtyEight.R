library(tidyverse)
library(data.table)

spi_matches <- read.csv('spi_matches.csv')


colnames(spi_matches)

unique(spi_matches$league)

spi_matches <- spi_matches%>%
  drop_na()

top5 <- spi_matches%>%
  filter(league == c("French Ligue 1", "Barclays Premier League", "German Bundesliga", "Spanish Primera Division",
                     "Italy Serie A"))%>%
  select(season, date, league, team1, team2, xg1, xg2, score1, score2)


games <- 
  rbind(
    data.frame(team = top5$team1,
               opponent = top5$team2,
               home = 1,
               goals = top5$score1,
               conceded = top5$score2,
               xG = top5$xg1,
               xGA = top5$xg2,
               date = top5$date),
    data.frame(team = top5$team2,
               opponent = top5$team1,
               home = 0,
               goals = top5$score2,
               conceded = top5$score1,
               xG = top5$xg2,
               xGA = top5$xg1,
               date = top5$date))


games$GD <- games$goals - games$conceded
games$xGD <- games$xG - games$xGA

games$diff <- games$GD - games$xGD

games <- games[reorder(games$diff),]

biggest_10 <- games[1:10,]

label <- c()

for(i in 1:10){
  if (biggest_10$home[i] == 1){
    game <- paste(biggest_10$team[i], biggest_10$opponent[i], sep = " - ")
    result <- paste(toString(biggest_10$goals[i]), toString(biggest_10$conceded[i]), sep = ":")
    label <- c(label, paste(game, result, sep = " "))
  } else{
    game <- paste(biggest_10$opponent[i], biggest_10$team[i], sep = " - ")
    result <- paste(toString(biggest_10$conceded[i]), toString(biggest_10$goals[i]), sep = ":")
    label <- c(label, paste(game, result, sep = " "))
  }
}

biggest_10$label <- label

biggest_10_melt <- reshape2::melt(biggest_10, id.vars = "label", variable.name = "category", value.name = "scores")


biggest_10_melt <- biggest_10_melt%>%
  filter(category == "GD" | category == "xGD")

biggest_10_melt$scores <- as.numeric(biggest_10_melt$scores)

ggplot(biggest_10_melt) +
  geom_bar(aes(x = label , y= scores, fill = category),position="dodge", stat = "identity")+
  scale_x_discrete(labels = function(label) str_wrap(label, width = 10))


write_csv(biggest_10, 'biggest_10.csv')
