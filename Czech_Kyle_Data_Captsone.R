
#Clears all current Variables
rm(list=ls())

#Library for graphs
library(ggplot2)  # Already loaded if we loaded the tidyverse

#Titles of games analyzed 
title <- c('Assassins Creed 3','GTA 5','Fallout 4',
          'Skyrim', 'Doom', 'Far Cry 3',
          'Destiny 2', 'Titanfall', 'NHL 15', 'Madden 19',
          'FIFA 15', 'Rocket League', 'Firestorm', 'Blackout',
          'Apex Legends', 'Fortnite')

#Genre of the games
genre <- c('Single Player', 'Single Player', 'Single Player',
           'Single Player', 'First Person Shooter', 'First Person Shooter',
            'First Person Shooter', 'First Person Shooter', 'Sports', 'Sports'
           , 'Sports', 'Sports', 'Battle Royale', 'Battle Royale', 'Battle Royale',
           'Battle Royale')

#Proportion per 100 words of sexist remarks
proportion_sexist <- c((320/434015)*100, (292/293109)*100, (351/589882)*100,
                       (506/481379)*100, (24/409016)*100, (244/200311)*100,
                       (160/286274)*100, (209/285206)*100, (11/32259)*100, 
                       (7/42898)*100, (45/91451)*100,(5/22990)*100,
                       (9/61083)*100, (15/132100)*100, (38/106763)*100,
                       (54/190001)*100)

#Proportion per 100 words of racist remarks
proportion_racist <- c((153/434015)*100, (33/293109)*100, (26/589882)*100,
                       (23/481379)*100, (0/40901)*100, (54/200311)*100,
                       (7/286274)*100, (6/285206)*100, (2/32259)*100,
                       (2/42898)*100, (5/91451)*100, (0/22990)*100,
                       (6/61083)*100, (7/132100)*100,(3/106763)*100,
                       (9/190001)*100)

#Proportion of profanity per 100 comments
proportion_profan <- c((2669/24999)*100, (1788/17333)*100, (2855/24994)*100,
                       (2616/25000)*100, (369/2697)*100, (2009/13095)*100,
                       (2158/17709)*100, (1651/12811)*100, (178/1811)*100,
                       (179/2768)*100, (437/6347)*100, (110/1876)*100,
                       (95/5011)*100, (329/9685)*100,(304/8014)*100,
                       (410/15699)*100)

#Dataframe for graph
game.data <- data.frame(title, genre, proportion_sexist, proportion_racist, proportion_profan)

str(game.data)

#Boxplot for proportion of racist words by genre
boxplot_racist <- ggplot(data = game.data, aes(x = genre, y = proportion_racist)) +
  geom_boxplot(aes(fill=genre)) +
  xlab('Genre') +
  ylab('Usage') +
  ggtitle('Proportion of racist terms per 100 words by Genre')

boxplot_racist

#Boxplot for proportion of sexist words by genre
boxplot_sexist <- ggplot(data = game.data, aes(x = genre, y = proportion_sexist)) +
  geom_boxplot(aes(fill=genre)) +
  xlab('Genre') +
  ylab('Usage') +
  ggtitle('Proportion of Sexist terms per 100 words by Genre')

boxplot_sexist

#Proportion of profanity per 100 comments
boxplot_profan <- ggplot(data = game.data, aes(x = genre, y = proportion_profan)) +
  geom_boxplot(aes(fill=genre)) +
  xlab('Genre') +
  ylab('Usage') +
  ggtitle('Proportion of Profan terms per 100 words by Genre')


boxplot_profan

# Compute the analysis of variance
res.aov_racist <- aov(proportion_racist ~ genre, data = game.data)

res.aov_sexist <- aov(proportion_sexist ~ genre, data = game.data)

res.aov_profan <- aov(proportion_profan ~ genre, data = game.data)

# Summary of the analysis
summary(res.aov_racist)
summary(res.aov_sexist)
summary(res.aov_profan)
