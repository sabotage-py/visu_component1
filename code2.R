library(tidyverse)
library(jsonlite)

movie = read.csv("dataset/tmdb_5000_movies.csv", header=T)
colnames(movie)[4] = "movie_id"
movie$genres = as.character(movie$genres)
all_genres = movie %>%
  filter(nchar(genres) > 2) %>%
  mutate(
    js = lapply(genres, fromJSON)
  ) %>%
  unnest(js) %>%
  mutate(genre=name)

colnames(all_genres)
movie = all_genres[, 
                   c('movie_id', 'original_title', 'genre', 
                     'runtime', 'vote_average', 'vote_count', 'title',
                     'budget'
                    )
                  ]
rm(all_genres)
colnames(movie)[1] = "id"
movie = subset(movie, runtime > 0 & vote_count > 9)
movie = subset(movie, !(genre %in% c("Foreign", "TV Movie")))


## RATING vs GENRE
ggplot(movie, aes(y=genre, x =vote_average, fill = genre)) +
  geom_boxplot(alpha = 0.7,
               outlier.colour = "black", outlier.shape = 20) +
  xlab("Avg rating (out of 10)") +
  scale_y_discrete(limits = sort(unique(movie$genre), decreasing = T), name = "Genre") +
  ggtitle("Boxplot of avg. rating by Genre") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(movie, aes(x=vote_average, fill=genre)) +
  geom_density() +
  facet_wrap(~genre) +
  ggtitle("Probability density plot for avg. RATING by different GENREs")


## BUDGET vs GENRE
budget_movie = subset(movie, budget > 100000)
budget_movie$budget = budget_movie$budget / 1000000

ggplot(budget_movie, aes(y=genre, x =budget, fill = genre)) +
  geom_boxplot(alpha = 0.7,
               outlier.colour = "#1F3552", outlier.shape = 20) +
  xlab("Budget (million USD)") +
  scale_y_discrete(limits = sort(unique(movie$genre), decreasing = T), name = "Genre") +
  ggtitle("Boxplot of Budget by Genre")


