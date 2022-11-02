getwd()
library(tidyverse)

df = read.csv("dataset/tmdb_5000_movies.csv")
colnames(df)
df = subset(df, runtime > 0 & nchar(genres) > 2 & vote_count > 9)

genres = c('Action', 'Adventure', 'Animation', 'Comedy', 'Crime', 'Documentary', 
           'Drama', 'Family', 'Fantasy', 'History', 'Horror', 
           'Music', 'Mystery', 'Romance', 'Science Fiction', 
           'Thriller', 'War', 'Western')
## note that we've removed 'Foreign' and 'TV Movie'

foo = function(unclean_string, genre) {
  return (str_detect(unclean_string, genre))
}

## one hot encoding of genres
for (g in genres) {
  df[g] = apply(
    df["genres"], FUN = foo, MARGIN = 1, genre = g)
}

df = df[, append(c('id', 'budget', 'runtime', 'vote_count', 'vote_average', 
                   'title', 'original_title'), 
                 genres)
        ]

View(df)


ggplot(data=df, aes(x=vote_count)) +
  geom_histogram(bins=100, fill="plum", colour="purple") + 
  labs(title="Histogram of vote counts of movies", x="Number of votes")

ggplot(data=df, aes(x=vote_average)) +
  geom_histogram(bins=20, fill="yellowgreen", colour="wheat2") + 
  labs(title="Histogram of ratings of movies", x="Rating (out of 10)")

ggplot(df, aes(x=vote_average)) +
  geom_density(fill = 'purple') +
  ggtitle("Probability density plot for Average Rating of Movies") +
  xlab("Rating (out of 10)") + 
  ylab("Density")


## RATING vs RUNTIME
ggplot(data=subset(df, Action == TRUE), aes(x=runtime, y=vote_average)) + 
  geom_point(shape=16, color="purple") +
  geom_smooth(formula = NULL, method=NULL,  linetype="dashed",
              color="violet", fill="plum") + 
  ylab("Avg rating (out of 10)") +
  xlab("Runtime (minutes)")


## RATING vs BUDGET
budget_df = subset(df, budget > 100000)
budget_df$budget = budget_df$budget / 1000000

ggplot(data=budget_df, aes(x=budget, y=vote_average)) + 
  geom_point(shape=16, color="violetred4")+
  geom_smooth(formula = y~x, method = NULL, linetype="dashed",
              color="orchid4", fill="orchid")+
  ylab("Average rating (out of 10)")+
  xlab("Budget (million USD)")


## shares of GENRES
most_common_genres = function(df, n=18) {
  genre_percent = c()
  for (g in genres) {
    genre_percent = append(genre_percent, sum(df[g])/nrow(df))
  }
  
  for (i in 1:(length(genres)-1)) {
    for (j in (i+1):length(genres)) {
      if (genre_percent[i] < genre_percent[j]) {
        temp = genre_percent[i]
        genre_percent[i] = genre_percent[j]
        genre_percent[j] = temp
        temp = genres[i]
        genres[i] = genres[j]
        genres[j] = temp
      }
    }
  }
  
  genres = reorder(genres, genre_percent, decreasing = T)
  df_genre = data.frame("Genre"=genres[2:n], "share"=genre_percent[2:n])
  
  ggplot(df_genre, aes(x=Genre, y=share)) +
    geom_bar(stat="identity", fill = "saddlebrown") +
    coord_flip() +
    scale_x_discrete(limits = genres[2:n]) +
    ylim(c(0, min(1, max(genre_percent[-1]) + 0.1))) +
    geom_text(aes(label = scales::percent(share), hjust = -0.1)) +
    ggtitle("Share of genres with ____") +
    ylab('Share ratio')
}

most_common_genres(subset(df, Horror==T))
#most_common_genres(df, 18)
