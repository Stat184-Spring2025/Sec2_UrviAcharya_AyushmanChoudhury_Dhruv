# Load packages 
library(tidyverse)
library(dplyr)
library(knitr)
library(kableExtra)
library(ggplot2)
#--------------------------------------------------------------------------------------
# import data
netflixRaw <- read.csv(
  file = "~/Desktop/184_group_project/netflix_titles.csv"
)
# -------------------------------------------------------------------------------------
## Tidying data
# remove and rename columns as needed
netflixTidy <- netflixRaw %>%
  rename(
    genre = listed_in
  ) %>%
  select(
    -show_id, -director,
    -cast, -description
  )

# remove null values
netflixTidy <- netflixTidy %>%
  filter(country != "")
# -------------------------------------------------------------------------------------
## creating subset dataframes for movies and tv shows
# create subset dataset of tv shows
tvTable <- netfilxTidy %>%
  filter(type == "TV Show")
# visualize subset  
tvTable %>%
  head(20) %>% # can change how many rows are visible
  kbl(caption = "Dataset Sample of TV Shows on Netflix") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F)

# create subset dataset of movies
movieTable <- netfilxTidy %>%
  filter(type == "Movie")
# visualize subset
movieTable %>%
  head(20) %>% # can change how many rows are visible
  kbl(caption = "Dataset Sample of Movies on Netflix") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F)

# creating prelim bar chart visualizations ---------------------------------------------

# function counts the ocurrences of each genre
genreCount <- function(genre_occurence, type) {
  genre_occurence %>%
    separate_rows(genre, sep = ", ") %>%
    filter(genre != "TV Shows" & genre != "Movies") %>%
    count(genre, name = "count") %>%
    arrange(desc(count)) %>%
    mutate(type = type)
}

# count tv show genres
tvGenre <- genreCount(tvTable, "TV Show")

# creating plot for tv genre
tvGenrePlot <- ggplot(
  tvGenre,
  aes(
    x = reorder(genre, count),
    y = count,
    fill = genre
  )
) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    legend.position = "none"
  ) +
  labs(
    title = "TV Show Genre Popularity on Netflix",
    x = "Genre",
    y = "Number of Titles"
  )

print(tvGenrePlot)

# count movie genres
movieGenre <- genreCount(movieTable, "Movie")

# creating plot for movies
movieGenrePlot <- ggplot(
  movieGenre,
  aes(
    x = reorder(genre, count),
    y = count,
    fill = genre
  )
) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    legend.position = "none"
  ) +
  labs(
    title = "Movie Genre Popularity on Netflix",
    x = "Genre",
    y = "Number of Titles"
  )

print(movieGenrePlot)


#View(netflixTidy)
#View(tvTable)
#View(movieTable)

