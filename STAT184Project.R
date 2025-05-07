# Load necessary packages 
library(tidyverse)
library(dplyr)
library(knitr)
library(kableExtra)
library(ggplot2)

# Before running the code make sure the datasets are downloaded
# import data
netflixRaw <-read.csv(file="/Users/dhruvnasit/Downloads/netflix_dataset.csv")
disneyRaw <-read.csv(file="/Users/dhruvnasit/Downloads/disney_plus_dataset.csv")

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

# Tidying Disney+
# remove null values
netflixTidy <- netflixTidy %>%
  filter(country != "")

disneyTidy <- disneyRaw %>%
  rename(
    genre = `listed_in`,
    type = `type`,
    country = `country`
  ) %>%
  select(-show_id, -director, -cast, -description) %>%
  filter(!is.na(country), country != "") %>%
  separate_rows(country, sep = ", ")

# filter the combined data set to only include the rows that are movies
moviesOnly <- read.csv(
  file="/Users/dhruvnasit/Downloads/combined_streaming_data.csv"
) %>%
  filter(type == "Movie")

# movie_duration_mins is a new column that is created by getting the duration,
# from the duration columns
# "min" is re moved from the text and the duration is turned into a numeric

moviesOnly <- moviesOnly %>%
  mutate(movie_duration_mins = as.numeric(str_replace(duration, " min", "")))

moviesOnlyPlot <- ggplot(
  moviesOnly,
  aes(
    x = platform,
    y = movie_duration_mins,
    fill = platform
  )
) +
  geom_boxplot() +
  scale_fill_manual(values = c("Disney+" = "royalblue", "Netflix" = "brown1")) +
  labs(
    title = "Movie Duration on Each Platform",
    x = "Platform",
    y = "Duration in Minutes"
  ) +
  theme_minimal()

print(moviesOnlyPlot)

# function counts the occurrences of each genre
genreCount <- function(genre_occurence, type) {
  genre_occurence %>%
    separate_rows(genre, sep = ", ") %>%
    filter(genre != "TV Shows" & genre != "Movies") %>%
    count(genre, name = "count") %>%
    arrange(desc(count)) %>% # arranged in descending order
    mutate(type = type)
}

# create subset dataset of tv shows for Netflix
netflixTvTable <- netflixTidy %>%
  filter(type == "TV Show")


# create subset dataset of movies
netflixMovieTable <- netflixTidy %>%
  filter(type == "Movie")
# count tv show genres Netflix
tvGenre <- genreCount(netflixTvTable, "TV Show")

# creating plot for tv genre
tvGenrePlot <- ggplot(
  tvGenre,
  aes(
    x = reorder(genre, count),
    y = count
  )
) +
  geom_bar(stat = "identity", fill = "darkred") + 
  # darkred makes all the bars the same color
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    legend.position = "none" 
    # legend removed since all the bars are the same color
  ) +
  labs(
    title = "TV Show Genre Popularity on Netflix",
    x = "Genre",
    y = "Number of Titles"
  )
print(tvGenrePlot)

# Plot for Disney+
genreCount <- function(df, type_label) {
  df %>%
    separate_rows(genre, sep = ", ") %>%
    filter(genre != "TV Shows" & genre != "Movies") %>%
    count(genre, name = "count") %>%
    arrange(desc(count)) %>%
    mutate(type = type_label)
}
disneyTV <- disneyTidy %>% filter(type == "TV Show")
disneyMovies <- disneyTidy %>% filter(type == "Movie")
# Apply genre count
tvGenres <- genreCount(disneyTV, "TV Show")
movieGenres <- genreCount(disneyMovies, "Movie")
tvPlot <- ggplot(tvGenres, aes(x = reorder(genre, count), y = count)) +
  # darkred makes all the bars the same color
  geom_bar(stat = "identity", fill = "darkred") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    legend.position = "none"
  ) +
  labs(
    title = "TV Show Genre Popularity on Disney+",
    x = "Genre",
    y = "Number of Titles"
  )
print(tvPlot)

# count movie genres
movieGenre <- genreCount(netflixMovieTable, "Movie")

# creating plot for movies
movieGenrePlot <- ggplot(
  movieGenre,
  aes(
    x = reorder(genre, count),
    y = count
  )
) +
  # darkorange makes all the bars the same color
  geom_bar(stat = "identity", fill = "darkorange") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    # legend removed since all the bars are the same color
    legend.position = "none"
  ) +
  labs(
    title = "Movie Genre Popularity on Netflix",
    x = "Genre",
    y = "Number of Titles"
  )

print(movieGenrePlot)

# plot for disney+
moviePlot <- ggplot(movieGenres, aes(x = reorder(genre, count), y = count)) +
  # darkorange makes all the bars the same color
  geom_bar(stat = "identity", fill = "darkorange") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    legend.position = "none"
  ) +
  labs(
    title = "Movie Genre Popularity on Disney+",
    x = "Genre",
    y = "Number of Titles"
  )
print(moviePlot)

# counting instances of repeated titles
repeatedTitles <- read_csv(
  file="/Users/dhruvnasit/Downloads/combined_streaming_data.csv"
)%>%
  group_by(title) %>%  # grouping by movie and tv show title 
  summarise(
    count = n(), # count the occurrences of each title in disneyNetflixdata
    #title type and title turned into one string
    title_type = paste(unique(type), collapse = ", "),
    # country and title turned into one string
    countries = paste(unique(country), collapse = ", ")
  ) %>%
  arrange(desc(count)) # arranged in descending order

# visualizing in table form
# visualizing in table form
repeatedTitles %>%
  head(20) %>%
  knitr::kable(
    caption = "Titles Found Both on Netflix and Disney Plus (Top 20)",
    format = "latex",
    longtable = TRUE,
    booktabs = TRUE
  ) %>%
  kable_styling(
    latex_options = c("repeat_header"),  # no scale_down!
    full_width = FALSE,
    position = "center"
  ) %>%
  column_spec(4, width = "12em",
              extra_css = "word-wrap: break-word; white-space: normal;")