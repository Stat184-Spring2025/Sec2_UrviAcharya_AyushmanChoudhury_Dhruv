# Load necessary packages 
library(tidyverse)
library(dplyr)
library(knitr)
library(kableExtra)
library(ggplot2)
#-----------------------------------------------------------------------------------------------------
# import data
netflixRaw <- read.csv(
  file = "~/Desktop/184_group_project/netflix_titles.csv"
)
#-----------------------------------------------------------------------------------------------------
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

#-----------------------------------------------------------------------------------------------------
## creating subset dataframes for movies and tv shows

# create subset dataset of tv shows
netflixTvTable <- netflixTidy %>%
  filter(type == "TV Show")
# visualize subset  
#netflixTvTable %>%
  #head(20) %>% # can change how many rows are visible
  #kbl(caption = "Dataset Sample of TV Shows on Netflix") %>%
  #kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F)

# create subset dataset of movies
netflixMovieTable <- netflixTidy %>%
  filter(type == "Movie")
# visualize subset
#netflixMovieTable %>%
  #head(20) %>% # can change how many rows are visible
  #kbl(caption = "Dataset Sample of Movies on Netflix") %>%
  #kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F)

#-----------------------------------------------------------------------------------------------------
## Visualizing the popularity of genres across movies and tv shows
## Separate bar charts for movies and tv shows

# function counts the occurrences of each genre
genreCount <- function(genre_occurence, type) {
  genre_occurence %>%
    separate_rows(genre, sep = ", ") %>%
    filter(genre != "TV Shows" & genre != "Movies") %>%
    count(genre, name = "count") %>%
    arrange(desc(count)) %>% # arranged in descending order
    mutate(type = type)
}

# count tv show genres
tvGenre <- genreCount(netflixTvTable, "TV Show")

# creating plot for tv genre
tvGenrePlot <- ggplot(
  tvGenre,
  aes(
    x = reorder(genre, count),
    y = count
  )
) +
  geom_bar(stat = "identity", fill = "darkred") + # darkred makes all the bars the same color
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    legend.position = "none" # legend removed since all the bars are the same color
  ) +
  labs(
    title = "TV Show Genre Popularity on Netflix",
    x = "Genre",
    y = "Number of Titles"
  )

print(tvGenrePlot)

# tv show genre occurrences table
#tvGenre %>%
  #kbl(caption = "Genre Count for Netflix TV Shows") %>%
  #kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE)


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
  geom_bar(stat = "identity", fill = "darkorange") + # darkorange makes all the bars the same color
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    legend.position = "none" # legend removed since all the bars are the same color
  ) +
  labs(
    title = "Movie Genre Popularity on Netflix",
    x = "Genre",
    y = "Number of Titles"
  )

print(movieGenrePlot)

# movie genre occurrence table
#movieGenre %>%
  #kbl(caption = "Genre Count for Netflix Movies") %>%
  #kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE)

#-----------------------------------------------------------------------------------------------------
## Counting the overlap of titles between Netflix and Disney+ from joined table
## There will be more than two instances of the same title due to it appearing in,
## more than one country

# import combined data file
disneyNetflixdata <- read.csv(
  file = "~/Desktop/184_group_project/combined_streaming_data.csv"
)
#-----------------------------------------------------------------------------------------------------

# counting instances of repeated titles
repeatedTitles <- disneyNetflixdata %>%
  group_by(title) %>%  # grouping by movie and tv show title 
  summarise(
    count = n(), # count the occurrences of each title in disneyNetflixdata
    title_type = paste(unique(type), collapse = ", "), #title type and title turned into one string
    countries = paste(unique(country), collapse = ", ")# country and title turned into one string
  ) %>%
  arrange(desc(count)) # arranged in descending order

# visualizing in table form
repeatedTitles %>%
  head(20)%>%
  kbl(caption = "Titles Found Both on Netflix and Disney Plus (Top 20)") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE) %>%
  column_spec(4, width = "200px", extra_css = "word-wrap: break-word; white-space: normal;")
  # reduced column length for final qmd file

#-----------------------------------------------------------------------------------------------------
# answering research question: Are Disney+ movies shorter or longer compared to Netflix?

# filter the combined data set to only include the rows that are movies
moviesOnly <- disneyNetflixdata %>%
  filter(type == "Movie")

# movie_duration_mins is a new column that is created by getting the duration,
# from the duration columns
# "min" is re moved from the text and the duration is turned into a numeric
moviesOnly <- moviesOnly %>%
  mutate(movie_duration_mins = as.numeric(str_replace(duration, " min", "")))

# box-plot created to compare movie duration of Netflix and Disney+
moviesOnlyPlot <- ggplot(
  moviesOnly,
  aes(
    x = platform,            # platform on the x-axis
    y = movie_duration_mins, # movie duration on y-axis
    fill = platform          # color filled based on platform
  )
) +
  geom_boxplot() + # box-plot layer added
  # colors manually set for Disney+ and Netflix
  scale_fill_manual(values = c("Disney+" = "royalblue", "Netflix" = "brown1")) +
  labs(
    title = "Movie Duration on Each Platform",
    x = "Platform",
    y = "Duration in Minutes"
  ) +
  theme_minimal()

print(moviesOnlyPlot)



