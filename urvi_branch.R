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
# creating CSV file of netflixTidy

write.csv(
  netfilxTidy,
  file = "~/Desktop/184_group_project/netflix_tidied.csv",
  row.names = FALSE
)

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
    y = count
  )
) +
  geom_bar(stat = "identity", fill = "darkred") +
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

# tv show genre ocurrence table
tvGenre %>%
  kbl(caption = "Genre Count for Netflix TV Shows") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE)


# count movie genres
movieGenre <- genreCount(movieTable, "Movie")

# creating plot for movies
movieGenrePlot <- ggplot(
  movieGenre,
  aes(
    x = reorder(genre, count),
    y = count
  )
) +
  geom_bar(stat = "identity", fill = "darkorange") +
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

# movie genre occurrence table
movieGenre %>%
  kbl(caption = "Genre Count for Netflix Movies") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE)

#---------------------------------------------------------------------------
# count the ocurence of each genre for each country for both TV shows and movies
countGenreForCountry <- function(country_genre_count) {
  country_genre_count %>%
    separate_rows(genre, sep = ", ") %>%
    filter(genre != "TV Shows" & genre != "Movies") %>%
    group_by(country, genre) %>%
    count(name = "count") %>%
    ungroup() %>%
    arrange(country, desc(count))
}

# counting the genres for each country for tv shows
countCountryGenreTV <- countGenreForCountry(tvTable)

# select top 5-10 countries to plot so there aren't too many countries
tvContentTotalByCountry <- tvTable %>%
  count(country, name = "content_total") %>%
  arrange(desc(content_total))

topCountries <- tvContentTotalByCountry %>%
  head(10) %>%
  pull(country) # getting country names as a vector

# filtering top 5-10 countries for plotting
topCountryTvGenreCount <- countCountryGenreTV %>%
  filter(country %in% topCountries)

tvGenreCountryPlot <- ggplot(
  topCountryTvGenreCount,
  aes(
    x = reorder(genre, count),
    y = count,
    fill = genre  # bars are filled with color by genre
  )
) +
  geom_bar(stat = "identity") + 
  facet_wrap(~country, scales = "free_y") + # facet_wrap to separate the countries and 
  theme_minimal() +                         # what genres are popular in them.
  theme(
    axis.text.x = element_blank(), # removed genre names from x-axis because of clutter
    axis.ticks.x = element_blank(), # no genre names = no x-axis ticks
    legend.position = "right",      # legend added so that color corresponds to genre
    strip.text = element_text(size = 9)
  ) +
  labs(
    title = "Genre Popularity of TV Shows by Country on Netflix (Top Ten Countries)",
    x = "Genre",
    y = "Number of Titles"
  )

print(tvGenreCountryPlot)

## create same plot for movies

# counting the genres for each country for tv shows
countCountryGenreMovie <- countGenreForCountry(movieTable)

# select top 5-10 countries to plot so there aren't too many countries
movieContentTotalByCountry <- movieTable %>%
  count(country, name = "content_total") %>%
  arrange(desc(content_total))

topCountries <- movieContentTotalByCountry %>%
  head(10) %>%
  pull(country) # getting country names as a vector

# filtering top 5-10 countries for plotting
topCountryMovieGenreCount <- countCountryGenreMovie %>%
  filter(country %in% topCountries)

movieGenreCountryPlot <- ggplot(
  topCountryMovieGenreCount,
  aes(
    x = reorder(genre, count),
    y = count,
    fill = genre # bars are filled with color by genre
  )
) +
  geom_bar(stat = "identity") + 
  facet_wrap(~country, scales = "free_y") + # facet_wrap to separate the countries and 
  theme_minimal() +                         # what genres are popular in them.
  theme(
    axis.text.x = element_blank(), # removed genre names from x-axis because of clutter
    axis.ticks.x = element_blank(), # no genre names = no x-axis ticks
    legend.position = "right",      # legend added so that color corresponds to genre
    strip.text = element_text(size = 9)
  ) +
  labs(
    title = "Genre Popularity of Movies by Country on Netflix (Top Ten Countries)",
    x = "Genre",
    y = "Number of Titles"
  )

print(movieGenreCountryPlot)

#---------------------------------------------------------------------------
# counting the overlap of titles between Netflix and Disney Plus from joined table
# there will be more than two instances of the same title due to it appearing in,
# more than one country

# reading combined data file
disneyNetflixdata <- read.csv(
  file = "~/Desktop/184_group_project/combined_streaming_data.csv"
)

# counting instances of repeated titles
repeatedTitles <- disneyNetflixdata %>%
  group_by(title) %>%
  summarise(
    count = n(),
    title_type = paste(unique(type), collapse = ", "),
    countries = paste(unique(country), collapse = ", ") 
  ) %>%
  arrange(desc(count))

# visualizing in table form
repeatedTitles %>%
  head(20)%>%
  kbl(caption = "Titles Found Both on Netflix and Disney Plus (Top 20)") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE)






