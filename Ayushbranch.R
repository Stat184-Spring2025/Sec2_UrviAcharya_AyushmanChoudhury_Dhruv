# Load required packages
library(readxl)
library(tidyverse)
library(kableExtra)
library(ggplot2)

#--------------------------------------------------------------------------------------
# 1. Load the Disney+ Excel file
disneyRaw <- read_excel("~/Downloads/Untitled spreadsheet.xlsx")

#--------------------------------------------------------------------------------------
# 2. Clean and tidy the dataset
disneyTidy <- disneyRaw %>%
  rename(
    genre = `listed_in`,
    type = `type`,
    country = `country`
  ) %>%
  select(-show_id, -director, -cast, -description) %>%
  filter(!is.na(country), country != "")

#--------------------------------------------------------------------------------------
# 3. Create TV Show and Movie subsets
disneyTV <- disneyTidy %>% filter(type == "TV Show")
disneyMovies <- disneyTidy %>% filter(type == "Movie")

#--------------------------------------------------------------------------------------
# 4. Genre counting function
genreCount <- function(df, type_label) {
  df %>%
    separate_rows(genre, sep = ", ") %>%
    filter(genre != "TV Shows" & genre != "Movies") %>%
    count(genre, name = "count") %>%
    arrange(desc(count)) %>%
    mutate(type = type_label)
}

# Apply genre count
tvGenres <- genreCount(disneyTV, "TV Show")
movieGenres <- genreCount(disneyMovies, "Movie")

#--------------------------------------------------------------------------------------
# 5. Plot TV Show genres
tvPlot <- ggplot(tvGenres, aes(x = reorder(genre, count), y = count, fill = genre)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(
    title = "Disney+ TV Show Genre Popularity",
    x = "Genre",
    y = "Number of Titles"
  ) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = "right"
  )
print(tvPlot)

#--------------------------------------------------------------------------------------
# 6. Plot Movie genres
moviePlot <- ggplot(movieGenres, aes(x = reorder(genre, count), y = count, fill = genre)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(
    title = "Disney+ Movie Genre Popularity",
    x = "Genre",
    y = "Number of Titles"
  ) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = "right"
  )
print(moviePlot)

#--------------------------------------------------------------------------------------
# 7. Function to count genres by country
countGenreByCountry <- function(df) {
  df %>%
    separate_rows(genre, sep = ", ") %>%
    group_by(country, genre) %>%
    count(name = "count") %>%
    ungroup() %>%
    arrange(country, desc(count))
}

# Count genres by country for TV and Movies
tvByCountry <- countGenreByCountry(disneyTV)
movieByCountry <- countGenreByCountry(disneyMovies)

#--------------------------------------------------------------------------------------
# 8. Top 10 countries by content count
topTVCountries <- disneyTV %>%
  count(country, name = "total") %>%
  top_n(10, total) %>%
  pull(country)

topMovieCountries <- disneyMovies %>%
  count(country, name = "total") %>%
  top_n(10, total) %>%
  pull(country)

# Filter top countries
tvGenresTop <- tvByCountry %>% filter(country %in% topTVCountries)
movieGenresTop <- movieByCountry %>% filter(country %in% topMovieCountries)

#--------------------------------------------------------------------------------------
# 9. Plot TV genres by country
tvCountryPlot <- ggplot(tvGenresTop, aes(x = reorder(genre, count), y = count, fill = genre)) +
  geom_bar(stat = "identity") +
  facet_wrap(~country, scales = "free_y") +
  theme_minimal() +
  labs(
    title = "Disney+ TV Genres by Country (Top 10)",
    x = "Genre",
    y = "Number of Titles"
  ) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = "right",
    strip.text = element_text(size = 9)
  )
print(tvCountryPlot)

#--------------------------------------------------------------------------------------
# 10. Plot Movie genres by country
movieCountryPlot <- ggplot(movieGenresTop, aes(x = reorder(genre, count), y = count, fill = genre)) +
  geom_bar(stat = "identity") +
  facet_wrap(~country, scales = "free_y") +
  theme_minimal() +
  labs(
    title = "Disney+ Movie Genres by Country (Top 10)",
    x = "Genre",
    y = "Number of Titles"
  ) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = "right",
    strip.text = element_text(size = 9)
  )
print(movieCountryPlot)
