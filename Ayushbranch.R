# Load required packages
library(readxl)
library(tidyverse)
library(kableExtra)
library(ggplot2)
library(readr)
library(dplyr)

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
  filter(!is.na(country), country != "") %>%
  separate_rows(country, sep = ", ")

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
# 5. Plot TV Show genres (dark red with genre labels)
tvPlot <- ggplot(tvGenres, aes(x = reorder(genre, count), y = count)) +
  geom_bar(stat = "identity", fill = "darkred") + # darkred makes all the bars the same color
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

#--------------------------------------------------------------------------------------
# 6. Plot Movie genres (dark orange with genre labels)
moviePlot <- ggplot(movieGenres, aes(x = reorder(genre, count), y = count)) +
  geom_bar(stat = "identity", fill = "darkorange") + # darkorange makes all the bars the same color
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

#--------------------------------------------------------------------------------------
# 7. Combine both Tidied Datasets
disney <- read_csv("~/Downloads/disney_tidy.csv")
netflix <- read_csv("~/Downloads/netflix_tidied.csv")

# Standardize date format and add platform label
disney <- disney %>% mutate(
  date_added = as.character(date_added),
  platform = "Disney+"
)

netflix <- netflix %>% mutate(
  date_added = as.character(date_added),
  platform = "Netflix"
)

# Combine and save
combined <- bind_rows(disney, netflix)
write_csv(combined, "~/Downloads/combined_streaming_data.csv")

