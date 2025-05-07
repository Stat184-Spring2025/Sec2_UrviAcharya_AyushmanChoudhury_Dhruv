# Streaming Service Data Exploration

This project explores and compares content across major streaming platforms, specifically Disney+ and Netflix, using data-driven insights and visualizations.

## Overview

The goal of this project is to analyze content trends, genres, and characteristics from Disney+ and Netflix using datasets obtained from Kaggle. We aimed to answer key questions about release trends, genre popularity, and content duration differences across platforms. The findings are visualized using tools in R, and this analysis is part of a collaborative academic project for STAT 184.

### Insights

**Movie Duration Trends:**  
Netflix movies generally have a higher median runtime and display greater variability, with many titles exceeding 300 minutes. In contrast, Disney+ movies tend to be shorter and more consistent in length, reflecting the platform’s focus on concise, family-friendly storytelling.

**TV Genre Distribution:**  
On Netflix, *International TV Shows*, *TV Dramas*, and *TV Comedies* dominate the catalog, showcasing the platform’s global reach and appeal to diverse audiences. Meanwhile, Disney+ heavily features *Animation*, *Action-Adventure*, and *Kids* genres, reinforcing its brand identity as a provider of family-centric entertainment.

**Movie Genre Popularity:**  
Netflix’s movie library is led by *International Movies*, *Dramas*, and *Comedies*, indicating a broad and emotionally rich content strategy. Disney+, by comparison, focuses on *Family*, *Comedy*, and *Animation* movies, which aligns with its historical strengths in animated and wholesome content.

**Shared Titles Across Platforms:**  
A small subset of titles, such as *Mulan II* and *Bonkers*, appear on both platforms across multiple regions. Most of these are legacy Disney properties available through overlapping licensing agreements, providing insight into content distribution strategies prior to exclusivity deals.


## Repo Structure

- `disney_plus_dataset.csv`: Raw dataset containing information about Disney+ titles  
- `netflix_dataset.csv`: Raw dataset containing information about Netflix titles  
- `plots/`: Folder containing all exported visualizations as `.png` or `.jpg`  
- `code/`: R scripts used for data wrangling, exploration, and visualization  
- `presentation.qmd`: Final project presentation written in Quarto Markdown  
- `README.md`: Project description and structure overview (this file)

## Data Sources and Acknowledgements

The data used in this project was sourced from Kaggle:

- [Disney+ Dataset](https://www.kaggle.com/datasets/shivamb/disney-movies-and-tv-shows)  
- [Netflix Dataset](https://www.kaggle.com/datasets/shivamb/netflix-shows)  

We acknowledge the contributors of these public datasets. Some elements of the data cleaning and visualization strategies were inspired by examples from the R community and ggplot2 documentation.

## Authors

- **Dhruv Nasit** (drn5166@psu.edu)
- **Urvi Acharya** (ufa5024@psu.edu)
- **Ayushman Choudhury** (amc8837@psu.edu)

If you have any questions or suggestions, feel free to reach out via GitHub or email any of the team members.
