# IPL Data Analysis (2008–2024)

An in-depth exploratory data analysis (EDA) and visualization project using IPL datasets from Kaggle. This project dives into team rivalries, player performances, venue stats, and key match moments to uncover trends and insights from over 15 years of IPL history.

## Project Structure

```
IPL_Analysis/
│
├── data/                # Raw and cleaned IPL datasets
├── plots/               # Exported visualizations (PNG/PDF)
├── scripts/             # R scripts for analysis and plotting
├── README.md            # Project documentation
└── IPL_Analysis.Rproj   # RStudio project file
```

## Tools & Libraries

- `tidyverse` – data manipulation and plotting
- `ggplot2` – visualizations
- `dplyr`, `tidyr` – wrangling
- `viridis` – color palettes
- `patchwork` – combining plots
- `lubridate` – date-time parsing
- `usethis` – Git/GitHub integration

## Analysis Highlights

- **Top Rivalries** – Most frequently played team matchups
- **Top Venues** – Stadiums with the most hosted matches
- **Most Sixes & Fours** – Aggressive batters through the years
- **Dot Ball Impact** – Bowlers who kept things tight
- **Clutch Performers** – Best under pressure (Death Overs, Close Finishes)
- **Scoring by Phases** – Powerplay vs Death Overs comparisons

##️ Dataset

Sourced from [Kaggle IPL Dataset](https://www.kaggle.com/datasets), including:

- `matches.csv` – match-level data (venue, date, winner, toss, etc.)
- `deliveries.csv` – ball-by-ball data (runs, wickets, players)


## How to Run

1. Clone this repo:
   ```bash
   git clone https://github.com/kalpanaviki/IPL_Analysis.git
   ```

2. Open in RStudio as a project:
   ```r
   setwd("IPL_Analysis")
   ```

3. Run `scripts/01_data_cleaning.R` and proceed through analysis scripts step by step.

## Contributing

Feel free to fork and extend this project with your own visualizations, machine learning models, or web dashboards!

## Next Step

- [ ] Add a Shiny Dashboard
- [ ] Perform Player Clustering
- [ ] Predictive Modeling for Match Outcomes




