# IPL 2016 Analysis using R

This project provides an end-to-end analysis of the 2016 Indian Premier League (IPL) season using ball-by-ball and match-level data.

The goal of this analysis is to:
- Identify top batsmen and bowlers
- Evaluate batting performance using total runs, strike rate, average, and boundaries
- Analyze bowler performance using wickets and economy rate
- Study player consistency through match-by-match scoring
- Compare Virat Kohli vs David Warner performance
- Determine team standings using points table
- Build a basic MVP scoring model

---

## Project Structure

IPL-2016-Analysis/
```
├── data/
│   ├── matches.csv
│   └── deliveries.csv
├── ipl_2016_analysis.R
├── README.md
└── .gitignore
```

---

## Tools and Libraries Used

- R Programming
- tidyverse
- sqldf
- janitor
- ggplot2
- ggeasy
- ggthemes

---

## How to Run the Project

1. Place `matches.csv` and `deliveries.csv` inside the `data` folder
2. Open the script in RStudio
3. Run the entire script:

```r
source("ipl_2016_linear_analysis.R")
```

The script automatically generates all charts and analysis results.

---

## Analysis Overview

This project includes:

- Batting statistics for the season
- Bowling statistics for the season
- Per-innings performance for both batsmen and bowlers
- Virat Kohli performance progression charts
- Kohli vs Warner cumulative scoring comparison
- Team wins and losses visualization
- MVP player scoring model

---

## Author

Ankasandra Naveen Kumar Karthik  
ALY6000 — Introduction to Analytics  
Northeastern University (2023)
