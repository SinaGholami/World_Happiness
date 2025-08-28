# World Happiness Report — Data Analysis with R

This project explores the **World Happiness Report** dataset using R.  
The goal was to understand what drives the *Life Ladder* (Happiness Score) across countries, and to practice building clear, shareable visualizations for LinkedIn.  

---

# What’s inside

1. **Data Cleaning**
   - Handling missing values
   - Keeping the latest available record per country

2. **Exploratory Data Analysis (EDA)**
   - Distribution of happiness scores
   - GDP vs Happiness scatter plot (with Pearson correlation)
   - Boxplots by continent
   - Correlation heatmap

3. **Regression Modeling**
   - Linear regression with standardized coefficients
   - R² ≈ 0.80  
   - Top predictors: Social support, freedom, GDP, life expectancy

4. **Visualizations**
   - Built with `ggplot2` and `ggcorrplot`
   - Exported for sharing on LinkedIn

---

## Key Insights
- **Social support** is the strongest driver of happiness (β ≈ 0.55).  
- **Freedom** and **GDP per capita** are also significant contributors.  
- **Generosity** has a minor effect, while **perceptions of corruption** negatively impact happiness.  
- The regression model explains **~80% of variance** in the happiness score.

---

## Tech Stack
- **R** (tidyverse, ggplot2, ggcorrplot, broom)
- Dataset: World Happiness Report (2008–2023)

---

## Usage
Clone this repo and run the scripts in R or RStudio:

```bash
git clone https://github.com/yourusername/world-happiness-analysis.git
