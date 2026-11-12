# SDSU Salary Data Project

An R regression project using San Diego State University staff salary data (36 position titles with job grades and estimated salaries). It estimates a fair salary for a job that falls between two grades.

## Repository Contents

| File | Description |
|---|---|
| `sdsu salary project.R` | R script with linear, polynomial and decision tree regression models |
| `sdsu salary data from class .csv` | Salary data by position title and job grade |

## Commit History

### `2f32a18`: Upload of the R script and dataset (2022-05-18)
- **Simple linear regression:** job grade vs. salary showed a decent relationship (75.3%). For a job between grades 9 and 10, it predicted **$72,353**, about $10K more than those grades actually pay.
- **Polynomial regression:** the best fit (96.6%). For a job between grades 9 and 10, it predicted about **$61,500–63,000**, which falls within the real pay ranges (grade 9: $53K–57K, grade 10: $59K–70K).
- **Decision tree regression:** overfit and overestimated the salary by about $20K (predicting $91,786), with outliers at the high end.
- **Conclusion:** the polynomial model is the most reliable for setting a salary between job grades.

## Tools
R, RStudio, ggplot2, caTools, rpart, randomForest
