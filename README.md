# Students' Social Media Addiction: Econometrics Analysis

## Overview
Explored a dataset on student social media habits to model impacts on daily usage, sleep, and mental health. Removed outliers (e.g., extreme usage hrs), ran descriptives, and built LMs testing interactions (e.g., Mental_Health * Conflicts) and piecewise effects (breakpoints at age=20, usage=3 hrs).

## Skills Demonstrated
- Cleaning: Boxplot-based outlier removal (e.g., ~some  rows dropped on Avg_Daily_Usage_Hours), factor conversions.
- EDA: Freq/prop tables, mosaic plots (vcd for Gender/Relationship/Addiction associations), desc stats (mean/median/SD via summarise).
- Modeling: LM baselines, interaction terms, nested F-tests/AIC/BIC for selection; piecewise (segmented pkg for non-linear Age/Usage/Conflicts).
- Diagnostics: Plots (par mfrow), Cook's D/dfbetas for influencers, Shapiro for residuals.

## Key Findings
- Descriptives: Avg usage ~3.2 hrs (SD=1.1); 55% female, High addiction score in 30%.
- Interactions: Mental_Health * Conflicts significant (p<0.05, higher conflicts amplify low mental health effects on usage).
- Piecewise: Sleep drops sharply post-usage=3 hrs (slope change ); Age breakpoint ~20 (younger less affected by gender).
- Model Fit: Nested F-test favors interactions (p<0.01); AIC lower for piecewise vs. linear.

## Files
- `social_media_addiction_analysis.R`: Full script (EDA, models, diags).

## Report
- [Full PDF](https://drive.google.com/file/d/1t37RHD8qdA2cK8Q7PeWLFhLSvCoG5zg-/view?usp=drive_link)

## Dataset
"Students Social Media Addiction.csv" (https://www.kaggle.com/datasets/adilshamim8/social-media-addiction-vs-relationships).
