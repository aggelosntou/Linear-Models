# Global Development Indicators & Corruption Control — A Statistical Analysis 📊

## 📑 Overview
This project analyzes how **global development indicators** relate to **corruption control**, using data from the **World Bank (WDI, 2018)**.  
It was completed as part of a university course on **Linear Models**, with full methodology and results documented in the report [Report (GR).pdf](./Report%20(GR).pdf).

---

## 🎯 Objectives
- Explore socioeconomic and governance indicators across **120 countries**.  
- Categorize indicators into quantitative (e.g., GDP, CO₂ emissions, internet use) and qualitative (e.g., fertility categories).  
- Apply **multiple linear regression** to explain corruption control.  
- Validate model assumptions (linearity, independence, homoscedasticity, normality).  
- Use **ANOVA** to assess categorical factors like fertility and inflation classes.  
- Compare initial vs reduced models using **stepwise selection**.

---

## 🛠️ Methodology
1. **Data Preparation**: 27 variables across 120 countries, cleaned and structured.  
2. **Descriptive Statistics**: Means, variances, distributions per category.  
3. **Linear Model**: Regression of corruption control (`CorControl`) on multiple indicators.  
4. **Model Selection**: Backward stepwise regression to identify the best subset of predictors.  
5. **Diagnostics**: Residual analysis, confidence intervals, significance testing.  
6. **ANOVA**: Tested group effects of fertility categories (`FertCat`) and inflation categories (`InflCat`).  

---

## 📊 Key Results
- **Best linear model** (after stepwise selection):  
  - Predictors include **ElectrAccess, Internet use, GDPdollars, PopGrowth, Adolescent Fertility Rate, Women in Business**, among others .  
  - **R² = 0.807** (Adjusted R² = 0.790) → strong explanatory power.  
  - Residual Standard Error (RSE) = 0.44.  
  - F-statistic = 45.66, p < 2.2e−16 → highly significant.  
  - Lower AIC (157.4 vs 179.3) → better model fit after variable reduction.  

- **Significant predictors**:  
  - *Negative*: Population growth, adolescent fertility rate.  
  - *Positive*: GDP, internet usage, women in business.  

- **ANOVA findings**  :  
  - Fertility category and inflation category both had **statistically significant effects** on corruption control (p < 0.01).  
  - Interaction effect not significant.  
  - Tukey post-hoc test showed differences between fertility categories (low vs high).  

- **Interpretation**: Countries with stronger infrastructure, economic development, and gender inclusion tend to score higher in corruption control, while high fertility and rapid population growth are linked to weaker scores.  

---

## 📑 Deliverables
- Full Report (Greek): [Report (GR).pdf](./Report%20(GR).pdf)  
Contains tables, regression outputs, ANOVA results, and discussion.  

