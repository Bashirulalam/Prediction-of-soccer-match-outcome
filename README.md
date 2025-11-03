# ⚽ Euro Football Analytics – Predicting Goals & Match Outcomes  
**Erasmus+ Blended Intensive Program (BIP) – Advanced Sports Analytics**

This project analyzes UEFA European Championship (Euro) data to understand key factors influencing match performance and to predict goals and match outcomes. The analysis applies both classical statistical methods and machine learning models to data from Euro 2008–2024.

---

## 📌 Project Overview

The objective of this project is to:

✅ Identify key variables that influence goal scoring in Euro tournaments  
✅ Predict expected goals for each team using statistical and ML models  
✅ Convert goal predictions into match outcome probabilities  
✅ Compare model performance using **Rank Probability Score (RPS)**  

The project was conducted as part of the **Erasmus+ BIP Advanced Sports Analytics program** in collaboration with students from multiple European universities.

---

## 📂 Dataset

- **Training Data:** Euro 2008–2020  
- **Test Data:** Euro 2024  
- **Observation Level:** Each row represents a *team’s performance in a match*  
- **Target Variable:** Goals scored (goal difference representation)  

### Features Used  
| Feature | Description |
|--------|--------------|
| `GDP` | Country’s GDP per capita |
| `MarketValue` | Squad market value |
| `FifaRank` | FIFA ranking prior to the tournament |
| `UefaPoints` | UEFA coefficient points |
| `CLPlayers` | # of Champions League semifinalists in squad |
| `GroupStage` | 1 = Group stage, 0 = Knockout |

---

## 🔍 Exploratory Data Analysis – Key Insights

| Insight | Summary |
|---------|----------|
| Goal Difference Trend | Avg. goal difference stable between **1.1–1.3** across tournaments |
| Market Value Effect | Higher squad value → higher average goal difference |
| Champions League Experience | Positive effect especially above 6th decile |
| Performance Drivers | Financial strength explains performance better than ranking metrics |

---

## 🧠 Models Applied

### 🔸 **1. Poisson Regression (GLM)**  
Used for goal prediction as a count variable.

**Results:**  
- Significant predictors: **GDP**, **MarketValue**  
- Test MAE: **0.732**  
- Reduced model (GDP + MarketValue) kept performance (**MAE: 0.731**) with lower AIC  

> Interpretation: Financial strength is more predictive than FIFA/UEFA rankings.

---

### 🌳 **2. Regression Tree (rpart)**  
- MarketValue = most influential variable  
- Train/Test Performance: **MAE: 0.779 / 0.784**, **RMSE: 0.971 / 1.055**

---

### 🌲 **3. Conditional Inference Forest (cforest)**  
- More robust model than a single tree  
- Cross-validated using leave-one-tournament-out approach  
- **RPS:**  
  - cforest: **0.193**  
  - rpart tree: **0.201**  

> ✅ **Lower RPS is better** → cforest provides better-calibrated outcome probabilities.

---

## 🏆 Final Conclusion

- **Market Value** and **GDP** are the strongest predictors of performance in Euros.  
- Ranking metrics (FIFA & UEFA points) add limited predictive value.  
- **cforest** produces the most reliable match outcome probabilities.  
- Future improvements: incorporate home/away effect, opponent strength, xG, and hierarchical models.

---

## 📁 Project Structure


