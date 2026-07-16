# WORLD HAPPINESS PROJECT — PRESENTATION GUIDE
### Everything you need to present this project confidently

---

## THE BIG PICTURE IN ONE SENTENCE

> **"We showed that culture explains happiness better than wealth — GDP ranks only 5th when all factors compete."**

---

## THE RESEARCH QUESTION

**Why are some countries happier than their GDP would predict?**

Examples:
- Mexico is **1.4 points happier** than its GDP suggests
- Germany is **0.5 points less happy** than its GDP suggests
- Same wealth, very different happiness → something else must explain it

---

## WHAT WE DID — STEP BY STEP

### STEP 1 — Prove the gap exists (164 countries)
We took the full World Happiness Report dataset (164 countries, 2015–2019) and asked: how well does GDP alone predict happiness?

**Result:** GDP explains only **61%** of happiness. 39% is unexplained.
→ The "Happiness Gap" is real and large.

---

### STEP 2 — Find what explains the gap (62 countries)
We added **Hofstede cultural dimensions** to explain the gap.
Hofstede measures how cultures think and behave — not what they have.

Merging with Hofstede reduced coverage from 164 to **62 countries**.
We accepted this trade-off to answer our cultural question.

**The three Hofstede variables we used:**

| Variable | What it measures |
|---|---|
| **Indulgence** | How much a society allows enjoyment, fun and leisure |
| **Individualism** | How much people prioritize personal freedom over group harmony |
| **Uncertainty Avoidance** | How uncomfortable a society is with uncertainty and change |

---

### STEP 3 — Train 4 models to predict the gap
We trained 4 models from the course and compared them.

---

## THE 4 MODELS — WHAT THEY DO AND HOW THEY PERFORM

### Model 1 — Linear Regression
**What it does:** Draws a straight line through the data. Each variable gets a coefficient — you can read off exactly how much each factor contributes.

**Example output:**
- Indulgence coefficient = 0.011 → one point more Indulgence = 0.011 more happiness gap
- Freedom coefficient = 1.28 → strong positive effect

**Performance:**
- Test R² = **0.43** → explains 43% of the gap on unseen countries
- Simple, interpretable, good baseline

---

### Model 2 — Decision Tree (CART)
**What it does:** Asks a series of yes/no questions to make a prediction.

```
Is Indulgence > 51?
├── YES → Is Corruption > 0.38? → Predict: +0.82 (happier than expected)
└── NO  → Is Individualism > 22.5? → ...
```

**Performance:**
- Test R² = **0.22** → weakest model
- Why? With only 43 training countries it memorizes too specifically
- But: it visually explains HOW the model thinks → useful for presentation

---

### Model 3 — Random Forest ⭐ BEST MODEL
**What it does:** Builds 500 Decision Trees, each on slightly different data, then averages all predictions.

**Why it wins:** A single tree is unstable with small data. 500 trees averaged out = much more reliable. This is the "Wisdom of Crowds" principle from Session 5.

**Performance:**
- Test R² = **0.55** → explains 55% of the gap on unseen countries
- Test RMSE = 0.40 (lowest = best)
- Test MAE = 0.33 (lowest = best)
- CV R² = 0.35

---

### Model 4 — Gradient Boosting (GBM)
**What it does:** Also builds many trees, but sequentially — each new tree learns from the mistakes of the previous one.

**Why it underperforms here:** Boosting needs a lot of data to shine. With only 62 countries it doesn't have enough to outperform simpler models.

**Performance:**
- Test R² = **0.36**
- Better than Decision Tree, worse than Random Forest and even Linear Regression

---

## MODEL COMPARISON TABLE

| Model | Test R² | Test RMSE | Test MAE | CV R² |
|---|---|---|---|---|
| Linear Regression | 0.43 | 0.45 | 0.36 | 0.13 |
| Decision Tree | 0.22 | 0.52 | 0.42 | -0.12 |
| **Random Forest** | **0.55** | **0.40** | **0.33** | **0.35** |
| Gradient Boosting | 0.36 | 0.47 | 0.39 | 0.31 |

**What these numbers mean:**
- **R²** = how much of the gap the model explains (higher = better, max = 1.0)
- **RMSE** = average prediction error (lower = better)
- **MAE** = average absolute prediction error (lower = better, more intuitive)
- **CV R²** = R² across 10-fold cross-validation (most reliable, but noisy with small N)

---

## THE MOST IMPORTANT RESULT — VARIABLE IMPORTANCE RANKING

When all variables compete simultaneously (including GDP), this is the ranking:

| Rank | Variable | Importance | Type |
|---|---|---|---|
| 🥇 1 | **Indulgence** | 34.6 | Cultural (Hofstede) |
| 🥈 2 | **Individualism** | 27.8 | Cultural (Hofstede) |
| 🥉 3 | **Uncertainty Avoidance** | 23.1 | Cultural (Hofstede) |
| 4 | Corruption | 21.2 | WHR Social |
| **5** | **GDP** | **17.0** | **Economic** |
| 6 | Freedom | 15.9 | WHR Social |
| 7 | Generosity | 14.1 | WHR Social |
| 8 | Social Support | 12.6 | WHR Social |
| 9 | Health | 8.9 | WHR Social |

**→ The top 3 are all cultural. GDP is only 5th.**

---

## DOES HOFSTEDE ACTUALLY HELP? (ANOVA TEST)

We directly compared a model with and without Hofstede:

| Model | R² |
|---|---|
| GDP + WHR variables only | 0.705 |
| GDP + WHR + **Hofstede** | **0.781** |

**F-test p-value = 0.000000000000037 (p < 0.001 ***)**

→ Hofstede adds **+7.6% explanatory power** — statistically highly significant.

---

## KEY NUMBERS TO REMEMBER

| Number | What it means |
|---|---|
| **61%** | GDP alone explains happiness (164 countries) |
| **39%** | The unexplained Happiness Gap |
| **78%** | Explained when adding Hofstede to all WHR variables |
| **0.55** | Best model (Random Forest) explains the gap |
| **p < 0.001** | Hofstede statistically proven to help |
| **Rank 1** | Indulgence — most important factor overall |
| **Rank 5** | GDP — less important than all 3 cultural variables |

---

## WHAT EACH VARIABLE MEANS

| Variable | Simple explanation |
|---|---|
| **GDP** | Economic wealth of the country |
| **Social Support** | "Do you have someone to count on if in trouble?" |
| **Health** | Healthy life expectancy in years |
| **Freedom** | "Are you satisfied with your freedom to choose?" |
| **Generosity** | "Did you donate to charity last month?" |
| **Corruption** | "Is corruption widespread in government?" |
| **Indulgence** | How much society allows enjoyment and fun |
| **Individualism** | Personal freedom vs. group harmony |
| **Uncertainty Avoidance** | Comfort with uncertainty and change |

---

## THE SURPRISING FINDING

Most people assume: **more money = more happiness**.

Our data shows: **culture beats wealth**.

- Mexico (Indulgence = 97) → 1.4 points happier than GDP predicts
- Germany (Indulgence = 40) → slightly less happy than GDP predicts
- Same logic for Individualism and Uncertainty Avoidance

**Societies that allow enjoyment, value personal freedom, and tolerate uncertainty are systematically happier than their wealth alone would suggest.**

---

## METHODOLOGY — HOW WE AVOIDED COMMON MISTAKES

**Problem 1: Data leakage in train/test split**
Normal split: random rows → same country in train AND test
Our solution: split by country → each country fully in train OR test

**Problem 2: GDP contaminating cultural analysis**
Normal approach: all variables together → GDP dominates
Our solution: two-stage — first remove GDP effect, then model what's left

**Problem 3: Small dataset (62 countries)**
We first validated the happiness gap on 164 countries, then accepted the trade-off to 62 countries for the cultural analysis. Explicitly acknowledged as a limitation.

---

## LIKELY QUESTIONS AND ANSWERS

**"Why only 62 countries?"**
> We started with 164. Merging with Hofstede reduced it to 62 — a conscious trade-off. We first proved the gap exists on all 164 countries before going smaller.

**"Why is GDP only 5th?"**
> When cultural variables are already in the model, GDP adds less new information — because rich countries automatically tend to be individualistic and indulgent. Culture and wealth are correlated, but culture explains more of the variation.

**"Why is Decision Tree so weak?"**
> With 43 training countries, a single tree overfits — it memorizes rather than generalizes. Random Forest fixes this by averaging 500 trees.

**"Why is Health negative in the regression?"**
> Health correlates strongly with Social Support. When both are in the model, their effects partially cancel out — a known multicollinearity issue. Our VIF check confirmed it. The main findings are not affected.

**"Could you improve the model?"**
> Yes — more countries, more cultural variables (Hofstede has 6, we used 3), or adding religion and political freedom indices could explain more of the remaining 22%.

**"What is R²?"**
> How much of the variation the model explains. R² = 0.55 means the Random Forest explains 55% of why countries deviate from their GDP-predicted happiness. The remaining 45% is things like religion, history, climate — not in our dataset.

**"What is the two-stage approach?"**
> Step 1: model what GDP predicts. Step 2: look at what's left over — the gap. Step 3: model what explains the gap with cultural variables. This isolates the pure cultural effect, independent of wealth.

---

## THE STORY FOR YOUR PRESENTATION

1. **Hook:** "Most people think money buys happiness. Our data disagrees."
2. **Gap:** "GDP explains only 61% of happiness across 164 countries."
3. **Question:** "What explains the other 39%?"
4. **Answer:** "Culture — specifically how much a society allows enjoyment."
5. **Proof:** "Adding Hofstede cultural dimensions improves prediction from 70% to 78% — statistically proven with p < 0.001."
6. **Surprise:** "Indulgence is the single most important factor. GDP is only 5th."
7. **Best model:** "Random Forest explains 55% of the happiness gap on countries it has never seen."
