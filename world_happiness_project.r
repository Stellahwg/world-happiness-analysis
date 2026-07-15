# ==========================================
# WORLD HAPPINESS PROJECT
# Core Question: Why are some countries significantly 
# happier than their economic situation would predict?
# ==========================================

# --- PART 1 - LOAD DATA ---

happiness <- read.csv("data/processed/happiness_master.csv")

# Count and document missing values before conversion
cat("Number of missing values in Corruption before conversion:\n")
print(sum(is.na(happiness$Corruption) | happiness$Corruption == "N/A" | happiness$Corruption == ""))

happiness$Corruption <- as.numeric(as.character(happiness$Corruption))

str(happiness)
summary(happiness$Corruption)
dim(happiness)
head(happiness)


# --- PART 2 - REGION VARIABLE ---

happiness$Region <- "Other"

# Nordic
happiness$Region[happiness$Country %in% c("Finland", "Denmark", "Norway", "Sweden", "Iceland")] <- "Nordic"

# North America
happiness$Region[happiness$Country %in% c("Canada", "United States")] <- "NorthAmerica"

# Latin America
happiness$Region[happiness$Country %in% c(
  "Mexico", "Costa Rica", "Guatemala", "Panama", "Nicaragua", "Honduras", 
  "El Salvador", "Colombia", "Venezuela", "Peru", "Chile", "Argentina", 
  "Brazil", "Uruguay", "Paraguay", "Bolivia", "Ecuador", "Dominican Republic"
)] <- "LatinAmerica"

# Western Europe
happiness$Region[happiness$Country %in% c(
  "Germany", "France", "Belgium", "Netherlands", "Austria", "Switzerland", 
  "Ireland", "Luxembourg", "United Kingdom", "Spain", "Portugal", "Italy"
)] <- "WesternEurope"

# East Asia
happiness$Region[happiness$Country %in% c("China", "Japan", "South Korea", "Hong Kong", "Singapore", "Taiwan")] <- "EastAsia"

# Middle East
happiness$Region[happiness$Country %in% c(
  "Israel", "Saudi Arabia", "United Arab Emirates", "Qatar", "Kuwait", 
  "Bahrain", "Oman", "Iran", "Iraq", "Jordan", "Lebanon"
)] <- "MiddleEast"

# Sub-Saharan Africa
happiness$Region[happiness$Country %in% c(
  "South Africa", "Botswana", "Nigeria", "Kenya", "Uganda", "Tanzania", 
  "Rwanda", "Zimbabwe", "Malawi", "Ethiopia", "Ghana", "Zambia", 
  "Namibia", "Mozambique", "Benin", "Sierra Leone"
)] <- "SubSaharanAfrica"


# --- PART 3 - INITIAL SPLIT ---

set.seed(123)
train_index <- sample(1:nrow(happiness), size = 0.7 * nrow(happiness))


# --- PART 4 - ECONOMIC BASELINE MODEL ---
# Predict Score using only GDP to establish economic expectations

train_initial <- happiness[train_index, ]
gdp_model <- lm(Score ~ GDP, data = train_initial)

cat("\n====================\n")
cat("ECONOMIC MODEL RESULTS (GDP ONLY)\n")
cat("====================\n")
summary(gdp_model)

# Calculate economic residuals (the 'Happiness Gap')
happiness$expected_by_gdp <- predict(gdp_model, newdata = happiness)
happiness$gdp_residual <- happiness$Score - happiness$expected_by_gdp


# --- PART 4.1 - DEFINE SUBSETS WITH RESIDUALS ---
# Recreate train and test subsets containing the new residual column

train_subset <- happiness[train_index, ]
test_subset <- happiness[-train_index, ]

cat("\nTraining observations:\n")
print(nrow(train_subset))

cat("\nTesting observations:\n")
print(nrow(test_subset))


# --- PART 5 - EXPLAINING THE RESIDUALS (MULTIPLE REGRESSION) ---
# We explain the happiness gap using non-economic variables

explanation_model <- lm(
  gdp_residual ~ SocialSupport + Health + Freedom + Generosity + Corruption + factor(Year),
  data = train_subset
)

cat("\n====================\n")
cat("EXPLAINING ECONOMIC RESIDUALS (REGRESSION)\n")
cat("====================\n")
summary(explanation_model)


# --- PART 5.1 - MULTICOLLINEARITY (VIF) ---

library(car)

# We check VIF to assess multicollinearity among our explanatory features
vif_model <- lm(
  gdp_residual ~ SocialSupport + Health + Freedom + Generosity + Corruption,
  data = train_subset
)

cat("\n====================\n")
cat("VARIANCE INFLATION FACTORS\n")
cat("====================\n")
print(vif(vif_model))


# --- PART 6 - TEST SET PERFORMANCE ---

predictions <- predict(explanation_model, newdata = test_subset)

SSE <- sum((test_subset$gdp_residual - predictions)^2)
SST <- sum((test_subset$gdp_residual - mean(train_subset$gdp_residual))^2)
OSR2 <- 1 - SSE / SST

cat("\n====================\n")
cat("OUT OF SAMPLE R2 (EXPLANATION MODEL)\n")
cat("====================\n")
print(OSR2)


# --- PART 7 - DETAILED RESIDUAL ANALYSIS ---

cat("\n====================\n")
cat("HAPPIER THAN ECONOMIC EXPECTATIONS (TOP COUNTRY-YEARS)\n")
cat("====================\n")
head(
  happiness[order(-happiness$gdp_residual), c("Country", "Year", "Score", "expected_by_gdp", "gdp_residual")],
  15
)

cat("\n====================\n")
cat("LESS HAPPY THAN ECONOMIC EXPECTATIONS (BOTTOM COUNTRY-YEARS)\n")
cat("====================\n")
head(
  happiness[order(happiness$gdp_residual), c("Country", "Year", "Score", "expected_by_gdp", "gdp_residual")],
  15
)


# --- PART 8 - AGGREGATED COUNTRY RESIDUALS ---
# Calculate average happiness gap per country across all years

country_residuals <- aggregate(gdp_residual ~ Country, data = happiness, mean)
country_residuals <- country_residuals[order(-country_residuals$gdp_residual), ]

cat("\n====================\n")
cat("TOP 15 OVERPERFORMING COUNTRIES (ALL-TIME AVERAGE)\n")
cat("====================\n")
head(country_residuals, 15)

cat("\n====================\n")
cat("TOP 15 UNDERPERFORMING COUNTRIES (ALL-TIME AVERAGE)\n")
cat("====================\n")
tail(country_residuals, 15)


# --- PART 9 - REGIONAL ANALYSIS & SIGNIFICANCE ---

# Descriptive analysis
region_results <- aggregate(gdp_residual ~ Region, data = happiness, mean)
region_results <- region_results[order(-region_results$gdp_residual), ]

cat("\n====================\n")
cat("AVERAGE ECONOMIC OVERPERFORMANCE BY REGION\n")
cat("====================\n")
print(region_results)

# Statistical significance test (ANOVA)
anova_model <- lm(gdp_residual ~ Region, data = happiness)

cat("\n====================\n")
cat("ANOVA: ARE REGIONAL DIFFERENCES STATISTICALLY SIGNIFICANT?\n")
cat("====================\n")
print(anova(anova_model))


# --- PART 10 - SAVE RESULTS ---

write.csv(happiness, "report/final_happiness_results.csv", row.names = FALSE)
cat("\nResults saved successfully.\n")


# --- PART 11 - DECISION TREE ---

library(rpart)
library(rpart.plot)

tree_model <- rpart(
  gdp_residual ~ SocialSupport + Health + Freedom + Generosity + Corruption,
  data = train_subset,
  method = "anova"
)

cat("\n====================\n")
cat("DECISION TREE (RESIDUAL ANALYSIS)\n")
cat("====================\n")
print(tree_model)

# Plot Tree
rpart.plot(tree_model, type = 2, extra = 101)

# Predict and calculate R2
tree_predictions <- predict(tree_model, newdata = test_subset)
tree_SSE <- sum((test_subset$gdp_residual - tree_predictions)^2)
tree_SST <- sum((test_subset$gdp_residual - mean(train_subset$gdp_residual))^2)
tree_R2 <- 1 - tree_SSE / tree_SST

cat("\n====================\n")
cat("DECISION TREE R2\n")
cat("====================\n")
print(tree_R2)


# --- PART 12 - RANDOM FOREST COMPARISON ---

# Omit NAs for Random Forest models
train_rf <- na.omit(train_subset)
test_rf <- na.omit(test_subset)

library(randomForest)

set.seed(123)

# FOREST A: Explaining the Happiness Gap (Residuals)
forest_model_residuals <- randomForest(
  gdp_residual ~ SocialSupport + Health + Freedom + Generosity + Corruption,
  data = train_rf,
  ntree = 500,
  importance = TRUE
)

# FOREST B: Predicting Original Happiness Score (Full Model with GDP)
forest_model_full <- randomForest(
  Score ~ GDP + SocialSupport + Health + Freedom + Generosity + Corruption,
  data = train_rf,
  ntree = 500,
  importance = TRUE
)

cat("\n====================\n")
cat("RANDOM FOREST A (RESIDUALS)\n")
cat("====================\n")
print(forest_model_residuals)

cat("\n====================\n")
cat("RANDOM FOREST B (FULL MODEL WITH GDP)\n")
cat("====================\n")
print(forest_model_full)

# Evaluate Forest A (Residuals)
forest_predictions <- predict(forest_model_residuals, newdata = test_rf)
forest_SSE <- sum((test_rf$gdp_residual - forest_predictions)^2)
forest_SST <- sum((test_rf$gdp_residual - mean(train_rf$gdp_residual))^2)
forest_R2 <- 1 - forest_SSE / forest_SST

cat("\n====================\n")
cat("RANDOM FOREST A (RESIDUALS) R2\n")
cat("====================\n")
print(forest_R2)

# Compare Variable Importance Plots
cat("\n====================\n")
cat("VARIABLE IMPORTANCE (RESIDUALS VS FULL SCORE)\n")
cat("====================\n")
print(importance(forest_model_residuals))
print(importance(forest_model_full))

# Plots
par(mfrow = c(1, 2))
varImpPlot(forest_model_residuals, main = "RF: Explaining the Happiness Gap")
varImpPlot(forest_model_full, main = "RF: Predicting Original Score")
par(mfrow = c(1, 1))


# --- PART 13 - K-FOLD CROSS-VALIDATION ---

library(caret)

cv_control <- trainControl(method = "cv", number = 10)

set.seed(123)

# 1. Cross-validate Linear Regression (Residuals)
cv_lm <- train(
  gdp_residual ~ SocialSupport + Health + Freedom + Generosity + Corruption, 
  data = train_rf, 
  method = "lm", 
  trControl = cv_control
)

# 2. Cross-validate CART Model (Residuals)
cv_cart <- train(
  gdp_residual ~ SocialSupport + Health + Freedom + Generosity + Corruption, 
  data = train_rf, 
  method = "rpart", 
  trControl = cv_control
)

# 3. Cross-validate Random Forest Model (Residuals)
cv_rf <- train(
  gdp_residual ~ SocialSupport + Health + Freedom + Generosity + Corruption, 
  data = train_rf, 
  method = "rf", 
  trControl = cv_control
)

cat("\n====================\n")
cat("CROSS VALIDATION R-SQUARED COMPARISON\n")
cat("====================\n")

cat("\nLinear Regression CV R-Squared:\n")
print(mean(cv_lm$results$Rsquared))

cat("\nCART (Decision Tree) CV R-Squared:\n")
print(mean(cv_cart$results$Rsquared))

cat("\nRandom Forest CV R-Squared:\n")
print(mean(cv_rf$results$Rsquared))