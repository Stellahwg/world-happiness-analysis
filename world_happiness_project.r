# ==========================================
# WORLD HAPPINESS PROJECT: CULTURAL EXTENSION
# Core Question: Why are some countries significantly 
# happier than their economic situation would predict?
# ==========================================

# --- PART 0 - INSTALL MISSING PACKAGES ---
# Automatically install required packages if they are not already installed
required_packages <- c("car", "caret", "randomForest", "rpart", "rpart.plot")
new_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
if (length(new_packages)) install.packages(new_packages)

# --- PART 1 - LOAD CORE DATA ---

happiness <- read.csv("data/processed/happiness_master.csv")

# Count and document missing values before conversion
cat("Number of missing values in Corruption before conversion:\n")
print(sum(is.na(happiness$Corruption) | happiness$Corruption == "N/A" | happiness$Corruption == ""))

# Convert Corruption with a warning check
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


# --- PART 3 - INITIAL SPLIT FOR BASELINE MODEL ---

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


# --- PART 5 - REGIONAL ANALYSIS & SIGNIFICANCE (ANOVA) ---

# Descriptive analysis of regional performance
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


# --- PART 6 - HOFSTEDE CULTURAL EXTENSION ---
# Introduction of external cultural metrics to explain regional gaps

# 1. Read the raw Hofstede dataset (Semicolon separated)
hofstede_raw <- read.csv("6-dimensions-for-website-2015-08-16.csv", sep = ";", stringsAsFactors = FALSE)

# 2. Convert #NULL! string expressions to actual R NA values and coerce columns to numeric type
hofstede_raw$idv <- as.numeric(ifelse(hofstede_raw$idv == "#NULL!", NA, hofstede_raw$idv))
hofstede_raw$uai <- as.numeric(ifelse(hofstede_raw$uai == "#NULL!", NA, hofstede_raw$uai))
hofstede_raw$ivr <- as.numeric(ifelse(hofstede_raw$ivr == "#NULL!", NA, hofstede_raw$ivr))

# 3. Keep only targeted cultural indicators and match column naming structure
# idv = Individualism, uai = Uncertainty Avoidance, ivr = Indulgence
hofstede_clean <- hofstede_raw[, c("country", "idv", "uai", "ivr")]
colnames(hofstede_clean) <- c("Country", "Individualism", "UncertaintyAvoidance", "Indulgence")

cat("\n====================\n")
cat("GLOBAL CULTURAL AVERAGES VS LATIN AMERICA EXAMPLES\n")
cat("====================\n")
cat("Global Means:\n")
print(colMeans(hofstede_clean[, 2:4], na.rm = TRUE))
cat("\nExample Colombia:\n")
print(hofstede_clean[hofstede_clean$Country == "Colombia", ])
cat("\nExample Costa Rica:\n")
print(hofstede_clean[hofstede_clean$Country == "Costa Rica", ])

# 4. Merge with the base dataset (Inner Join filters out unmatched regional codes automatically)
happiness_extended <- merge(happiness, hofstede_clean, by = "Country")

cat("\nObservations available for cultural analysis:\n")
print(dim(happiness_extended))


# --- PART 7 - SUBSET SPLITTING WITH CULTURAL DATA ---

set.seed(123)
train_extended_idx <- sample(1:nrow(happiness_extended), size = 0.7 * nrow(happiness_extended))
train_ext <- happiness_extended[train_extended_idx, ]
test_ext <- happiness_extended[-train_extended_idx, ]

cat("\nExtended Training observations:\n")
print(nrow(train_ext))
cat("Extended Testing observations:\n")
print(nrow(test_ext))


# --- PART 8 - LINEAR REGRESSION EXPLAINING RESIDUALS WITH CULTURE ---

kultur_model <- lm(
  gdp_residual ~ SocialSupport + Health + Freedom + Generosity + Corruption + 
                 Individualism + Indulgence + UncertaintyAvoidance, 
  data = train_ext
)

cat("\n====================\n")
cat("DOES CULTURE EXPLAIN THE HAPPINESS GAP? (REGRESSION WITH HOFSTEDE)\n")
cat("====================\n")
summary(kultur_model)


# --- PART 8.1 - MULTICOLLINEARITY TEST (VIF) ---

library(car)
cat("\n====================\n")
cat("VARIANCE INFLATION FACTORS (CULTURAL EXTENDED MODEL)\n")
cat("====================\n")
# Calculate VIF on the numeric features without factor levels to assess collinearity stability
print(vif(kultur_model))


# --- PART 8.2 - LINEAR MODEL TEST PERFORMANCE ---

predictions_lm <- predict(kultur_model, newdata = test_ext)

SSE_lm <- sum((test_ext$gdp_residual - predictions_lm)^2, na.rm = TRUE)
SST_lm <- sum((test_ext$gdp_residual - mean(train_ext$gdp_residual, na.rm = TRUE))^2, na.rm = TRUE)
OSR2_lm <- 1 - SSE_lm / SST_lm

cat("\n====================\n")
cat("OUT OF SAMPLE R2 (CULTURAL REGRESSION MODEL)\n")
cat("====================\n")
print(OSR2_lm)


# --- PART 9 - SAVE DETAILED CULTURAL RESULTS ---

write.csv(happiness_extended, "report/final_happiness_cultural_results.csv", row.names = FALSE)
cat("\nCultural analytical results saved successfully.\n")


# --- PART 10 - DECISION TREE WITH CULTURE ---

library(rpart)
library(rpart.plot)

tree_model <- rpart(
  gdp_residual ~ SocialSupport + Health + Freedom + Generosity + Corruption + 
                 Individualism + Indulgence + UncertaintyAvoidance,
  data = train_ext,
  method = "anova"
)

cat("\n====================\n")
cat("CULTURAL DECISION TREE (RESIDUAL ANALYSIS)\n")
cat("====================\n")
print(tree_model)

# Plot the built tree architecture
rpart.plot(tree_model, type = 2, extra = 101, main = "Decision Tree with Cultural Dimensions")

# Evaluate tree performance via out-of-sample R2
tree_predictions <- predict(tree_model, newdata = test_ext)
tree_SSE <- sum((test_ext$gdp_residual - tree_predictions)^2, na.rm = TRUE)
tree_SST <- sum((test_ext$gdp_residual - mean(train_ext$gdp_residual, na.rm = TRUE))^2, na.rm = TRUE)
tree_R2 <- 1 - tree_SSE / tree_SST

cat("\n====================\n")
cat("CULTURAL DECISION TREE TEST R2\n")
cat("====================\n")
print(tree_R2)


# --- PART 11 - RANDOM FOREST WITH CULTURE ---

library(randomForest)

# Omit missing records across the targeted analytical variables for strict Random Forest compatibility
forest_vars <- c("gdp_residual", "Score", "GDP", "SocialSupport", "Health", 
                 "Freedom", "Generosity", "Corruption", "Individualism", 
                 "Indulgence", "UncertaintyAvoidance")

train_rf_ext <- na.omit(train_ext[, forest_vars])
test_rf_ext <- na.omit(test_ext[, forest_vars])

set.seed(123)

# FOREST A: Explaining the Happiness Gap (Unerringly targets the residuals via full feature space)
forest_model_residuals <- randomForest(
  gdp_residual ~ SocialSupport + Health + Freedom + Generosity + Corruption + 
                 Individualism + Indulgence + UncertaintyAvoidance,
  data = train_rf_ext,
  ntree = 500,
  importance = TRUE
)

# FOREST B: Predicting Original Happiness Score (Comprehensive reference mapping including economy)
forest_model_full <- randomForest(
  Score ~ GDP + SocialSupport + Health + Freedom + Generosity + Corruption + 
          Individualism + Indulgence + UncertaintyAvoidance,
  data = train_rf_ext,
  ntree = 500,
  importance = TRUE
)

cat("\n====================\n")
cat("CULTURAL RANDOM FOREST A (RESIDUALS)\n")
cat("====================\n")
print(forest_model_residuals)

cat("\n====================\n")
cat("CULTURAL RANDOM FOREST B (FULL MODEL WITH GDP)\n")
cat("====================\n")
print(forest_model_full)

# Evaluate performance metrics of Forest A on unseen data
forest_predictions <- predict(forest_model_residuals, newdata = test_rf_ext)
forest_SSE <- sum((test_rf_ext$gdp_residual - forest_predictions)^2)
forest_SST <- sum((test_rf_ext$gdp_residual - mean(train_rf_ext$gdp_residual))^2)
forest_R2 <- 1 - forest_SSE / forest_SST

cat("\n====================\n")
cat("CULTURAL RANDOM FOREST A TEST R2\n")
cat("====================\n")
print(forest_R2)

# Comparative diagnostics of structural feature importance
cat("\n====================\n")
cat("VARIABLE IMPORTANCE WITH CULTURAL DIMENSIONS\n")
cat("====================\n")
print(importance(forest_model_residuals))
print(importance(forest_model_full))

# Render relative metric importance outputs graphically
par(mfrow = c(1, 2))
varImpPlot(forest_model_residuals, main = "RF: Culture & Happiness Gap")
varImpPlot(forest_model_full, main = "RF: Full Score Prediction")
par(mfrow = c(1, 1))


# --- PART 12 - K-FOLD CROSS-VALIDATION WITH CULTURE ---

library(caret)

cv_control <- trainControl(method = "cv", number = 10)

set.seed(123)

# 1. Cross-validate Linear Regression Model with Cultural Metrics
cv_lm <- train(
  gdp_residual ~ SocialSupport + Health + Freedom + Generosity + Corruption + 
                 Individualism + Indulgence + UncertaintyAvoidance, 
  data = train_rf_ext, method = "lm", trControl = cv_control
)

# 2. Cross-validate CART Model with Cultural Metrics
cv_cart <- train(
  gdp_residual ~ SocialSupport + Health + Freedom + Generosity + Corruption + 
                 Individualism + Indulgence + UncertaintyAvoidance, 
  data = train_rf_ext, method = "rpart", trControl = cv_control
)

# 3. Cross-validate Random Forest Model with Cultural Metrics
cv_rf <- train(
  gdp_residual ~ SocialSupport + Health + Freedom + Generosity + Corruption + 
                 Individualism + Indulgence + UncertaintyAvoidance, 
  data = train_rf_ext, method = "rf", trControl = cv_control
)

cat("\n====================\n")
cat("CROSS VALIDATION R-SQUARED COMPARISON (WITH CULTURE)\n")
cat("====================\n")
cat("Linear Regression CV R-Squared: ", mean(cv_lm$results$Rsquared, na.rm = TRUE), "\n")
cat("CART (Decision Tree) CV R-Squared: ", mean(cv_cart$results$Rsquared, na.rm = TRUE), "\n")
cat("Random Forest CV R-Squared: ", mean(cv_rf$results$Rsquared, na.rm = TRUE), "\n")