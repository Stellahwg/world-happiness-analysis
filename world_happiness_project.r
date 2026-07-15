# ==============================================================================
# WORLD HAPPINESS PROJECT: CULTURAL & METHODOLOGICAL ANALYSIS
# Core Question: Why are some countries significantly happier than their 
# economic situation would predict?
# ==============================================================================

# --- PART 0 - ENVIRONMENT SETUP & DEPENDENCY RESOLUTION ---
# Automatically install and load required libraries to ensure reproducibility
required_packages <- c("car", "caret", "randomForest", "rpart", "rpart.plot")
new_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
if (length(new_packages)) {
  install.packages(new_packages)
}

library(car)
library(caret)
library(rpart)
library(rpart.plot)
library(randomForest)


# --- PART 1 - CORE DATA ACQUISITION & INITIAL CLEANING ---

# Load the primary World Happiness dataset
happiness <- read.csv("data/processed/happiness_master.csv")

# Inspect the initial structure of the dataset
cat("Initial Happiness Dataset Structure:\n")
str(happiness)

# Check for missing or corrupted records in the critical 'Corruption' column
cat("\nChecking for missing values in Corruption before conversion:\n")
missing_corruption <- sum(is.na(happiness$Corruption) | 
                            happiness$Corruption == "N/A" | 
                            happiness$Corruption == "")
print(missing_corruption)

# Convert Corruption to a numeric format (with warning handling for non-numeric strings)
happiness$Corruption <- as.numeric(as.character(happiness$Corruption))

# Display summary statistics to verify successful conversion
summary(happiness$Corruption)


# --- PART 2 - REGIONAL CLASSIFICATION (FEATURE ENGINEERING) ---
# We categorize countries into distinct cultural and geographic regions
happiness$Region <- "Other"

# Nordic Countries
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

# Verify regional distribution
cat("\nObservations per defined Region:\n")
print(table(happiness$Region))


# --- PART 3 - DETAILED HOFSTEDE CULTURAL DATA INTEGRATION ---
# Integrating Hofstede's multi-dimensional cultural dataset

# 1. Load raw Hofstede dataset (Semicolon separated as exported from official sources)
hofstede_raw <- read.csv("6-dimensions-for-website-2015-08-16.csv", sep = ";", stringsAsFactors = FALSE)

cat("\nRaw Hofstede Dataset Dimensions:\n")
print(dim(hofstede_raw))

# 2. Detailed Data Cleaning: Hofstede uses '#NULL!' for missing entries.
# We must locate and explicitly convert these text strings into proper R 'NA' values.
cat("\nAnalyzing missing values in Hofstede cultural variables:\n")
cat("Missing Individualism (idv) strings: ", sum(hofstede_raw$idv == "#NULL!"), "\n")
cat("Missing Uncertainty Avoidance (uai) strings: ", sum(hofstede_raw$uai == "#NULL!"), "\n")
cat("Missing Indulgence (ivr) strings: ", sum(hofstede_raw$ivr == "#NULL!"), "\n")

# Coerce '#NULL!' strings to real logical NAs, then cast to numeric types
hofstede_raw$idv <- as.numeric(ifelse(hofstede_raw$idv == "#NULL!", NA, hofstede_raw$idv))
hofstede_raw$uai <- as.numeric(ifelse(hofstede_raw$uai == "#NULL!", NA, hofstede_raw$uai))
hofstede_raw$ivr <- as.numeric(ifelse(hofstede_raw$ivr == "#NULL!", NA, hofstede_raw$ivr))

# 3. Structural Selection: Extract and rename relevant features
# idv = Individualism, uai = Uncertainty Avoidance, ivr = Indulgence
hofstede_clean <- hofstede_raw[, c("country", "idv", "uai", "ivr")]
colnames(hofstede_clean) <- c("Country", "Individualism", "UncertaintyAvoidance", "Indulgence")

# Display global vs regional descriptive differences (diagnostic step)
cat("\nGlobal Hofstede Cultural Means:\n")
print(colMeans(hofstede_clean[, 2:4], na.rm = TRUE))

cat("\nExample Colombia Cultural Profile (Latin American Model):\n")
print(hofstede_clean[hofstede_clean$Country == "Colombia", ])

# 4. Master Merge: Combine Happiness and Cultural dimensions
# An Inner Join automatically filters out non-matching regional/untracked entities
happiness_extended <- merge(happiness, hofstede_clean, by = "Country")

cat("\nCombined Master Dataset Dimensions after Merge:\n")
print(dim(happiness_extended))


# --- PART 4 - ENSURING MODEL COMPARABILITY (DATA HARMONIZATION) ---
# To guarantee a completely fair comparison between LM, Trees, and Forests, 
# all models must run on the exact same dataset. We filter out any incomplete rows.

target_variables <- c("Score", "GDP", "SocialSupport", "Health", "Freedom", 
                      "Generosity", "Corruption", "Individualism", "Indulgence", "UncertaintyAvoidance")

# Create a clean, consolidated master frame
happiness_clean_master <- na.omit(happiness_extended[, c("Country", "Year", "Region", target_variables)])

cat("\nFinal Clean Master Dataset available for identical modeling comparison:\n")
cat("Total Complete Rows (N): ", nrow(happiness_clean_master), "\n")
cat("Unique Countries Modelled: ", length(unique(happiness_clean_master$Country)), "\n")


# --- PART 5 - LEAKAGE-FREE TRAIN/TEST COUNTRY GROUP SPLIT ---
# CRITICAL METHODOLOGICAL CORRECTION:
# Standard row-wise random splitting creates dependencies (temporal correlation).
# We sample unique countries to keep a country's entire timespan either in train OR test.

set.seed(123)
all_countries <- unique(happiness_clean_master$Country)
train_countries_pool <- sample(all_countries, size = 0.70 * length(all_countries))

# Partition the actual master dataset based on the country pool
train_set <- happiness_clean_master[happiness_clean_master$Country %in% train_countries_pool, ]
test_set  <- happiness_clean_master[!happiness_clean_master$Country %in% train_countries_pool, ]

cat("\nCountry-Group Split Statistics:\n")
cat("Training Set: ", length(unique(train_set$Country)), " countries, ", nrow(train_set), " observations.\n")
cat("Testing Set:  ", length(unique(test_set$Country)), " countries, ", nrow(test_set), " observations.\n")


# --- PART 6 - STAGE 1: ECONOMIC BASELINE MODEL & RESIDUAL ISOLATION ---
# Predict Happiness Score using only GDP to capture economic expectations.
# Model is trained STRICTLY on the training set to prevent any data leaks.

gdp_baseline_model <- lm(Score ~ GDP, data = train_set)

cat("\n==================================================\n")
cat("STAGE 1: ECONOMIC BASELINE MODEL SUMMARY (GDP ONLY)\n")
cat("==================================================\n")
print(summary(gdp_baseline_model))

# Calculate expectations and isolate residuals (The 'Happiness Gap')
# Training Residuals
train_set$expected_by_gdp <- predict(gdp_baseline_model, newdata = train_set)
train_set$gdp_residual    <- train_set$Score - train_set$expected_by_gdp

# Testing Residuals (estimated completely out-of-sample)
test_set$expected_by_gdp <- predict(gdp_baseline_model, newdata = test_set)
test_set$gdp_residual    <- test_set$Score - test_set$expected_by_gdp


# --- PART 7 - STAGE 2: MODEL TRAINING (EXPLAINING THE HAPPINESS GAP) ---
# We train all three competitive models on the exact same target variable and predictors.

# Define our unified predictive formula
kultur_formula <- gdp_residual ~ SocialSupport + Health + Freedom + Generosity + 
                                Corruption + Individualism + Indulgence + UncertaintyAvoidance

# 1. Multiple Linear Regression Model
cat("\nTraining Multiple Linear Regression Model...\n")
model_lm <- lm(kultur_formula, data = train_set)

# 2. Decision Tree (CART) Model
cat("Training Decision Tree Model...\n")
model_tree <- rpart(kultur_formula, data = train_set, method = "anova")

# 3. Random Forest Model
cat("Training Random Forest Model...\n")
set.seed(123)
model_rf <- randomForest(kultur_formula, data = train_set, ntree = 500, importance = TRUE)


# --- PART 8 - DETAILED MODEL EVALUATION & EXPLICIT INTERPRETATION ---

# Print detailed model outputs for the academic report
cat("\n==================================================\n")
cat("A) MULTIPLE LINEAR REGRESSION SUMMARY\n")
cat("==================================================\n")
print(summary(model_lm))

# Check for Multicollinearity in the linear model using Variance Inflation Factors (VIF)
cat("\nVariance Inflation Factors (VIF) for the Linear Model:\n")
print(vif(model_lm))

cat("\n==================================================\n")
cat("B) DECISION TREE (CART) STRUCTURE\n")
cat("==================================================\n")
print(model_tree)

# Plot the decision tree structure
rpart.plot(model_tree, type = 2, extra = 101, main = "Methodologically Correct Cultural Decision Tree")

cat("\n==================================================\n")
cat("C) RANDOM FOREST DIAGNOSTICS\n")
cat("==================================================\n")
print(model_rf)

# Display feature importance measurements
cat("\nVariable Importance Scores:\n")
print(importance(model_rf))

# Visualizing Variable Importance
varImpPlot(model_rf, main = "RF Feature Importance: Explaining the Happiness Gap")


# --- PART 9 - STAGE 2: OUT-OF-SAMPLE TEST PERFORMANCE COMPARISON ---
# Evaluating model generalizability on completely unseen countries.

# Generate predictions on the test set
test_pred_lm   <- predict(model_lm, newdata = test_set)
test_pred_tree <- predict(model_tree, newdata = test_set)
test_pred_rf   <- predict(model_rf, newdata = test_set)

# Calculate baseline metric values (Unconditioned variance of training residuals)
mean_train_residual <- mean(train_set$gdp_residual)
sst_test            <- sum((test_set$gdp_residual - mean_train_residual)^2)

# --- Metric Calculations for Linear Regression ---
sse_lm  <- sum((test_set$gdp_residual - test_pred_lm)^2)
r2_lm   <- 1 - (sse_lm / sst_test)
rmse_lm <- sqrt(mean((test_set$gdp_residual - test_pred_lm)^2))

# --- Metric Calculations for Decision Tree ---
sse_tree  <- sum((test_set$gdp_residual - test_pred_tree)^2)
r2_tree   <- 1 - (sse_tree / sst_test)
rmse_tree <- sqrt(mean((test_set$gdp_residual - test_pred_tree)^2))

# --- Metric Calculations for Random Forest ---
sse_rf  <- sum((test_set$gdp_residual - test_pred_rf)^2)
r2_rf   <- 1 - (sse_rf / sst_test)
rmse_rf <- sqrt(mean((test_set$gdp_residual - test_pred_rf)^2))

# Construct a comprehensive comparison table
evaluation_summary <- data.frame(
  Model = c("Linear Regression", "Decision Tree (CART)", "Random Forest"),
  Test_R2 = c(r2_lm, r2_tree, r2_rf),
  Test_RMSE = c(rmse_lm, rmse_tree, rmse_rf)
)

cat("\n==================================================\n")
cat("OUT-OF-SAMPLE PERFORMANCE ON UNSEEN COUNTRIES\n")
cat("==================================================\n")
print(evaluation_summary)


# --- PART 10 - METHODOLOGICALLY CLEAN GROUPED 10-FOLD CROSS-VALIDATION ---
# This manual loop guarantees:
# 1. No country-leakage (Folds are built on unique countries, not observations).
# 2. Zero residual-leakage (GDP baseline is recalculated for every fold training set).

cat("\nExecuting Leakage-Free Grouped 10-Fold Cross-Validation Loop...\n")

set.seed(123)
unique_countries_cv <- unique(happiness_clean_master$Country)
cv_folds <- createFolds(unique_countries_cv, k = 10, list = TRUE)

# Initialize vectors to hold R-Squared results for each fold
cv_r2_vector_lm   <- numeric(10)
cv_r2_vector_tree <- numeric(10)
cv_r2_vector_rf   <- numeric(10)

for (fold_idx in 1:10) {
  # Identify testing countries for this specific fold
  testing_countries <- unique_countries_cv[cv_folds[[fold_idx]]]
  
  # Split the clean master dataset
  fold_train_data <- happiness_clean_master[!happiness_clean_master$Country %in% testing_countries, ]
  fold_test_data  <- happiness_clean_master[happiness_clean_master$Country %in% testing_countries, ]
  
  # Step 1: Re-estimate Stage 1 GDP baseline strictly on fold training data
  fold_gdp_baseline <- lm(Score ~ GDP, data = fold_train_data)
  
  # Step 2: Compute isolated residuals (Happiness Gap) for training and testing fold
  fold_train_data$gdp_residual <- fold_train_data$Score - predict(fold_gdp_baseline, newdata = fold_train_data)
  fold_test_data$gdp_residual  <- fold_test_data$Score - predict(fold_gdp_baseline, newdata = fold_test_data)
  
  # Step 3: Fit our models using the newly computed residuals
  fold_model_lm   <- lm(kultur_formula, data = fold_train_data)
  fold_model_tree <- rpart(kultur_formula, data = fold_train_data, method = "anova")
  fold_model_rf   <- randomForest(kultur_formula, data = fold_train_data, ntree = 150)
  
  # Step 4: Out-of-sample predictions
  fold_pred_lm   <- predict(fold_model_lm, newdata = fold_test_data)
  fold_pred_tree <- predict(fold_model_tree, newdata = fold_test_data)
  fold_pred_rf   <- predict(fold_model_rf, newdata = fold_test_data)
  
  # Step 5: Evaluate R2 based on training fold variance reference
  fold_train_mean <- mean(fold_train_data$gdp_residual)
  sst_fold_test   <- sum((fold_test_data$gdp_residual - fold_train_mean)^2)
  
  # Save R-Squared for each competitor
  cv_r2_vector_lm[fold_idx]   <- 1 - (sum((fold_test_data$gdp_residual - fold_pred_lm)^2) / sst_fold_test)
  cv_r2_vector_tree[fold_idx] <- 1 - (sum((fold_test_data$gdp_residual - fold_pred_tree)^2) / sst_fold_test)
  cv_r2_vector_rf[fold_idx]   <- 1 - (sum((fold_test_data$gdp_residual - fold_pred_rf)^2) / sst_fold_test)
}

# Print average Cross-Validation results
cat("\n==================================================\n")
cat("FINAL LEAKAGE-FREE GROUPED CROSS-VALIDATION COMPARISON\n")
cat("==================================================\n")
cat("Average Linear Regression CV R2: ", mean(cv_r2_vector_lm), "\n")
cat("Average Decision Tree (CART) CV R2: ", mean(cv_r2_vector_tree), "\n")
cat("Average Random Forest CV R2:      ", mean(cv_r2_vector_rf), "\n")


# --- PART 11 - ROBUSTNESS CHECK: DIRECT MODEL SPECIFICATION ---
# As recommended in academic methodology, we test a single-stage model 
# to ensure our cultural effects are not an artifact of the residual design.

direct_formula <- Score ~ GDP + SocialSupport + Health + Freedom + Generosity + 
                          Corruption + Individualism + Indulgence + UncertaintyAvoidance

direct_regression_model <- lm(direct_formula, data = train_set)

cat("\n==================================================\n")
cat("ROBUSTNESS CHECK: DIRECT SINGLE-STAGE REGRESSION\n")
cat("==================================================\n")
print(summary(direct_regression_model))


# --- PART 12 - EXPORT ARCHIVING ---
# Save the final cleaned and integrated dataset for group distribution
write.csv(happiness_clean_master, "report/final_clean_cultural_results.csv", row.names = FALSE)
cat("\nExecution complete. Datasets successfully saved to report/final_clean_cultural_results.csv.\n")