# ==========================================
# WORLD HAPPINESS PROJECT
#
# Research Question:
# Why are some countries happier than their
# economic wealth would predict?
# ==========================================

# ==========================================
# PART 1 - LOAD DATA
# ==========================================

happiness <- read.csv(
  "data/processed/happiness_master.csv"
)

happiness$Corruption <- as.numeric(
  as.character(happiness$Corruption)
)

str(happiness)

summary(happiness$Corruption)

cat("\nDataset loaded successfully\n")

dim(happiness)

head(happiness)

# ==========================================
# PART 2 - REGION VARIABLE
# ==========================================

happiness$Region <- "Other"

# ------------------------------------------
# Nordic
# ------------------------------------------

happiness$Region[happiness$Country %in%
  c(
    "Finland",
    "Denmark",
    "Norway",
    "Sweden",
    "Iceland"
  )] <- "Nordic"

# ------------------------------------------
# North America
# ------------------------------------------

happiness$Region[happiness$Country %in%
  c(
    "Canada",
    "United States"
  )] <- "NorthAmerica"

# ------------------------------------------
# Latin America
# ------------------------------------------

happiness$Region[happiness$Country %in%
  c(
    "Mexico",
    "Costa Rica",
    "Guatemala",
    "Panama",
    "Nicaragua",
    "Honduras",
    "El Salvador",
    "Colombia",
    "Venezuela",
    "Peru",
    "Chile",
    "Argentina",
    "Brazil",
    "Uruguay",
    "Paraguay",
    "Bolivia",
    "Ecuador",
    "Dominican Republic"
  )] <- "LatinAmerica"

# ------------------------------------------
# Western Europe
# ------------------------------------------

happiness$Region[happiness$Country %in%
  c(
    "Germany",
    "France",
    "Belgium",
    "Netherlands",
    "Austria",
    "Switzerland",
    "Ireland",
    "Luxembourg",
    "United Kingdom",
    "Spain",
    "Portugal",
    "Italy"
  )] <- "WesternEurope"

# ------------------------------------------
# East Asia
# ------------------------------------------

happiness$Region[happiness$Country %in%
  c(
    "China",
    "Japan",
    "South Korea",
    "Hong Kong",
    "Singapore",
    "Taiwan"
  )] <- "EastAsia"

# ------------------------------------------
# Middle East
# ------------------------------------------

happiness$Region[happiness$Country %in%
  c(
    "Israel",
    "Saudi Arabia",
    "United Arab Emirates",
    "Qatar",
    "Kuwait",
    "Bahrain",
    "Oman",
    "Iran",
    "Iraq",
    "Jordan",
    "Lebanon"
  )] <- "MiddleEast"

# ------------------------------------------
# Sub-Saharan Africa
# ------------------------------------------

happiness$Region[happiness$Country %in%
  c(
    "South Africa",
    "Botswana",
    "Nigeria",
    "Kenya",
    "Uganda",
    "Tanzania",
    "Rwanda",
    "Zimbabwe",
    "Malawi",
    "Ethiopia",
    "Ghana",
    "Zambia",
    "Namibia",
    "Mozambique",
    "Benin",
    "Sierra Leone"
  )] <- "SubSaharanAfrica"

# ==========================================
# PART 3 - TRAIN TEST SPLIT
# ==========================================

set.seed(123)

train_index <- sample(
  1:nrow(happiness),
  size = 0.7 * nrow(happiness)
)

train <- happiness[train_index, ]

test <- happiness[-train_index, ]

cat("\nTraining observations:\n")
print(nrow(train))

cat("\nTesting observations:\n")
print(nrow(test))

# ==========================================
# PART 4 - BASELINE MODEL
# ==========================================

baseline_prediction <- mean(train$Score)

baseline_predictions <- rep(
  baseline_prediction,
  nrow(test)
)

baseline_SSE <- sum(
  (test$Score - baseline_predictions)^2
)

baseline_SST <- sum(
  (test$Score - mean(train$Score))^2
)

baseline_R2 <- 1 -
  baseline_SSE / baseline_SST

cat("\n====================\n")
cat("BASELINE MODEL\n")
cat("====================\n")

print(baseline_R2)

# ==========================================
# PART 5 - MULTIPLE REGRESSION
# ==========================================

model <- lm(
  Score ~
    GDP +
    SocialSupport +
    Health +
    Freedom +
    Generosity +
    Corruption +
    factor(Year),
  data = train
)

cat("\n====================\n")
cat("REGRESSION RESULTS\n")
cat("====================\n")

summary(model)

# ==========================================
# PART 6 - TEST PERFORMANCE
# ==========================================

predictions <- predict(
  model,
  newdata = test
)

SSE <- sum(
  (test$Score - predictions)^2
)

SST <- sum(
  (test$Score - mean(train$Score))^2
)

OSR2 <- 1 - SSE / SST

cat("\n====================\n")
cat("OUT OF SAMPLE R2\n")
cat("====================\n")

print(OSR2)

# ==========================================
# PART 7 - RESIDUAL ANALYSIS
# ==========================================

happiness$predicted_score <- predict(
  model,
  newdata = happiness
)

happiness$residual <- happiness$Score -
  happiness$predicted_score

# ==========================================
# PART 8 - TOP POSITIVE RESIDUALS
# ==========================================

cat("\n====================\n")
cat("HAPPIER THAN EXPECTED\n")
cat("====================\n")

head(

  happiness[
    order(-happiness$residual),

    c(
      "Country",
      "Year",
      "Score",
      "predicted_score",
      "residual"
    )
  ],

  20

)

# ==========================================
# PART 9 - TOP NEGATIVE RESIDUALS
# ==========================================

cat("\n====================\n")
cat("LESS HAPPY THAN EXPECTED\n")
cat("====================\n")

head(

  happiness[
    order(happiness$residual),

    c(
      "Country",
      "Year",
      "Score",
      "predicted_score",
      "residual"
    )
  ],

  20

)

# ==========================================
# PART 10 - REGIONAL ANALYSIS
# ==========================================

region_results <- aggregate(
  residual ~ Region,
  data = happiness,
  mean
)

cat("\n====================\n")
cat("AVERAGE RESIDUAL BY REGION\n")
cat("====================\n")

print(region_results)

# ==========================================
# TOP COUNTRIES BY AVERAGE RESIDUAL
# ==========================================

country_residuals <- aggregate(
  residual ~ Country,
  data = happiness,
  mean
)

country_residuals <- country_residuals[
  order(-country_residuals$residual),
]

cat("\n====================\n")
cat("TOP 15 OVERPERFORMING COUNTRIES\n")
cat("====================\n")

head(country_residuals, 15)

cat("\n====================\n")
cat("TOP 15 UNDERPERFORMING COUNTRIES\n")
cat("====================\n")

tail(country_residuals, 15)

# ==========================================
# PART 11 - SAVE RESULTS
# ==========================================

write.csv(
  happiness,
  "report/final_happiness_results.csv",
  row.names = FALSE
)

cat("\nResults saved successfully.\n")

# ==========================================
# PART 12 - DECISION TREE
# ==========================================

library(rpart)
library(rpart.plot)

tree_model <- rpart(

  Score ~
    GDP +
    SocialSupport +
    Health +
    Freedom +
    Generosity +
    Corruption,

  data = train,

  method = "anova"

)

cat("\n====================\n")
cat("DECISION TREE\n")
cat("====================\n")

print(tree_model)

# Plot Tree

rpart.plot(
  tree_model,
  type = 2,
  extra = 101
)

# Predictions

tree_predictions <- predict(
  tree_model,
  newdata = test
)

# Calculate R²

tree_SSE <- sum(
  (test$Score - tree_predictions)^2
)

tree_SST <- sum(
  (test$Score - mean(train$Score))^2
)

tree_R2 <- 1 - tree_SSE / tree_SST

cat("\n====================\n")
cat("DECISION TREE R2\n")
cat("====================\n")

print(tree_R2)

# ==========================================
# PART 13 - RANDOM FOREST
# ==========================================

# Remove missing values

train_rf <- na.omit(train)

test_rf <- na.omit(test)

cat("Training observations after NA removal:",
    nrow(train_rf), "\n")

cat("Testing observations after NA removal:",
    nrow(test_rf), "\n")

library(randomForest)

set.seed(123)

forest_model <- randomForest(

  Score ~
    GDP +
    SocialSupport +
    Health +
    Freedom +
    Generosity +
    Corruption,

  data = train_rf,

  ntree = 500,

  importance = TRUE

)

cat("\n====================\n")
cat("RANDOM FOREST\n")
cat("====================\n")

print(forest_model)

# ==========================================
# Predictions
# ==========================================

forest_predictions <- predict(
  forest_model,
  newdata = test_rf
)

# ==========================================
# Forest R²
# ==========================================

forest_SSE <- sum(
  (test_rf$Score - forest_predictions)^2
)

forest_SST <- sum(
  (test_rf$Score - mean(train_rf$Score))^2
)

forest_R2 <- 1 - forest_SSE / forest_SST

cat("\n====================\n")
cat("RANDOM FOREST R2\n")
cat("====================\n")

print(forest_R2)

# ==========================================
# Variable Importance
# ==========================================

cat("\n====================\n")
cat("VARIABLE IMPORTANCE\n")
cat("====================\n")

print(importance(forest_model))

# Importance Plot

varImpPlot(
  forest_model,
  main = "Random Forest Variable Importance"
)