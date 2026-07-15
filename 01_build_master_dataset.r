# ==========================================
# Build Master Dataset (2015-2019)
# ==========================================

# Load datasets

h2015 <- read.csv("data/raw/2015.csv")
h2016 <- read.csv("data/raw/2016.csv")
h2017 <- read.csv("data/raw/2017.csv")
h2018 <- read.csv("data/raw/2018.csv")
h2019 <- read.csv("data/raw/2019.csv")

# ==========================================
# Standardize column names
# ==========================================

# 2015

h2015 <- h2015[, c(
  "Country",
  "Happiness.Score",
  "Economy..GDP.per.Capita.",
  "Family",
  "Health..Life.Expectancy.",
  "Freedom",
  "Generosity",
  "Trust..Government.Corruption."
)]

colnames(h2015) <- c(
  "Country",
  "Score",
  "GDP",
  "SocialSupport",
  "Health",
  "Freedom",
  "Generosity",
  "Corruption"
)

h2015$Year <- 2015

# ==========================================

# 2016

h2016 <- h2016[, c(
  "Country",
  "Happiness.Score",
  "Economy..GDP.per.Capita.",
  "Family",
  "Health..Life.Expectancy.",
  "Freedom",
  "Generosity",
  "Trust..Government.Corruption."
)]

colnames(h2016) <- c(
  "Country",
  "Score",
  "GDP",
  "SocialSupport",
  "Health",
  "Freedom",
  "Generosity",
  "Corruption"
)

h2016$Year <- 2016

# ==========================================

# 2017

h2017 <- h2017[, c(
  "Country",
  "Happiness.Score",
  "Economy..GDP.per.Capita.",
  "Family",
  "Health..Life.Expectancy.",
  "Freedom",
  "Generosity",
  "Trust..Government.Corruption."
)]

colnames(h2017) <- c(
  "Country",
  "Score",
  "GDP",
  "SocialSupport",
  "Health",
  "Freedom",
  "Generosity",
  "Corruption"
)

h2017$Year <- 2017

# ==========================================

# 2018

h2018 <- h2018[, c(
  "Country.or.region",
  "Score",
  "GDP.per.capita",
  "Social.support",
  "Healthy.life.expectancy",
  "Freedom.to.make.life.choices",
  "Generosity",
  "Perceptions.of.corruption"
)]

colnames(h2018) <- c(
  "Country",
  "Score",
  "GDP",
  "SocialSupport",
  "Health",
  "Freedom",
  "Generosity",
  "Corruption"
)

h2018$Year <- 2018

# ==========================================

# 2019

h2019 <- h2019[, c(
  "Country.or.region",
  "Score",
  "GDP.per.capita",
  "Social.support",
  "Healthy.life.expectancy",
  "Freedom.to.make.life.choices",
  "Generosity",
  "Perceptions.of.corruption"
)]

colnames(h2019) <- c(
  "Country",
  "Score",
  "GDP",
  "SocialSupport",
  "Health",
  "Freedom",
  "Generosity",
  "Corruption"
)

h2019$Year <- 2019

# ==========================================
# Combine all years
# ==========================================

happiness_master <- rbind(
  h2015,
  h2016,
  h2017,
  h2018,
  h2019
)

# ==========================================
# Quick checks
# ==========================================

cat("\nMASTER DATASET\n")

print(dim(happiness_master))

head(happiness_master)

summary(happiness_master)

# ==========================================
# Save dataset
# ==========================================

write.csv(
  happiness_master,
  "data/processed/happiness_master.csv",
  row.names = FALSE
)

cat("\nMaster dataset saved successfully.\n")