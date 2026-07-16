## =============================================================================
## WORLD HAPPINESS PROJECT
## 01 - BUILD CORE MASTER DATASET
##
## Purpose:
## - Combine World Happiness Report data from 2015 to 2019
## - Preserve every observed country-year record
## - Harmonise country names across years
## - Create one unique row per country and year
## - Add coverage metadata without removing incomplete countries
##
## Output:
## data/processed/happiness_master.csv
## =============================================================================


## -----------------------------------------------------------------------------
## 0. CONFIGURATION
## -----------------------------------------------------------------------------

raw_data_path <- "data/raw"
processed_data_path <- "data/processed"

output_file <- file.path(
  processed_data_path,
  "happiness_master.csv"
)

dir.create(
  processed_data_path,
  recursive = TRUE,
  showWarnings = FALSE
)


## -----------------------------------------------------------------------------
## 1. HELPER FUNCTIONS
## -----------------------------------------------------------------------------


## Check whether all required columns exist.
assert_required_columns <- function(data, required_columns, dataset_name) {

  missing_columns <- setdiff(
    required_columns,
    colnames(data)
  )

  if (length(missing_columns) > 0) {

    stop(
      paste0(
        dataset_name,
        " is missing the following required columns: ",
        paste(missing_columns, collapse = ", ")
      )
    )
  }
}


## Convert values safely to numeric.
## Invalid text values become NA but are not silently removed.
safe_numeric <- function(x) {

  x <- trimws(as.character(x))

  x[x %in% c(
    "",
    "NA",
    "N/A",
    "#NULL!",
    "#NULL\\!"
  )] <- NA

  suppressWarnings(
    as.numeric(x)
  )
}


## Harmonise known country-name differences across the five reports.
harmonise_country_names <- function(country) {

  country <- trimws(as.character(country))

  country_name_map <- c(
    "Taiwan Province of China" = "Taiwan",
    "Hong Kong S.A.R., China"  = "Hong Kong",
    "Northern Cyprus"          = "North Cyprus",
    "North Macedonia"          = "Macedonia",
    "Trinidad & Tobago"        = "Trinidad and Tobago",
    "Somaliland Region"        = "Somaliland region",
    "Swaziland"                = "Eswatini"
  )

  mapped_values <- unname(
    country_name_map[country]
  )

  has_mapping <- !is.na(mapped_values)

  country[has_mapping] <-
    mapped_values[has_mapping]

  return(country)
}


## Standardise data from 2015 and 2016.
standardise_2015_2016 <- function(data, year) {

  required_columns <- c(
    "Country",
    "Happiness.Score",
    "Economy..GDP.per.Capita.",
    "Family",
    "Health..Life.Expectancy.",
    "Freedom",
    "Generosity",
    "Trust..Government.Corruption."
  )

  assert_required_columns(
    data,
    required_columns,
    paste0("World Happiness ", year)
  )

  result <- data.frame(
    CountryOriginal = data$Country,
    Country = harmonise_country_names(
      data$Country
    ),
    Score = safe_numeric(
      data$Happiness.Score
    ),
    GDP = safe_numeric(
      data$Economy..GDP.per.Capita.
    ),
    SocialSupport = safe_numeric(
      data$Family
    ),
    Health = safe_numeric(
      data$Health..Life.Expectancy.
    ),
    Freedom = safe_numeric(
      data$Freedom
    ),
    Generosity = safe_numeric(
      data$Generosity
    ),
    Corruption = safe_numeric(
      data$Trust..Government.Corruption.
    ),
    Year = as.integer(year),
    stringsAsFactors = FALSE
  )

  return(result)
}


## Standardise data from 2017.
standardise_2017 <- function(data) {

  required_columns <- c(
    "Country",
    "Happiness.Score",
    "Economy..GDP.per.Capita.",
    "Family",
    "Health..Life.Expectancy.",
    "Freedom",
    "Generosity",
    "Trust..Government.Corruption."
  )

  assert_required_columns(
    data,
    required_columns,
    "World Happiness 2017"
  )

  result <- data.frame(
    CountryOriginal = data$Country,
    Country = harmonise_country_names(
      data$Country
    ),
    Score = safe_numeric(
      data$Happiness.Score
    ),
    GDP = safe_numeric(
      data$Economy..GDP.per.Capita.
    ),
    SocialSupport = safe_numeric(
      data$Family
    ),
    Health = safe_numeric(
      data$Health..Life.Expectancy.
    ),
    Freedom = safe_numeric(
      data$Freedom
    ),
    Generosity = safe_numeric(
      data$Generosity
    ),
    Corruption = safe_numeric(
      data$Trust..Government.Corruption.
    ),
    Year = 2017L,
    stringsAsFactors = FALSE
  )

  return(result)
}


## Standardise data from 2018 and 2019.
standardise_2018_2019 <- function(data, year) {

  required_columns <- c(
    "Country.or.region",
    "Score",
    "GDP.per.capita",
    "Social.support",
    "Healthy.life.expectancy",
    "Freedom.to.make.life.choices",
    "Generosity",
    "Perceptions.of.corruption"
  )

  assert_required_columns(
    data,
    required_columns,
    paste0("World Happiness ", year)
  )

  result <- data.frame(
    CountryOriginal = data$Country.or.region,
    Country = harmonise_country_names(
      data$Country.or.region
    ),
    Score = safe_numeric(
      data$Score
    ),
    GDP = safe_numeric(
      data$GDP.per.capita
    ),
    SocialSupport = safe_numeric(
      data$Social.support
    ),
    Health = safe_numeric(
      data$Healthy.life.expectancy
    ),
    Freedom = safe_numeric(
      data$Freedom.to.make.life.choices
    ),
    Generosity = safe_numeric(
      data$Generosity
    ),
    Corruption = safe_numeric(
      data$Perceptions.of.corruption
    ),
    Year = as.integer(year),
    stringsAsFactors = FALSE
  )

  return(result)
}


## -----------------------------------------------------------------------------
## 2. LOAD RAW DATA
## -----------------------------------------------------------------------------

h2015_raw <- read.csv(
  file.path(raw_data_path, "2015.csv"),
  stringsAsFactors = FALSE,
  check.names = TRUE,
  na.strings = c("", "NA", "N/A")
)

h2016_raw <- read.csv(
  file.path(raw_data_path, "2016.csv"),
  stringsAsFactors = FALSE,
  check.names = TRUE,
  na.strings = c("", "NA", "N/A")
)

h2017_raw <- read.csv(
  file.path(raw_data_path, "2017.csv"),
  stringsAsFactors = FALSE,
  check.names = TRUE,
  na.strings = c("", "NA", "N/A")
)

h2018_raw <- read.csv(
  file.path(raw_data_path, "2018.csv"),
  stringsAsFactors = FALSE,
  check.names = TRUE,
  na.strings = c("", "NA", "N/A")
)

h2019_raw <- read.csv(
  file.path(raw_data_path, "2019.csv"),
  stringsAsFactors = FALSE,
  check.names = TRUE,
  na.strings = c("", "NA", "N/A")
)


## -----------------------------------------------------------------------------
## 3. STANDARDISE ANNUAL DATASETS
## -----------------------------------------------------------------------------

h2015 <- standardise_2015_2016(
  h2015_raw,
  2015
)

h2016 <- standardise_2015_2016(
  h2016_raw,
  2016
)

h2017 <- standardise_2017(
  h2017_raw
)

h2018 <- standardise_2018_2019(
  h2018_raw,
  2018
)

h2019 <- standardise_2018_2019(
  h2019_raw,
  2019
)


annual_datasets <- list(
  `2015` = h2015,
  `2016` = h2016,
  `2017` = h2017,
  `2018` = h2018,
  `2019` = h2019
)


## -----------------------------------------------------------------------------
## 4. VALIDATE THE ANNUAL INPUTS
## -----------------------------------------------------------------------------

expected_rows <- c(
  `2015` = 158L,
  `2016` = 157L,
  `2017` = 155L,
  `2018` = 156L,
  `2019` = 156L
)


cat("\n==================================================\n")
cat("ANNUAL INPUT VALIDATION\n")
cat("==================================================\n")


for (year_name in names(annual_datasets)) {

  current_data <- annual_datasets[[year_name]]

  current_rows <- nrow(current_data)

  current_countries <- length(
    unique(current_data$Country)
  )

  duplicate_country_years <- sum(
    duplicated(
      current_data[, c("Country", "Year")]
    )
  )

  cat(
    year_name,
    ": ",
    current_rows,
    " rows | ",
    current_countries,
    " unique countries | ",
    duplicate_country_years,
    " duplicate country-year rows\n",
    sep = ""
  )


  if (current_rows != expected_rows[[year_name]]) {

    stop(
      paste0(
        "Unexpected number of rows in ",
        year_name,
        ". Expected ",
        expected_rows[[year_name]],
        " but found ",
        current_rows,
        "."
      )
    )
  }


  if (duplicate_country_years > 0) {

    stop(
      paste0(
        "Duplicate country-year observations found in ",
        year_name,
        "."
      )
    )
  }
}


## -----------------------------------------------------------------------------
## 5. COMBINE ALL OBSERVED COUNTRY-YEAR RECORDS
## -----------------------------------------------------------------------------

happiness_master <- do.call(
  rbind,
  annual_datasets
)

rownames(happiness_master) <- NULL


## Sort the master dataset for readability.
happiness_master <- happiness_master[
  order(
    happiness_master$Country,
    happiness_master$Year
  ),
]


## -----------------------------------------------------------------------------
## 6. ADD TRANSPARENCY METADATA
## -----------------------------------------------------------------------------


## Number of years available for each country.
years_available <- tapply(
  happiness_master$Year,
  happiness_master$Country,
  function(x) {
    length(unique(x))
  }
)

happiness_master$YearsAvailable <- as.integer(
  years_available[happiness_master$Country]
)


## Indicates whether a country is available in all five years.
happiness_master$CompleteFiveYears <-
  happiness_master$YearsAvailable == 5L


## Stable unique key for every observation.
happiness_master$ObservationID <- paste(
  happiness_master$Country,
  happiness_master$Year,
  sep = "_"
)


## Place columns in a logical order.
happiness_master <- happiness_master[, c(
  "ObservationID",
  "Country",
  "CountryOriginal",
  "Year",
  "YearsAvailable",
  "CompleteFiveYears",
  "Score",
  "GDP",
  "SocialSupport",
  "Health",
  "Freedom",
  "Generosity",
  "Corruption"
)]


## -----------------------------------------------------------------------------
## 7. FINAL QUALITY CHECKS
## -----------------------------------------------------------------------------

analysis_variables <- c(
  "Score",
  "GDP",
  "SocialSupport",
  "Health",
  "Freedom",
  "Generosity",
  "Corruption"
)


duplicate_keys <- happiness_master[
  duplicated(
    happiness_master[, c("Country", "Year")]
  ),
]


missing_values <- colSums(
  is.na(
    happiness_master[, analysis_variables]
  )
)


countries_per_year <- tapply(
  happiness_master$Country,
  happiness_master$Year,
  function(x) {
    length(unique(x))
  }
)


countries_by_coverage <- table(
  happiness_master[
    !duplicated(happiness_master$Country),
    "YearsAvailable"
  ]
)


## Hard validation conditions.
if (nrow(happiness_master) != 782L) {

  stop(
    paste0(
      "Final master dataset should contain 782 rows, but contains ",
      nrow(happiness_master),
      "."
    )
  )
}


if (nrow(duplicate_keys) > 0) {

  print(duplicate_keys)

  stop(
    "Duplicate Country-Year combinations remain after harmonisation."
  )
}


if (length(unique(happiness_master$ObservationID)) !=
    nrow(happiness_master)) {

  stop(
    "ObservationID is not unique."
  )
}


## -----------------------------------------------------------------------------
## 8. PRINT FINAL DATA AUDIT
## -----------------------------------------------------------------------------

cat("\n==================================================\n")
cat("FINAL HAPPINESS MASTER DATASET\n")
cat("==================================================\n")

cat(
  "Total observed country-year rows: ",
  nrow(happiness_master),
  "\n",
  sep = ""
)

cat(
  "Unique harmonised countries: ",
  length(unique(happiness_master$Country)),
  "\n",
  sep = ""
)

cat(
  "Duplicate country-year rows: ",
  nrow(duplicate_keys),
  "\n",
  sep = ""
)

cat("\nCountries per year:\n")
print(countries_per_year)

cat("\nCountries by number of available years:\n")
print(countries_by_coverage)

cat("\nMissing values in analytical variables:\n")
print(missing_values)


incomplete_countries <- unique(
  happiness_master[
    !happiness_master$CompleteFiveYears,
    c(
      "Country",
      "YearsAvailable"
    )
  ]
)

incomplete_countries <- incomplete_countries[
  order(
    incomplete_countries$YearsAvailable,
    incomplete_countries$Country
  ),
]


cat("\nCountries with fewer than five available years:\n")
print(
  incomplete_countries,
  row.names = FALSE
)


## -----------------------------------------------------------------------------
## 9. SAVE CORE MASTER DATASET
## -----------------------------------------------------------------------------

write.csv(
  happiness_master,
  output_file,
  row.names = FALSE,
  na = ""
)


cat("\n==================================================\n")
cat("DATASET SAVED SUCCESSFULLY\n")
cat("==================================================\n")

cat(
  "Output file: ",
  output_file,
  "\n",
  sep = ""
)

cat(
  "The master dataset preserves all observed records.\n"
)

cat(
  "No country was removed because of incomplete yearly coverage.\n"
)

cat(
  "No Happiness Gap, prediction or Hofstede variable was added.\n"
)