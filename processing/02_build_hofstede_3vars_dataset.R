# ==============================================================================
# WORLD HAPPINESS PROJECT
# 02 - BUILD HAPPINESS + HOFSTEDE DATASET (3 VARIABLES)
#
# Run from the project root.
#
# Inputs:
#   data/processed/happiness_master.csv
#   data/raw/hofstede/6-dimensions-for-website-2015-08-16.csv
#
# Output:
#   data/processed/happiness_master_hofstede_3vars.csv
#
# Hofstede variables retained:
#   idv = Individualism
#   uai = Uncertainty Avoidance
#   ivr = Indulgence
#
# Only rows with complete values in all three Hofstede variables are retained.
# The output contains only variables needed for modelling.
# ==============================================================================

# ------------------------------------------------------------------------------
# 0. File paths
# ------------------------------------------------------------------------------

happiness_file <- file.path(
  "data", "processed", "happiness_master.csv"
)

hofstede_file <- file.path(
  "data", "raw", "hofstede",
  "6-dimensions-for-website-2015-08-16.csv"
)

output_file <- file.path(
  "data", "processed", "happiness_master_hofstede_3vars.csv"
)

dir.create(
  file.path("data", "processed"),
  recursive = TRUE,
  showWarnings = FALSE
)

# ------------------------------------------------------------------------------
# 1. Helper functions
# ------------------------------------------------------------------------------

assert_columns <- function(data, required_columns, dataset_name) {
  missing_columns <- setdiff(required_columns, names(data))

  if (length(missing_columns) > 0L) {
    stop(
      paste0(
        dataset_name,
        " is missing required columns: ",
        paste(missing_columns, collapse = ", ")
      )
    )
  }
}

clean_hofstede_numeric <- function(x) {
  x <- trimws(as.character(x))

  # Convert escaped forms such as #NULL\! into #NULL!.
  x <- gsub("\\\\", "", x)
  x[x %in% c("", "NA", "N/A", "NULL", "#NULL!")] <- NA_character_

  suppressWarnings(as.numeric(x))
}

harmonise_hofstede_country_names <- function(country) {
  country <- trimws(as.character(country))

  mapping <- c(
    "Czech Rep" = "Czech Republic",
    "Dominican Rep" = "Dominican Republic",
    "Great Britain" = "United Kingdom",
    "Korea South" = "South Korea",
    "Kyrgyz Rep" = "Kyrgyzstan",
    "Macedonia Rep" = "Macedonia",
    "Slovak Rep" = "Slovakia",
    "U.S.A." = "United States"
  )

  replacement <- unname(mapping[country])
  replace_rows <- !is.na(replacement)
  country[replace_rows] <- replacement[replace_rows]

  country
}

# ------------------------------------------------------------------------------
# 2. Load input files
# ------------------------------------------------------------------------------

if (!file.exists(happiness_file)) {
  stop(
    paste0(
      "Missing file: ", happiness_file,
      ". Run processing/01_build_master_dataset.r first."
    )
  )
}

if (!file.exists(hofstede_file)) {
  stop(paste0("Missing file: ", hofstede_file))
}

happiness <- read.csv(
  happiness_file,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  na.strings = c("", "NA", "N/A")
)

hofstede_raw <- read.csv(
  hofstede_file,
  sep = ";",
  stringsAsFactors = FALSE,
  check.names = FALSE,
  na.strings = NULL
)

# ------------------------------------------------------------------------------
# 3. Validate inputs
# ------------------------------------------------------------------------------

required_happiness_columns <- c(
  "Country",
  "Year",
  "Score",
  "GDP",
  "SocialSupport",
  "Health",
  "Freedom",
  "Generosity",
  "Corruption"
)

required_hofstede_columns <- c(
  "country",
  "idv",
  "uai",
  "ivr"
)

assert_columns(
  happiness,
  required_happiness_columns,
  "happiness_master.csv"
)

assert_columns(
  hofstede_raw,
  required_hofstede_columns,
  "Hofstede source file"
)

if (anyDuplicated(happiness[c("Country", "Year")]) > 0L) {
  stop("happiness_master.csv contains duplicate Country-Year rows.")
}

# ------------------------------------------------------------------------------
# 4. Prepare the three Hofstede variables
# ------------------------------------------------------------------------------

hofstede <- data.frame(
  Country = harmonise_hofstede_country_names(hofstede_raw$country),
  Individualism = clean_hofstede_numeric(hofstede_raw$idv),
  UncertaintyAvoidance = clean_hofstede_numeric(hofstede_raw$uai),
  Indulgence = clean_hofstede_numeric(hofstede_raw$ivr),
  stringsAsFactors = FALSE
)

# Remove regional/subgroup profiles that are not national country observations.
non_country_entries <- c(
  "Africa East",
  "Africa West",
  "Arab countries",
  "Belgium French",
  "Belgium Netherl",
  "Canada French",
  "Germany East",
  "South Africa white",
  "Switzerland French",
  "Switzerland German"
)

original_hofstede_names <- trimws(as.character(hofstede_raw$country))

hofstede <- hofstede[
  !(original_hofstede_names %in% non_country_entries),
  ,
  drop = FALSE
]

rownames(hofstede) <- NULL

if (anyDuplicated(hofstede$Country) > 0L) {
  duplicate_names <- unique(
    hofstede$Country[
      duplicated(hofstede$Country) |
        duplicated(hofstede$Country, fromLast = TRUE)
    ]
  )

  stop(
    paste0(
      "Duplicate Hofstede country keys after harmonisation: ",
      paste(sort(duplicate_names), collapse = ", ")
    )
  )
}

# Retain only countries with all three selected Hofstede values.
hofstede_complete <- hofstede[
  complete.cases(
    hofstede[c(
      "Individualism",
      "UncertaintyAvoidance",
      "Indulgence"
    )]
  ),
  ,
  drop = FALSE
]

# ------------------------------------------------------------------------------
# 5. Attach Hofstede values to the Happiness observations
# ------------------------------------------------------------------------------

match_index <- match(
  happiness$Country,
  hofstede_complete$Country
)

keep_rows <- !is.na(match_index)

# Deliberately keep only columns required for regression/modelling.
output <- data.frame(
  Country = happiness$Country[keep_rows],
  Year = happiness$Year[keep_rows],
  Score = happiness$Score[keep_rows],
  GDP = happiness$GDP[keep_rows],
  SocialSupport = happiness$SocialSupport[keep_rows],
  Health = happiness$Health[keep_rows],
  Freedom = happiness$Freedom[keep_rows],
  Generosity = happiness$Generosity[keep_rows],
  Corruption = happiness$Corruption[keep_rows],
  Individualism = hofstede_complete$Individualism[match_index[keep_rows]],
  UncertaintyAvoidance = hofstede_complete$UncertaintyAvoidance[match_index[keep_rows]],
  Indulgence = hofstede_complete$Indulgence[match_index[keep_rows]],
  stringsAsFactors = FALSE
)

output <- output[
  order(output$Country, output$Year),
  ,
  drop = FALSE
]

rownames(output) <- NULL

# ------------------------------------------------------------------------------
# 6. Final quality checks
# ------------------------------------------------------------------------------

if (nrow(output) == 0L) {
  stop("The output dataset is empty. Check country names and source values.")
}

if (anyDuplicated(output[c("Country", "Year")]) > 0L) {
  stop("The output contains duplicate Country-Year rows.")
}

selected_hofstede_columns <- c(
  "Individualism",
  "UncertaintyAvoidance",
  "Indulgence"
)

if (any(!complete.cases(output[selected_hofstede_columns]))) {
  stop("The output still contains missing values in selected Hofstede variables.")
}

# The selected Hofstede values are country-level constants and must not vary
# across years within the same country.
check_constant <- function(values, countries, variable_name) {
  unique_counts <- tapply(
    values,
    countries,
    function(x) length(unique(x))
  )

  problematic_countries <- names(unique_counts)[unique_counts != 1L]

  if (length(problematic_countries) > 0L) {
    stop(
      paste0(
        variable_name,
        " varies within: ",
        paste(problematic_countries, collapse = ", ")
      )
    )
  }
}

check_constant(
  output$Individualism,
  output$Country,
  "Individualism"
)

check_constant(
  output$UncertaintyAvoidance,
  output$Country,
  "UncertaintyAvoidance"
)

check_constant(
  output$Indulgence,
  output$Country,
  "Indulgence"
)

# ------------------------------------------------------------------------------
# 7. Save output
# ------------------------------------------------------------------------------

write.csv(
  output,
  output_file,
  row.names = FALSE,
  na = ""
)

# ------------------------------------------------------------------------------
# 8. Console summary
# ------------------------------------------------------------------------------

cat("\n==================================================\n")
cat("HAPPINESS + HOFSTEDE 3-VARIABLE DATASET BUILT\n")
cat("==================================================\n")
cat("Rows:", nrow(output), "\n")
cat("Countries:", length(unique(output$Country)), "\n")
cat("Rows by year:\n")
print(table(output$Year))
cat("Missing values by column:\n")
print(colSums(is.na(output)))
cat("Saved to:", output_file, "\n")
