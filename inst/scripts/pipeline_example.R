# =============================================================================
# Seasonal Adjustment Validation Pipeline
# Workshop on seasonal Adjustment 17.11.2025
# =============================================================================

# To adjust the loaded file, adapt lines 58-67 



# Load required packages with error handling
required_packages <- c(
  "remotes",
  "dplyr",
  "tidyr",
  "purrr",
  "tibble",
  "readsdmx"
)

# Install missing packages
install_missing_packages <- function(packages) {
  missing_packages <- packages[!packages %in% installed.packages()[, "Package"]]
  if (length(missing_packages) > 0) {
    message("Installing missing packages: ", paste(missing_packages, collapse = ", "))
    install.packages(missing_packages)
  }
}

# Load all packages
load_packages <- function(packages) {
  invisible(lapply(packages, function(pkg) {
    if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
      stop("Failed to load package: ", pkg)
    }
  }))
}

# Install and load packages
install_missing_packages(required_packages)
load_packages(required_packages)

# Install development package from GitHub if needed
if (!require("SAvalidation", quietly = TRUE)) {
  message("Installing SAvalidation from GitHub...")
  remotes::install_github("SeasAdjwG/SAvalidation")
}
if (!require("rjd3revisions", quietly = TRUE)) {
  message("Installing rjd3revisions from GitHub...")
  remotes::install_github("rjdverse/rjd3revisions")
}
library(SAvalidation)
library(rjd3revisions)


# =============================================================================
# Configuration Constants
# =============================================================================

# If you wish to load a different file, you need to adjust SDMX_FILE_PATH accordingly. 
# If the data is not from NAMA, DESIRED_ATTRIBUTES might need to be adapted

SDMX_FILE_PATH <- system.file(
  "extdata",
  "NAMAIN_T01GDP_Q_2024_0002.xml",
  package = "SAvalidation"
)

# Define data structure constants
DESIRED_ATTRIBUTES <- c(
  "FREQ", "ADJUSTMENT", "REF_AREA", "COUNTERPART_AREA",
  "REF_SECTOR", "COUNTERPART_SECTOR", "ACCOUNTING_ENTRY",
  "STO", "INSTR_ASSET", "ACTIVITY", "EXPENDITURE",
  "UNIT_MEASURE", "PRICES", "TRANSFORMATION", 
  "TIME_PERIOD", "OBS_VALUE"
)

CONCAT_ATTRIBUTES <- setdiff(DESIRED_ATTRIBUTES, c("OBS_VALUE", "TIME_PERIOD"))

ADJUSTMENT_CODES <- list(
  NA_CODE = "N",  # Not adjusted
  SA_CODE = "S",  # Seasonally adjusted
  SCA_CODE = "Y"  # Seasonally and calendar adjusted
)

# =============================================================================
# Helper Functions
# =============================================================================

#' Safely extract attribute value from a row
#' 
#' @param row A data frame row
#' @param colname Column name to extract
#' @return Character value of the attribute or "NaN" if not present
get_attribute_value <- function(row, colname) {
  if (colname %in% names(row)) {
    as.character(row[[colname]])
  } else {
    "NaN"
  }
}

#' Extract valid numerical time range from a series
#' 
#' Removes leading/trailing NAs and handles gaps in time series
#' 
#' @param x Data frame column containing time series data
#' @return Data frame with valid numerical range or NULL if no valid data
extract_numerical_range <- function(x) {
  # Convert to numeric vector
  x_numeric <- as.numeric(unlist(x))
  
  # Find first non-NA value
  start_index <- which(!is.na(x_numeric))[1]
  if (is.na(start_index)) return(NULL)
  
  # Find valid indices
  valid_indices <- which(!is.na(x_numeric))
  
  # Detect gaps in time series (differences > 1 indicate gaps)
  index_differences <- diff(valid_indices)
  gap_positions <- which(index_differences > 1)
  
  # Determine end index (before first gap or last valid value)
  if (length(gap_positions) > 0) {
    end_index <- valid_indices[gap_positions[1]]
  } else {
    end_index <- valid_indices[length(valid_indices)]
  }
  
  # Return trimmed data frame
  x[start_index:end_index, , drop = FALSE]
}

#' Validate pair of time series using level 1 validation
#' 
#' @param index Column index for tracking
#' @param col_a First time series column (non-adjusted series)
#' @param col_b Second time series column (seasonally adjusted series)
#' @param series_label Label for the series being validated
#' @param faulty_series_df Data frame to collect validation issues
#' @return Updated faulty_series data frame
validate_series_pair <- function(index, col_a, col_b, series_label, faulty_series_df) {
  # Check if both series have valid data
  if (any(!is.nan(col_a[[1]])) && any(!is.nan(col_b[[1]]))) {
    
    col_a_trimmed <- extract_numerical_range(col_a)
    col_b_trimmed <- extract_numerical_range(col_b)
    
    # Skip if either series has no valid data after trimming
    if (is.null(col_a_trimmed) || is.null(col_b_trimmed)) {
      return(faulty_series_df)
    }
    
    # Convert to numeric and check for all-zero series
    col_a_numeric <- as.numeric(col_a_trimmed[[1]])
    col_b_numeric <- as.numeric(col_b_trimmed[[1]])
    
    if (all(col_a_numeric == 0) || all(col_b_numeric == 0)) {
      return(faulty_series_df)
    }
    
    # Create time series objects
    start_a <- rownames(col_a_trimmed)[1]
    start_b <- rownames(col_b_trimmed)[1]
    
    series_a <- ts(
      col_a_numeric,
      start = c(as.integer(substr(start_a, 1, 4)), as.integer(substr(start_a, 7, 7))),
      frequency = 4
    )
    
    series_b <- ts(
      col_b_numeric, 
      start = c(as.integer(substr(start_b, 1, 4)), as.integer(substr(start_b, 7, 7))),
      frequency = 4
    )
    
    # Perform level 1 validation
    validation_result <- level1_validation(series_a, series_b)[1]
    message("Validation result for ", series_label, ": ", validation_result)
    
    # Record failures
    if (!startsWith(validation_result, "PASS:")) {
      faulty_series_df <- rbind(faulty_series_df, data.frame(
        index = index,
        Series_Name = series_label,
        Issue = validation_result,
        stringsAsFactors = FALSE
      ))
    }
  }
  
  return(faulty_series_df)
}

# =============================================================================
# Main Data Processing Pipeline
# =============================================================================

# Read and process SDMX data
message("Reading SDMX data from: ", SDMX_FILE_PATH)
sdmx_data <- read_sdmx(SDMX_FILE_PATH)
raw_data <- as.data.frame(sdmx_data)

# Filter columns to desired attributes
message("Filtering data columns...")
available_attributes <- DESIRED_ATTRIBUTES[DESIRED_ATTRIBUTES %in% names(raw_data)]
processed_data <- raw_data[, available_attributes, drop = FALSE]

# Generate unique SeriesID for each time series
message("Generating SeriesID identifiers...")
processed_data$SeriesID <- apply(processed_data, 1, function(row) {
  attribute_values <- sapply(CONCAT_ATTRIBUTES, function(attr) {
    get_attribute_value(row, attr)
  })
  paste(attribute_values, collapse = "-")
})

# Reshape data to wide format
message("Reshaping data to wide format...")
wide_data <- processed_data %>%
  select(TIME_PERIOD, SeriesID, OBS_VALUE) %>%
  pivot_wider(names_from = SeriesID, values_from = OBS_VALUE) %>%
  column_to_rownames("TIME_PERIOD")

# =============================================================================
# Separate Series by Adjustment Type
# =============================================================================

message("Separating series by adjustment type...")

# Initialize data frames for different adjustment types
not_adjusted_df <- data.frame(row.names = rownames(wide_data))
seasonally_adjusted_df <- data.frame(row.names = rownames(wide_data))  
seasonally_calendar_adjusted_df <- data.frame(row.names = rownames(wide_data))

# Classify series based on adjustment codes
for (series_name in colnames(wide_data)) {
  series_parts <- strsplit(series_name, "-", fixed = TRUE)[[1]]
  
  # Skip if insufficient parts or not starting with "N" (not adjusted baseline)
  if (length(series_parts) < 2 || series_parts[2] != ADJUSTMENT_CODES$NA_CODE) next
  
  # Create corresponding SA and SCA series names
  sa_parts <- series_parts
  sa_parts[2] <- ADJUSTMENT_CODES$SA_CODE
  sa_series_name <- paste(sa_parts, collapse = "-")
  
  sca_parts <- series_parts  
  sca_parts[2] <- ADJUSTMENT_CODES$SCA_CODE
  sca_series_name <- paste(sca_parts, collapse = "-")
  
  # Assign values to respective data frames
  not_adjusted_df[[series_name]] <- as.numeric(wide_data[[series_name]])
  seasonally_adjusted_df[[sa_series_name]] <- as.numeric(wide_data[[sa_series_name]] %||% NaN)
  seasonally_calendar_adjusted_df[[sca_series_name]] <- as.numeric(wide_data[[sca_series_name]] %||% NaN)
}

# =============================================================================
# Validation Pipeline
# =============================================================================

message("Starting validation pipeline...")

# Initialize results collector
validation_issues <- data.frame(
  index = integer(),
  Series_Name = character(), 
  Issue = character(),
  stringsAsFactors = FALSE
)

# Validate NA vs SA series
message("Validating Not Adjusted vs Seasonally Adjusted series...")
for (i in seq_along(not_adjusted_df)) {
  validation_issues <- validate_series_pair(
    index = i,
    col_a = not_adjusted_df[i],
    col_b = seasonally_adjusted_df[i],
    series_label = colnames(seasonally_adjusted_df)[i],
    faulty_series_df = validation_issues
  )
}

# Validate NA vs SCA series  
message("Validating Not Adjusted vs Seasonally & Calendar Adjusted series...")
for (i in seq_along(not_adjusted_df)) {
  validation_issues <- validate_series_pair(
    index = i,
    col_a = not_adjusted_df[i], 
    col_b = seasonally_calendar_adjusted_df[i],
    series_label = colnames(seasonally_calendar_adjusted_df)[i],
    faulty_series_df = validation_issues
  )
}

# Filter to relevant issues for demonstration
message("Filtering validation results...")
demonstration_issues <- validation_issues[c(11,16,17), ]
rownames(demonstration_issues) <- NULL

# =============================================================================
# Detailed Analysis for Selected Issues
# =============================================================================

message("Performing detailed analysis on selected series...")

if (dir.exists("HTML_Reports")) {
  unlink("HTML_Reports/*")
} else {
  dir.create("HTML_Reports")
}

for (row_idx in seq_len(nrow(demonstration_issues))) {
  series_name <- demonstration_issues$Series_Name[row_idx]
  series_index <- demonstration_issues$index[row_idx]
  
  message("Processing series: ", series_name)
  
  # Extract series data
  reference_series <- not_adjusted_df[series_index]
  comparison_series <- wide_data[, series_name, drop = FALSE]
  
  # Trim to valid numerical ranges
  reference_trimmed <- extract_numerical_range(reference_series)
  comparison_trimmed <- extract_numerical_range(comparison_series)
  
  if (is.null(reference_trimmed) || is.null(comparison_trimmed)) next
  
  # Create time series objects
  start_ref <- rownames(reference_trimmed)[1]
  start_comp <- rownames(comparison_trimmed)[1]
  
  ts_reference <- ts(
    as.numeric(reference_trimmed[[1]]),
    start = c(as.integer(substr(start_ref, 1, 4)), as.integer(substr(start_ref, 7, 7))),
    frequency = 4
  )
  
  ts_comparison <- ts(
    as.numeric(comparison_trimmed[[1]]),
    start = c(as.integer(substr(start_comp, 1, 4)), as.integer(substr(start_comp, 7, 7))),
    frequency = 4
  )
  
  # Perform level 2 validation with error handling
  validation_result <- tryCatch(
    {
      level2_validation(
        ts_reference, 
        ts_comparison, 
        paste0(series_name, "_level2"), 
        output_directory = "HTML_Reports"
      )
      
      data(vintages, package = "SAvalidation")
      
      level3_validation(
        nsa = ts_reference,
        sa = ts_comparison, 
        series_name = paste0(series_name, "_level3"),
        vintages = vintages,
        output_directory = normalizePath("HTML_Reports", winslash = "\\")  # Use Windows-style paths
      )
    },
    error = function(e) {
      message("Error in level2_validation for series: ", series_name)
      message("Reference series length: ", length(ts_reference))
      message("Comparison series length: ", length(ts_comparison))  
      message("Error: ", conditionMessage(e))
      return(NULL)
    }
  )
  
  if (!is.null(validation_result)) {
    message("Successfully generated validation report for: ", series_name)
  }
}

message("Validation pipeline completed successfully!")
message("Total issues identified: ", nrow(validation_issues))
message("Demonstration issues processed: ", nrow(demonstration_issues))