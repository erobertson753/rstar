# Build complete input file with Q3 2025 data
# Extract from individual FRED CSVs and apply LW methodology transformations

library(zoo)

cat("=== BUILDING Q3 2025 INPUT FILE ===\n\n")

# Read existing data as template
existing <- read.csv("LW_2023_Replication_Code/inputData/LW_official_input_data.csv", stringsAsFactors = FALSE)

cat("Existing data: ", nrow(existing), "rows\n")
cat("Date format in existing data:", head(existing$Date, 3), "\n\n")

# ===========================
# EXTRACT Q3 2025 RAW DATA
# ===========================

cat("Extracting Q3 2025 data from source files...\n")

# 1. GDP (GDPC1) - Latest available is Q2 2025
gdp_data <- read.csv("LW_2023_Replication_Code/inputData/GDPC1.csv", stringsAsFactors = FALSE)
gdp_q2_2025 <- gdp_data[gdp_data$observation_date == "2025-04-01", "GDPC1"]
gdp_q1_2025 <- gdp_data[gdp_data$observation_date == "2025-01-01", "GDPC1"]
cat("  GDP Q2 2025:", gdp_q2_2025, "\n")
cat("  GDP Q1 2025:", gdp_q1_2025, "\n")

# Project Q3 2025 GDP using recent growth rate
gdp_growth_rate <- (gdp_q2_2025 / gdp_q1_2025) - 1
gdp_q3_2025_projected <- gdp_q2_2025 * (1 + gdp_growth_rate)
cat("  GDP Q3 2025 (projected):", round(gdp_q3_2025_projected, 2), "\n")

# 2. CPI YoY inflation
cpi <- read.csv("LW_2023_Replication_Code/inputData/cpi_yoy.csv", skip=5, stringsAsFactors = FALSE)
names(cpi) <- c("date", "value", "bid")
cpi_q3_2025 <- as.numeric(cpi[cpi$date == "9/30/2025", "value"])
cat("  CPI YoY Q3 2025:", cpi_q3_2025, "%\n")

# 3. Fed Funds Rate
fedfunds <- read.csv("LW_2023_Replication_Code/inputData/fedl01.csv", skip=5, stringsAsFactors = FALSE)
names(fedfunds) <- c("date", "value", "bid")
ff_q3_2025 <- as.numeric(fedfunds[fedfunds$date == "9/30/2025", "value"])
cat("  Fed Funds Q3 2025:", ff_q3_2025, "%\n")

# 4. Inflation Expectations - Average Q3 months
infexp <- read.csv("LW_2023_Replication_Code/inputData/EXPINF10YR.csv", stringsAsFactors = FALSE)
infexp_q3 <- infexp[infexp$observation_date %in% c("2025-07-01", "2025-08-01", "2025-09-01"), "EXPINF10YR"]
infexp_q3_avg <- mean(as.numeric(infexp_q3), na.rm=TRUE)
cat("  Inflation Expectations Q3 2025:", round(infexp_q3_avg, 4), "%\n")

# 5. Oil prices - Average Q3
oil <- read.csv("LW_2023_Replication_Code/inputData/DCOILWTICO.csv", stringsAsFactors = FALSE)
oil$observation_date <- as.Date(oil$observation_date)
oil$quarter <- as.yearqtr(oil$observation_date)
oil_q3 <- mean(as.numeric(oil[oil$quarter == as.yearqtr("2025 Q3"), "DCOILWTICO"]), na.rm=TRUE)
oil_q2 <- mean(as.numeric(oil[oil$quarter == as.yearqtr("2025 Q2"), "DCOILWTICO"]), na.rm=TRUE)
cat("  Oil price Q3 2025:", round(oil_q3, 2), "\n")
cat("  Oil price Q2 2025:", round(oil_q2, 2), "\n")

# 6. Import prices
imports <- read.csv("LW_2023_Replication_Code/inputData/imp_1_comm.csv", skip=5, stringsAsFactors = FALSE)
names(imports) <- c("date", "value", "bid")
imp_q3_2025 <- as.numeric(imports[imports$date == "9/30/2025", "value"])
imp_q2_2025 <- as.numeric(imports[imports$date == "6/30/2025", "value"])
cat("  Import price Q3 2025:", imp_q3_2025, "\n")
cat("  Import price Q2 2025:", imp_q2_2025, "\n\n")

# ===========================
# APPLY LW TRANSFORMATIONS
# ===========================

cat("Applying LW methodology transformations...\n")

# 1. Log GDP
gdp_log_q3 <- log(gdp_q3_2025_projected)
cat("  gdp.log:", round(gdp_log_q3, 6), "\n")

# 2. Inflation (from CPI YoY)
inflation_q3 <- cpi_q3_2025
cat("  inflation:", inflation_q3, "%\n")

# 3. Inflation expectations
cat("  inflation.expectations:", round(infexp_q3_avg, 4), "%\n")

# 4. Oil price inflation (annualized quarterly % change)
oil_inflation_q3 <- ((oil_q3 / oil_q2) - 1) * 400  # Annualized
cat("  oil.price.inflation:", round(oil_inflation_q3, 4), "%\n")

# 5. Import price inflation (annualized quarterly % change)
imp_inflation_q3 <- ((imp_q3_2025 / imp_q2_2025) - 1) * 400  # Annualized
cat("  import.price.inflation:", round(imp_inflation_q3, 4), "%\n")

# 6. Interest rate (Fed Funds)
cat("  interest:", ff_q3_2025, "%\n")

# 7. COVID indicator (0 for 2025)
cat("  covid.ind: 0\n\n")

# ===========================
# CREATE NEW INPUT FILE
# ===========================

cat("Creating updated input file...\n")

# Create new row - match the date format of existing data
new_row <- data.frame(
  Date = "7/1/2025",
  gdp.log = gdp_log_q3,
  inflation = inflation_q3,
  inflation.expectations = infexp_q3_avg,
  oil.price.inflation = oil_inflation_q3,
  import.price.inflation = imp_inflation_q3,
  interest = ff_q3_2025,
  covid.ind = 0,
  stringsAsFactors = FALSE
)

# Combine with existing data
updated_data <- rbind(existing, new_row)

# Verify date consistency
cat("\nVerifying date format consistency:\n")
cat("  Last existing date:", tail(existing$Date, 1), "\n")
cat("  New Q3 2025 date:", new_row$Date, "\n")

# Save
write.csv(updated_data, "LW_2023_Replication_Code/inputData/LW_input_with_Q3_2025.csv", 
          row.names = FALSE)

cat("\nUpdated input file created!\n")
cat("  File: LW_2023_Replication_Code/inputData/LW_input_with_Q3_2025.csv\n")
cat("  Total rows:", nrow(updated_data), "\n\n")

cat("Q3 2025 row:\n")
print(new_row)

cat("\n=== VALIDATION CHECKS ===\n")
cat("GDP log - Q3 2025:", round(gdp_log_q3, 6), "\n")
cat("  Recent range (Q1 2024 - Q2 2025):", round(min(tail(existing$gdp.log, 6)), 6), 
    "to", round(max(tail(existing$gdp.log, 6)), 6), "\n")
cat("  Status:", ifelse(gdp_log_q3 >= min(tail(existing$gdp.log, 6)) & 
                         gdp_log_q3 <= max(tail(existing$gdp.log, 6)) + 0.05, 
                         "✓ Within expected range", "⚠ Outside expected range"), "\n\n")

cat("Inflation - Q3 2025:", inflation_q3, "%\n")
cat("  Recent range (Q1 2024 - Q2 2025):", round(min(tail(existing$inflation, 6)), 2), 
    "to", round(max(tail(existing$inflation, 6)), 2), "%\n")
cat("  Status:", ifelse(inflation_q3 >= 1.5 & inflation_q3 <= 5, 
                         "✓ Within reasonable range", "⚠ Outside reasonable range"), "\n\n")

cat("Interest rate - Q3 2025:", ff_q3_2025, "%\n")
cat("  Recent range (Q1 2024 - Q2 2025):", round(min(tail(existing$interest, 6)), 2), 
    "to", round(max(tail(existing$interest, 6)), 2), "%\n")
cat("  Status:", ifelse(ff_q3_2025 >= 3 & ff_q3_2025 <= 6, 
                         "✓ Within reasonable range", "⚠ Outside reasonable range"), "\n\n")

cat("Inflation expectations - Q3 2025:", round(infexp_q3_avg, 4), "%\n")
cat("  Recent range (Q1 2024 - Q2 2025):", round(min(tail(existing$inflation.expectations, 6)), 2), 
    "to", round(max(tail(existing$inflation.expectations, 6)), 2), "%\n")
cat("  Status:", ifelse(infexp_q3_avg >= 1.5 & infexp_q3_avg <= 4, 
                         "✓ Within reasonable range", "⚠ Outside reasonable range"), "\n\n")

cat("\n=== BUILD COMPLETE ===\n")
