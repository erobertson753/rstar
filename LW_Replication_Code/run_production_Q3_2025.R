# Production Run: LW Model with Official Parameters through Q3 2025
# Uses official lambda values and extends data to 2025 Q3

cat("=== PRODUCTION RUN: R* ESTIMATION THROUGH Q3 2025 ===\n\n")

# Set working directory
working.dir <- getwd()
code.dir <- getwd()

# =================
# LOAD R PACKAGES
# =================

if (!require("tis")) {install.packages("tis"); library("tis")}
if (!require("nloptr")) {install.packages("nloptr"); library("nloptr")}
if (!require("mFilter")) {install.packages("mFilter"); library("mFilter")}
if (!require("openxlsx")) {install.packages("openxlsx"); library("openxlsx")}

# ==================
# LOAD CODE PACKAGES
# ==================

source("kalman.log.likelihood.R")
source("kalman.states.R")
source("kalman.standard.errors.R")
source("median.unbiased.estimator.stage1.R")
source("median.unbiased.estimator.stage2.R")
source("calculate.covariance.R")
source("log.likelihood.wrapper.R")
source("kalman.states.wrapper.R")
source("unpack.parameters.stage1.R")
source("unpack.parameters.stage2.R")
source("unpack.parameters.stage3.R")
source("rstar.stage1.R")
source("rstar.stage2.R")
source("rstar.stage3.R")
source("utilities.R")
source("format.output.R")

# =================
# DEFINE VARIABLES
# =================

sample.start <- c(1961,1)
sample.end   <- c(2025,3)  # Extended to Q3 2025
est.data.start <- shiftQuarter(sample.start,-8)

xi.00.stage1 <- NA
xi.00.stage2 <- NA
xi.00.stage3 <- NA

P.00.stage1 <- NA
P.00.stage2 <- NA
P.00.stage3 <- NA

a.r.constraint <- -0.0025
b.y.constraint <- 0.025

# Disable standard errors for speed (can enable for final production run)
run.se <- FALSE
niter <- 5000

# =================
# COVID SETTINGS
# =================

use.kappa <- TRUE
fix.phi <- NA

# Kappa inputs for time-varying volatility
kappa.inputs <- data.frame(
  name = c("kappa2020Q2-Q4", "kappa2021", "kappa2022"),
  year = c(2020, 2021, 2022),
  init = c(1, 1, 1),
  lower.bound = c(1, 1, 1),
  upper.bound = c(Inf, Inf, Inf),
  T.start = rep(NA, 3),
  T.end = rep(NA, 3)
)

if (use.kappa) {
  n.kappa <- nrow(kappa.inputs)
  for (k in 1:n.kappa) {
    covid.variance.start.yq <- c(kappa.inputs$year[k],1) - sample.start
    kappa.inputs$T.start[k] <- max(covid.variance.start.yq[1]*4 + covid.variance.start.yq[2] +1,0)
    
    covid.variance.end.yq <- c(kappa.inputs$year[k],4) - sample.start
    kappa.inputs$T.end[k] <- max(covid.variance.end.yq[1]*4 + covid.variance.end.yq[2] +1,0)
    
    if (kappa.inputs$year[k]==2020) {
      kappa.inputs$T.start[k] <- kappa.inputs$T.start[k] + 1
    }
  }
}

# =================
# INPUT DATA
# =================

cat("Reading data from CSV file (extended to Q3 2025)...\n")

# CHANGE 1: Read CSV with Q3 2025 data instead of Excel
data <- read.csv("inputData/LW_input_with_Q3_2025.csv", stringsAsFactors = FALSE)

# Convert date column
data$Date <- as.Date(data$Date, format = "%m/%d/%Y")

cat("Data loaded successfully!\n")
cat("Sample size:", nrow(data), "quarters\n")
cat("Date range:", format(min(data$Date)), "to", format(max(data$Date)), "\n\n")

# Extract series using exact column names from CSV
log.output                      <- data$gdp.log
inflation                       <- data$inflation
relative.oil.price.inflation    <- data$oil.price.inflation - inflation
relative.import.price.inflation <- data$import.price.inflation - inflation
nominal.interest.rate           <- data$interest
inflation.expectations          <- data$inflation.expectations
covid.dummy                     <- data$covid.ind

real.interest.rate              <- nominal.interest.rate - inflation.expectations

# =================
# ESTIMATION
# =================

cat("Starting Stage 1...\n")
out.stage1 <- rstar.stage1(log.output=log.output,
                           inflation=inflation,
                           relative.oil.price.inflation=relative.oil.price.inflation,
                           relative.import.price.inflation=relative.import.price.inflation,
                           covid.dummy=covid.dummy,
                           sample.end=sample.end,
                           b.y.constraint=b.y.constraint,
                           xi.00=xi.00.stage1,
                           P.00=P.00.stage1,
                           use.kappa=use.kappa,
                           kappa.inputs=kappa.inputs,
                           fix.phi=fix.phi)

# CHANGE 2: Hard-code official lambda_g value
lambda.g.estimated <- median.unbiased.estimator.stage1(log.output)
lambda.g <- 0.064  # OFFICIAL VALUE
cat("lambda_g estimated:", round(lambda.g.estimated, 4), "\n")
cat("lambda_g OVERRIDDEN to official value:", round(lambda.g, 4), "\n\n")

cat("Starting Stage 2...\n")
out.stage2 <- rstar.stage2(log.output=log.output,
                           inflation=inflation,
                           relative.oil.price.inflation=relative.oil.price.inflation,
                           relative.import.price.inflation=relative.import.price.inflation,
                           real.interest.rate=real.interest.rate,
                           covid.dummy=covid.dummy,
                           lambda.g=lambda.g,
                           sample.end=sample.end,
                           a.r.constraint=a.r.constraint,
                           b.y.constraint=b.y.constraint,
                           xi.00=xi.00.stage2,
                           P.00=P.00.stage2,
                           use.kappa=use.kappa,
                           kappa.inputs=kappa.inputs,
                           fix.phi=fix.phi)

# CHANGE 3: Hard-code official lambda_z value
lambda.z.estimated <- median.unbiased.estimator.stage2(out.stage2$y, out.stage2$x, out.stage2$kappa.vec)
lambda.z <- 0.022  # OFFICIAL VALUE
cat("lambda_z estimated:", lambda.z.estimated, "\n")
cat("lambda_z OVERRIDDEN to official value:", round(lambda.z, 4), "\n\n")

cat("Starting Stage 3...\n")
out.stage3 <- rstar.stage3(log.output=log.output,
                           inflation=inflation,
                           relative.oil.price.inflation=relative.oil.price.inflation,
                           relative.import.price.inflation=relative.import.price.inflation,
                           real.interest.rate=real.interest.rate,
                           covid.dummy=covid.dummy,
                           lambda.g=lambda.g,
                           lambda.z=lambda.z,
                           sample.end=sample.end,
                           a.r.constraint=a.r.constraint,
                           b.y.constraint=b.y.constraint,
                           run.se=run.se,
                           xi.00=xi.00.stage3,
                           P.00=P.00.stage3,
                           use.kappa=use.kappa,
                           kappa.inputs=kappa.inputs,
                           fix.phi=fix.phi)

# =================
# OUTPUT
# =================

output.us <- format.output(estimation=out.stage3,
                           real.rate=real.interest.rate,
                           covid.dummy=covid.dummy,
                           start=sample.start,
                           end=sample.end,
                           run.se=run.se)

# Save to CSV
write.csv(output.us, "output/rstar_production_Q3_2025.csv", row.names=FALSE)

cat("\n=== PRODUCTION ESTIMATION COMPLETE ===\n")
cat("Sample period:", format(sample.start[1]), "Q", sample.start[2], "to", 
    format(sample.end[1]), "Q", sample.end[2], "\n")
cat("Number of quarters:", nrow(output.us), "\n\n")

# Get the actual r* column
rstar_col <- which(sapply(output.us, is.numeric))[1]

cat("LATEST R* ESTIMATES:\n")
cat("====================\n")
cat("Q3 2025 (one-sided):", round(tail(output.us[,rstar_col], 1), 4), "%\n\n")

cat("Last 5 quarters:\n")
last_5 <- tail(output.us[,1:6], 5)
print(last_5)

cat("\nParameters used:\n")
cat("  lambda_g:", lambda.g, "(official value)\n")
cat("  lambda_z:", lambda.z, "(official value)\n")
cat("  use.kappa:", use.kappa, "\n")
cat("  fix.phi:", fix.phi, "(estimated)\n")

cat("\nOutput saved to: output/rstar_production_Q3_2025.csv\n")
cat("======================================\n")
