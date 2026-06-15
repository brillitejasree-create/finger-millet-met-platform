# ==========================================================
# FIXED WAASB ANALYSIS FOR MET DATA
# ==========================================================

library(metan)
library(writexl)
library(dplyr)

# ---------- 1. VERIFY DATA STRUCTURE ----------
print("MET data structure:")
str(MET)
print("Column names:")
print(colnames(MET))

# Ensure columns are properly named/typed
MET <- MET %>%
  mutate(
    ENVIRONMENT = as.factor(ENVIRONMENT),
    GENOTYPE = as.factor(GENOTYPE),
    REPLICATION = as.factor(REPLICATION)
  )
# ---------- 2. BASIC WAASB MODEL (FIXED EFFECTS) ----------
# CORRECT: No 'random' argument for fixed effects model
model_fixed <- waasb(
  MET,
  env = ENVIRONMENT,
  gen = GENOTYPE,
  rep = REPLICATION,
  resp = everything()  # Analyzes all numeric response variables
)

# Genetic parameters for fixed model
genpar_fixed <- get_model_data(model_fixed, "genpar")
print("Fixed effects genetic parameters:")
print(genpar_fixed)

# ---------- 3. RANDOM EFFECTS MODEL (CORRECT SYNTAX) ----------
# CORRECT: random = "gen" (not "all")
# "all" is NOT valid - only "gen", "env", or "block"

model_random <- waasb(
  MET,
  env = ENVIRONMENT,
  gen = GENOTYPE,
  rep = REPLICATION,
  resp = everything(),
  random = "gen",      # Random genotype effects only
  verbose = TRUE
)

# Genetic parameters
genpar_random <- get_model_data(model_random, "genpar")
print("Random effects genetic parameters:")
print(genpar_random)

# WAASBY index - CORRECT syntax
waasby_random <- get_model_data(model_random, what = "WAASBY")
print("WAASBY index (random model):")
print(waasby_random)

# Save to Excel
write_xlsx(waasby_random, "waasby_random.xlsx")

# ---------- 4. MULTI-TRAIT MODEL WITH CUSTOM WEIGHTS ----------
# CORRECT mresp format: vector with "l" (lower) or "h" (higher) is better
# One value per response variable, in order

# First, identify response variables (numeric columns that are not env/gen/rep)
resp_vars <- setdiff(
  names(MET)[sapply(MET, is.numeric)],
  c("ENVIRONMENT", "GENOTYPE", "REPLICATION")
)

print("Response variables detected:")
print(resp_vars)

# Create mresp vector - "h" for higher is better, "l" for lower is better
# GYP = "h" (higher yield is better)
# Most physiological traits = "h" (higher is better)
# Example: if first 5 are "l" (lower is better) and rest "h":
mresp_vec <- c("l", "l", "l", "l", "l", "h", "h", "h", "h", "h", "h", "l", "h", "h", "h", "h", "h", "h", "h")

# CORRECT: mresp must match number of response variables exactly
# If you have 14 traits, mresp must have 14 elements
n_resp <- length(resp_vars)
mresp_correct <- rep("h", n_resp)  # Default: higher is better for all
# Modify specific traits if needed:
# mresp_correct[1:5] <- "l"  # First 5 traits: lower is better

# CORRECT: wresp as single value (applied to all) or vector matching resp
model_weighted <- waasb(
  MET,
  env = ENVIRONMENT,
  gen = GENOTYPE,
  rep = REPLICATION,
  resp = everything(),
  random = "gen",
  mresp = mresp_correct,    # Must match number of response variables
  wresp = 50,               # Single value: 50% weight on stability
  verbose = TRUE
)

# Genetic parameters
genpar_weighted <- get_model_data(model_weighted, "genpar")
print("Weighted model genetic parameters:")
print(genpar_weighted)

# Get WAASBY for specific trait
waasby_gyp <- get_model_data(model_weighted, what = "WAASBY")
print("WAASBY (all traits):")
print(waasby_gyp)

# Save
write_xlsx(waasby_gyp, "waasby_weighted_all.xlsx")

# ---------- 5. SINGLE-TRAIT GYP MODEL ----------
model_gyp <- waasb(
  MET,
  env = ENVIRONMENT,
  gen = GENOTYPE,
  rep = REPLICATION,
  resp = GYP,              # Single response variable
  random = "gen"
)

# Plot GGE biplot for GYP
plot_scores(model_gyp, type = 3)

# Alternative: Save plot
png("GYP_WAASB_biplot.png", width = 800, height = 600)
plot_scores(model_gyp, type = 3)
dev.off()

# ---------- 6. EXTRACT AND SAVE ALL RESULTS ----------
# Get complete model data for GYP
gyp_data <- model_gyp$GYP$model
print("GYP model data:")
print(gyp_data, n = Inf)  # CORRECT: print.data.frame method, not pipe
write_xlsx(gyp_data, "gyp_data.xlsx")

# WAASBY for GYP specifically
waasby_gyp_only <- get_model_data(model_gyp, what = "WAASBY")
print("WAASBY for GYP:")
print(waasby_gyp_only)

write_xlsx(waasby_gyp_only, "waasby_GYP_only.xlsx")

# ---------- 7. COMPREHENSIVE OUTPUT FOR ALL TRAITS ----------
# Loop through all response variables and save WAASBY
all_waasby <- lapply(names(model_weighted), function(trait) {
  df <- get_model_data(model_weighted, what = "WAASBY")[[trait]]
  if(!is.null(df)) {
    df$Trait <- trait
    return(df)
  }
  return(NULL)
})

# Combine all
all_waasby_df <- bind_rows(all_waasby)
write_xlsx(all_waasby_df, "ALL_TRAITS_waasby.xlsx")

# ---------- 8. GENETIC PARAMETERS SUMMARY ----------
# Extract all genetic parameters into a summary table
genpar_summary <- get_model_data(model_weighted, "genpar")
print("Complete genetic parameters summary:")
print(genpar_summary)

# Save
write_xlsx(genpar_summary, "Genetic_Parameters_Summary.xlsx")

# ---------- 9. FINAL SAVES ----------
save(model_fixed, model_random, model_weighted, model_gyp,
     genpar_fixed, genpar_random, genpar_weighted,
     waasby_random, waasby_gyp, all_waasby_df,
     file = "WAASB_Results.RData")

cat("
========================================
WAASB ANALYSIS COMPLETE
========================================

Models fitted:
  1. Fixed effects (model_fixed)
  2. Random genotype effects (model_random)
  3. Weighted multi-trait (model_weighted)
  4. GYP single-trait (model_gyp)

Files saved:
  - waasby_random.xlsx
  - waasby_weighted_all.xlsx
  - waasby_GYP_only.xlsx
  - ALL_TRAITS_waasby.xlsx
  - Genetic_Parameters_Summary.xlsx
  - GYP_WAASB_biplot.png
  - WAASB_Results.RData

========================================
")
