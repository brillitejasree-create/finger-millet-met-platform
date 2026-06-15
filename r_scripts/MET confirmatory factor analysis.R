library(readxl)
library(lavaan)
library(lavaanPlot)
library(haven)
library(foreign)
library(DiagrammeR)
library(semPlot)

############

library(lavaan)
library(semPlot)

# Step 1: Standardize all variables to resolve scaling issues
MET_scaled <- as.data.frame(scale(MET5R[, c("PH", "LL", "LW", "LMT", 
                                            "DFF", "TP", "PTP", "FL", "EL", "FW", 
                                            "NFE", "TSW", "DM", "GYP")]))

m1_respecified <- '
  # Measurement model
    vegetative.growth =~ PH + LL + LW + LMT
    yield.attributes =~ DFF + TP + PTP + FL + EL + FW + NFE + TSW + DM + GYP
  
  # Structural regression
    yield.attributes ~ vegetative.growth
  
  # Theoretically justified residual correlations
  # (within-construct or methodologically related pairs)
    TP ~~ PTP          # Both measure tillering capacity
    FL ~~ EL           # Both measure panicle/inflorescence dimensions
    FL ~~ FW           # Flag leaf dimensions (length and width)
    NFE ~~ TSW         # Both measure grain filling components
    DM ~~ GYP          # Biomass and yield are physiologically coupled
    DFF ~~ DM          # Phenology and biomass accumulation
'

fit_respec <- sem(m1_respecified, data = MET_scaled, estimator = "MLR")

# Check fit
fitMeasures(fit_respec, c("chisq", "df", "pvalue", "CFI", "TLI", "RMSEA", "SRMR", "AIC", "BIC"))

# If still poor, consider:
# Option 1: Higher-order factor model (yield sub-factors)
# Option 2: Exploratory SEM (ESEM) for yield attributes
# Option 3: Bayesian SEM with informative priors

###################

library(lavaan)
library(semPlot)

# CRITICAL FIX 1: Standardize variables to resolve variance scaling issue
MET_scaled <- as.data.frame(scale(MET5R[, c("PH", "LL", "LW", "LMT", 
                                            "DFF", "TP", "PTP", "FL", "EL", "FW", 
                                            "NFE", "TSW", "DM", "GYP")]))

# CRITICAL FIX 2: Remove redundant indicators (based on near-perfect correlations)
# Drop: LMT (r≈0.995 with PH), PTP (r≈0.94 with TP), FW (r≈0.94 with EL), TSW (r≈0.985 with NFE)
# Also consider dropping DM or GYP (r≈0.88)

m1 <- '
  # Measurement model
    vegetative.growth =~ PH + LL + LW
    yield.attributes =~ DFF + TP + FL + EL + NFE + DM + GYP
  
  # Structural regression
    yield.attributes ~ vegetative.growth
  
  # Residual correlations (within-factor only, if theoretically justified)
  # TP ~~ PTP  # if keeping PTP
  # NFE ~~ TSW # if keeping TSW
'

# Use robust estimator for non-normal data
fit <- sem(m1, data = MET_scaled, estimator = "MLR")

# Check convergence and diagnostics
inspect(fit, "converged")
fitMeasures(fit, c("chisq", "df", "pvalue", "CFI", "TLI", "RMSEA", "SRMR"))

# Standardized estimates
summary(fit, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE)

# Modification indices (if needed)
modindices(fit, sort = TRUE, maximum.number = 10)

# Publication-quality path diagram
semPaths(fit, "std", layout = "tree2", style = "lisrel",
         residuals = TRUE, intercepts = TRUE,
         nCharNodes = 3, edge.color = "black",
         sizeMan = 8, sizeLat = 10,
         label.prop = 0.9, curvePivot = TRUE)

############################################################

library(lavaan)
library(semPlot)

MET_scaled <- as.data.frame(scale(MET5R[, c("PH", "LL", "LW", "LMT", 
                                            "DFF", "TP", "PTP", "FL", "EL", "FW", 
                                            "NFE", "TSW", "DM", "GYP")]))

m_path <- '
  # Vegetative growth as latent (this works)
    vegetative.growth =~ PH + LL + LW + LMT
  
  # All yield variables as observed outcomes
    TP ~ vegetative.growth
    PTP ~ vegetative.growth
    FL ~ vegetative.growth
    EL ~ vegetative.growth
    FW ~ vegetative.growth
    NFE ~ vegetative.growth
    TSW ~ vegetative.growth
    DM ~ vegetative.growth
    GYP ~ vegetative.growth
    DFF ~ vegetative.growth
  
  # Correlated residuals among yield variables
    TP ~~ PTP
    FL ~~ EL + FW
    NFE ~~ TSW
    DM ~~ GYP
    EL ~~ FL
'

fit_path <- sem(m_path, data = MET_scaled, estimator = "MLR")
fitMeasures(fit_path, c("chisq", "df", "pvalue", "CFI", "TLI", "RMSEA", "SRMR"))
summary(fit_path, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE)

library(lavaan)

# ESEM with 3-4 factors for yield indicators
m_esem <- '
  # Vegetative growth (confirmatory)
    vegetative.growth =~ PH + LL + LW + LMT
  
  # ESEM block: yield indicators load on 3 factors with rotation
    yield1 =~ DFF + TP + PTP + FL + EL + FW + NFE + TSW + DM + GYP
    yield2 =~ DFF + TP + PTP + FL + EL + FW + NFE + TSW + DM + GYP
    yield3 =~ DFF + TP + PTP + FL + EL + FW + NFE + TSW + DM + GYP
  
  # Target rotation (simplified)
    yield1 ~ vegetative.growth
    yield2 ~ vegetative.growth
    yield3 ~ vegetative.growth
'

# Note: True ESEM requires Mplus or specialized R packages (semTools, esem)

# Standardized multiple regression
lm_yield <- lm(GYP ~ PH + LL + LW + LMT + DFF + TP + PTP + FL + EL + FW + NFE + TSW + DM, 
               data = MET_scaled)
summary(lm_yield)

fitMeasures(fit_path, c("chisq", "df", "pvalue", "CFI", "TLI", "RMSEA", "SRMR", "AIC", "BIC"))
summary(fit_path, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE)

# Modification indices (if needed)
modindices(fit_path, sort = TRUE, maximum.number = 10)

# Publication-quality path diagram
semPaths(fit, "std", layout = "tree2", style = "lisrel",
         residuals = TRUE, intercepts = TRUE,
         nCharNodes = 3, edge.color = "black",
         sizeMan = 8, sizeLat = 10,
         label.prop = 0.9, curvePivot = TRUE)
