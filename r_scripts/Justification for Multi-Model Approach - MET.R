#Justification for Multi-Model Approach
##### Recommended Further Analyses for Publication Enhancement

# ============================================
# 1. VARIANCE COMPONENTS (matches waasb)
# ============================================

# Option A: ENV and REP as FIXED (matches waasb exactly)
library(lmerTest)  # for p-values
model_fixed_env <- lmer(GYP ~ ENVIRONMENT + (1|GENOTYPE) + (1|GENOTYPE:ENVIRONMENT) + 
                          (1|REPLICATION:ENVIRONMENT), 
                        data = MET)
summary(model_fixed_env)

# Option B: All random (your current model - also valid)
model_random <- lmer(GYP ~ (1|GENOTYPE) + (1|ENVIRONMENT) + (1|GENOTYPE:ENVIRONMENT) + 
                       (1|REPLICATION:ENVIRONMENT), 
                     data = MET)
summary(model_random)

# Extract variance components
vc <- VarCorr(model_random)
print(vc, comp = "Variance")

#### Improve Model with BLUP Extraction

library(lme4)

blups <- ranef(model_fixed_env)$GENOTYPE %>%
  as.data.frame()

colnames(blups) <- c("BLUP")

blups$GEN <- rownames(ranef(model_fixed_env)$GENOTYPE)

head(blups)

library(ggplot2)

ggplot(blups,
       aes(x = reorder(GEN, BLUP),
           y = BLUP,
           fill = BLUP > 0)) +
  
  geom_col(width = 0.7) +
  
  coord_flip() +
  
  scale_fill_manual(values = c("tomato", "royalblue")) +
  
  labs(title = "Genotypic BLUPs for Grain Yield",
       x = "Genotype",
       y = "BLUP") +
  
  theme_bw(base_size = 14)

####### Add Random Regression / Reaction Norm

env_means <- MET %>%
  group_by(ENVIRONMENT) %>%
  summarise(env_mean = mean(GYP))

grand_mean <- mean(MET$GYP)

env_means$EI <- env_means$env_mean - grand_mean

MET2 <- left_join(MET, env_means, by = "ENVIRONMENT")

library(lme4)

rn_model <- lmer(
  GYP ~ EI +
    (EI | GENOTYPE),
  data = MET2
)

summary(rn_model)

# ============================================
# 2. FINLAY-WILKINSON REGRESSION
# ============================================

# Compute environmental means
env_means <- MET %>%
  group_by(ENVIRONMENT) %>%
  summarise(env_mean = mean(GYP), .groups = "drop")

# Finlay-Wilkinson: regress each genotype's yield on environmental means
fw_data <- MET %>%
  left_join(env_means, by = "ENVIRONMENT") %>%
  group_by(GENOTYPE) %>%
  summarise(
    mean_yield = mean(GYP),
    b_fw = coef(lm(GYP ~ env_mean))[2],      # Regression slope (stability)
    a_fw = coef(lm(GYP ~ env_mean))[1],      # Intercept
    r2_fw = summary(lm(GYP ~ env_mean))$r.squared,
    .groups = "drop"
  ) %>%
  mutate(
    fw_rank = rank(b_fw),                      # Lower slope = more stable
    stability_class = case_when(
      b_fw < 0.9 ~ "Stable (b < 0.9)",
      b_fw <= 1.1 ~ "Average (0.9 ≤ b ≤ 1.1)",
      TRUE ~ "Unstable (b > 1.1)"
    )
  )

print(fw_data)

# ============================================
# 3. SHUKLA'S STABILITY VARIANCE
# ============================================

# Compute Shukla's stability variance for each genotype
shukla_data <- MET %>%
  group_by(ENVIRONMENT) %>%
  mutate(env_mean = mean(GYP)) %>%
  ungroup() %>%
  group_by(GENOTYPE) %>%
  summarise(
    mean_yield = mean(GYP),
    shukla_var = sum((GYP - mean_yield - env_mean + mean(GYP))^2) / (n_distinct(ENVIRONMENT) - 1),
    .groups = "drop"
  ) %>%
  mutate(shukla_rank = rank(shukla_var))

print(shukla_data)

# ============================================
# 4. COMBINE ALL INDICES INTO MASTER TABLE
# ============================================

master_table <- fw_data %>%
  left_join(shukla_data %>% select(GENOTYPE, shukla_var, shukla_rank), by = "GENOTYPE") %>%
  arrange(b_fw)

print(master_table)

# ============================================
# 5. PUBLICATION FIGURES
# ============================================

# Figure 1: Finlay-Wilkinson Regression Plot
fw_plot <- ggplot(MET %>% left_join(env_means, by = "ENVIRONMENT"), 
                  aes(x = env_mean, y = GYP, color = GENOTYPE)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.8) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(x = "Environmental Mean Yield", 
       y = "Genotype Yield",
       title = "Finlay-Wilkinson Regression: Genotype Response to Environment") +
  theme_minimal() +
  theme(legend.position = "none")
print(fw_plot)

# Figure 2: Stability Slope vs Mean Yield
stability_plot <- ggplot(fw_data, aes(x = mean_yield, y = b_fw)) +
  geom_point(size = 3, color = "#2E86AB") +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  geom_text(aes(label = GENOTYPE), vjust = -1, size = 3) +
  labs(x = "Mean Yield", 
       y = "Finlay-Wilkinson Regression Slope (b)",
       title = "Yield vs. Stability (Finlay-Wilkinson b)") +
  theme_minimal()
print(stability_plot)

#5. Bayesian Stability Analysis
library(brms)
bayes_model <- brm(GYP ~ GENOTYPE * ENVIRONMENT + (1|REPLICATION:ENVIRONMENT), 
                   data = MET, 
                   family = gaussian(),
                   prior = c(prior(normal(0, 10), class = b)),
                   chains = 4, iter = 2000)
#Publication value: Posterior probability that G9 > G26 in mean yield, or probability that G9's stability variance is lower than G10's, provides intuitive decision-support metrics for breeders. Journals like Theoretical and Applied Genetics increasingly value Bayesian frameworks for their direct probabilistic interpretation.

#8. Spatial Analysis and Field Trend Correction
library(SpATS)
spatial_model <- SpATS(response = "GYP", genotype = "GENOTYPE", 
                       row = "ROW", col = "COL", 
                       data = MET, nseg = c(10, 10))
#Publication value: Field trials inevitably exhibit fertility gradients, irrigation irregularities, and pest patches. Spatial correction via P-splines or autoregressive models reduces error variance, increasing heritability and selection accuracy. If spatial trends are strong at E5 (Dharwad), uncorrected analysis may misrank genotypes.
