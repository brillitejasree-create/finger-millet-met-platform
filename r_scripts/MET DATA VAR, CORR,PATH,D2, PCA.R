library(variability)
library(readxl)
library(writexl)

gen.var(MET5R[3:16],MET5R$GENOTYPE,MET5R$REPLICATION)

pheno.corr(MET5R[3:16],MET5R$GENOTYPE,MET5R$REPLICATION)

geno.corr(MET5R[3:16],MET5R$GENOTYPE,MET5R$REPLICATION)

pheno.path(MET5R[16],MET5R[5:15],MET5R$GENOTYPE,MET5R$REPLICATION)

geno.path(MET5R[16],MET5R[5:15],MET5R$GENOTYPE,MET5R$REPLICATION)


################### plots 

# ============================================
# COMPLETE ROBUST FIGURE GENERATION SCRIPT
# ============================================

# Install packages if needed
packages <- c("readxl", "variability", "corrplot", "ggplot2", "reshape2", 
              "gridExtra", "RColorBrewer")
install_if_missing <- function(pkg) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}
invisible(sapply(packages, install_if_missing))

# Set working directory
setwd("C:/Users/BHARGAVI/Documents")

# Define traits
trait_names <- c("DFF", "PH", "LL", "LW", "LMT", "TP", "PTP", "EL", "FL", 
                 "NFE", "FW", "DM", "TSW", "GYP")

# ============================================
# HELPER FUNCTION: Clean correlation matrix
# ============================================
clean_corr_matrix <- function(corr_obj) {
  # Extract matrix (handles both pheno.corr and geno.corr output)
  if ("PhenotypicCorrelation" %in% names(corr_obj)) {
    mat <- corr_obj$PhenotypicCorrelation
  } else if ("GenotypicCorrelation" %in% names(corr_obj)) {
    mat <- corr_obj$GenotypicCorrelation
  } else {
    mat <- corr_obj
  }
  
  # Convert to numeric (remove significance markers)
  mat_clean <- apply(mat, c(1, 2), function(x) {
    val <- suppressWarnings(as.numeric(gsub("[^0-9.-]", "", x)))
    if (is.na(val)) 0 else val
  })
  
  # Set diagonal to 1
  diag(mat_clean) <- 1
  
  # CRITICAL: Clip to [-1, 1] to prevent corrplot errors
  mat_clean <- pmax(pmin(mat_clean, 1), -1)
  
  # Replace NaN/Inf with 0
  mat_clean[is.na(mat_clean) | is.infinite(mat_clean)] <- 0
  
  return(mat_clean)
}

# ============================================
# FIGURE 1: GENETIC VARIABILITY
# ============================================
cat("Generating Figure 1...\n")

genetic_params <- data.frame(
  Trait = factor(trait_names, levels = trait_names),
  GCV = c(6.89, 9.04, 6.58, 8.29, 7.89, 9.12, 7.89, 7.34, 8.18, 7.05, 14.28, 5.74, 12.71, 11.09),
  PCV = c(9.28, 12.10, 11.28, 16.77, 13.80, 16.59, 18.19, 11.32, 12.82, 13.22, 20.01, 7.30, 18.38, 18.16),
  ECV = c(6.22, 8.04, 9.16, 14.59, 11.32, 13.86, 16.39, 8.62, 9.88, 11.19, 14.02, 4.51, 13.28, 14.39),
  h2 = c(0.551, 0.558, 0.341, 0.244, 0.327, 0.302, 0.188, 0.420, 0.407, 0.284, 0.509, 0.618, 0.478, 0.373),
  GAM = c(10.54, 13.90, 7.92, 8.44, 9.29, 10.33, 7.04, 9.80, 10.74, 7.74, 20.99, 9.31, 18.09, 13.94)
)

genetic_long <- melt(genetic_params, id.vars = "Trait", variable.name = "Parameter")

p1a <- ggplot(subset(genetic_long, Parameter %in% c("GCV", "PCV", "ECV")), 
              aes(x = Trait, y = value, fill = Parameter)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7, color = "black", size = 0.2) +
  scale_fill_manual(values = c("GCV" = "#2E7D32", "PCV" = "#1565C0", "ECV" = "#C62828")) +
  labs(x = "Trait", y = "Coefficient of Variation (%)", title = "(a) Coefficients of Variation") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        plot.title = element_text(face = "bold", size = 12),
        legend.position = "top", legend.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, size = 0.5)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 22))

p1b <- ggplot(subset(genetic_long, Parameter %in% c("h2", "GAM")), 
              aes(x = Trait, y = value, fill = Parameter)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7, color = "black", size = 0.2) +
  scale_fill_manual(values = c("h2" = "#6A1B9A", "GAM" = "#E65100"),
                    labels = c("Heritability (h²)", "Genetic Advance (%)")) +
  labs(x = "Trait", y = "Value", title = "(b) Heritability and Genetic Advance") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        plot.title = element_text(face = "bold", size = 12),
        legend.position = "top", legend.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, size = 0.5)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 22))

fig1 <- grid.arrange(p1a, p1b, ncol = 1, 
                     top = "Figure 1. Genetic variability parameters for 14 quantitative traits in 35 genotypes")
ggsave("Figure_1_Genetic_Variability.png", fig1, width = 10, height = 12, dpi = 600, bg = "white")

# ============================================
# FIGURE 2: GCV vs HERITABILITY
# ============================================
cat("Generating Figure 2...\n")

scatter_data <- data.frame(
  Trait = trait_names,
  GCV = genetic_params$GCV,
  h2 = genetic_params$h2,
  GAM = genetic_params$GAM,
  Group = ifelse(genetic_params$h2 >= 0.5 & genetic_params$GCV >= 10, "Group I: High h² + High GCV",
                 ifelse(genetic_params$h2 >= 0.3 & genetic_params$GCV >= 6, "Group II: Moderate h² + Moderate GCV",
                        "Group III: Low h² + Low GCV"))
)

fig2 <- ggplot(scatter_data, aes(x = h2, y = GCV, color = Group, size = GAM)) +
  geom_point(alpha = 0.85, stroke = 1) +
  geom_text(aes(label = Trait), vjust = -1.3, hjust = 0.5, size = 3.5, fontface = "bold") +
  scale_color_manual(values = c("Group I: High h² + High GCV" = "#D32F2F",
                                "Group II: Moderate h² + Moderate GCV" = "#F57C00",
                                "Group III: Low h² + Low GCV" = "#7B1FA2")) +
  scale_size_continuous(range = c(4, 12), name = "Genetic Advance (%)") +
  geom_vline(xintercept = 0.5, linetype = "dashed", color = "gray50", size = 0.5) +
  geom_hline(yintercept = 10, linetype = "dashed", color = "gray50", size = 0.5) +
  labs(x = "Broad-sense Heritability (h²)", y = "Genotypic Coefficient of Variation (%)",
       title = "Figure 2. Scatter plot of GCV versus heritability showing selection strategy groups") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
        legend.position = "bottom", legend.title = element_text(face = "bold", size = 10),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, size = 0.5)) +
  scale_x_continuous(limits = c(0.1, 0.7), breaks = seq(0.1, 0.7, 0.1)) +
  scale_y_continuous(limits = c(4, 22), breaks = seq(5, 20, 5))

ggsave("Figure_2_GCV_vs_Heritability.png", fig2, width = 10, height = 8, dpi = 600, bg = "white")

# ============================================
# FIGURE 3: PHENOTYPIC CORRELATION
# ============================================
cat("Generating Figure 3...\n")

pheno_corr <- pheno.corr(MET5R[3:16], MET5R$GENOTYPE, MET5R$REPLICATION)
pheno_clean <- clean_corr_matrix(pheno_corr)

pheno_melt <- melt(pheno_clean)
colnames(pheno_melt) <- c("Trait1", "Trait2", "Correlation")

fig3 <- ggplot(pheno_melt, aes(x = Trait2, y = Trait1, fill = Correlation)) +
  geom_tile(color = "white", size = 0.2) +
  geom_text(aes(label = sprintf("%.2f", Correlation)), 
            size = 2.6, fontface = "bold",
            color = ifelse(abs(pheno_melt$Correlation) > 0.6, "white", "black")) +
  scale_fill_gradient2(low = "#D32F2F", mid = "white", high = "#1565C0",
                       midpoint = 0, limit = c(-1, 1), name = "Phenotypic\nCorrelation") +
  labs(title = "Figure 3. Phenotypic correlation coefficients among 14 quantitative traits") +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9, face = "bold"),
        axis.text.y = element_text(size = 9, face = "bold"),
        plot.title = element_text(face = "bold", size = 13, hjust = 0.5, margin = margin(b = 20)),
        legend.position = "right", panel.grid = element_blank()) +
  coord_fixed()

ggsave("Figure_3_Phenotypic_Correlation.png", fig3, width = 12, height = 11, dpi = 600, bg = "white")

# ============================================
# FIGURE 4: GENOTYPIC CORRELATION (FIXED)
# ============================================
cat("Generating Figure 4...\n")

geno_corr <- geno.corr(MET5R[3:16], MET5R$GENOTYPE, MET5R$REPLICATION)
geno_clean <- clean_corr_matrix(geno_corr)

cat("Genotypic matrix range after cleaning:", range(geno_clean), "\n")

geno_melt <- melt(geno_clean)
colnames(geno_melt) <- c("Trait1", "Trait2", "Correlation")

fig4 <- ggplot(geno_melt, aes(x = Trait2, y = Trait1, fill = Correlation)) +
  geom_tile(color = "white", size = 0.2) +
  geom_text(aes(label = ifelse(abs(Correlation) > 0.995, "1.00", sprintf("%.2f", Correlation))), 
            size = 2.6, fontface = "bold",
            color = ifelse(abs(geno_melt$Correlation) > 0.6, "white", "black")) +
  scale_fill_gradient2(low = "#B71C1C", mid = "white", high = "#0D47A1",
                       midpoint = 0, limit = c(-1, 1), name = "Genotypic\nCorrelation") +
  labs(title = "Figure 4. Genotypic correlation coefficients among 14 quantitative traits") +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9, face = "bold"),
        axis.text.y = element_text(size = 9, face = "bold"),
        plot.title = element_text(face = "bold", size = 13, hjust = 0.5, margin = margin(b = 20)),
        legend.position = "right", panel.grid = element_blank()) +
  coord_fixed()

ggsave("Figure_4_Genotypic_Correlation.png", fig4, width = 12, height = 11, dpi = 600, bg = "white")

# Also save corrplot version
png("Figure_4_Genotypic_Corrplot.png", width = 10, height = 10, units = "in", res = 600, bg = "white")
par(mar = c(4, 4, 4, 4))
corrplot(geno_clean, method = "color", type = "upper", order = "original",
         col = colorRampPalette(c("#B71C1C", "white", "#0D47A1"))(200),
         tl.col = "black", tl.srt = 45, tl.cex = 0.85, number.cex = 0.6,
         addCoef.col = "black", diag = TRUE, cl.lim = c(-1, 1),
         title = "\nFigure 4. Genotypic correlation coefficients", mar = c(0, 0, 2, 0))
dev.off()

# ============================================
# FIGURE 5: PHENOTYPIC PATH ANALYSIS
# ============================================
cat("Generating Figure 5...\n")

pheno_path <- pheno.path(MET5R[16], MET5R[5:15], MET5R$GENOTYPE, MET5R$REPLICATION)
path_traits <- colnames(pheno_path$Effects)

direct_pheno <- data.frame(
  Trait = factor(path_traits, levels = path_traits),
  Effect = diag(as.matrix(pheno_path$Effects)),
  Color = ifelse(diag(as.matrix(pheno_path$Effects)) > 0, "Positive", "Negative")
)

p5a <- ggplot(direct_pheno, aes(x = Trait, y = Effect, fill = Color)) +
  geom_bar(stat = "identity", width = 0.6, color = "black", size = 0.3) +
  geom_hline(yintercept = 0, color = "black", size = 0.5) +
  scale_fill_manual(values = c("Positive" = "#2E7D32", "Negative" = "#C62828")) +
  labs(x = "Trait", y = "Direct Effect", title = "(a) Direct effects on grain yield") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10, face = "bold"),
        plot.title = element_text(face = "bold", size = 12),
        legend.position = "none",
        panel.grid.major.x = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, size = 0.5))

indirect_pheno <- as.matrix(pheno_path$Effects)
diag(indirect_pheno) <- NA
indirect_melt <- melt(indirect_pheno, na.rm = TRUE)

p5b <- ggplot(indirect_melt, aes(x = Var2, y = Var1, fill = value)) +
  geom_tile(color = "white", size = 0.2) +
  geom_text(aes(label = sprintf("%.3f", value)), size = 2.4,
            color = ifelse(abs(indirect_melt$value) > 0.04, "white", "black")) +
  scale_fill_gradient2(low = "#C62828", mid = "white", high = "#2E7D32",
                       midpoint = 0, name = "Indirect\nEffect") +
  labs(x = "Via Trait", y = "Effect Trait", title = "(b) Indirect effects matrix") +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9, face = "bold"),
        axis.text.y = element_text(size = 9, face = "bold"),
        plot.title = element_text(face = "bold", size = 12),
        panel.grid = element_blank()) +
  coord_fixed()

fig5 <- grid.arrange(p5a, p5b, ncol = 1, heights = c(1, 1.3),
                     top = "Figure 5. Phenotypic path coefficients on grain yield per plant")
ggsave("Figure_5_Phenotypic_Path.png", fig5, width = 10, height = 12, dpi = 600, bg = "white")

# ============================================
# FIGURE 6: GENOTYPIC PATH ANALYSIS
# ============================================
cat("Generating Figure 6...\n")

geno_path <- geno.path(MET5R[16], MET5R[5:15], MET5R$GENOTYPE, MET5R$REPLICATION)

direct_geno <- data.frame(
  Trait = factor(path_traits, levels = path_traits),
  Effect = diag(as.matrix(geno_path$Effects)),
  Color = ifelse(diag(as.matrix(geno_path$Effects)) > 0, "Positive", "Negative")
)

p6a <- ggplot(direct_geno, aes(x = Trait, y = Effect, fill = Color)) +
  geom_bar(stat = "identity", width = 0.6, color = "black", size = 0.3) +
  geom_hline(yintercept = 0, color = "black", size = 0.5) +
  scale_fill_manual(values = c("Positive" = "#1565C0", "Negative" = "#D32F2F")) +
  labs(x = "Trait", y = "Direct Effect", title = "(a) Genotypic direct effects on grain yield") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10, face = "bold"),
        plot.title = element_text(face = "bold", size = 12),
        legend.position = "none",
        panel.grid.major.x = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, size = 0.5))

indirect_geno <- as.matrix(geno_path$Effects)
# Clip extreme values for visualization
indirect_geno <- apply(indirect_geno, c(1, 2), function(x) max(min(x, 5), -5))
diag(indirect_geno) <- NA
indirect_geno_melt <- melt(indirect_geno, na.rm = TRUE)

p6b <- ggplot(indirect_geno_melt, aes(x = Var2, y = Var1, fill = value)) +
  geom_tile(color = "white", size = 0.2) +
  geom_text(aes(label = sprintf("%.2f", value)), size = 2.4,
            color = ifelse(abs(indirect_geno_melt$value) > 2, "white", "black")) +
  scale_fill_gradient2(low = "#D32F2F", mid = "white", high = "#1565C0",
                       midpoint = 0, limit = c(-5, 5), name = "Indirect\nEffect\n(clipped)") +
  labs(x = "Via Trait", y = "Effect Trait", title = "(b) Indirect effects matrix (clipped to ±5)") +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9, face = "bold"),
        axis.text.y = element_text(size = 9, face = "bold"),
        plot.title = element_text(face = "bold", size = 12),
        panel.grid = element_blank()) +
  coord_fixed()

fig6 <- grid.arrange(p6a, p6b, ncol = 1, heights = c(1, 1.3),
                     top = "Figure 6. Genotypic path coefficients on grain yield per plant")
ggsave("Figure_6_Genotypic_Path.png", fig6, width = 10, height = 12, dpi = 600, bg = "white")

# ============================================
# FIGURE 7: CORRELATION COMPARISON
# ============================================
cat("Generating Figure 7...\n")

gyp_pheno <- pheno_clean[1:13, "GYP"]
gyp_geno <- geno_clean[1:13, "GYP"]

gyp_corr_df <- data.frame(
  Trait = factor(trait_names[1:13], levels = rev(trait_names[1:13])),
  Phenotypic = gyp_pheno,
  Genotypic = gyp_geno
)

gyp_long <- melt(gyp_corr_df, id.vars = "Trait", variable.name = "Type")

fig7 <- ggplot(gyp_long, aes(x = Trait, y = value, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7, color = "black", size = 0.2) +
  geom_hline(yintercept = 0, color = "black", size = 0.5) +
  scale_fill_manual(values = c("Phenotypic" = "#E65100", "Genotypic" = "#1A237E")) +
  coord_flip() +
  labs(x = "Trait", y = "Correlation with Grain Yield", 
       title = "Figure 7. Comparison of phenotypic and genotypic correlations with grain yield") +
  theme_minimal(base_size = 12) +
  theme(axis.text.y = element_text(size = 10, face = "bold"),
        plot.title = element_text(face = "bold", size = 13, hjust = 0.5),
        legend.position = "top", legend.title = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, size = 0.5)) +
  scale_y_continuous(limits = c(-0.2, 0.7), breaks = seq(-0.2, 0.6, 0.2))

ggsave("Figure_7_Correlation_Comparison.png", fig7, width = 10, height = 8, dpi = 600, bg = "white")

# ============================================
# COMPLETION MESSAGE
# ============================================
cat("\n========================================\n")
cat("ALL 7 FIGURES GENERATED SUCCESSFULLY!\n")
cat("========================================\n")
cat("Working directory:", getwd(), "\n\n")
cat("Generated files:\n")
for(i in 1:7) {
  cat(sprintf("%d. Figure_%d_*.png\n", i, i))
}
cat("\nAll figures are publication-ready at 600 DPI.\n")

############################################################

# ============================================================================
# PATH DIAGRAM PLOTTER - Phenotypic and Genotypic Path Analysis
# ============================================================================

# Required packages
library(ggplot2)
library(grid)
library(gridExtra)

# ----------------------------------------------------------------------------
# DATA INPUT
# ----------------------------------------------------------------------------

# Trait labels
traits <- c("LL", "LW", "LMT", "TP", "PTP", "EL", "FL", "NFE", "FW", "DM", "TSW")
n_traits <- length(traits)

# Phenotypic path coefficients matrix (direct on diagonal, indirect off-diagonal)
# Rows = cause traits, Columns = effect on each trait via correlation
pheno_effects <- matrix(c(
  0.05619, -0.02593,  0.04371,  0.01461, -0.01783, -0.00200,  0.00063, -0.02866,  0.02117,  0.07785,  0.01786,
  -0.00963,  0.15221,  0.01581, -0.00680,  0.01580, -0.00332,  0.00081, -0.00780, -0.02079, -0.01956, -0.00764,
  0.01382,  0.01347,  0.17767,  0.01092, -0.00958,  0.00144, -0.00137, -0.02254, -0.00506,  0.07275, -0.00672,
  -0.00715,  0.00896, -0.01690, -0.11472,  0.09974, -0.01275,  0.00684,  0.07340, -0.01406, -0.02980,  0.02954,
  -0.00727,  0.01734, -0.01235, -0.08304,  0.13795, -0.01862,  0.00751,  0.08041, -0.01110,  0.01077,  0.04429,
  0.00168,  0.00751, -0.00382, -0.02184,  0.03832, -0.06702,  0.02744,  0.04236, -0.00200,  0.00210,  0.05279,
  0.00120,  0.00418, -0.00832, -0.02678,  0.03535, -0.06274,  0.02929,  0.03942, -0.00549, -0.00479,  0.05387,
  -0.01000, -0.00733, -0.02486, -0.05230,  0.06882, -0.01762,  0.00717,  0.16115, -0.00729, -0.04993,  0.04528,
  0.01347, -0.03563, -0.01018,  0.01827, -0.01733,  0.00151, -0.00182, -0.01329,  0.08778,  0.03616,  0.01306,
  0.01836, -0.01243,  0.05427,  0.01436,  0.00623, -0.00059, -0.00059, -0.03377,  0.01341,  0.23822,  0.04142,
  0.00445, -0.00513, -0.00530, -0.01504,  0.02708, -0.01569,  0.00700,  0.03235,  0.00511,  0.04376,  0.22549
), nrow = 11, byrow = TRUE)

pheno_residual <- 0.7388

# Genotypic path coefficients matrix
geno_effects <- matrix(c(
  -2.15412,  0.08641,  2.72383, -1.10505,  0.29249, -0.90817,   1.28841, -0.28311,  0.05528,  0.57383, -0.01293,
  0.85501, -0.21771,  0.60692,  1.21631, -1.07596,  2.31252,  -3.15505,  0.01560, -0.10060, -0.27716, -0.11347,
  -1.82553, -0.04111,  3.21411, -2.32649,  1.48043, -0.00891,   0.39178, -0.73877,  0.00589,  0.43213, -0.09597,
  0.57084, -0.06350, -1.79318,  4.17001, -3.09152,  4.31914,  -5.27330,  0.76604, -0.04971, -0.20526,  0.58769,
  0.19480, -0.07242, -1.47113,  3.98578, -3.23442,  6.18592,  -6.98763,  0.80560, -0.02782,  0.07021,  0.97755,
  0.15361, -0.03953, -0.00225,  1.41422, -1.57102, 12.73557, -13.44411,  0.61369, -0.00952, -0.06137,  0.59337,
  0.20572, -0.05091, -0.09334,  1.62993, -1.67524, 12.69112, -13.49119,  0.59754, -0.02405, -0.09904,  0.64316,
  0.55869, -0.00311, -2.17528,  2.92639, -2.38704,  7.15994,  -7.38521,  1.09159, -0.03605, -0.37342,  0.56638,
  -0.52729,  0.09699,  0.08386, -0.91794,  0.39845, -0.53668,   1.43668, -0.17425,  0.22582,  0.24806, -0.16497,
  -1.32577,  0.06472,  1.48968, -0.91803, -0.24355, -0.83826,   1.43317, -0.43719,  0.06008,  0.93236,  0.23992,
  0.01999,  0.01773, -0.22139,  1.75883, -2.26921,  5.42357,  -6.22740,  0.44372, -0.02674,  0.16054,  1.39335
), nrow = 11, byrow = TRUE)

geno_residual <- 0.8555

# ----------------------------------------------------------------------------
# HELPER FUNCTIONS
# ----------------------------------------------------------------------------

# Function to draw curved arrow (correlation arc)
draw_curve <- function(x0, y0, x1, y1, curvature = 0.3, n = 100, col = "red", lwd = 0.8) {
  # Control point for quadratic Bezier curve
  mid_x <- (x0 + x1) / 2
  mid_y <- (y0 + y1) / 2
  # Perpendicular offset
  dx <- x1 - x0
  dy <- y1 - y0
  len <- sqrt(dx^2 + dy^2)
  if (len > 0) {
    perp_x <- -dy / len * curvature
    perp_y <- dx / len * curvature
    cx <- mid_x + perp_x
    cy <- mid_y + perp_y
  } else {
    cx <- mid_x
    cy <- mid_y
  }
  
  t <- seq(0, 1, length.out = n)
  x <- (1-t)^2 * x0 + 2*(1-t)*t * cx + t^2 * x1
  y <- (1-t)^2 * y0 + 2*(1-t)*t * cy + t^2 * y1
  
  lines(x, y, col = col, lwd = lwd)
}

# Function to draw straight arrow with arrowhead
draw_arrow <- function(x0, y0, x1, y1, col = "blue", lwd = 1.2, length = 0.08) {
  arrows(x0, y0, x1, y1, col = col, lwd = lwd, length = length)
}

# Function to draw circle node
draw_node <- function(x, y, r = 0.015, fill = "white", border = "darkred", lwd = 1.5) {
  theta <- seq(0, 2*pi, length.out = 100)
  polygon(x + r*cos(theta), y + r*sin(theta), col = fill, border = border, lwd = lwd)
}

# Function to draw square node (residual)
draw_square <- function(x, y, s = 0.018, fill = "white", border = "darkred", lwd = 1.5) {
  rect(x - s, y - s, x + s, y + s, col = fill, border = border, lwd = lwd)
}

# ----------------------------------------------------------------------------
# MAIN PLOTTING FUNCTION
# ----------------------------------------------------------------------------

draw_path_diagram <- function(effects, residual, title_text, filename, 
                              is_pheno = TRUE, width = 12, height = 10) {
  
  # Open PNG device
  png(filename, width = width * 300, height = height * 300, res = 300)
  
  # Set up plot with light steel blue background
  par(mar = c(2, 2, 2, 2), bg = "#B0C4DE")
  plot(0, 0, type = "n", xlim = c(0, 1), ylim = c(0, 1), 
       axes = FALSE, xlab = "", ylab = "", asp = 1)
  
  # Layout parameters
  left_x <- 0.12
  right_x <- 0.88
  top_y <- 0.92
  bottom_y <- 0.08
  
  # Calculate trait positions (evenly spaced vertically)
  trait_y <- seq(top_y, bottom_y, length.out = n_traits)
  
  # Node positions
  left_nodes <- data.frame(x = rep(left_x, n_traits), y = trait_y)
  right_node <- c(x = right_x, y = (top_y + bottom_y) / 2)
  
  # Residual position (below left nodes)
  residual_pos <- c(x = left_x, y = bottom_y - 0.08)
  
  # --- Draw correlation curves (red arcs) among left nodes ---
  for (i in 1:(n_traits - 1)) {
    for (j in (i + 1):n_traits) {
      corr_val <- effects[i, j]  # upper triangle = correlations
      if (abs(corr_val) > 0.001) {
        # Curve to the right of left nodes
        curve_x <- left_x + 0.08 + abs(corr_val) * 0.15
        if (is_pheno) {
          curve_x <- left_x + 0.06 + abs(corr_val) * 0.3
        } else {
          curve_x <- left_x + 0.06 + abs(corr_val) * 0.02  # smaller for geno (large values)
        }
        
        # Draw Bezier curve
        y0 <- trait_y[i]
        y1 <- trait_y[j]
        mid_y <- (y0 + y1) / 2
        
        # Control point offset
        dy <- y1 - y0
        dx <- 0.3
        len <- sqrt(dx^2 + dy^2)
        perp_x <- -dy / len * 0.15
        perp_y <- dx / len * 0.15
        
        cx <- curve_x + perp_x
        cy <- mid_y + perp_y
        
        t <- seq(0, 1, length.out = 100)
        x_curve <- (1-t)^2 * left_x + 2*(1-t)*t * cx + t^2 * left_x
        y_curve <- (1-t)^2 * y0 + 2*(1-t)*t * cy + t^2 * y1
        
        lines(x_curve, y_curve, col = "red", lwd = 0.7)
        
        # Label on curve
        label_x <- cx + 0.02
        label_y <- cy
        text(label_x, label_y, sprintf("%.3f", corr_val), 
             cex = 0.45, col = "black", font = 1)
      }
    }
  }
  
  # --- Draw correlation curves on right side (lower triangle) ---
  for (i in 1:(n_traits - 1)) {
    for (j in (i + 1):n_traits) {
      corr_val <- effects[j, i]  # lower triangle
      if (abs(corr_val) > 0.001) {
        if (is_pheno) {
          curve_x <- right_x + 0.06 + abs(corr_val) * 0.3
        } else {
          curve_x <- right_x + 0.06 + abs(corr_val) * 0.02
        }
        
        y0 <- trait_y[i]
        y1 <- trait_y[j]
        mid_y <- (y0 + y1) / 2
        
        dy <- y1 - y0
        dx <- 0.3
        len <- sqrt(dx^2 + dy^2)
        perp_x <- -dy / len * 0.15
        perp_y <- dx / len * 0.15
        
        cx <- curve_x + perp_x
        cy <- mid_y + perp_y
        
        t <- seq(0, 1, length.out = 100)
        x_curve <- (1-t)^2 * right_x + 2*(1-t)*t * cx + t^2 * right_x
        y_curve <- (1-t)^2 * y0 + 2*(1-t)*t * cy + t^2 * y1
        
        lines(x_curve, y_curve, col = "red", lwd = 0.7)
        
        label_x <- cx + 0.02
        label_y <- cy
        text(label_x, label_y, sprintf("%.3f", corr_val), 
             cex = 0.45, col = "black", font = 1)
      }
    }
  }
  
  # --- Draw blue arrows (direct effects) from left nodes to right node ---
  for (i in 1:n_traits) {
    x0 <- left_x + 0.015  # edge of circle
    y0 <- trait_y[i]
    x1 <- right_x - 0.015  # edge of circle
    y1 <- right_node["y"]
    
    # Draw arrow
    arrows(x0, y0, x1, y1, col = "blue", lwd = 1.0, length = 0.06)
    
    # Label direct effect on arrow
    direct_eff <- effects[i, i]
    mid_x <- (x0 + x1) / 2 + 0.02
    mid_y <- (y0 + y1) / 2
    
    # Calculate rotation angle
    angle <- atan2(y1 - y0, x1 - x0) * 180 / pi
    if (angle > 90) angle <- angle - 180
    if (angle < -90) angle <- angle + 180
    
    text(mid_x, mid_y, sprintf("%.3f", direct_eff), 
         cex = 0.55, col = "black", font = 2, srt = angle - 90)
  }
  
  # --- Draw residual arrow ---
  res_x0 <- residual_pos["x"]
  res_y0 <- residual_pos["y"] + 0.018
  res_x1 <- left_x
  res_y1 <- bottom_y - 0.02
  
  arrows(res_x0, res_y0, res_x1, res_y1, col = "blue", lwd = 1.0, length = 0.06)
  
  # --- Draw nodes ---
  # Left trait nodes (circles)
  for (i in 1:n_traits) {
    draw_node(left_x, trait_y[i], r = 0.015, fill = "white", border = "darkred", lwd = 1.5)
  }
  
  # Right node (circle for dependent variable)
  draw_node(right_x, right_node["y"], r = 0.015, fill = "white", border = "darkred", lwd = 1.5)
  
  # Residual node (square)
  draw_square(residual_pos["x"], residual_pos["y"], s = 0.018, 
              fill = "white", border = "darkred", lwd = 1.5)
  
  # --- Add labels ---
  # Trait labels on left
  for (i in 1:n_traits) {
    text(left_x - 0.025, trait_y[i], traits[i], 
         cex = 0.7, col = "black", font = 2, adj = 1)
  }
  
  # Residual label (vertical text)
  text(residual_pos["x"] - 0.03, residual_pos["y"], 
       sprintf("Residual effect %.4f", residual), 
       cex = 0.65, col = "black", font = 2, srt = 90, adj = 0.5)
  
  # Right side labels (indirect effects / total)
  for (i in 1:n_traits) {
    indirect_sum <- sum(effects[setdiff(1:n_traits, i), i])
    text(right_x + 0.03, trait_y[i], sprintf("%.3f", indirect_sum), 
         cex = 0.5, col = "black", font = 1, adj = 0)
  }
  
  # Title
  text(0.5, 0.02, title_text, cex = 0.8, col = "black", 
       font = 2, adj = 0.5)
  
  dev.off()
  cat("Saved:", filename, "\n")
}

# ----------------------------------------------------------------------------
# GENERATE BOTH DIAGRAMS
# ----------------------------------------------------------------------------

# Phenotypic path diagram
draw_path_diagram(
  effects = pheno_effects,
  residual = pheno_residual,
  title_text = "Fig. 4.3. Phenotypic path diagram showing direct and indirect effects of yield components on grain yield per plant in finger millet.",
  filename = "phenotypic_path_diagram.png",
  is_pheno = TRUE
)

# Genotypic path diagram
draw_path_diagram(
  effects = geno_effects,
  residual = geno_residual,
  title_text = "Fig. 4.4. Genotypic path diagram showing direct and indirect effects of yield components on grain yield per plant in finger millet.",
  filename = "genotypic_path_diagram.png",
  is_pheno = FALSE
)

cat("Both diagrams generated successfully!\n")

#####biotools######

library(biotools)
###seperating the  dependent variable #####
MET
dv <- as.matrix(MET[,5:18])
dv
##### model for manova ########
mod <- manova(dv~as.factor(GENOTYPE)+as.factor(REPLICATION), data=MET)
summary(mod)
ss <- SSD(mod)
ss
covar <- ss$SSD/ss$df
covar
###### percentage contribution #####
imp <- singh(dv, covar)
imp
plot(imp)

#### mahalanobis matrix #####
df <- MET
data_avg <- aggregate(MET[,5:18],by = list(GENOTYPE = MET$GENOTYPE), mean)
print(data_avg)
d <- D2.dist(data_avg[,-1], covar)
##### TOCHER METHOD #####
toc <- tocher(d)
toc$clusters
toc$distClust
##### modified tocher ####
tocm <- tocher(d, algorithm = "sequential")
tocm$clusters
tocm$distClust

################ Dendrogram based on Mahalanobis D² ################

library(dendextend)

################ Tocher clusters ################

clust_list <- tocm$clusters
k <- length(clust_list)

# Automatic genotype order
tocher_order <- unlist(clust_list)

# Distance matrix
D2m <- as.matrix(d)
D2m_ordered <- D2m[tocher_order, tocher_order]

################ Hierarchical clustering ################

hc <- hclust(as.dist(D2m_ordered), method = "average")
dend <- as.dendrogram(hc)

################ Cluster membership vector ################

cluster_membership <- rep(NA, length(tocher_order))

start <- 1
for(i in seq_along(clust_list)){
  end <- start + length(clust_list[[i]]) - 1
  cluster_membership[start:end] <- i
  start <- end + 1
}

################ AUTO COLOR GENERATION ################

# Automatically generate as many colors as needed
cluster_colors <- rainbow(k)

label_colors <- cluster_colors[cluster_membership]

# Apply colors to dendrogram labels
dend <- dend %>%
  set("labels_col", label_colors[order.dendrogram(dend)]) %>%
  set("branches_lwd", 2) %>%
  set("labels_cex", 1.1)

################ SAVE HIGH QUALITY IMAGE ################

png("Tocher_Dendrogram_Auto.png",
    width = 4200,
    height = 2600,
    res = 300)

par(bg = "white", mar = c(8,5,5,2))

################ PLOT ################

plot(dend,
     main = "Tocher Cluster Aligned Dendrogram (Auto Colors)",
     ylab = expression("Mahalanobis " * D^2),
     cex.main = 1.8,
     cex.lab = 1.3,
     xlab = "")

################ RECTANGLES ################

rect.dendrogram(dend,
                k = k,
                border = cluster_colors,
                lwd = 3)

################ LEGEND ################

legend("topright",
       legend = paste("Cluster", 1:k),
       fill = cluster_colors,
       border = "black",
       cex = 0.9,
       bty = "n")

dev.off()


library(igraph)

# Convert Mahalanobis D² to matrix
D2m <- as.matrix(d)

# Extract Tocher clusters
clusters <- tocm$clusters
k <- length(clusters)

################ Intra-cluster distances ################
intra <- numeric(k)

for(i in 1:k){
  g <- clusters[[i]]
  if(length(g) > 1){
    sub <- D2m[g, g]
    intra[i] <- mean(sub[upper.tri(sub)])
  } else {
    intra[i] <- 0
  }
}

################ Inter-cluster distances ################
inter <- matrix(0, k, k)

for(i in 1:k){
  for(j in 1:k){
    inter[i, j] <- mean(D2m[clusters[[i]], clusters[[j]]])
  }
}

################ Create edge list ################
edges <- data.frame()

for(i in 1:(k-1)){
  for(j in (i+1):k){
    edges <- rbind(edges,
                   data.frame(
                     from = i,
                     to = j,
                     weight = round(inter[i, j], 3)
                   )
    )
  }
}

################ Build graph ################
g <- graph_from_data_frame(edges, directed = FALSE)

# Node labels: Roman numeral + intra-cluster distance
V(g)$label <- paste0(
  as.roman(V(g)$name),
  "\n",
  round(intra, 3)
)

################ Plot (INDOSTAT style) ################
set.seed(123)
L <- layout_with_fr(g)

# Expand layout (increase spacing)
L <- L * 5
par(mar = c(1,1,3,1))   

plot(g,
     layout = L,
     vertex.size = 35,
     vertex.color = "white",
     vertex.frame.color = "black",
     vertex.label.cex = 0.9,
     vertex.label.font = 1,
     edge.label = round(E(g)$weight, 2),
     edge.label.cex = 0.75,
     edge.color = "grey30",
     main = "Inter- and Intra-Cluster Mahalanobis D² Distances")

# KM: first column = Genotype, remaining = traits
geno <- data_avg[,1]
traits <- data_avg[,-1]
clusters <- tocm$clusters
cluster_id <- rep(NA, nrow(data_avg))

for(i in seq_along(clusters)){
  cluster_id[clusters[[i]]] <- paste0("Cluster_", i)
}

cluster_id <- factor(cluster_id)
cluster_data <- cbind(Cluster = cluster_id, traits)
cluster_means <- aggregate(. ~ Cluster, 
                           data = cluster_data, 
                           FUN = mean, na.rm = TRUE)

cluster_means


library(openxlsx)

write.xlsx(
  list(
    Character_Contribution = character_contribution,
    IV = imp,
    Cluster_Distance = tocm$distClust,
    Cluster_Membership = data.frame(
      Genotype = geno,
      Cluster = cluster_id
    ),
    Cluster_Means = cluster_means,
    Rank_First_Count = first_rank_summary
  ),
  file = "D2.xlsx",
  rowNames = FALSE
)

################### pca analysis #####################
data_avg
############# SCALE ########################
scaled <- scale(data_avg[,-1], center = TRUE)
scaled
################# options ####################
options(max.print = 10000)
options(scipen = 100)
############### visualization packages ###########
library(factoextra)
library(pca3d)
############# buit in ############3
# spectral decomposition
pca1 <- princomp(scaled)
pca1
1.78220712^2
biplot(pca1)
pca1$scores
# singular value decomposition
pca2 <- prcomp(scaled)
pca2
pca2$x
summary(pca2)
biplot(pca2)
pca1$scores == pca2$x
# visulize
fviz_pca(pca2)
fviz_pca_biplot(pca2, repel = TRUE)
fviz_pca_var(pca2, repel = TRUE)
fviz_pca_ind(pca2)
fviz_screeplot(pca2)
############### 3d visualization ########
pca3d(pca2)
pca3d(pca2, show.labels = TRUE, fancy = TRUE)
################### FACTO MINE R###########
library(FactoMineR)
################## pca svd ################
fpca <- PCA(scaled, ncp = 8)
fpca
fpca$eig
fpca$ind$coord
fpca$ind$cos2
fpca$var$contrib
# bplot
fviz_pca(fpca)
fviz_pca_biplot(fpca, repel = TRUE)
fviz_pca_biplot(fpca, repel = TRUE, col.ind = "cos2", col.var = "red")
# scatter plot
fviz_pca_ind(fpca)
fviz_pca_ind(fpca, repel = T, col.ind = "cos2")
# variables
fviz_pca_var(fpca)
fviz_pca_var(fpca, repel = TRUE)
fviz_pca_var(fpca, repel = TRUE, col.var = "contrib")
# scree plot
fviz_screeplot(fpca)
fviz_screeplot(fpca, ncp=8)
fviz_screeplot(fpca, ncp=8, geom="line")
fviz_screeplot(fpca, ncp=8, geom="bar", barfill="red")
fviz_screeplot(fpca, choice="eigenvalue", ncp=8)
fpca$eig
table1 <- fpca$eig
class(table1)
table1<- as.data.frame(table1)
class(table1)
library(writexl)
write_xlsx(table1, "eigentable.xlsx")
################### elbow method #################
plot(table1$`cumulative percentage of variance`)
############ rotated components #################
library(psych)
rpca <- principal(scaled, nfactors = 3, rotate = "varimax", scores = TRUE)
rpca
rpca$communality
rpca$loadings
print(rpca$loadings, digits = 3, cutoff = 0)
barplot(rpca$loadings)
barplot(rpca$loadings, beside = TRUE)
barplot(rpca$loadings, beside = TRUE, col = "blue", 
        main = "Rotated component matrix")
# R color palette
install.packages("pals")
library(pals)
barplot(rpca$loadings, beside = TRUE, col = brewer.accent(19), 
        main = "Rotated component matrix")
barplot(rpca$loadings, beside = TRUE, col = brewer.greens(19), 
        main = "Rotated component matrix")
barplot(rpca$loadings, beside = TRUE, col = alphabet(19), main = "Rotated component matrix")
# import scores
rpca$scores
scores <- rpca$scores
class(scores)
scores <- as.data.frame(scores)
write_xlsx(scores, "scores.xlsx")

#######################################################

data<-MET5R
data
head(data)
tail(data)
str(data)
data$GENOTYPE<-as.character(x = data$GENOTYPE)
str(data)
head(data)
newdata <- data[ ,-1]
head(data)
df <- newdata
df.scaled <-scale(df)
head(df.scaled)
library(stats)
require(stats)
res.dist <- dist(x = data_avg, method = "euclidean")
x <-as.matrix(res.dist)[1:10,1:10]
x
round(x,digits = 3)
require(stats)
res.hc <-hclust(d = res.dist, method = "complete")
plot(x=res.hc)
library(factoextra)
require(factoextra)
fviz_dend(x = res.hc, cex = 0.7, lwd =0.7)
library(grDevices)
require(grDevices)
colours()
library(scales)
require(scales)
palette()
show_col(palette(rainbow(6))
         require("ggsci")         
         show_col(pal_jco(palette = c("default"))(10))
         show_col(pal_jco("default", alpha = 0.6)(10))
         fviz_dend(x = res.hc, cex = 0.8, lwd = 0.8, k= 5, k_colors = c("red","green3", "blue", "purple1","snow1"))
         fviz_dend(res.hc, cex = 0.8, lwd = 0.8, k = 10,
                   rect = TRUE,
                   rect_border = "gray",
                   rect_fill = TRUE)
         fviz_dend(res.hc, cex = 0.8, lwd = 0.8, k = 10,
                   rect = TRUE,
                   rect_border = "jco",
                   rect_fill = TRUE)
         fviz_dend(res.hc, cex = 0.8, lwd = 0.8, k = 10,
                   rect = TRUE,
                   k_colors = "jco",
                   rect_border = "jco",
                   rect_fill = TRUE,
                   type = "circular")
         require(igraph)
         fviz_dend(res.hc, cex = 0.8, lwd = 0.8, k = 4,
                   rect = TRUE,
                   k_colors = "jco",
                   rect_border = "jco",
                   rect_fill = TRUE,
                   type = "circular",
                   repel = TRUE,
                   phylo_layout = "layout_as_tree")
         
         # Required packages
         install.packages("dplyr")
         library(dplyr)
         
         # Load data
         rice <- data_avg
         
         # Remove genotype column
         rice_traits <- rice[, -1]
         
         # Step 1: Compute covariance matrix
         S <- cov(rice_traits)
         
         # Step 2: Inverse covariance matrix
         S_inv <- solve(S)
         
         # Step 3: Function to compute Mahalanobis D² distance matrix
         mahalanobis_dist <- function(X) {
           n <- nrow(X)
           D2 <- matrix(0, n, n)
           for (i in 1:(n-1)) {
             for (j in (i+1):n) {
               d <- as.numeric(t(X[i,] - X[j,]) %*% S_inv %*% (X[i,] - X[j,]))
               D2[i, j] <- d
               D2[j, i] <- d
             }
           }
           return(D2)
         }
         mahalanobis_dist <- function(X) {
           X <- as.matrix(X)            # ensure numeric matrix
           n <- nrow(X)
           p <- ncol(X)
           S <- cov(X)
           S_inv <- solve(S)
           
           D2 <- matrix(0, n, n)
           for (i in 1:(n-1)) {
             for (j in (i+1):n) {
               diff <- as.numeric(X[i, ] - X[j, ])        # 1×p vector
               d <- t(diff) %*% S_inv %*% diff            # Mahalanobis D²
               D2[i, j] <- d
               D2[j, i] <- d
             }
           }
           return(D2)
         }
         
         
         # Compute Mahalanobis distance matrix
         D2 <- mahalanobis_dist(rice_traits)
         
         # Step 4: Tocher’s method function
         tocher <- function(dist_mat){
           n <- nrow(dist_mat)
           clusters <- list()
           assigned <- rep(FALSE, n)
           
           for (i in 1:n) {
             if (!assigned[i]) {
               cluster <- c(i)
               assigned[i] <- TRUE
               for (j in 1:n) {
                 if (!assigned[j]) {
                   avg_dist <- mean(dist_mat[j, cluster])
                   max_dist <- mean(dist_mat[cluster, cluster][lower.tri(dist_mat[cluster, cluster])])
                   if (is.nan(max_dist)) max_dist <- avg_dist
                   if (avg_dist <= max_dist) {
                     cluster <- c(cluster, j)
                     assigned[j] <- TRUE
                   }
                 }
               }
               clusters[[length(clusters) + 1]] <- cluster
             }
           }
           return(clusters)
         }
         
         clusters <- tocher(as.matrix(D2))
         print("Tocher Clusters:")
         print(clusters)
         
         # Step 5: Intra-cluster distances
         intra_dist <- lapply(clusters, function(cl){
           if (length(cl) > 1) {
             mean(D2[cl, cl][lower.tri(D2[cl, cl])])
           } else {
             0
           }
         })
         
         # Step 6: Inter-cluster distances
         inter_dist <- matrix(NA, length(clusters), length(clusters))
         for (i in 1:length(clusters)){
           for (j in 1:length(clusters)){
             if (i < j){
               inter_dist[i,j] <- mean(D2[clusters[[i]], clusters[[j]]])
             }
           }
         }
         
         print("Intra-cluster Distances:")
         print(intra_dist)
         
         print("Inter-cluster Distance Matrix:")
         print(inter_dist)
         
         



