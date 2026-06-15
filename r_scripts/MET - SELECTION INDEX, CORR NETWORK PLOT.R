# ==========================================================
# COMPLETELY FIXED METAN SMITH-HAZEL PIPELINE
# ==========================================================

library(dplyr)
library(metan)
library(MASS)

# ---------- 1. DATA PREPARATION ----------
seldata_df <- as.data.frame(seldata)
colnames(seldata_df)[1] <- "Genotype"
colnames(seldata_df)[2] <- "Rep"

# Calculate genotype means
metan_df <- seldata_df %>%
  group_by(Genotype) %>%
  summarise(across(where(is.numeric), ~mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  dplyr::select(-any_of("Rep")) %>%
  as.data.frame()

# CRITICAL: Keep Genotype as column, extract trait matrix separately
trait_names <- setdiff(colnames(metan_df), "Genotype")
n_traits <- length(trait_names)

# Genotype names vector
genotype_names <- metan_df$Genotype

# Pure numeric trait matrix (NO genotype column)
trait_matrix <- as.matrix(metan_df[, trait_names])
rownames(trait_matrix) <- genotype_names

print("Trait matrix dimensions:")
print(dim(trait_matrix))
print("First few rows:")
print(head(trait_matrix))

# ---------- 2. CLEAN VARIANCE-COVARIANCE MATRICES ----------
# Ensure strict numeric matrices with proper dimensions

clean_phen <- as.matrix(phenMat)
clean_gen <- as.matrix(genMat)

# Trim to match actual trait count
if(nrow(clean_phen) > n_traits) {
  clean_phen <- clean_phen[1:n_traits, 1:n_traits]
  clean_gen <- clean_gen[1:n_traits, 1:n_traits]
}

# Force numeric and assign names
clean_phen <- matrix(as.numeric(clean_phen), 
                     nrow = n_traits, 
                     ncol = n_traits,
                     dimnames = list(trait_names, trait_names))
clean_gen <- matrix(as.numeric(clean_gen), 
                    nrow = n_traits, 
                    ncol = n_traits,
                    dimnames = list(trait_names, trait_names))

# Verify no NA/NaN/Inf
print("Any NA in phen:")
print(any(is.na(clean_phen)))
print("Any Inf in phen:")
print(any(is.infinite(clean_phen)))

# ---------- 3. PREPARE WEIGHTS ----------
# Named numeric vector - CRITICAL: must match trait order exactly
clean_weights <- setNames(
  as.numeric(weight_mat[1:n_traits, 1]),
  trait_names
)

print("Weights:")
print(clean_weights)
print("Weight names match trait names:")
print(all(names(clean_weights) == trait_names))

# ---------- 4. ATTEMPT METAN SMITH_HAZEL ----------
# The metan function expects specific structure

hz_results <- tryCatch({
  Smith_Hazel(
    .data = metan_df,        # Data frame with Genotype column + traits
    pcov = clean_phen,       # Strict numeric matrix
    gcov = clean_gen,        # Strict numeric matrix
    weights = clean_weights  # Named numeric vector
  )
}, error = function(e) {
  print(paste("Smith_Hazel error:", e$message))
  return(NULL)
})

# ---------- 5. FIXED MANUAL SMITH-HAZEL ----------
# Robust implementation with proper genotype handling

manual_smith_hazel <- function(trait_mat, phen_cov, gen_cov, weights, geno_names) {
  
  # 1. Coerce to strict base matrices
  P <- matrix(as.numeric(phen_cov), nrow = nrow(phen_cov), ncol = ncol(phen_cov))
  G <- matrix(as.numeric(gen_cov), nrow = nrow(gen_cov), ncol = ncol(gen_cov))
  w <- matrix(as.numeric(weights), ncol = 1)
  
  # Ensure rownames match for alignment
  rownames(P) <- colnames(P) <- names(weights)
  rownames(G) <- colnames(G) <- names(weights)
  
  # 2. Calculate index coefficients: b = P^-1 G w
  # Use ginv for numerical stability, or solve if invertible
  P_inv <- tryCatch(solve(P), error = function(e) ginv(P))
  b <- P_inv %*% G %*% w
  rownames(b) <- names(weights)
  
  # 3. Calculate selection index for each genotype
  # Ensure trait_mat columns match weight names
  trait_mat <- trait_mat[, names(weights), drop = FALSE]
  index_values <- as.matrix(trait_mat) %*% b
  
  # 4. Create ranking dataframe with proper genotype names
  results_df <- data.frame(
    Genotype = geno_names,
    Index_Score = as.vector(index_values),
    stringsAsFactors = FALSE
  )
  
  # Sort by index score descending
  results_df <- results_df[order(-results_df$Index_Score), ]
  results_df$Rank <- seq_len(nrow(results_df))
  rownames(results_df) <- NULL  # Clean rownames
  
  # 5. Select top 5%
  n_top <- max(1, round(0.05 * nrow(results_df)))
  selected_idx <- seq_len(n_top)
  selected_genotypes <- results_df$Genotype[selected_idx]
  
  # 6. Calculate Genetic Advance on first trait (GYP)
  # Use genotype names for proper indexing
  pop_mean <- mean(trait_mat[, 1])
  selected_mean <- mean(trait_mat[selected_genotypes, 1])
  GA_percent <- ((selected_mean - pop_mean) / pop_mean) * 100
  
  return(list(
    b = b,
    index = index_values,
    rankings = results_df,
    selected = selected_genotypes,
    GA_percent = GA_percent,
    selected_mean = selected_mean,
    pop_mean = pop_mean,
    n_selected = n_top
  ))
}

# ---------- 6. RUN MANUAL FALLBACK ----------
manual_results <- manual_smith_hazel(
  trait_mat = trait_matrix,
  phen_cov = clean_phen,
  gen_cov = clean_gen,
  weights = clean_weights,
  geno_names = genotype_names
)

print("=")
print("MANUAL SMITH-HAZEL RESULTS")
print("=")

print("Index Weights (b):")
print(round(manual_results$b, 4))

print("Top 10 Genotype Rankings:")
print(head(manual_results$rankings, 10))

print(paste("Selected Genotypes (top 5%):", 
            paste(manual_results$selected, collapse = ", ")))

print(paste("Genetic Advance:", round(manual_results$GA_percent, 2), "%"))
print(paste("Population Mean (GYP):", round(manual_results$pop_mean, 3)))
print(paste("Selected Mean (GYP):", round(manual_results$selected_mean, 3)))

# ---------- 7. VALIDATION: CHECK AGAINST METAN IF AVAILABLE ----------
if(!is.null(hz_results) && "b" %in% names(hz_results)) {
  print("=")
  print("METAN VS MANUAL COMPARISON")
  print("=")
  
  comparison <- data.frame(
    Trait = trait_names,
    Metan_b = as.vector(hz_results$b),
    Manual_b = as.vector(manual_results$b),
    Difference = as.vector(hz_results$b) - as.vector(manual_results$b)
  )
  print(comparison)
}

# ---------- 8. SAVE RESULTS ----------
save(metan_df, trait_matrix, clean_phen, clean_gen, clean_weights,
     manual_results, hz_results,
     file = "Smith_Hazel_fixed_results.RData")

# ==========================================================
# DISPLAY ALL RESULTS FROM MANUAL SMITH-HAZEL
# ==========================================================

# 1. INDEX WEIGHTS (b coefficients) - Shows which traits drive selection
print("========================================")
print("1. SMITH-HAZEL INDEX WEIGHTS (b)")
print("========================================")
weights_df <- data.frame(
  Trait = rownames(manual_results$b),
  Weight = round(as.vector(manual_results$b), 4)
)
print(weights_df[order(-abs(weights_df$Weight)), ])  # Sort by absolute importance

# 2. GENOTYPE RANKINGS - All 35 genotypes sorted by index
print("========================================")
print("2. GENOTYPE RANKINGS (Top 15)")
print("========================================")
print(head(manual_results$rankings, 15))

print("...")
print("Bottom 5:")
print(tail(manual_results$rankings, 5))

# 3. SELECTED GENOTYPES - Top 5% for breeding
print("========================================")
print("3. SELECTED GENOTYPES (Top 5%)")
print("========================================")
print(paste("Number selected:", manual_results$n_selected))
print("Selected genotypes:")
print(manual_results$selected)

# 4. GENETIC ADVANCE - The improvement from selection
print("========================================")
print("4. GENETIC ADVANCE SUMMARY")
print("========================================")
print(paste("Population mean (GYP):", round(manual_results$pop_mean, 3), "g"))
print(paste("Selected mean (GYP):", round(manual_results$selected_mean, 3), "g"))
print(paste("Genetic Advance:", round(manual_results$GA_percent, 2), "%"))
print(paste("Absolute gain:", round(manual_results$selected_mean - manual_results$pop_mean, 3), "g"))

# 5. FULL RANKINGS TABLE - Exportable
print("========================================")
print("5. COMPLETE RANKINGS TABLE")
print("========================================")
print(manual_results$rankings)

# 6. SAVE AS CSV FOR EXCEL/PUBLICATION
write.csv(manual_results$rankings, "Smith_Hazel_Rankings.csv", row.names = FALSE)
write.csv(weights_df, "Smith_Hazel_Weights.csv", row.names = FALSE)

cat("
========================================
SUMMARY
========================================
Index Weights:     ", nrow(weights_df), "traits evaluated
Genotypes Ranked:  ", nrow(manual_results$rankings), "entries
Selected (top 5%): ", manual_results$n_selected, "genotypes (", paste(manual_results$selected, collapse=", "), ")
Genetic Gain:      ", round(manual_results$GA_percent, 2), "% improvement in GYP
Output files:      Smith_Hazel_Rankings.csv, Smith_Hazel_Weights.csv
========================================
")


# ==========================================================
# FIXED CORRELATION NETWORK ANALYSIS
# ==========================================================

# ==========================================================
# COMPLETELY FIXED CORRELATION NETWORK ANALYSIS
# ==========================================================

# ==========================================================
# FULLY FIXED CORRELATION NETWORK ANALYSIS
# ==========================================================

library(dplyr)
library(corrr)
library(igraph)
library(corrplot)

select <- dplyr::select

# ---------- 1. PREPARE CLEAN TRAIT DATA ----------
# CRITICAL: Exclude REPLICATION, GENOTYPE, and any non-trait columns

# First, identify what columns are actually traits
all_cols <- colnames(seldata_df)
non_trait_cols <- c("GENOTYPE", "Rep", "rep", "REPLICATION", "treat", "block", "env")
trait_cols <- setdiff(all_cols, non_trait_cols)

print("Trait columns identified:")
print(trait_cols)

# Create clean data: genotype + traits only
clean_data <- seldata_df %>%
  select(GENOTYPE, all_of(trait_cols))

# Convert traits to numeric
for(col in trait_cols) {
  clean_data[[col]] <- as.numeric(as.character(clean_data[[col]]))
}

# ---------- 2. CALCULATE GENOTYPE MEANS ----------
genotype_means <- clean_data %>%
  group_by(GENOTYPE) %>%
  summarise(across(where(is.numeric), ~mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  as.data.frame()

print("Genotype means (first 3 rows):")
print(head(genotype_means, 3))

# ---------- 3. EXTRACT PURE TRAIT MATRIX ----------
# CRITICAL: Remove GENOTYPE column, keep only traits
trait_names <- setdiff(colnames(genotype_means), "GENOTYPE")
trait_matrix <- as.matrix(genotype_means[, trait_names])

# Verify numeric
print("Trait matrix class and mode:")
print(class(trait_matrix))
print(mode(trait_matrix))
print("Dimensions:")
print(dim(trait_matrix))

# ---------- 4. CALCULATE CORRELATION MATRIX ----------
cor_matrix <- cor(trait_matrix, use = "pairwise.complete.obs")

# Clean up any remaining NA/NaN
cor_matrix[is.na(cor_matrix)] <- 0

print("Correlation matrix (rounded):")
print(round(cor_matrix, 3))

# ---------- 5. CORRR TIBBLE (OPTIONAL) ----------
x <- correlate(as.data.frame(trait_matrix),
               use = "pairwise.complete.obs",
               method = "pearson",
               diagonal = 1,  # Changed from NA to avoid issues
               quiet = TRUE)

# ---------- 6. FIXED CUSTOM NETWORK PLOT ----------
custom_network_plot <- function(data_mat, 
                                min_cor = 0.3,
                                colours = c("indianred2", "white", "skyblue")) {
  
  # Calculate correlation
  cor_mat <- cor(data_mat, use = "pairwise.complete.obs")
  cor_mat[is.na(cor_mat)] <- 0
  
  # Filter by threshold
  cor_mat[abs(cor_mat) < min_cor] <- 0
  diag(cor_mat) <- 0
  
  # Create graph from upper triangle only (avoid duplicates)
  upper_tri <- cor_mat
  upper_tri[lower.tri(upper_tri)] <- 0
  
  g <- graph_from_adjacency_matrix(abs(upper_tri), 
                                   weighted = TRUE, 
                                   mode = "undirected", 
                                   diag = FALSE)
  
  # Remove isolated vertices
  isolates <- which(degree(g) == 0)
  if(length(isolates) > 0) {
    g <- delete_vertices(g, isolates)
  }
  
  # CRITICAL FIX: Edge colors must match number of edges exactly
  # Get edge list and look up original correlations
  edges <- as_edgelist(g)
  edge_cors <- sapply(1:nrow(edges), function(i) {
    cor_mat[edges[i,1], edges[i,2]]
  })
  
  # Set edge attributes
  E(g)$color <- ifelse(edge_cors > 0, colours[1], colours[3])
  E(g)$width <- abs(edge_cors) * 8
  E(g)$correlation <- edge_cors
  
  # Plot
  set.seed(42)
  plot(g, 
       edge.color = E(g)$color,
       edge.width = E(g)$width,
       vertex.color = colours[2],
       vertex.size = 25,
       vertex.label.cex = 0.9,
       vertex.label.color = "black",
       vertex.frame.color = "gray40",
       layout = layout_with_fr(g, niter = 500),
       main = paste("Correlation Network (|r| >", min_cor, ")"))
  
  invisible(list(graph = g, cor_matrix = cor_mat, edges_df = data.frame(
    from = edges[,1], to = edges[,2], 
    correlation = edge_cors, abs_cor = abs(edge_cors)
  )))
}

# ---------- 7. RUN NETWORK PLOTS ----------
print("Network plot (min_cor = 0.3):")
net1 <- custom_network_plot(trait_matrix, min_cor = 0.3)

print("Network plot (min_cor = 0.5):")
net2 <- custom_network_plot(trait_matrix, min_cor = 0.5, 
                            colours = c("red", "white", "blue"))

print("Network plot (min_cor = 0.1):")
net3 <- custom_network_plot(trait_matrix, min_cor = 0.1)

# ---------- 8. CORRPLOT ----------
# Use clean matrix with no NA/NaN
corrplot(cor_matrix,
         method = "color",
         type = "upper",
         order = "hclust",
         hclust.method = "ward.D2",
         addrect = 2,
         tl.col = "black",
         tl.srt = 45,
         tl.cex = 0.8,
         diag = FALSE,
         title = "Trait Correlation Matrix",
         mar = c(0,0,2,0))

# ---------- 9. EXPORT RESULTS ----------
write.csv(cor_matrix, "Correlation_Matrix.csv")
write.csv(genotype_means, "Genotype_Means.csv", row.names = FALSE)

# Edges from network 1 (min_cor = 0.3)
edges_df <- net1$edges_df
write.csv(edges_df, "Network_Edges.csv", row.names = FALSE)

# ---------- 10. SUMMARY ----------
strong_pos <- sum(cor_matrix > 0.7 & cor_matrix < 1, na.rm = TRUE) / 2
strong_neg <- sum(cor_matrix < -0.7, na.rm = TRUE) / 2
moderate <- sum(abs(cor_matrix) > 0.5 & abs(cor_matrix) <= 0.7, na.rm = TRUE) / 2

cat("
========================================
CORRELATION NETWORK SUMMARY
========================================
Traits analyzed:        ", ncol(trait_matrix), "
Genotypes:              ", nrow(genotype_means), "

Correlation pairs:
  Strong positive (r>0.7):  ", strong_pos, "
  Strong negative (r<-0.7): ", strong_neg, "
  Moderate (|r|>0.5):       ", moderate, "

Network edges (|r|>0.3):  ", nrow(edges_df), "

Files saved:
  - Correlation_Matrix.csv
  - Genotype_Means.csv
  - Network_Edges.csv

========================================
")

# ---------- 11. SAVE ALL ----------
save(genotype_means, trait_matrix, cor_matrix, edges_df, net1, net2, net3,
     file = "Correlation_Network_Results.RData")
