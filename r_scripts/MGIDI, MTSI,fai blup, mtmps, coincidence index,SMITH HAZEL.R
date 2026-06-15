
library(metan)
library(ggplot2)
library(graphics)
library(factoextra)
library(FactoMineR)

########### fai_blup ######## Multi-trait selection index #############
library(metan)

mod <- waasb(MET,
             env = ENVIRONMENT,
             gen = GENOTYPE,
             rep = REPLICATION,
             resp = c(DFF, PH, LL, LW, LMT, TP, PTP, EL, FL, NFE, FW, DM, 
                      TSW, GYP))
names(mod)
FAI <- fai_blup(mod,
                SI = 15,
                DI = rep('max', 14),
                UI = rep('min', 14))
############# mtmps ########### Multi-trait mean performance and stability index ######
model <-
  mps(MET,
      env = ENVIRONMENT,
      gen = GENOTYPE,
      rep = REPLICATION,
      resp = everything())
selection <- mtmps(model)

# gains for stability
selection$sel_dif_stab

# gains for mean performance
selection$sel_dif_trait
mtmps(model, SI = 15, mineval = 1, verbose = TRUE)

########### coincidence_index #### Computes the coincidence index of genotype selection ###
library(metan)

# Your 35 genotypes
all_genotypes <- paste0("G", c(1, 10:35, 2:9))  # G1, G10-G35, G2-G9

# Smith-Hazel top 7 genotypes (20% selection intensity)
sel_sh <- c("G10", "G18", "G30", "G14", "G15", "G17", "G33")

# GYP-based top 7 genotypes
sel_gyp <- c("G9", "G2", "G10", "G26", "G29", "G17", "G16")

# MGIDI top 7 genotypes (hypothetical)
sel_mgidi <- c("G10", "G18", "G2", "G14", "G27", "G17", "G30")

# ============================================
# TWO-METHOD COMPARISON
# ============================================
ci_sh_gyp <- coincidence_index(sel1 = sel_sh, 
                               sel2 = sel_gyp, 
                               total = 35)
print(ci_sh_gyp)
# Expected output: numeric value (coincidence index %)

# ============================================
# THREE-METHOD COMPARISON (using model objects)
# ============================================
# If you have fitted models:
# ci_all <- coincidence_index(sh_model, mgidi_model, fai_model, total = 35)

# Manual three-way comparison requires pairwise calls
ci_sh_mgidi <- coincidence_index(sel1 = sel_sh, sel2 = sel_mgidi, total = 35)
ci_gyp_mgidi <- coincidence_index(sel1 = sel_gyp, sel2 = sel_mgidi, total = 35)

cat("SH vs GYP:", ci_sh_gyp, "%\n")
cat("SH vs MGIDI:", ci_sh_mgidi, "%\n")
cat("GYP vs MGIDI:", ci_gyp_mgidi, "%\n")

#############################################################

library(metan)
library(dplyr)
MTSI_index <- MET %>%
  waasb(ENVIRONMENT, GENOTYPE, REPLICATION,
        resp = c(DFF, PH, LL, LW, LMT, TP, PTP, EL, FL, NFE, FW, DM, 
                 TSW, GYP)) %>%
  mtsi(verbose = FALSE, index = "waasb")
MTSI_index2 <- 
  MET %>%
  waasb(ENVIRONMENT, GENOTYPE, REPLICATION,
        resp = c(DFF, PH, LL, LW, LMT, TP, PTP, EL, FL, NFE, FW, DM, 
                 TSW, GYP),
        mresp = c("l,l,l,l,l,h,h,h,h,h,h,l,h,h,h,h,h,h,h"),
        wresp = 50) %>% # Default is 50
  mtsi(SI = 20)
# Factor analysis
gmd(MTSI_index2, "FA") %>% print_table()
clean_data <- MET %>%
  select(where(~ mean(!is.na(.)) > 0.8)) %>%  # keep columns with <=20% missing
  drop_na()  # remove rows with missing values
data_filtered <- MET %>% select(-LL, -TP)
mod <- gamem(data_filtered,
             gen = GENOTYPE,
             rep = REPLICATION,
             resp = everything())
mgidi_index <- mgidi(mod,
                     SI = 20) # Selection intensity
p1 <- plot(mgidi_index, SI = 20)
p2 <- plot(mgidi_index, type = "contribution")
arrange_ggplot(p1, p2)
# Extract selected genotypes
MGIDI <- gmd(mgidi_index, "sel_gen")
FAI <- gmd(fai, "sel_gen")
SH <- gmd(smith, "sel_gen")

# Venn plot
venn_plot(MGIDI, FAI, SH, show_elements = TRUE)
smith <- Smith_Hazel(mod, SI = 20)

fai <- fai_blup(mod, SI = 20)

fai <- fai_blup(mod, SI = 20)
coincidence <- coincidence_index(mgidi_index, fai, smith, total = 13)
coincidence
coincidence <- coincidence_index(mgidi_index, fai, smith, total = 13)
print(coincidence)
# ---------------------------------------------------------------------------
# Coincidence index and common genotypes
# ---------------------------------------------------------------------------
# # A tibble: 3 × 5
#   V1          V2     index common genotypes
#   <chr>       <chr>  <dbl>  <int> <chr>    
# 1 mgidi_index fai   100         3 H13,H5,H2
# 2 mgidi_index smith  56.71      2 H13,H5   
# 3 fai         smith  56.71      2 H5,H13
MGIDI <- gmd(mgidi_index, "sel_gen")
# Class of the model: mgidi
# Variable extracted: sel_gen
FAI <- gmd(fai, "sel_gen")
# Class of the model: fai_blup
# Variable extracted: sel_gen
SH <- gmd(smith, "sel_gen")
# Class of the model: sh
# Variable extracted: sel_gen

# Create the plot
venn_plot(MGIDI, FAI, SH, show_elements = TRUE)

library(DT) # Used to make the tables
# Function to make HTML tables
print_table <- function(table, rownames = FALSE, digits = 3, ...){
  df <- datatable(table, rownames = rownames, extensions = 'Buttons',
                  options = list(scrollX = TRUE, 
                                 dom = '<<t>Bp>',
                                 buttons = c('copy', 'excel', 'pdf', 'print')), ...)
  num_cols <- c(as.numeric(which(sapply(table, class) == "numeric")))
  if(length(num_cols) > 0){
    formatSignif(df, columns = num_cols, digits = digits)
  } else{
    df
  }
}

######################################################################################

library(metan)
model <- gamem(MET,
               gen = GENOTYPE,
               rep = REPLICATION,
               resp = everything())
############ BLUPS FOR GENOTYPES############
G <- gmd(model,"blupg")
G
print(G,n=50)

mgidi_ind <- mgidi(model)
L <- gmd(mgidi_ind,"MGIDI")
L
print(L,n=50)
mgidi_ind2 <- mgidi(model,
                    weights = c(1,0.5,0.2,0.8,1,0.3,0.4,0.9,0.1,0.3,0.2,0.6,0.4,0.9),
                    SI=30)
K <- gmd(mgidi_ind2,"MGIDI")
K
print(K,n=50)                    
########## compute the MGIDI INDEX #################
Gku <- mgidi(model)
############ plot the contribution of each factor on the mgidi index #########
p1 <- plot(Gku,type = "contribution")
p2 <- plot(Gku)
p1+p2

mgidi_ind3 <- mgidi(model,ideotype = c("l,l,l,l,l,h,h,h,h,h,h,l,h,h"))
Y<- gmd(mgidi_ind3,"MGIDI")
Y
print(Y,n=50)


###########################################################################

library(metan)

mtsi_model <- waasb(MET,
                    env = ENVIRONMENT,
                    gen = GENOTYPE,
                    rep = BLOCK,
                    resp = everything())
mtsi_index <- mtsi(mtsi_model,index = "waasb")

mtsi_index2 <- MET %>%
  waasb(
    ENVIRONMENT, 
    GENOTYPE, 
    BLOCK,
    resp = everything(), # Removed c() wrapping
    mresp = c("l","l","l","l","l","h","h","h","h","h","h","l","h","h"), # Properly separated strings
    wresp = 65
  ) %>%
  mtsi(SI = 20)

# 3. Diagnostic plots and tables will now work
PLOT1MTSI <- mtsi(mtsi_model)
P1 <- plot(PLOT1MTSI)
P1
p2 <- plot(mtsi_index2, type = "contribution")
p2

gmd(mtsi_index2, "FA") %>% print(n=50)
gmd(mtsi_index2, "scores_gen") %>% print(n=Inf)


