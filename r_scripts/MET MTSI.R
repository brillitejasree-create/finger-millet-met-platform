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

