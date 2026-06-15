##################### AMMI MET DATA - R CODE UPDATED ##############################
library(readxl)
MET <- read_excel("C:/Users/BHARGAVI/Desktop/PG articles/MET.xlsx", 
                  sheet = "Sheet1")
View(MET)

library(metan)
library(ggplot2)
options(max.print = 10000)

##################### importing data ##########################
View(MET)
nrow(MET)
ncol(MET)
dim(MET)
dim.data.frame(MET)
inspect(MET,plot = FALSE, threshold = 15, verbose = TRUE)
head(MET)
tail(MET)
class(MET)
str(MET)
################ factors with unique levels #######################
MET$ENVIRONMENT <-factor(MET$ENVIRONMENT, levels = unique(MET$ENVIRONMENT))
MET$GENOTYPE <-factor(MET$GENOTYPE, levels = unique(MET$GENOTYPE))
MET$REPLICATION <-factor(MET$REPLICATION, levels = unique(MET$REPLICATION))
MET$BLOCK <-factor(MET$BLOCK, levels = unique(MET$BLOCK))
str(MET)
############## data inspection and manipulation ####################
inspect(MET, plot = TRUE)
#################### check for outliers ######################
find_outliers(MET, var = DFF, plots = TRUE)
find_outliers(MET, var = PH, plots = TRUE)
find_outliers(MET, var = LMT, plots = TRUE)
find_outliers(MET, var = PTP, plots = TRUE)
find_outliers(MET, var = EL, plots = TRUE)
find_outliers(MET, var = FL, plots = TRUE)
find_outliers(MET, var = NFE, plots = TRUE)
find_outliers(MET, var = FW, plots = TRUE)
find_outliers(MET, var = TSW, plots = TRUE)
find_outliers(MET, var = FE, plots = TRUE)
find_outliers(MET, var = ZN, plots = TRUE)
find_outliers(MET, var = MN, plots = TRUE)
find_outliers(MET, var = CU, plots = TRUE)
find_outliers(MET, var = GYP, plots = TRUE)
find_outliers(MET, var = LL, plots = TRUE)
find_outliers(MET, var = LW, plots = TRUE)
find_outliers(MET, var = TP, plots = TRUE)
find_outliers(MET, var = CA, plots = TRUE)
find_outliers(MET, var = DM, plots = TRUE)
############## extra clean functions ###################
############### remove NA #####################
remove_rows_na(MET)
################## replace zero ###############
replace_zero(MET)
################# find text in numbers ####################
find_text_in_num(MET$DFF)
find_text_in_num(MET$PH)
find_text_in_num(MET$LL)
find_text_in_num(MET$LW)
find_text_in_num(MET$LMT)
find_text_in_num(MET$TP)
find_text_in_num(MET$PTP)
find_text_in_num(MET$EL)
find_text_in_num(MET$FL)
find_text_in_num(MET$NFE)
find_text_in_num(MET$FW)
find_text_in_num(MET$DM)
find_text_in_num(MET$TSW)
find_text_in_num(MET$FE)
find_text_in_num(MET$ZN)
find_text_in_num(MET$MN)
find_text_in_num(MET$CU)
find_text_in_num(MET$CA)
find_text_in_num(MET$GYP)
################################## data analysis #######################################
########################## descriptive stats ###############################
desc_stat(MET)
desc_stat(MET, stats = "all")
ds <- desc_stat(MET, stats = "all")
ds
View(ds)
class(ds)
############# importing a table ############
library(writexl)
write_xlsx(ds, "ds.xlsx")
########################### mean perfomance #############################
################# mean of genotypes #########################
mg <-means_by(MET, GENOTYPE)
mg
View(mg)
write_xlsx(mg, "mg.xlsx")
############### mean of environments ####################
me <- means_by(MET, ENVIRONMENT)
me
View(me)
write_xlsx(me, "me.xlsx")
################ mean perfomance of genotypes across environments ####################
mge <- MET %>%
  group_by(ENVIRONMENT, GENOTYPE) %>%
  desc_stat(DFF, PH, LL, LW, LMT, TP, PTP, EL, FL, NFE, FW, DM, TSW, GYP, stats = "mean")
mge
View(mge)
write_xlsx(mge, "mge.xlsx")
############ plotting perfomance across environments ##################
## DFF
pdff <- ge_plot(MET, ENVIRONMENT, GENOTYPE, DFF)
pdff
pdff2 <-ge_plot(MET, ENVIRONMENT, GENOTYPE, DFF, type = 2)
pdff2
## PH
pph <-ge_plot(MET, ENVIRONMENT, GENOTYPE, PH)
pph
pph2 <-ge_plot(MET, ENVIRONMENT, GENOTYPE, PH, type = 2)
pph2
## LL
pll <- ge_plot(MET, ENVIRONMENT, GENOTYPE, LL)
pll
pll2 <- ge_plot(MET, ENVIRONMENT, GENOTYPE, LL, type = 2)
pll2
## LW
plw <- ge_plot(MET, ENVIRONMENT, GENOTYPE, LW)
plw
plw2 <- ge_plot(MET, ENVIRONMENT, GENOTYPE, LW, type = 2)
plw2
## LMT
plmt <- ge_plot(MET, ENVIRONMENT, GENOTYPE, LMT)
plmt
plmt2 <- ge_plot(MET, ENVIRONMENT, GENOTYPE, LMT, type = 2)
plmt2
## TP
ptp <- ge_plot(MET, ENVIRONMENT, GENOTYPE, TP)
ptp
ptp2 <- ge_plot(MET, ENVIRONMENT, GENOTYPE, TP, type = 2)
ptp2
## PTP
pptp <- ge_plot(MET, ENVIRONMENT, GENOTYPE, PTP)
pptp
pptp2 <- ge_plot(MET, ENVIRONMENT, GENOTYPE, PTP, type = 2)
pptp2
## EL
pel <- ge_plot(MET, ENVIRONMENT, GENOTYPE, EL)
pel
pel2 <- ge_plot(MET, ENVIRONMENT, GENOTYPE, EL, type = 2)
pel2
## FL
pfl <- ge_plot(MET, ENVIRONMENT, GENOTYPE, FL)
pfl
pfl2 <- ge_plot(MET, ENVIRONMENT, GENOTYPE, FL, type = 2)
pfl2
## NFE
pnfe <- ge_plot(MET, ENVIRONMENT, GENOTYPE, NFE)
pnfe
pnfe2 <- ge_plot(MET, ENVIRONMENT, GENOTYPE, NFE, type = 2)
pnfe2
## FW
pfw <- ge_plot(MET, ENVIRONMENT, GENOTYPE, FW)
pfw
pfw2 <- ge_plot(MET, ENVIRONMENT, GENOTYPE, FW, type = 2)
pfw2
## DM
pdm <- ge_plot(MET, ENVIRONMENT, GENOTYPE, DM)
pdm
pdm2 <- ge_plot(MET, ENVIRONMENT, GENOTYPE, DM, type = 2)
pdm2
## TSW
ptsw <- ge_plot(MET, ENVIRONMENT, GENOTYPE, TSW)
ptsw
ptsw2 <- ge_plot(MET, ENVIRONMENT, GENOTYPE, TSW, type = 2)
ptsw2
## Fe (IRON)
pfe <- ge_plot(MET, ENVIRONMENT, GENOTYPE, FE)
pfe
pfe2 <- ge_plot(MET, ENVIRONMENT, GENOTYPE, FE, type = 2)
pfe2
## ZINC
pzn <- ge_plot(MET, ENVIRONMENT, GENOTYPE, ZN)
pzn
pzn2 <- ge_plot(MET, ENVIRONMENT, GENOTYPE, ZN, type = 2)
pzn2
## MANGANESE
pmn <- ge_plot(MET, ENVIRONMENT, GENOTYPE, MN)
pmn
pmn2 <- ge_plot(MET, ENVIRONMENT, GENOTYPE, MN, type = 2)
pmn2
## COPPER
pcu <- ge_plot(MET, ENVIRONMENT, GENOTYPE, CU)
pcu
pcu2 <- ge_plot(MET, ENVIRONMENT, GENOTYPE, CU, type = 2)
pcu2
## CALCIUM
pca <- ge_plot(MET, ENVIRONMENT, GENOTYPE, CA)
pca
pca2 <- ge_plot(MET, ENVIRONMENT, GENOTYPE, CA, type = 2)
pca2
## YIELD
pgyp <- ge_plot(MET, ENVIRONMENT, GENOTYPE, GYP)
pgyp
pgyp2 <- ge_plot(MET, ENVIRONMENT, GENOTYPE, GYP, type = 2)
pgyp2
############## WINNERS WITH IN EACH ENVIRONMENT #######################
win <- ge_winners(MET, ENVIRONMENT, GENOTYPE, resp = everything())
View(win)
write_xlsx(win, "win.xlsx")
################# ranks of genotypes ######################
ranks <- ge_winners(MET, ENVIRONMENT, GENOTYPE, resp = everything(), type ="ranks" )
View(ranks)
write_xlsx(ranks, "ranks.xlsx")
########### more details on perfomance #######################
ge_details(MET, ENVIRONMENT, GENOTYPE, resp = everything())
details <- ge_details(MET, ENVIRONMENT, GENOTYPE, resp = everything())
View(details)
write_xlsx(details, "details.xlsx")
############################## fixed effect models ##################################
######################## individual anova #####################
indav <- anova_ind(MET, ENVIRONMENT, GENOTYPE, REPLICATION, BLOCK, resp = c(DFF, PH, LL, LW, LMT, TP, PTP, EL, FL, NFE, FW, DM, TSW, GYP))
indav
options(width = 200)  # or more
print(indav)
## anova for dff
indav$DFF$individual
iadff <- indav$DFF$individual
View(iadff)
write_xlsx(iadff, "iadff.xlsx")
## anova for PH
indav$PH$individual
iaph <- indav$PH$individual
View(iaph)
write_xlsx(iaph, "iaph.xlsx")
## anova for LL
indav$LL$individual
iall <- indav$LL$individual
View(iall)
write_xlsx(iall, "iall.xlsx")
## anova for LW
indav$LW$individual
ialw <- indav$LW$individual
View(ialw)
write_xlsx(ialw, "ialw.xlsx")
## anova for LMT
indav$LMT$individual
ialmt <- indav$LMT$individual 
View(ialmt)
write_xlsx(ialmt, "ialmt.xlsx")
## anova for TP
indav$TP$individual
iatp <- indav$TP$individual
View(iatp)
write_xlsx(iatp, "iatp.xlsx")
## anova for PTP
indav$PTP$individual
iaptp <- indav$PTP$individual
View(iaptp)
write_xlsx(iaptp, "iaptp.xlsx")
## anova for EL
indav$EL$individual
iael <- indav$EL$individual
View(iael)
write_xlsx(iael, "iael.xlsx")
## anova for FL
indav$FL$individual
iafl <- indav$FL$individual
View(iafl)
write_xlsx(iafl, "iafl.xlsx")
## anova for NFE
indav$NFE$individual
ianfe <- indav$NFE$individual 
View(ianfe)
write_xlsx(ianfe, "ianfe.xlsx")
## anova for FW
indav$FW$individual
iafw <- indav$FW$individual
View(iafw)
write_xlsx(iafw, "iafw.xlsx")
## anova for DM
indav$DM$individual
iadm <- indav$DM$individual
View(iadm)
write_xlsx(iadm, "iadm.xlsx")
## anova for TSW
indav$TSW$individual
iatsw <- indav$TSW$individual
View(iatsw)
write_xlsx(iatsw, "iatsw.xlsx")
## anova for IRON
indav$FE$individual
iafe <- indav$FE$individual
View(iafe)
write_xlsx(iafe, "iafe.xlsx")
## anova for zinc
indav$ZN$individual
iazn <- indav$ZN$individual
View(iazn)
write_xlsx(iazn, "iazn.xlsx")
## anova for manganese
indav$MN$individual
iamn <- indav$MN$individual
View(iamn)
write_xlsx(iamn, "iamn.xlsx")
## anova for copper
indav$CU$individual
iacu <- indav$CU$individual
View(iacu)
write_xlsx(iacu, "iacu.xlsx")
## anova for calcium
indav$CA$individual
iaca <- indav$CA$individual
View(iaca)
write_xlsx(iaca, "iaca.xlsx")
## anova for yield
indav$GYP$individual
iagyp <- indav$GYP$individual
View(iagyp)
write_xlsx(iagyp, "iagyp.xlsx")
################ bartlett test ######################
bartlett.test(MET$DFF~MET$ENVIRONMENT, data = MET)
bartlett.test(MET$PH~MET$ENVIRONMENT, data = MET)
bartlett.test(MET$LL~MET$ENVIRONMENT, data = MET)
bartlett.test(MET$LW~MET$ENVIRONMENT, data = MET)
bartlett.test(MET$LMT~MET$ENVIRONMENT, data = MET)
bartlett.test(MET$TP~MET$ENVIRONMENT, data = MET)
bartlett.test(MET$PTP~MET$ENVIRONMENT, data = MET)
bartlett.test(MET$EL~MET$ENVIRONMENT, data = MET)
bartlett.test(MET$FL~MET$ENVIRONMENT, data = MET)
bartlett.test(MET$NFE~MET$ENVIRONMENT, data = MET)
bartlett.test(MET$FW~MET$ENVIRONMENT, data = MET)
bartlett.test(MET$DM~MET$ENVIRONMENT, data = MET)
bartlett.test(MET$TSW~MET$ENVIRONMENT, data = MET)
bartlett.test(MET$FE~MET$ENVIRONMENT, data = MET)
bartlett.test(MET$ZN~MET$ENVIRONMENT, data = MET)
bartlett.test(MET$MN~MET$ENVIRONMENT, data = MET)
bartlett.test(MET$CU~MET$ENVIRONMENT, data = MET)
bartlett.test(MET$CA~MET$ENVIRONMENT, data = MET)
bartlett.test(MET$GYP~MET$ENVIRONMENT, data = MET)
####################POOLED ANOVA##########################################
## pooled anova for DFF
panv1 <- anova_joint(MET, ENVIRONMENT, GENOTYPE, REPLICATION, DFF, BLOCK)
pavt1 <- panv1$DFF$anova
View(pavt1)
write_xlsx(pavt1, "pavt1.xlsx")
## pooled anova for PH
panv2 <- anova_joint(MET, ENVIRONMENT, GENOTYPE, REPLICATION, PH, BLOCK)
pavt2 <- panv2$PH$anova
View(pavt2)
write_xlsx(pavt2, "pavt2.xlsx")
## pooled anova for LL
panv3 <- anova_joint(MET, ENVIRONMENT, GENOTYPE, REPLICATION, LL, BLOCK)
pavt3 <- panv3$LL$anova
View(pavt3)
write_xlsx(pavt3, "pavt3.xlsx")
## pooled anova for LW
panv4 <- anova_joint(MET, ENVIRONMENT, GENOTYPE, REPLICATION, LW, BLOCK)
pavt4 <- panv4$LW$anova
View(pavt4)
write_xlsx(pavt4, "pavt4.xlsx")
## pooled anova for LMT
panv5 <- anova_joint(MET, ENVIRONMENT, GENOTYPE, REPLICATION, LMT, BLOCK)
pavt5 <- panv5$LMT$anova
View(pavt5)
write_xlsx(pavt5, "pavt5.xlsx")
## pooled anova for TP
panv6 <- anova_joint(MET, ENVIRONMENT, GENOTYPE, REPLICATION, TP, BLOCK)
pavt6 <- panv6$TP$anova
View(pavt6)
write_xlsx(pavt6, "pavt6.xlsx")
## pooled anova for PTP
panv7 <- anova_joint(MET, ENVIRONMENT, GENOTYPE, REPLICATION, PTP, BLOCK)
pavt7 <- panv7$PTP$anova
View(pavt7)
write_xlsx(pavt7, "pavt7.xlsx")
## pooled anova for EL
panv8 <- anova_joint(MET, ENVIRONMENT, GENOTYPE, REPLICATION, EL, BLOCK)
pavt8 <- panv8$EL$anova
View(pavt8)
write_xlsx(pavt8, "pavt8.xlsx")
## pooled anova for FL
panv9 <- anova_joint(MET, ENVIRONMENT, GENOTYPE, REPLICATION, FL, BLOCK)
pavt9 <- panv9$FL$anova
View(pavt9)
write_xlsx(pavt9, "pavt9.xlsx")
## pooled anova for NFE
panv10 <- anova_joint(MET, ENVIRONMENT, GENOTYPE, REPLICATION, NFE, BLOCK)
pavt10 <- panv10$NFE$anova
View(pavt10)
write_xlsx(pavt10, "pavt10.xlsx")
## pooled anova for FW
panv11 <- anova_joint(MET, ENVIRONMENT, GENOTYPE, REPLICATION, FW, BLOCK)
pavt11 <- panv11$FW$anova
View(pavt11)
write_xlsx(pavt11, "pavt11.xlsx")
## pooled anova for DM
panv12 <- anova_joint(MET, ENVIRONMENT, GENOTYPE, REPLICATION, DM, BLOCK)
pavt12 <- panv12$DM$anova
View(pavt12)
write_xlsx(pavt12, "pavt12.xlsx")
## pooled anova for TSW
panv13 <- anova_joint(MET, ENVIRONMENT, GENOTYPE, REPLICATION, TSW, BLOCK)
pavt13 <- panv13$TSW$anova
View(pavt13)
write_xlsx(pavt13, "pavt13.xlsx")
## pooled anova for IRON
panv14 <- anova_joint(MET, ENVIRONMENT, GENOTYPE, REPLICATION, FE, BLOCK)
pavt14 <- panv14$FE$anova
View(pavt14)
write_xlsx(pavt14, "pavt14.xlsx")
## pooled anova for ZINC
panv15 <- anova_joint(MET, ENVIRONMENT, GENOTYPE, REPLICATION, ZN, BLOCK)
pavt15 <- panv15$ZN$anova
View(pavt15)
write_xlsx(pavt15, "pavt15.xlsx")
## pooled anova for MANGANESE
panv16 <- anova_joint(MET, ENVIRONMENT, GENOTYPE, REPLICATION, MN, BLOCK)
pavt16 <- panv16$MN$anova
View(pavt16)
write_xlsx(pavt16, "pavt16.xlsx")
## pooled anova for COPPER
panv17 <- anova_joint(MET, ENVIRONMENT, GENOTYPE, REPLICATION, CU, BLOCK)
pavt17 <- panv17$CU$anova
View(pavt17)
write_xlsx(pavt17, "pavt17.xlsx")
## pooled anova for CALCIUM
panv18 <- anova_joint(MET, ENVIRONMENT, GENOTYPE, REPLICATION, CA, BLOCK)
pavt18 <- panv18$CA$anova
View(pavt18)
write_xlsx(pavt18, "pavt18.xlsx")
## pooled anova for yield
panv19 <- anova_joint(MET, ENVIRONMENT, GENOTYPE, REPLICATION, GYP, BLOCK)
pavt19 <- panv19$GYP$anova
View(pavt19)
write_xlsx(pavt19, "pavt19.xlsx")
##################### stability analysis ############################################
############# anova based stability ####################
############ Annichiarico env index ######################
## DFF
ann1 <- Annicchiarico(MET, ENVIRONMENT, GENOTYPE, REPLICATION, DFF)
print(ann1)
anndffg <- ann1$DFF$general
View(anndffg)
write_xlsx(anndffg, "anndffg.xlsx")
anndfff <- ann1$DFF$favorable
View(anndfff)
write_xlsx(anndfff, "anndfff.xlsx")
anndffuf <- ann1$DFF$unfavorable
View(anndffuf)
write_xlsx(anndffuf, "anndffuf.xlsx")
View(ann1$DFF$environments)
write_xlsx(ann1$DFF$environments, "ann1$DFF$environments.xlsx")
## PH
ann2 <- Annicchiarico(MET, ENVIRONMENT, GENOTYPE, REPLICATION, PH)
print(ann2)
annphg <- ann2$PH$general
View(annphg)
write_xlsx(annphg, "annphg.xlsx")
annphf <- ann2$PH$favorable
View(annphf)
write_xlsx(annphf, "annphf.xlsx")
annphuf <- ann2$PH$unfavorable
View(annphuf)
write_xlsx(annphuf, "annphuf.xlsx")
View(ann2$PH$environments)
write_xlsx(ann2$PH$environments, "ann2$PH$environments.xlsx")
## LL
ann3 <- Annicchiarico(MET, ENVIRONMENT, GENOTYPE, REPLICATION, LL)
print(ann3)
annllg <- ann3$LL$general
View(annllg)
write_xlsx(annllg, "annllg.xlsx")
annllf <- ann3$LL$favorable
View(annllf)
write_xlsx(annllf, "annllf.xlsx")
annlluf <- ann3$LL$unfavorable
View(annlluf)
write_xlsx(annlluf, "annlluf.xlsx")
View(ann3$LL$environments)
write_xlsx(ann3$LL$environments, "ann3$LL$environments.xlsx")
## LW
ann4 <- Annicchiarico(MET, ENVIRONMENT, GENOTYPE, REPLICATION, LW)
print(ann4)
annlwg <- ann4$LW$general
View(annlwg)
write_xlsx(annlwg, "annlwg.xlsx")
annlwf <- ann4$LW$favorable
View(annlwf)
write_xlsx(annlwf, "annlwf.xlsx")
annlwuf <- ann4$LW$unfavorable
View(annlwuf)
write_xlsx(annlwuf, "annlwuf.xlsx")
View(ann4$LW$environments)
write_xlsx(ann4$LW$environments, "ann4$LW$environments.xlsx")
## LMT
ann5 <- Annicchiarico(MET, ENVIRONMENT, GENOTYPE, REPLICATION, LMT)
print(ann5)
annlmtg <- ann5$LMT$general
View(annlmtg)
write_xlsx(annlmtg, "annlmtg.xlsx")
annlmtf <- ann5$LMT$favorable
View(annlmtf)
write_xlsx(annlmtf, "annlmtf.xlsx")
annlmtuf <- ann5$LMT$unfavorable
View(annlmtuf)
write_xlsx(annlmtuf, "annlmtuf.xlsx")
View(ann5$LMT$environments)
write_xlsx(ann5$LMT$environments, "ann5$LMT$environments.xlsx")
## TP
ann6 <- Annicchiarico(MET, ENVIRONMENT, GENOTYPE, REPLICATION, TP)
print(ann6)
annltpg <- ann6$TP$general
View(annltpg)
write_xlsx(annltpg, "annltpg.xlsx")
annltpf <- ann6$TP$favorable
View(annltpf)
write_xlsx(annltpf, "annltpf.xlsx")
annltpuf <- ann6$TP$unfavorable
View(annltpuf)
write_xlsx(annltpuf, "annltpuf.xlsx")
View(ann6$TP$environments)
write_xlsx(ann6$TP$environments, "ann6$TP$environments.xlsx")
## PTP
ann7 <- Annicchiarico(MET, ENVIRONMENT, GENOTYPE, REPLICATION, PTP)
print(ann7)
annptpg <- ann7$PTP$general
View(annptpg)
write_xlsx(annptpg, "annptpg.xlsx")
annptpf <- ann7$PTP$favorable
View(annptpf)
write_xlsx(annptpf, "annptpf.xlsx")
annptpuf <- ann7$PTP$unfavorable
View(annptpuf)
write_xlsx(annptpuf, "annptpuf.xlsx")
View(ann7$PTP$environments)
write_xlsx(ann7$PTP$environments, "ann7$PTP$environments.xlsx")
## EL
ann8 <- Annicchiarico(MET, ENVIRONMENT, GENOTYPE, REPLICATION, EL)
print(ann8)
annelg <- ann8$EL$general
View(annelg)
write_xlsx(annelg, "annelg.xlsx")
annelf <- ann8$EL$favorable
View(annelf)
write_xlsx(annelf, "annelf.xlsx")
anneluf <- ann8$EL$unfavorable
View(anneluf)
write_xlsx(anneluf, "anneluf.xlsx")
View(ann8$EL$environments)
write_xlsx(ann8$EL$environments, "ann8$EL$environments.xlsx")
## FL
ann9 <- Annicchiarico(MET, ENVIRONMENT, GENOTYPE, REPLICATION, FL)
print(ann9)
annflg <- ann9$FL$general
View(annflg)
write_xlsx(annflg, "annflg.xlsx")
annflf <- ann9$FL$favorable
View(annflf)
write_xlsx(annflf, "annflf.xlsx")
annfluf <- ann9$FL$unfavorable
View(annfluf)
write_xlsx(annfluf, "annfluf.xlsx")
View(ann9$FL$environments)
write_xlsx(ann9$FL$environments, "ann9$FL$environments.xlsx")
## NFE
ann10 <- Annicchiarico(MET, ENVIRONMENT, GENOTYPE, REPLICATION, NFE)
print(ann10)
annnfeg <- ann10$NFE$general
View(annnfeg)
write_xlsx(annnfeg, "annnfeg.xlsx")
annnfef <- ann10$NFE$favorable
View(annnfef)
write_xlsx(annnfef, "annnfef.xlsx")
annnfeuf <- ann10$NFE$unfavorable
View(annnfeuf)
write_xlsx(annnfeuf, "annnfeuf.xlsx")
View(ann10$NFE$environments)
write_xlsx(ann10$NFE$environments, "ann10$NFE$environments.xlsx")
## FW
ann11 <- Annicchiarico(MET, ENVIRONMENT, GENOTYPE, REPLICATION, FW)
print(ann11)
annfwg <- ann11$FW$general
View(annfwg)
write_xlsx(annfwg, "annfwg.xlsx")
annfwf <- ann11$FW$favorable
View(annfwf)
write_xlsx(annfwf, "annfwf.xlsx")
annfwuf <- ann11$FW$unfavorable
View(annfwuf)
write_xlsx(annfwuf, "annfwuf.xlsx")
View(ann11$FW$environments)
write_xlsx(ann11$FW$environments, "ann11$FW$environments.xlsx")
## DM
ann12 <- Annicchiarico(MET, ENVIRONMENT, GENOTYPE, REPLICATION, DM)
print(ann12)
anndmg <- ann12$DM$general
View(anndmg)
write_xlsx(anndmg, "anndmg.xlsx")
anndmf <- ann12$DM$favorable
View(anndmf)
write_xlsx(anndmf, "anndmf.xlsx")
anndmuf <- ann12$DM$unfavorable
View(anndmuf)
write_xlsx(anndmuf, "anndmuf.xlsx")
View(ann12$DM$environments)
write_xlsx(ann12$DM$environments, "ann12$DM$environments.xlsx")
## TSW
ann13 <- Annicchiarico(MET, ENVIRONMENT, GENOTYPE, REPLICATION, TSW)
print(ann13)
anntswg <- ann13$TSW$general
View(anntswg)
write_xlsx(anntswg, "anntswg.xlsx")
anntswf <- ann13$TSW$favorable
View(anntswf)
write_xlsx(anntswf, "anntswf.xlsx")
anntswuf <- ann13$TSW$unfavorable
View(anntswuf)
write_xlsx(anntswuf, "anntswuf.xlsx")
View(ann13$TSW$environments)
write_xlsx(ann13$TSW$environments, "ann13$TSW$environments.xlsx")
## IRON
ann14 <- Annicchiarico(MET, ENVIRONMENT, GENOTYPE, REPLICATION, FE)
print(ann14)
annfeg <- ann14$FE$general
View(annfeg)
write_xlsx(annfeg, "annfeg.xlsx")
annfef <- ann14$FE$favorable
View(annfef)
write_xlsx(annfef, "annfef.xlsx")
annfeuf <- ann14$FE$unfavorable
View(annfeuf)
write_xlsx(annfeuf, "annfeuf.xlsx")
View(ann14$FE$environments)
write_xlsx(ann14$FE$environments, "ann14$FE$environments.xlsx")
## ZINC
ann15 <- Annicchiarico(MET, ENVIRONMENT, GENOTYPE, REPLICATION, ZN)
print(ann15)
annzng <- ann15$ZN$general
View(annzng)
write_xlsx(annzng, "annzng.xlsx")
annznf <- ann15$ZN$favorable
View(annznf)
write_xlsx(annznf, "annznf.xlsx")
annznuf <- ann15$ZN$unfavorable
View(annznuf)
write_xlsx(annznuf, "annznuf.xlsx")
View(ann15$ZN$environments)
write_xlsx(ann15$ZN$environments, "ann15$ZN$environments.xlsx")
## MANGANESE
ann16 <- Annicchiarico(MET, ENVIRONMENT, GENOTYPE, REPLICATION, MN)
print(ann16)
annmng <- ann16$MN$general
View(annmng)
write_xlsx(annmng, "annmng.xlsx")
annmnf <- ann16$MN$favorable
View(annmnf)
write_xlsx(annmnf, "annmnf.xlsx")
annmnuf <- ann16$MN$unfavorable
View(annmnuf)
write_xlsx(annmnuf, "annmnuf.xlsx")
View(ann16$MN$environments)
write_xlsx(ann16$MN$environments, "ann16$MN$environments.xlsx")
## COPPER
ann17 <- Annicchiarico(MET, ENVIRONMENT, GENOTYPE, REPLICATION, CU)
print(ann17)
anncug <- ann17$CU$general
View(anncug)
write_xlsx(anncug, "anncug.xlsx")
anncuf <- ann17$CU$favorable
View(anncuf)
write_xlsx(anncuf, "anncuf.xlsx")
anncuuf <- ann17$CU$unfavorable
View(anncuuf)
write_xlsx(anncuuf, "anncuuf.xlsx")
View(ann17$CU$environments)
write_xlsx(ann17$CU$environments, "ann17$CU$environments.xlsx")
## CALCIUM
ann18 <- Annicchiarico(MET, ENVIRONMENT, GENOTYPE, REPLICATION, CA)
print(ann18)
annCAg <- ann18$CA$general
View(annCAg)
write_xlsx(annCAg, "annCAg.xlsx")
annCAf <- ann18$CA$favorable
View(annCAf)
write_xlsx(annCAf, "annCAf.xlsx")
annCAuf <- ann18$CA$unfavorable
View(annCAuf)
write_xlsx(annCAuf, "annCAuf.xlsx")
View(ann18$CA$environments)
write_xlsx(ann18$CA$environments, "ann18$CA$environments.xlsx")
## YIELD
ann19 <- Annicchiarico(MET, ENVIRONMENT, GENOTYPE, REPLICATION, GYP)
print(ann19)
annGYPg <- ann19$GYP$general
View(annGYPg)
write_xlsx(annGYPg, "annGYPg.xlsx")
annGYPf <- ann19$GYP$favorable
View(annGYPf)
write_xlsx(annGYPf, "annGYPf.xlsx")
annGYPuf <- ann19$GYP$unfavorable
View(annGYPuf)
write_xlsx(annGYPuf, "annGYPuf.xlsx")
View(ann19$GYP$environments)
write_xlsx(ann19$GYP$environments, "ann19$GYP$environments.xlsx")

library(ggplot2)

#1. GENERAL Wi SCATTER PLOT

#(Mean Yield vs Annicchiarico Index)

library(ggrepel)

ggplot(annGYPg, aes(x = Wi, y = Y, label = GEN)) +
  geom_point(size = 4, color = "blue") +
  geom_text_repel(size = 4) +
  
  geom_vline(xintercept = 100, linetype = "dashed", color = "red") +
  geom_hline(yintercept = mean(annGYPg$Y), 
             linetype = "dashed", color = "darkgreen") +
  
  labs(title = "Annicchiarico Stability Analysis - General",
       x = "Confidence Index (Wi)",
       y = "Mean Grain Yield") +
  
  theme_bw(base_size = 14)

#2. FAVORABLE vs UNFAVORABLE ENVIRONMENTS PLOT

#(Very important for paper)

library(dplyr)

compare <- annGYPf %>%
  select(GEN, Wi_f = Wi, Y_f = Y) %>%
  left_join(
    annGYPuf %>%
      select(GEN, Wi_uf = Wi, Y_uf = Y),
    by = "GEN"
  )

ggplot(compare, aes(x = Wi_uf, y = Wi_f, label = GEN)) +
  
  geom_point(size = 4, color = "purple") +
  geom_text_repel(size = 3.5) +
  
  geom_vline(xintercept = 100, linetype = "dashed", color = "red") +
  geom_hline(yintercept = 100, linetype = "dashed", color = "red") +
  
  labs(title = "Wi in Favorable vs Unfavorable Environments",
       x = "Wi in Unfavorable Environments",
       y = "Wi in Favorable Environments") +
  
  theme_classic(base_size = 14)
#nterpretation:

#Upper-right = stable in both environments
#Upper-left = specifically adapted to favorable
#Lower-right = adapted to unfavorable
#Lower-left = unstable/poor

#3. BUBBLE PLOT (Excellent for publication)

#Bubble size = stability
ggplot(annGYPg,
       aes(x = Wi,
           y = Y,
           size = 1/Sd_rp,
           label = GEN)) +
  
  geom_point(alpha = 0.7, color = "darkorange") +
  geom_text_repel(size = 3.5) +
  
  labs(title = "Bubble Plot of Stability and Yield",
       x = "Wi",
       y = "Mean Yield",
       size = "Stability") +
  
  theme_minimal(base_size = 14)

#Smaller Sd_rp → larger bubble → more stable.

#4.TOP GENOTYPES BARPLOT

top10 <- annGYPg %>%
  arrange(desc(Wi)) %>%
  slice(1:10)

ggplot(top10,
       aes(x = reorder(GEN, Wi),
           y = Wi)) +
  
  geom_col(fill = "steelblue") +
  coord_flip() +
  
  labs(title = " Annicchiarico Stability Top 10 Stable Genotypes",
       x = "Genotype",
       y = "Wi") +
  
  theme_bw(base_size = 14)

#5.ENVIRONMENTAL INDEX PLOT

library(ggplot2)

env_index <- ann19$GYP$environments

print(env_index)

ggplot(env_index,
       aes(x = ENV,
           y = index,
           fill = class)) +
  
  geom_col(width = 0.7) +
  
  geom_text(aes(label = round(index, 2)),
            vjust = ifelse(env_index$index > 0, -0.4, 1.2),
            size = 5) +
  
  geom_hline(yintercept = 0,
             linetype = "dashed",
             linewidth = 0.8) +
  
  scale_fill_manual(values = c("favorable" = "forestgreen",
                               "unfavorable" = "tomato")) +
  
  labs(title = "Environmental Index Based on Mean Grain Yield",
       x = "Environment",
       y = "Environmental Index") +
  
  theme_bw(base_size = 16) +
  
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.title = element_blank()
  )

#6 QUADRANT SELECTION PLOT

#(Best for genotype recommendation)

ggplot(annGYPg,
       aes(x = Wi,
           y = Y,
           label = GEN)) +
  
  geom_point(size = 4, color = "darkgreen") +
  geom_text_repel(size = 3.5) +
  
  geom_vline(xintercept = mean(annGYPg$Wi),
             linetype = "dashed") +
  
  geom_hline(yintercept = mean(annGYPg$Y),
             linetype = "dashed") +
  
  labs(title = "Quadrant Selection Plot",
       x = "Wi",
       y = "Yield") +
  
  theme_classic(base_size = 14)

# Upper-right quadrant = ideal genotypes.

#7 HEATMAP OF Wi VALUES

library(tidyr)

heat <- compare %>%
  select(GEN, Wi_f, Wi_uf) %>%
  pivot_longer(-GEN,
               names_to = "Environment",
               values_to = "Wi")

ggplot(heat,
       aes(x = Environment,
           y = GEN,
           fill = Wi)) +
  
  geom_tile() +
  
  labs(title = "Heatmap of Wi Values") +
  
  theme_minimal(base_size = 12)

###################### ECOVALENCE################################
## DFF
eco1 <- ecovalence(MET, ENVIRONMENT, GENOTYPE, REPLICATION, DFF)
eco1
View(eco1$DFF)
write_xlsx(eco1$DFF, "eco1$DFF.xlsx")
## PH
eco2 <- ecovalence(MET, ENVIRONMENT, GENOTYPE, REPLICATION, PH)
eco2
View(eco2$PH)
write_xlsx(eco2$PH, "eco2$PH.xlsx")
## LL
eco3 <- ecovalence(MET, ENVIRONMENT, GENOTYPE, REPLICATION, LL)
eco3
View(eco3$LL)
write_xlsx(eco3$LL, "eco3$LL.xlsx")
## LW
eco4 <- ecovalence(MET, ENVIRONMENT, GENOTYPE, REPLICATION, LW)
eco4
View(eco4$LW)
write_xlsx(eco4$LW, "eco4$LW.xlsx")
## LMT
eco5 <- ecovalence(MET, ENVIRONMENT, GENOTYPE, REPLICATION, LMT)
eco5
View(eco5$LMT)
write_xlsx(eco5$LMT, "eco5$LMT.xlsx")
## TP
eco6 <- ecovalence(MET, ENVIRONMENT, GENOTYPE, REPLICATION, TP)
eco6
View(eco6$TP)
write_xlsx(eco6$TP, "eco6$TP.xlsx")
## PTP
eco7 <- ecovalence(MET, ENVIRONMENT, GENOTYPE, REPLICATION, PTP)
eco7
View(eco7$PTP)
write_xlsx(eco7$PTP, "eco7$PTP.xlsx")
## EL
eco8 <- ecovalence(MET, ENVIRONMENT, GENOTYPE, REPLICATION, EL)
eco8
View(eco8$EL)
write_xlsx(eco8$EL, "eco8$EL.xlsx")
## FL
eco9 <- ecovalence(MET, ENVIRONMENT, GENOTYPE, REPLICATION, FL)
eco9
View(eco9$FL)
write_xlsx(eco9$FL, "eco9$FL.xlsx")
## NFE
eco10 <- ecovalence(MET, ENVIRONMENT, GENOTYPE, REPLICATION, NFE)
eco10
View(eco10$NFE)
write_xlsx(eco10$NFE, "eco10$NFE.xlsx")
## FW
eco11 <- ecovalence(MET, ENVIRONMENT, GENOTYPE, REPLICATION, FW)
eco11
View(eco11$FW)
write_xlsx(eco11$FW, "eco11$FW.xlsx")
## DM
eco12 <- ecovalence(MET, ENVIRONMENT, GENOTYPE, REPLICATION, DM)
eco12
View(eco12$DM)
write_xlsx(eco12$DM, "eco12$DM.xlsx")
## TSW
eco13 <- ecovalence(MET, ENVIRONMENT, GENOTYPE, REPLICATION, TSW)
eco13
View(eco13$TSW)
write_xlsx(eco13$TSW, "eco13$TSW.xlsx")
## IRON
eco14 <- ecovalence(MET, ENVIRONMENT, GENOTYPE, REPLICATION, FE)
eco14
View(eco14$FE)
write_xlsx(eco14$FE, "eco14$FE.xlsx")
## ZINC
eco15 <- ecovalence(MET, ENVIRONMENT, GENOTYPE, REPLICATION, ZN)
eco15
View(eco15$ZN)
write_xlsx(eco15$ZN, "eco15$ZN.xlsx")
## MANGANESE
eco16 <- ecovalence(MET, ENVIRONMENT, GENOTYPE, REPLICATION, MN)
eco16
View(eco16$MN)
write_xlsx(eco16$MN, "eco16$MN.xlsx")
## COPPER
eco17 <- ecovalence(MET, ENVIRONMENT, GENOTYPE, REPLICATION, CU)
eco17
View(eco17$CU)
write_xlsx(eco17$CU, "eco17$CU.xlsx")
## CALCIUM
eco18 <- ecovalence(MET, ENVIRONMENT, GENOTYPE, REPLICATION, CA)
eco18
View(eco18$CA)
write_xlsx(eco18$CA, "eco18$CA.xlsx")
## YIELD
eco19 <- ecovalence(MET, ENVIRONMENT, GENOTYPE, REPLICATION, GYP)
eco19
View(eco19$GYP)
write_xlsx(eco19$GYP, "eco19$GYP.xlsx")

#LOAD PACKAGES

library(ggplot2)
library(dplyr)
library(tidyr)

#EXTRACT DATA
# Extract ecovalence results
eco_df <- eco19$GYP

# Check data
head(eco_df)

#1. OVERALL ECOVALENCE BAR PLOT

#Lower ecovalence = more stable genotype

eco_df2 <- eco_df %>%
  arrange(Ecoval)

ggplot(eco_df2,
       aes(x = reorder(GEN, Ecoval),
           y = Ecoval)) +
  
  geom_col(fill = "steelblue") +
  
  coord_flip() +
  
  labs(title = "Overall Ecovalence for Grain Yield",
       x = "Genotype",
       y = "Ecovalence") +
  
  theme_bw(base_size = 13)

#INTERPRETATION
#Smaller bars → stable genotypes
#Larger bars → unstable genotypes

#2. ECOVALENCE PERCENTAGE PLOT
#Contribution of each genotype to GE interaction

ggplot(eco_df2,
       aes(x = reorder(GEN, Ecov_perc),
           y = Ecov_perc)) +
  
  geom_col(fill = "darkorange") +
  
  coord_flip() +
  
  labs(title = "Percentage Contribution to GE Interaction",
       x = "Genotype",
       y = "Ecovalence Percentage (%)") +
  
  theme_classic(base_size = 13)

#INTERPRETATION
#Lower percentage = stable genotype
#Higher percentage = unstable genotype

#3. RANK PLOT
#Yield rank vs stability rank

eco_df2 <- eco_df %>%
  mutate(YieldRank = rank(-Ecoval),
         StabilityRank = rank(Ecoval))

ggplot(eco_df2,
       aes(x = YieldRank,
           y = StabilityRank,
           label = GEN)) +
  
  geom_point(size = 4,
             color = "purple") +
  
  geom_text(vjust = -0.6,
            size = 3.5) +
  
  labs(title = "Ecovalence Rank Plot",
       x = "Yield Rank",
       y = "Stability Rank") +
  
  theme_bw(base_size = 13)

#4. ENVIRONMENT CONTRIBUTION PLOT
#Shows genotype contribution across environments

eco_long <- eco_df %>%
  select(GEN, E1:E5) %>%
  pivot_longer(cols = E1:E5,
               names_to = "Environment",
               values_to = "Contribution")

ggplot(eco_long,
       aes(x = Environment,
           y = Contribution,
           fill = GEN)) +
  
  geom_bar(stat = "identity") +
  
  labs(title = "Genotype Contribution Across Environments",
       x = "Environment",
       y = "Interaction Contribution") +
  
  theme_bw(base_size = 12)

#5. PUBLICATION-QUALITY QUADRANT PLOT (BEST)

#This is the MOST IMPORTANT ecovalence plot.

# Add mean yield manually from your superiority/regression data

yield_df <- super19$GYP$index %>%
  select(GEN, Y)

eco_plot <- left_join(eco_df,
                      yield_df,
                      by = "GEN")

meanY  <- mean(eco_plot$Y)
meanEc <- mean(eco_plot$Ecoval)

eco_plot <- eco_plot %>%
  mutate(Group = case_when(
    
    Y >= meanY & Ecoval <= meanEc ~ "High Yield + Stable",
    
    Y >= meanY & Ecoval > meanEc ~ "High Yield + Unstable",
    
    Y < meanY & Ecoval <= meanEc ~ "Low Yield + Stable",
    
    TRUE ~ "Low Yield + Unstable"
  ))

ggplot(eco_plot,
       aes(x = Y,
           y = Ecoval,
           color = Group,
           label = GEN)) +
  
  geom_point(size = 5,
             alpha = 0.9) +
  
  geom_text(vjust = -0.7,
            size = 4) +
  
  geom_vline(xintercept = meanY,
             linetype = "dashed",
             linewidth = 1) +
  
  geom_hline(yintercept = meanEc,
             linetype = "dashed",
             linewidth = 1) +
  
  annotate("text",
           x = max(eco_plot$Y),
           y = min(eco_plot$Ecoval),
           label = "High Yield + Stable",
           hjust = 1,
           size = 4) +
  
  annotate("text",
           x = min(eco_plot$Y),
           y = min(eco_plot$Ecoval),
           label = "Low Yield + Stable",
           hjust = 0,
           size = 4) +
  
  annotate("text",
           x = max(eco_plot$Y),
           y = max(eco_plot$Ecoval),
           label = "High Yield + Unstable",
           hjust = 1,
           size = 4) +
  
  annotate("text",
           x = min(eco_plot$Y),
           y = max(eco_plot$Ecoval),
           label = "Low Yield + Unstable",
           hjust = 0,
           size = 4) +
  
  scale_color_manual(values = c(
    "High Yield + Stable" = "red",
    "High Yield + Unstable" = "orange",
    "Low Yield + Stable" = "blue",
    "Low Yield + Unstable" = "black"
  )) +
  
  labs(title = "GYP: Mean Yield vs Ecovalence",
       subtitle = "Lower ecovalence indicates greater stability",
       x = "Mean Grain Yield",
       y = "Ecovalence") +
  
  theme_classic(base_size = 15)

#MOST INFORMATIVE VERSION (Highly Recommended)

#Heatmap-style plot.

ggplot(eco_long,
       aes(x = Environment,
           y = GEN,
           fill = Contribution)) +
  
  geom_tile(color = "white") +
  
  geom_text(aes(label = round(Contribution, 1)),
            size = 3) +
  
  labs(title = "Ecovalence Contribution Heatmap",
       x = "Environment",
       y = "Genotype") +
  
  theme_bw(base_size = 12)

#Bubble Plot

#Interpretation
#Bubble size = magnitude of contribution
#Bubble color = positive or negative interaction
#Large bubbles = stronger GE interaction contribution
#Small bubbles = more stable response

library(ggplot2)
library(dplyr)
library(tidyr)

# Convert to long format
eco_long <- eco_df %>%
  select(GEN, E1:E5) %>%
  pivot_longer(cols = E1:E5,
               names_to = "Environment",
               values_to = "Contribution")

# Bubble plot
ggplot(eco_long,
       aes(x = Environment,
           y = GEN,
           size = abs(Contribution),
           fill = Contribution)) +
  
  geom_point(shape = 21,
             color = "black",
             alpha = 0.8) +
  
  scale_size_continuous(name = "Contribution Magnitude") +
  
  labs(title = "Ecovalence Contribution Bubble Plot",
       subtitle = "Bubble size indicates magnitude of GE interaction contribution",
       x = "Environment",
       y = "Genotype") +
  
  theme_bw(base_size = 13)

#BETTER PUBLICATION VERSION

#Add labels.
ggplot(eco_long,
       aes(x = Environment,
           y = GEN,
           size = abs(Contribution),
           fill = Contribution)) +
  
  geom_point(shape = 21,
             color = "black",
             alpha = 0.85) +
  
  geom_text(aes(label = round(Contribution, 1)),
            size = 2.8) +
  
  scale_size_continuous(range = c(2, 12),
                        name = "Magnitude") +
  
  labs(title = "Ecovalence Contribution Bubble Plot",
       subtitle = "Large bubbles indicate stronger GE interaction",
       x = "Environment",
       y = "Genotype") +
  
  theme_classic(base_size = 13)

n_gen <- length(unique(eco_df$GEN))

ggsave("GYP_LinBinn_BarPlot.png",
       width = 8,
       height = n_gen * 0.25,
       dpi = 600)

####################### shukla ##########################
# DFF
shu1 <- Shukla(MET, ENVIRONMENT, GENOTYPE, REPLICATION, DFF) 
shu1
View(shu1$DFF)
write_xlsx(shu1$DFF, "shu1$DFF.xlsx")
# PH 
shu2 <- Shukla(MET, ENVIRONMENT, GENOTYPE, REPLICATION, PH) 
shu2
View(shu2$PH)
write_xlsx(shu2$PH, "shu2$PH.xlsx")
# LL
shu3 <- Shukla(MET, ENVIRONMENT, GENOTYPE, REPLICATION, LL) 
shu3
View(shu3$LL)
write_xlsx(shu3$LL, "shu3$LL.xlsx")
# LW
shu4 <- Shukla(MET, ENVIRONMENT, GENOTYPE, REPLICATION, LW) 
shu4
View(shu4$LW)
write_xlsx(shu4$LW, "shu4$LW.xlsx")
# LMT
shu5 <- Shukla(MET, ENVIRONMENT, GENOTYPE, REPLICATION, LMT) 
shu5
View(shu5$LMT)
write_xlsx(shu5$LMT, "shu5$LMT.xlsx")
# TP
shu6 <- Shukla(MET, ENVIRONMENT, GENOTYPE, REPLICATION, TP) 
shu6
View(shu6$TP)
write_xlsx(shu6$TP, "shu6$TP.xlsx")
# PTP
shu7 <- Shukla(MET, ENVIRONMENT, GENOTYPE, REPLICATION, PTP) 
shu7
View(shu7$PTP)
write_xlsx(shu7$PTP, "shu7$PTP.xlsx")
# EL
shu8 <- Shukla(MET, ENVIRONMENT, GENOTYPE, REPLICATION, EL) 
shu8
View(shu8$EL)
write_xlsx(shu8$EL, "shu8$EL.xlsx")
# FL
shu9 <- Shukla(MET, ENVIRONMENT, GENOTYPE, REPLICATION, FL) 
shu9
View(shu9$FL)
write_xlsx(shu9$FL, "shu9$FL.xlsx")
# NFE
shu10 <- Shukla(MET, ENVIRONMENT, GENOTYPE, REPLICATION, NFE) 
shu10
View(shu10$NFE)
write_xlsx(shu10$NFE, "shu10$NFE.xlsx")
# FW
shu11 <- Shukla(MET, ENVIRONMENT, GENOTYPE, REPLICATION, FW) 
shu11
View(shu11$FW)
write_xlsx(shu11$FW, "shu11$FW.xlsx")
# DM
shu12 <- Shukla(MET, ENVIRONMENT, GENOTYPE, REPLICATION, DM) 
shu12
View(shu12$DM)
write_xlsx(shu12$DM, "shu12$DM.xlsx")
# TSW
shu13 <- Shukla(MET, ENVIRONMENT, GENOTYPE, REPLICATION, TSW) 
shu13
View(shu13$TSW)
write_xlsx(shu13$TSW, "shu13$TSW.xlsx")
# IRON
shu14 <- Shukla(MET, ENVIRONMENT, GENOTYPE, REPLICATION, FE) 
shu14
View(shu14$FE)
write_xlsx(shu14$FE, "shu14$FE.xlsx")
# ZINC 
shu15 <- Shukla(MET, ENVIRONMENT, GENOTYPE, REPLICATION, ZN) 
shu15
View(shu15$ZN)
write_xlsx(shu15$ZN, "shu15$ZN.xlsx")
# MANGANESE
shu16 <- Shukla(MET, ENVIRONMENT, GENOTYPE, REPLICATION, MN) 
shu16
View(shu16$MN)
write_xlsx(shu16$MN, "shu16$MN.xlsx")
# COPPER
shu17 <- Shukla(MET, ENVIRONMENT, GENOTYPE, REPLICATION, CU) 
shu17
View(shu17$CU)
write_xlsx(shu17$CU, "shu17$CU.xlsx")
# CALCIUM
shu18 <- Shukla(MET, ENVIRONMENT, GENOTYPE, REPLICATION, CA) 
shu18
View(shu18$CA)
write_xlsx(shu18$CA, "shu18$CA.xlsx")
# YIELD
shu19 <- Shukla(MET, ENVIRONMENT, GENOTYPE, REPLICATION, GYP) 
shu19
View(shu19$GYP)
write_xlsx(shu19$GYP, "shu19$GYP.xlsx")

#PLOTS

#1. Mean Yield vs Shukla Stability Variance ⭐ BEST

#This is the most informative figure. Ideal genotypes:
#✅ high yield
#✅ low Shukla variance

library(ggplot2)
library(dplyr)
library(ggrepel)

# Extract data
shu_df <- shu19$GYP

# Mean lines
meanY  <- mean(shu_df$Y)
meanSV <- mean(shu_df$ShuklaVar)

# Group classification
shu_df <- shu_df %>%
  mutate(Group = case_when(
    Y > meanY & ShuklaVar < meanSV ~ "High Yield + Stable",
    Y > meanY & ShuklaVar > meanSV ~ "High Yield + Unstable",
    Y < meanY & ShuklaVar < meanSV ~ "Low Yield + Stable",
    Y < meanY & ShuklaVar > meanSV ~ "Low Yield + Unstable"
  ))

# Plot
ggplot(shu_df,
       aes(x = Y,
           y = ShuklaVar,
           color = Group,
           label = GEN)) +
  
  geom_point(size = 4) +
  
  geom_text_repel(size = 3.5) +
  
  geom_vline(xintercept = meanY,
             linetype = "dashed") +
  
  geom_hline(yintercept = meanSV,
             linetype = "dashed") +
  
  labs(title = "Shukla Stability Variance Analysis",
       subtitle = "High yield and low stability variance are desirable",
       x = "Mean Grain Yield",
       y = "Shukla Stability Variance") +
  
  theme_classic(base_size = 13)

#2. Stability Variance Ranking Plot

#Lower variance = better stability.

shu_rank <- shu_df %>%
  arrange(ShuklaVar)

ggplot(shu_rank,
       aes(x = reorder(GEN, ShuklaVar),
           y = ShuklaVar)) +
  
  geom_col(fill = "steelblue") +
  
  coord_flip() +
  
  labs(title = "Shukla Stability Variance Ranking",
       subtitle = "Lower values indicate greater stability",
       x = "Genotype",
       y = "Shukla Stability Variance") +
  
  theme_bw(base_size = 12)

#3. Simultaneous Selection Index (SSI) Plot ⭐ Publication Quality

#Very useful for identifying stable and productive genotypes simultaneously.

#Lower SSI = better genotype.

shu_ssi <- shu_df %>%
  arrange(ssiShukaVar)

ggplot(shu_ssi,
       aes(x = reorder(GEN, ssiShukaVar),
           y = ssiShukaVar)) +
  
  geom_col(fill = "darkgreen") +
  
  coord_flip() +
  
  geom_text(aes(label = round(ssiShukaVar,1)),
            hjust = -0.1,
            size = 3) +
  
  labs(title = "Simultaneous Selection Index (SSI)",
       subtitle = "Lower SSI indicates desirable genotypes",
       x = "Genotype",
       y = "SSI") +
  
  theme_classic(base_size = 12)

#4. Yield Rank vs Stability Rank Plot

#Useful for visual genotype selection.

ggplot(shu_df,
       aes(x = rMean,
           y = rShukaVar,
           label = GEN)) +
  
  geom_point(size = 4,
             color = "purple") +
  
  geom_text_repel(size = 3.5) +
  
  labs(title = "Yield Rank vs Stability Rank",
       x = "Yield Rank",
       y = "Stability Rank") +
  
  theme_bw(base_size = 13)

#5. Publication-Quality Quadrant Plot ⭐ RECOMMENDED

#This is the best figure for papers.

ggplot(shu_df,
       aes(x = Y,
           y = ShuklaVar,
           label = GEN,
           color = Group)) +
  
  geom_point(size = 5) +
  
  geom_text_repel(size = 3.5) +
  
  geom_vline(xintercept = meanY,
             linetype = "dashed",
             linewidth = 0.8) +
  
  geom_hline(yintercept = meanSV,
             linetype = "dashed",
             linewidth = 0.8) +
  
  annotate("text",
           x = max(shu_df$Y),
           y = min(shu_df$ShuklaVar),
           label = "High Yield + Stable",
           hjust = 1,
           size = 4,
           fontface = "bold") +
  
  annotate("text",
           x = max(shu_df$Y),
           y = max(shu_df$ShuklaVar),
           label = "High Yield + Unstable",
           hjust = 1,
           size = 4,
           fontface = "bold") +
  
  annotate("text",
           x = min(shu_df$Y),
           y = min(shu_df$ShuklaVar),
           label = "Low Yield + Stable",
           hjust = 0,
           size = 4,
           fontface = "bold") +
  
  annotate("text",
           x = min(shu_df$Y),
           y = max(shu_df$ShuklaVar),
           label = "Low Yield + Unstable",
           hjust = 0,
           size = 4,
           fontface = "bold") +
  
  labs(title = "Mean Grain Yield vs Shukla Stability Variance",
       subtitle = "Ideal genotypes combine high yield and low variance",
       x = "Mean Grain Yield",
       y = "Shukla Stability Variance") +
  
  theme_classic(base_size = 13)

##################### REG BASED (EBERHART AND RUSSEL) ######################
# DFF
reg1 <- ge_reg(MET, ENVIRONMENT, GENOTYPE, REPLICATION, DFF)
regDFF <- reg1$DFF$regression
View(regDFF)
write_xlsx(regDFF,"regDFF.xlsx")
reg1anv <- reg1$DFF$anova
View(reg1anv)
write_xlsx(reg1anv, "reg1anv.xlsx")
plot(reg1)
# PH
reg2 <- ge_reg(MET, ENVIRONMENT, GENOTYPE, REPLICATION, PH)
print(reg2)
regPH <- reg2$PH$regression
View(regPH)
write_xlsx(regPH,"regPH.xlsx")
reg2anv <- reg2$PH$anova
View(reg2anv)
write_xlsx(reg2anv, "reg2anv.xlsx")
plot(reg2)
# LL
reg3 <- ge_reg(MET, ENVIRONMENT, GENOTYPE, REPLICATION, LL)
print(reg3)
regLL <- reg3$LL$regression
View(regLL)
write_xlsx(regLL,"regLL.xlsx")
reg3anv <- reg3$LL$anova
View(reg3anv)
write_xlsx(reg3anv, "reg3anv.xlsx")
plot(reg3)
# LW
reg4 <- ge_reg(MET, ENVIRONMENT, GENOTYPE, REPLICATION, LW)
print(reg4)
regLW <- reg4$LW$regression
View(regLW)
write_xlsx(regLW,"regLW.xlsx")
reg4anv <- reg4$LW$anova
View(reg4anv)
write_xlsx(reg4anv, "reg4anv.xlsx")
plot(reg4)
# LMT
reg5 <- ge_reg(MET, ENVIRONMENT, GENOTYPE, REPLICATION, LMT)
print(reg5)
regLMT <- reg5$LMT$regression
View(regLMT)
write_xlsx(regLMT,"regLMT.xlsx")
reg5anv <- reg5$LMT$anova
View(reg5anv)
write_xlsx(reg5anv, "reg5anv.xlsx")
plot(reg5)
# TP
reg6 <- ge_reg(MET, ENVIRONMENT, GENOTYPE, REPLICATION, TP)
print(reg6)
regTP <- reg6$TP$regression
View(regTP)
write_xlsx(regTP,"regTP.xlsx")
reg6anv <- reg6$TP$anova
View(reg6anv)
write_xlsx(reg6anv, "reg6anv.xlsx")
plot(reg6)
# PTP
reg7 <- ge_reg(MET, ENVIRONMENT, GENOTYPE, REPLICATION, PTP)
print(reg7)
regPTP <- reg7$PTP$regression
View(regPTP)
write_xlsx(regPTP,"regPTP.xlsx")
reg7anv <- reg7$PTP$anova
View(reg7anv)
write_xlsx(reg7anv, "reg7anv.xlsx")
plot(reg7)
# EL
reg8 <- ge_reg(MET, ENVIRONMENT, GENOTYPE, REPLICATION, EL)
print(reg8)
regEL <- reg8$EL$regression
View(regEL)
write_xlsx(regEL,"regEL.xlsx")
reg8anv <- reg8$EL$anova
View(reg8anv)
write_xlsx(reg8anv, "reg8anv.xlsx")
plot(reg8)
# FL
reg9 <- ge_reg(MET, ENVIRONMENT, GENOTYPE, REPLICATION, FL)
print(reg9)
regFL <- reg9$FL$regression
View(regFL)
write_xlsx(regFL,"regFL.xlsx")
reg9anv <- reg9$FL$anova
View(reg9anv)
write_xlsx(reg9anv, "reg9anv.xlsx")
plot(reg9)
# NFE
reg10 <- ge_reg(MET, ENVIRONMENT, GENOTYPE, REPLICATION, NFE)
print(reg10)
regNFE <- reg10$NFE$regression
View(regNFE)
write_xlsx(regNFE,"regNFE.xlsx")
reg10anv <- reg10$NFE$anova
View(reg10anv)
write_xlsx(reg10anv, "reg10anv.xlsx")
plot(reg10)
# FW
reg11 <- ge_reg(MET, ENVIRONMENT, GENOTYPE, REPLICATION, FW)
print(reg11)
regFW <- reg11$FW$regression
View(regFW)
write_xlsx(regFW,"regFW.xlsx")
reg11anv <- reg11$FW$anova
View(reg11anv)
write_xlsx(reg11anv, "reg11anv.xlsx")
plot(reg11)
# DM
reg12 <- ge_reg(MET, ENVIRONMENT, GENOTYPE, REPLICATION, DM)
print(reg12)
regDM <- reg12$DM$regression
View(regDM)
write_xlsx(regDM,"regDM.xlsx")
reg12anv <- reg12$DM$anova
View(reg12anv)
write_xlsx(reg12anv, "reg12anv.xlsx")
plot(reg12)
# TSW
reg13 <- ge_reg(MET, ENVIRONMENT, GENOTYPE, REPLICATION, TSW)
print(reg13)
regTSW <- reg13$TSW$regression
View(regTSW)
write_xlsx(regTSW,"regTSW.xlsx")
reg13anv <- reg13$TSW$anova
View(reg13anv)
write_xlsx(reg13anv, "reg13anv.xlsx")
plot(reg13)
# IRON
reg14 <- ge_reg(MET, ENVIRONMENT, GENOTYPE, REPLICATION, FE)
print(reg14)
regFE <- reg14$FE$regression
View(regFE)
write_xlsx(regFE,"regFE.xlsx")
reg14anv <- reg14$FE$anova
View(reg14anv)
write_xlsx(reg14anv, "reg14anv.xlsx")
plot(reg14)
# ZINC
reg15 <- ge_reg(MET, ENVIRONMENT, GENOTYPE, REPLICATION, ZN)
print(reg15)
regZN <- reg15$ZN$regression
View(regZN)
write_xlsx(regZN,"regZN.xlsx")
reg15anv <- reg15$ZN$anova
View(reg15anv)
write_xlsx(reg15anv, "reg15anv.xlsx")
plot(reg15)
# MANGANESE
reg16 <- ge_reg(MET, ENVIRONMENT, GENOTYPE, REPLICATION, MN)
print(reg16)
regMN <- reg16$MN$regression
View(regMN)
write_xlsx(regMN,"regMN.xlsx")
reg16anv <- reg16$MN$anova
View(reg16anv)
write_xlsx(reg16anv, "reg16anv.xlsx")
plot(reg16)
# COPPER
reg17 <- ge_reg(MET, ENVIRONMENT, GENOTYPE, REPLICATION, CU)
print(reg17)
regCU <- reg17$CU$regression
View(regCU)
write_xlsx(regCU,"regCU.xlsx")
reg17anv <- reg17$CU$anova
View(reg17anv)
write_xlsx(reg17anv, "reg17anv.xlsx")
plot(reg17)
# CALCIUM
reg18 <- ge_reg(MET, ENVIRONMENT, GENOTYPE, REPLICATION, CA)
print(reg18)
regCA <- reg18$CA$regression
View(regCA)
write_xlsx(regCA,"regCA.xlsx")
reg18anv <- reg18$CA$anova
View(reg18anv)
write_xlsx(reg18anv, "reg18anv.xlsx")
plot(reg18)
# YIELD
reg19 <- ge_reg(MET, ENVIRONMENT, GENOTYPE, REPLICATION, GYP)
print(reg19)
regGYP <- reg19$GYP$regression
View(regGYP)
write_xlsx(regGYP,"regGYP.xlsx")
reg19anv <- reg19$GYP$anova
View(reg19anv)
write_xlsx(reg19anv, "reg19anv.xlsx")
plot(reg19)

# PLOTS 

#1. Mean Yield vs Stability Deviation (s2di) — BEST PLOT

library(ggplot2)
library(dplyr)
library(ggrepel)

# Extract regression table
reg_df <- reg19$GYP$regression

# Mean lines
meanY  <- mean(reg_df$b0)
means2 <- mean(reg_df$s2di)

# Classification
reg_df <- reg_df %>%
  mutate(Group = case_when(
    b0 > meanY & s2di < means2 ~ "High Yield + Stable",
    b0 > meanY & s2di > means2 ~ "High Yield + Unstable",
    b0 < meanY & s2di < means2 ~ "Low Yield + Stable",
    b0 < meanY & s2di > means2 ~ "Low Yield + Unstable"
  ))

ggplot(reg_df,
       aes(x = b0,
           y = s2di,
           color = Group,
           label = GEN)) +
  
  geom_point(size = 4) +
  
  geom_text_repel(size = 3.5) +
  
  geom_vline(xintercept = meanY,
             linetype = "dashed") +
  
  geom_hline(yintercept = means2,
             linetype = "dashed") +
  
  labs(title = "Eberhart and Russell Stability Analysis",
       subtitle = "Mean Grain Yield vs Deviation from Regression",
       x = "Mean Grain Yield",
       y = expression(S^2[d])) +
  
  theme_classic(base_size = 13)

# 2. Mean Yield vs Regression Coefficient (b1)

meanb1 <- 1

ggplot(reg_df,
       aes(x = b0,
           y = b1,
           label = GEN)) +
  
  geom_point(size = 4,
             color = "blue") +
  
  geom_text_repel(size = 3.5) +
  
  geom_vline(xintercept = meanY,
             linetype = "dashed") +
  
  geom_hline(yintercept = meanb1,
             linetype = "dashed",
             color = "red") +
  
  labs(title = "Mean Grain Yield vs Regression Coefficient",
       subtitle = "b1 = 1 indicates wide adaptation",
       x = "Mean Grain Yield",
       y = expression(b[i])) +
  
  theme_bw(base_size = 13)

# Regression Coefficient (b1) vs Stability (s2di) ⭐ Publication Quality
ggplot(reg_df,
       aes(x = b1,
           y = s2di,
           label = GEN)) +
  
  geom_point(size = 4,
             color = "darkgreen") +
  
  geom_text_repel(size = 3.5) +
  
  geom_vline(xintercept = 1,
             linetype = "dashed",
             color = "red") +
  
  geom_hline(yintercept = means2,
             linetype = "dashed") +
  
  labs(title = "Regression Coefficient vs Stability Deviation",
       subtitle = "Ideal genotypes have b1 ≈ 1 and low s2di",
       x = expression(b[i]),
       y = expression(S^2[d])) +
  
  theme_classic(base_size = 13)

#4. RMSE vs R² Plot
ggplot(reg_df,
       aes(x = RMSE,
           y = R2,
           label = GEN)) +
  
  geom_point(size = 4,
             color = "purple") +
  
  geom_text_repel(size = 3.5) +
  
  labs(title = "Model Fit Statistics",
       x = "RMSE",
       y = expression(R^2)) +
  
  theme_bw(base_size = 13)

#5. Stability Ranking Plot
reg_df2 <- reg_df %>%
  arrange(s2di)

ggplot(reg_df2,
       aes(x = reorder(GEN, s2di),
           y = s2di)) +
  
  geom_col(fill = "steelblue") +
  
  coord_flip() +
  
  labs(title = "Deviation from Regression (s2di)",
       subtitle = "Lower values indicate higher stability",
       x = "Genotype",
       y = expression(S^2[d])) +
  
  theme_classic(base_size = 12)

############################ NON PARAMETRIC #######################################
#############  SUPERIORITY BY LINN AND BINN ###############
# DFF
super1 <- superiority(MET, ENVIRONMENT, GENOTYPE, DFF)
print(super1)
View(super1$DFF$index)
write_xlsx(super1$DFF$index, "super1$DFF$index.xlsx")
View(super1$DFF$environments)
write_xlsx(super1$DFF$environments, "super1$DFF$environments.xlsx")

library(ggplot2)
library(dplyr)
library(ggrepel)


# Extract data
sup_df <- super1[[1]]$index

# Mean lines
meanY  <- mean(sup_df$Y)
meanPi <- mean(sup_df$Pi_a)

# Classify quadrants for DFF
# Lower DFF = early
# Lower Pi = stable

sup_df <- sup_df %>%
  mutate(Group = case_when(
    Y < meanY  & Pi_a < meanPi ~ "Early + Stable",
    Y < meanY  & Pi_a > meanPi ~ "Early + Unstable",
    Y > meanY  & Pi_a < meanPi ~ "Late + Stable",
    Y > meanY  & Pi_a > meanPi ~ "Late + Unstable"
  ))

# Plot
ggplot(sup_df,
       aes(x = Y,
           y = Pi_a,
           color = Group,
           label = GEN)) +
  
  geom_point(size = 4) +
  
  geom_text_repel(vjust = -0.7,
            size = 3.5) +
  
  geom_vline(xintercept = meanY,
             linetype = "dashed",
             linewidth = 0.8) +
  
  geom_hline(yintercept = meanPi,
             linetype = "dashed",
             linewidth = 0.8) +
  
  annotate("text",
           x = min(sup_df$Y),
           y = min(sup_df$Pi_a),
           label = "Early + Stable",
           hjust = 0,
           size = 4,
           fontface = "bold") +
  
  annotate("text",
           x = min(sup_df$Y),
           y = max(sup_df$Pi_a),
           label = "Early + Unstable",
           hjust = 0,
           size = 4,
           fontface = "bold") +
  
  annotate("text",
           x = max(sup_df$Y),
           y = min(sup_df$Pi_a),
           label = "Late + Stable",
           hjust = 1,
           size = 4,
           fontface = "bold") +
  
  annotate("text",
           x = max(sup_df$Y),
           y = max(sup_df$Pi_a),
           label = "Late + Unstable",
           hjust = 1,
           size = 4,
           fontface = "bold") +
  
  labs(title = "DFF: Mean Performance vs Stability",
       subtitle = "Lower DFF and lower Pi indicate desirable genotypes",
       x = "Mean DFF",
       y = expression(P[i])) +
  
  theme_classic(base_size = 13)

#For DFF interpretation:
Early + Stable = ideal
Early + Unstable = early but inconsistent
Late + Stable = stable but late
Late + Unstable = undesirable

# PH
super2 <- superiority(MET, ENVIRONMENT, GENOTYPE, PH)
print(super2)
View(super2$PH$index)
write_xlsx(super2$PH$index, "super2$PH$index.xlsx")
View(super2$PH$environments)
write_xlsx(super2$PH$environments, "super2$PH$environments.xlsx")
# LL
super3 <- superiority(MET, ENVIRONMENT, GENOTYPE, LL)
print(super3)
View(super3$LL$index)
write_xlsx(super3$LL$index, "super3$LL$index.xlsx")
View(super3$LL$environments)
write_xlsx(super3$LL$environments, "super3$LL$environments.xlsx")
# LW
super4 <- superiority(MET, ENVIRONMENT, GENOTYPE, LW)
print(super4)
View(super4$LW$index)
write_xlsx(super4$LW$index, "super4$LW$index.xlsx")
View(super4$LW$environments)
write_xlsx(super4$LW$environments, "super4$LW$environments.xlsx")
# LMT
super5 <- superiority(MET, ENVIRONMENT, GENOTYPE, LMT)
print(super5)
View(super5$LMT$index)
write_xlsx(super5$LMT$index, "super5$LMT$index.xlsx")
View(super5$LMT$environments)
write_xlsx(super5$LMT$environments, "super5$LMT$environments.xlsx")
# TP
super6 <- superiority(MET, ENVIRONMENT, GENOTYPE, TP)
print(super6)
View(super6$TP$index)
write_xlsx(super6$TP$index, "super6$TP$index.xlsx")
View(super6$TP$environments)
write_xlsx(super6$TP$environments, "super6$TP$environments.xlsx")
# PTP
super7 <- superiority(MET, ENVIRONMENT, GENOTYPE, PTP)
print(super7)
View(super7$PTP$index)
write_xlsx(super7$PTP$index, "super7$PTP$index.xlsx")
View(super7$PTP$environments)
write_xlsx(super7$PTP$environments, "super7$PTP$environments.xlsx")
# EL
super8 <- superiority(MET, ENVIRONMENT, GENOTYPE, EL)
print(super8)
View(super8$EL$index)
write_xlsx(super8$EL$index, "super8$EL$index.xlsx")
View(super8$EL$environments)
write_xlsx(super8$EL$environments, "super8$EL$environments.xlsx")
# FL
super9 <- superiority(MET, ENVIRONMENT, GENOTYPE, FL)
print(super9)
View(super9$FL$index)
write_xlsx(super9$FL$index, "super9$FL$index.xlsx")
View(super9$FL$environments)
write_xlsx(super9$FL$environments, "super9$FL$environments.xlsx")
# NFE
super10 <- superiority(MET, ENVIRONMENT, GENOTYPE, NFE)
print(super10)
View(super10$NFE$index)
write_xlsx(super10$NFE$index, "super10$NFE$index.xlsx")
View(super10$NFE$environments)
write_xlsx(super10$NFE$environments, "super10$NFE$environments.xlsx")
# FW
super11 <- superiority(MET, ENVIRONMENT, GENOTYPE, FW)
print(super11)
View(super11$FW$index)
write_xlsx(super11$FW$index, "super11$FW$index.xlsx")
View(super11$FW$environments)
write_xlsx(super11$FW$environments, "super11$FW$environments.xlsx")
# DM
super12 <- superiority(MET, ENVIRONMENT, GENOTYPE, DM)
print(super12)
View(super12$DM$index)
write_xlsx(super12$DM$index, "super12$DM$index.xlsx")
View(super12$DM$environments)
write_xlsx(super12$DM$environments, "super12$DM$environments.xlsx")
# TSW
super13 <- superiority(MET, ENVIRONMENT, GENOTYPE, TSW)
print(super13)
View(super13$TSW$index)
write_xlsx(super13$TSW$index, "super13$TSW$index.xlsx")
View(super13$TSW$environments)
write_xlsx(super13$TSW$environments, "super13$TSW$environments.xlsx")
# IRON
super14 <- superiority(MET, ENVIRONMENT, GENOTYPE, FE)
print(super14)
View(super14$FE$index)
write_xlsx(super14$FE$index, "super14$FE$index.xlsx")
View(super14$FE$environments)
write_xlsx(super14$FE$environments, "super14$FE$environments.xlsx")
# ZINC
super15 <- superiority(MET, ENVIRONMENT, GENOTYPE, ZN)
print(super15)
View(super15$ZN$index)
write_xlsx(super15$ZN$index, "super15$ZN$index.xlsx")
View(super15$ZN$environments)
write_xlsx(super15$ZN$environments, "super15$ZN$environments.xlsx")
# MANGANESE
super16 <- superiority(MET, ENVIRONMENT, GENOTYPE, MN)
print(super16)
View(super16$MN$index)
write_xlsx(super16$MN$index, "super16$MN$index.xlsx")
View(super16$MN$environments)
write_xlsx(super16$MN$environments, "super16$MN$environments.xlsx")
# COPPER
super17 <- superiority(MET, ENVIRONMENT, GENOTYPE, CU)
print(super17)
View(super17$CU$index)
write_xlsx(super17$CU$index, "super17$CU$index.xlsx")
View(super17$CU$environments)
write_xlsx(super17$CU$environments, "super17$CU$environments.xlsx")
# CALCIUM
super18 <- superiority(MET, ENVIRONMENT, GENOTYPE, CA)
print(super18)
View(super18$CA$index)
write_xlsx(super18$CA$index, "super18$CA$index.xlsx")
View(super18$CA$environments)
write_xlsx(super18$CA$environments, "super18$CA$environments.xlsx")
# YIELD
super19 <- superiority(MET, ENVIRONMENT, GENOTYPE, GYP)
print(super19)
View(super19$GYP$index)
write_xlsx(super19$GYP$index, "super19$GYP$index.xlsx")
View(super19$GYP$environments)
write_xlsx(super19$GYP$environments, "super19$GYP$environments.xlsx")

###############################
# LINN & BINN PLOTS FOR GYP
###############################

library(ggplot2)
library(dplyr)
library(tidyr)
library(ggrepel)

# Extract data
sup_df <- super19$GYP$index

############################################################
# 1. Basic Bar Plot for Overall Superiority Index (Pi_a)
############################################################

sup_df1 <- sup_df %>%
  arrange(Pi_a)

ggplot(sup_df1,
       aes(x = reorder(GEN, Pi_a),
           y = Pi_a)) +
  
  geom_col(fill = "steelblue") +
  
  coord_flip() +
  
  labs(title = "Overall Superiority Index for Grain Yield",
       x = "Genotype",
       y = expression(P[i(a)])) +
  
  theme_bw()



############################################################
# 2. Combined Plot for Favorable and Unfavorable Environments
############################################################

sup_long <- sup_df %>%
  select(GEN, Pi_f, Pi_u) %>%
  pivot_longer(cols = c(Pi_f, Pi_u),
               names_to = "Environment",
               values_to = "Pi")

ggplot(sup_long,
       aes(x = reorder(GEN, Pi),
           y = Pi,
           fill = Environment)) +
  
  geom_col(position = "dodge") +
  
  coord_flip() +
  
  labs(title = "Superiority Index in Favorable and Unfavorable Environments",
       x = "Genotype",
       y = "Pi Value") +
  
  theme_bw()



############################################################
# 3. Mean GYP vs Stability (Most Informative)
############################################################

ggplot(sup_df,
       aes(x = Y,
           y = Pi_a,
           label = GEN)) +
  
  geom_point(size = 3,
             color = "blue") +
  
  geom_text_repel(size = 3.5) +
  
  labs(title = "Mean Grain Yield vs Superiority Index",
       subtitle = "Higher yield and lower Pi indicate desirable genotypes",
       x = "Mean Grain Yield",
       y = expression(P[i(a)])) +
  
  theme_bw()



############################################################
# 4. Rank Plot
############################################################

ggplot(sup_df,
       aes(x = reorder(GEN, R_a),
           y = R_a)) +
  
  geom_col(fill = "darkgreen") +
  
  coord_flip() +
  
  labs(title = "Ranking Based on Superiority Index",
       x = "Genotype",
       y = "Rank") +
  
  theme_bw()



############################################################
# 5. Publication-Quality Plot (Recommended)
############################################################

ggplot(sup_df,
       aes(x = reorder(GEN, Pi_a),
           y = Pi_a)) +
  
  geom_col(fill = "royalblue") +
  
  geom_text(aes(label = round(Pi_a, 1)),
            hjust = -0.1,
            size = 3) +
  
  coord_flip() +
  
  labs(title = "Linn and Binn Superiority Index for Grain Yield",
       subtitle = "Lower Pi indicates superior and stable genotype",
       x = "Genotype",
       y = expression(P[i])) +
  
  theme_classic(base_size = 12)



############################################################
# 6. AMMI/GGE-Style Quadrant Interpretation Plot
############################################################

# Mean lines
meanY  <- mean(sup_df$Y)
meanPi <- mean(sup_df$Pi_a)

# Group classification
sup_df2 <- sup_df %>%
  mutate(Group = case_when(
    Y > meanY & Pi_a < meanPi ~ "High Yield + Stable",
    Y > meanY & Pi_a > meanPi ~ "High Yield + Unstable",
    Y < meanY & Pi_a < meanPi ~ "Low Yield + Stable",
    Y < meanY & Pi_a > meanPi ~ "Low Yield + Unstable"
  ))

ggplot(sup_df2,
       aes(x = Y,
           y = Pi_a,
           color = Group,
           label = GEN)) +
  
  geom_point(size = 4) +
  
  geom_text_repel(size = 3.5) +
  
  geom_vline(xintercept = meanY,
             linetype = "dashed",
             linewidth = 0.8) +
  
  geom_hline(yintercept = meanPi,
             linetype = "dashed",
             linewidth = 0.8) +
  
  annotate("text",
           x = max(sup_df2$Y),
           y = min(sup_df2$Pi_a),
           label = "High Yield + Stable",
           hjust = 1,
           size = 4,
           fontface = "bold") +
  
  annotate("text",
           x = max(sup_df2$Y),
           y = max(sup_df2$Pi_a),
           label = "High Yield + Unstable",
           hjust = 1,
           size = 4,
           fontface = "bold") +
  
  annotate("text",
           x = min(sup_df2$Y),
           y = min(sup_df2$Pi_a),
           label = "Low Yield + Stable",
           hjust = 0,
           size = 4,
           fontface = "bold") +
  
  annotate("text",
           x = min(sup_df2$Y),
           y = max(sup_df2$Pi_a),
           label = "Low Yield + Unstable",
           hjust = 0,
           size = 4,
           fontface = "bold") +
  
  labs(title = "GYP: Mean Performance vs Stability",
       subtitle = "Higher yield and lower Pi indicate desirable genotypes",
       x = "Mean Grain Yield",
       y = expression(P[i])) +
  
  theme_classic(base_size = 13)


#Interpretation for GYP:

High Yield + Stable = ideal genotypes
High Yield + Unstable = productive but inconsistent
Low Yield + Stable = stable but poor yielding
Low Yield + Unstable = undesirable genotypes

############# FOX TOP THIRD CRITERIA #################
# DFF
fox1 <- Fox(MET, ENVIRONMENT, GENOTYPE, DFF)
View(fox1$DFF)
write_xlsx(fox1$DFF, "fox1$DFF.xlsx")
# PH
fox2 <- Fox(MET, ENVIRONMENT, GENOTYPE, PH)
View(fox2$PH)
write_xlsx(fox2$PH, "fox2$PH.xlsx")
# LL
fox3 <- Fox(MET, ENVIRONMENT, GENOTYPE, LL)
View(fox3$LL)
write_xlsx(fox3$LL, "fox3$LL.xlsx")
# LW
fox4 <- Fox(MET, ENVIRONMENT, GENOTYPE, LW)
View(fox4$LW)
write_xlsx(fox4$LW, "fox4$LW.xlsx")
# LMT
fox5 <- Fox(MET, ENVIRONMENT, GENOTYPE, LMT)
View(fox5$LMT)
write_xlsx(fox5$LMT, "fox4$LMT.xlsx")
# TP
fox6 <- Fox(MET, ENVIRONMENT, GENOTYPE, TP)
View(fox6$TP)
write_xlsx(fox6$TP, "fox6$TP.xlsx")
# PTP
fox7 <- Fox(MET, ENVIRONMENT, GENOTYPE, PTP)
View(fox7$PTP)
write_xlsx(fox7$PTP, "fox7$PTP.xlsx")
# EL
fox8 <- Fox(MET, ENVIRONMENT, GENOTYPE, EL)
View(fox8$EL)
write_xlsx(fox8$EL, "fox8$EL.xlsx")
# FL
fox9 <- Fox(MET, ENVIRONMENT, GENOTYPE, FL)
View(fox9$FL)
write_xlsx(fox9$FL, "fox9$FL.xlsx")
# NFE
fox10 <- Fox(MET, ENVIRONMENT, GENOTYPE, NFE)
View(fox10$NFE)
write_xlsx(fox10$NFE, "fox10$NFE.xlsx")
# FW
fox11 <- Fox(MET, ENVIRONMENT, GENOTYPE, FW)
View(fox11$FW)
write_xlsx(fox11$FW, "fox11$FW.xlsx")
# DM
fox12 <- Fox(MET, ENVIRONMENT, GENOTYPE, DM)
View(fox12$DM)
write_xlsx(fox12$DM, "fox12$DM.xlsx")
# TSW
fox13 <- Fox(MET, ENVIRONMENT, GENOTYPE, TSW)
View(fox13$TSW)
write_xlsx(fox13$TSW, "fox13$TSW.xlsx")
# IRON 
fox14 <- Fox(MET, ENVIRONMENT, GENOTYPE, FE)
View(fox14$FE)
write_xlsx(fox14$FE, "fox14$FE.xlsx")
# ZINC
fox15 <- Fox(MET, ENVIRONMENT, GENOTYPE, ZN)
View(fox15$ZN)
write_xlsx(fox15$ZN, "fox15$ZN.xlsx")
# MANGANESE
fox16 <- Fox(MET, ENVIRONMENT, GENOTYPE, MN)
View(fox16$MN)
write_xlsx(fox16$MN, "fox16$MN.xlsx")
# COPPER
fox17 <- Fox(MET, ENVIRONMENT, GENOTYPE, CU)
View(fox17$CU)
write_xlsx(fox17$CU, "fox17$CU.xlsx")
# CALCIUM
fox18 <- Fox(MET, ENVIRONMENT, GENOTYPE, CA)
View(fox18$CA)
write_xlsx(fox18$CA, "fox18$CA.xlsx")
# YIELD
fox19 <- Fox(MET, ENVIRONMENT, GENOTYPE, GYP)
View(fox19$GYP)
write_xlsx(fox19$GYP, "fox19$GYP.xlsx")

## plots gyp foxtop

########## fox top gyp

# 1 1. TOP GENOTYPE BARPLOT (BEST)

library(ggplot2)

fox_df <- fox19$GYP

ggplot(fox_df,
       aes(x = reorder(GEN, TOP),
           y = TOP)) +
  
  geom_col(fill = "steelblue",
           width = 0.7) +
  
  coord_flip() +
  
  geom_text(aes(label = TOP),
            hjust = -0.2,
            size = 5) +
  
  labs(title = "Fox TOP Third Criterion",
       x = "Genotype",
       y = "TOP Frequency") +
  
  theme_bw(base_size = 15)

####### 2 TOP vs MEAN YIELD SCATTER PLOT

#(Very useful)

library(ggrepel)

ggplot(fox_df,
       aes(x = TOP,
           y = Y,
           label = GEN)) +
  
  geom_point(size = 4,
             color = "darkred") +
  
  geom_text_repel(size = 4) +
  
  labs(title = "Mean Yield vs TOP Frequency",
       x = "TOP Frequency",
       y = "Mean Grain Yield") +
  
  theme_classic(base_size = 15)

############ Fox TOP Quadrant Selection

ggplot(fox_df,
       aes(x = TOP,
           y = Y,
           label = GEN)) +
  
  geom_point(size = 4,
             color = "forestgreen") +
  
  geom_text_repel(size = 4) +
  
  geom_vline(xintercept = mean(fox_df$TOP),
             linetype = "dashed") +
  
  geom_hline(yintercept = mean(fox_df$Y),
             linetype = "dashed") +
  
  labs(title = "Fox TOP Quadrant Selection",
       x = "TOP Frequency",
       y = "Mean Yield") +
  
  theme_bw(base_size = 15)
#Upper-right quadrant = superior genotypes.
########### bubble
ggplot(fox_df,
       aes(x = TOP,
           y = Y,
           size = Y,
           label = GEN)) +
  
  geom_point(alpha = 0.7,
             color = "purple") +
  
  geom_text_repel(size = 4) +
  
  labs(title = "Bubble Plot of TOP Performance",
       x = "TOP Frequency",
       y = "Mean Yield",
       size = "Yield") +
  
  theme_minimal(base_size = 15)
###### TOP GENOTYPES ONLY
topfox <- fox_df %>%
  arrange(desc(TOP), desc(Y)) %>%
  slice(1:10)

ggplot(topfox,
       aes(x = reorder(GEN, TOP),
           y = TOP,
           fill = Y)) +
  
  geom_col(width = 0.7) +
  
  coord_flip() +
  
  labs(title = "Top Genotypes by Fox Criterion",
       x = "Genotype",
       y = "TOP Frequency") +
  
  theme_bw(base_size = 15)
##genotypes with larger TOP values are more frequently among the best-performing group across environments, 
#these genotypes are considered broadly adapted and stable

############################## FACTOR BASED ###################################
# DFF
fact1 <- ge_factanal(MET, ENVIRONMENT, GENOTYPE, REPLICATION, DFF, mineval = 1, verbose = TRUE)
print(fact1)

fact1_2 <- ge_factanal(MET,
                        ENVIRONMENT,
                        GENOTYPE,
                        REPLICATION,
                        DFF,
                        mineval = 0.7)
plot(fact1_2)
# PH
fact2 <- ge_factanal(MET, ENVIRONMENT, GENOTYPE, REPLICATION, PH)
print(fact2)
# LL
fact3 <- ge_factanal(MET, ENVIRONMENT, GENOTYPE, REPLICATION, LL)
print(fact3)
# LW
fact4 <- ge_factanal(MET, ENVIRONMENT, GENOTYPE, REPLICATION, LW)
print(fact4)
# LMT
fact5 <- ge_factanal(MET, ENVIRONMENT, GENOTYPE, REPLICATION, LMT)
print(fact5)
# TP
fact6 <- ge_factanal(MET, ENVIRONMENT, GENOTYPE, REPLICATION, TP)
print(fact6)
# PTP
fact7 <- ge_factanal(MET, ENVIRONMENT, GENOTYPE, REPLICATION, PTP)
print(fact7)
# EL
fact8 <- ge_factanal(MET, ENVIRONMENT, GENOTYPE, REPLICATION, EL)
print(fact8)
# FL
fact9 <- ge_factanal(MET, ENVIRONMENT, GENOTYPE, REPLICATION, FL)
print(fact9)
# NFE
fact10 <- ge_factanal(MET, ENVIRONMENT, GENOTYPE, REPLICATION, NFE)
print(fact10)
# FW
fact11 <- ge_factanal(MET, ENVIRONMENT, GENOTYPE, REPLICATION, FW)
print(fact11)
# DM
fact12 <- ge_factanal(MET, ENVIRONMENT, GENOTYPE, REPLICATION, DM)
print(fact12)
# TSW
fact13 <- ge_factanal(MET, ENVIRONMENT, GENOTYPE, REPLICATION, TSW)
print(fact13)
# IRON
fact14 <- ge_factanal(MET, ENVIRONMENT, GENOTYPE, REPLICATION, FE)
print(fact14)
# ZINC
fact15 <- ge_factanal(MET, ENVIRONMENT, GENOTYPE, REPLICATION, ZN)
print(fact15)
# MANGANESE
fact16 <- ge_factanal(MET, ENVIRONMENT, GENOTYPE, REPLICATION, MN)
print(fact16)
plot(fact16)
View(fact16$MN$PCA)
write_xlsx(fact16$MN$PCA, "fact16$MN$PCA.xlsx")
# COPPER
fact17 <- ge_factanal(MET, ENVIRONMENT, GENOTYPE, REPLICATION, CU)
print(fact17)
# CALCIUM
fact18 <- ge_factanal(MET, ENVIRONMENT, GENOTYPE, REPLICATION, CA)
print(fact1)
# YIELD
fact19 <- ge_factanal(MET, ENVIRONMENT, GENOTYPE, REPLICATION, GYP)
print(fact19)

fact19_2 <- ge_factanal(MET,
                       ENVIRONMENT,
                       GENOTYPE,
                       REPLICATION,
                       GYP,
                       mineval = 0.7)
plot(fact19_2)

### plots gyp factor based

# =============================================================================
# CORRECTED FACTOR ANALYSIS PLOTTING FOR GYP
# Compatible with metan::ge_factanal() output structure
# =============================================================================

library(tidyverse)
library(ggpubr)
library(corrplot)

TRAIT <- "GYP"

# ============================================================================
# STEP 1: INSPECT THE ACTUAL STRUCTURE (run this first to verify)
# ============================================================================

# The metan output is a list of tibbles. Let's extract them properly:
str(fact19, max.level = 2)

# Extract each component by position/name from the printed output:
# Your print(fact19) showed these sections - we reconstruct them manually

# ============================================================================
# STEP 2: MANUALLY BUILD DATA FRAMES FROM YOUR PRINTED OUTPUT
# ============================================================================

# --- 2.1 Correlation Matrix ---
cor_mat <- data.frame(
  ENV = c("E1", "E2", "E3", "E4", "E5"),
  E1 = c(1.0000, 0.6516, 0.2690, 0.09538, 0.3554),
  E2 = c(0.6516, 1.0000, 0.3296, 0.5029, 0.6352),
  E3 = c(0.2690, 0.3296, 1.0000, 0.2193, 0.3770),
  E4 = c(0.09538, 0.5029, 0.2193, 1.0000, 0.4258),
  E5 = c(0.3554, 0.6352, 0.3770, 0.4258, 1.0000)
)

# --- 2.2 Eigenvalues ---
eigen_df <- data.frame(
  PCA = c("PC1", "PC2", "PC3", "PC4", "PC5"),
  Eigenvalues = c(2.605, 0.9244, 0.7885, 0.4849, 0.1969),
  Variance = c(52.11, 18.49, 15.77, 9.697, 3.938),
  Cumul_var = c(52.11, 70.59, 86.36, 96.06, 100.00)
)

# --- 2.3 Loadings (initial + rotated) ---
loadings_df <- data.frame(
  Env = c("E1", "E2", "E3", "E4", "E5"),
  FA1 = c(-0.6743, -0.8995, -0.5695, -0.6122, -0.8014),
  FA1_rotated = c(-0.6743, -0.8995, -0.5695, -0.6122, -0.8014),
  Communality = c(0.4547, 0.8092, 0.3244, 0.3748, 0.6422),
  Uniqueness = c(0.5453, 0.1908, 0.6756, 0.6252, 0.3578)
)

# --- 2.4 Environmental Stratification ---
env_strat <- data.frame(
  Env = c("E1", "E2", "E3", "E4", "E5"),
  Factor = c("FA1", "FA1", "FA1", "FA1", "FA1"),
  Mean = c(17.80, 21.25, 14.61, 16.41, 21.91),
  Min = c(12.3, 17.29, 10.49, 13.02, 16.16),
  Max = c(29.65, 27.07, 22.23, 24.12, 35.27),
  CV = c(16.13, 13.29, 20.20, 17.82, 21.57)
)

# ============================================================================
# STEP 3: CALCULATE ENVIRONMENT MEANS & PCA FROM YOUR RAW DATA
# ============================================================================

env_means <- MET %>%
  group_by(ENVIRONMENT, GENOTYPE) %>%
  summarise(mean_GYP = mean(GYP, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = ENVIRONMENT, values_from = mean_GYP) %>%
  column_to_rownames("GENOTYPE")

# PCA for biplot (2 dimensions)
pca_result <- prcomp(env_means, scale. = TRUE)
pca_scores <- as.data.frame(pca_result$x[, 1:2]) %>%
  rownames_to_column("GENOTYPE")

# Scale loadings for biplot arrows
pca_loadings <- as.data.frame(pca_result$rotation[, 1:2]) %>%
  rownames_to_column("Env") %>%
  mutate(PC1 = PC1 * 3, PC2 = PC2 * 3)

# ============================================================================
# PLOT 1: Correlation Matrix Heatmap
# ============================================================================
png(paste0("Rplot ", TRAIT, " FA correlation matrix.png"), 
    width = 800, height = 700, res = 120)

corr_mat_numeric <- as.matrix(cor_mat[, -1])
rownames(corr_mat_numeric) <- cor_mat$ENV

corrplot(corr_mat_numeric, 
         method = "color",
         type = "upper",
         addCoef.col = "black",
         tl.col = "black",
         tl.srt = 45,
         number.cex = 1.1,
         diag = FALSE,
         title = paste("Environment Correlation Matrix -", TRAIT),
         mar = c(0, 0, 2, 0),
         col = colorRampPalette(c("#E74C3C", "white", "#2ECC71"))(200))

dev.off()
cat("✓ Saved: Rplot", TRAIT, "FA correlation matrix.png\n")

# ============================================================================
# PLOT 2: Scree Plot
# ============================================================================
eigen_df$PCA <- factor(eigen_df$PCA, levels = eigen_df$PCA)

p_scree <- ggplot(eigen_df, aes(x = PCA, y = Eigenvalues)) +
  geom_col(fill = "steelblue", alpha = 0.85, width = 0.6) +
  geom_line(aes(group = 1), color = "darkred", linewidth = 1.2) +
  geom_point(color = "darkred", size = 4) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red", linewidth = 1) +
  geom_text(aes(label = round(Eigenvalues, 3)), vjust = -0.8, size = 3.5, fontface = "bold") +
  labs(
    title = paste("Scree Plot -", TRAIT),
    subtitle = "Red dashed line = Kaiser criterion (eigenvalue = 1)",
    x = "Principal Component",
    y = "Eigenvalue"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
    axis.text.x = element_text(face = "bold"),
    panel.grid.major.x = element_blank()
  )

ggsave(paste0("Rplot ", TRAIT, " FA scree plot.png"), p_scree, 
       width = 8, height = 6, dpi = 300)
cat("✓ Saved: Rplot", TRAIT, "FA scree plot.png\n")

# ============================================================================
# PLOT 3: Factor Loadings Bar Plot
# ============================================================================
p_loadings <- ggplot(loadings_df, aes(x = reorder(Env, -abs(FA1)), y = FA1)) +
  geom_col(aes(fill = FA1 > 0), alpha = 0.85, width = 0.6) +
  geom_text(aes(label = round(FA1, 3)), 
            vjust = ifelse(loadings_df$FA1 > 0, -0.5, 1.2), 
            size = 4, fontface = "bold") +
  geom_hline(yintercept = 0, linetype = "solid", color = "black", linewidth = 0.5) +
  scale_fill_manual(values = c("TRUE" = "#27AE60", "FALSE" = "#E74C3C"), guide = "none") +
  labs(
    title = paste("Factor Loadings -", TRAIT),
    subtitle = "Varimax rotation | Only 1 factor retained (eigenvalue > 1)",
    x = "Environment",
    y = "Factor 1 Loading"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold")
  )

ggsave(paste0("Rplot ", TRAIT, " FA loadings plot.png"), p_loadings, 
       width = 8, height = 6, dpi = 300)
cat("✓ Saved: Rplot", TRAIT, "FA loadings plot.png\n")

# ============================================================================
# PLOT 4: Environmental Stratification
# ============================================================================
p_strat <- ggplot(env_strat, aes(x = reorder(Env, Mean), y = Mean)) +
  geom_col(fill = "#3498DB", alpha = 0.9, width = 0.6) +
  geom_errorbar(aes(ymin = Min, ymax = Max), width = 0.2, linewidth = 1, color = "gray30") +
  geom_text(aes(label = paste0("CV=", round(CV, 1), "%")), 
            vjust = -0.8, size = 3.5, fontface = "bold", color = "darkblue") +
  geom_text(aes(label = paste0("Mean=", round(Mean, 1))), 
            vjust = 1.5, size = 3, color = "white", fontface = "bold") +
  labs(
    title = paste("Environmental Stratification -", TRAIT),
    subtitle = "Based on factor analysis | Error bars = Min to Max range",
    x = "Environment",
    y = paste("Mean", TRAIT)
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
    legend.position = "none"
  )

ggsave(paste0("Rplot ", TRAIT, " FA environmental stratification.png"), p_strat, 
       width = 9, height = 6, dpi = 300)
cat("✓ Saved: Rplot", TRAIT, "FA environmental stratification.png\n")

# ============================================================================
# PLOT 5: Biplot (PCA-based since only 1 FA factor)
# ============================================================================
p_biplot <- ggplot() +
  geom_point(data = pca_scores, aes(x = PC1, y = PC2), 
             color = "steelblue", alpha = 0.6, size = 2.5) +
  geom_text(data = pca_scores, aes(x = PC1, y = PC2, label = GENOTYPE),
            color = "steelblue", vjust = 1.8, size = 2.8, alpha = 0.8) +
  geom_segment(data = pca_loadings, 
               aes(x = 0, y = 0, xend = PC1, yend = PC2),
               arrow = arrow(length = unit(0.3, "cm")), 
               color = "darkred", linewidth = 1.2, alpha = 0.8) +
  geom_text(data = pca_loadings, aes(x = PC1, y = PC2, label = Env),
            color = "darkred", fontface = "bold", size = 4.5, vjust = -0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.4) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.4) +
  labs(
    title = paste("GEI Biplot -", TRAIT),
    subtitle = "Genotypes (blue) and Environment loadings (red arrows) | PCA-based (1 FA factor only)",
    x = paste0("PC1 (", eigen_df$Variance[1], "%)"),
    y = paste0("PC2 (", eigen_df$Variance[2], "%)")
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40", size = 11)
  )

ggsave(paste0("Rplot ", TRAIT, " FA biplot.png"), p_biplot, 
       width = 10, height = 8, dpi = 300)
cat("✓ Saved: Rplot", TRAIT, "FA biplot.png\n")

# ============================================================================
# PLOT 6: Genotype x Environment Heatmap
# ============================================================================
heatmap_data <- env_means %>%
  as.data.frame() %>%
  rownames_to_column("GENOTYPE") %>%
  pivot_longer(-GENOTYPE, names_to = "Environment", values_to = "Yield")

p_heatmap <- ggplot(heatmap_data, 
                    aes(x = Environment, y = reorder(GENOTYPE, Yield), fill = Yield)) +
  geom_tile(color = "white", linewidth = 0.15) +
  scale_fill_gradient(
    low = "#FEF9E7", 
    high = "#1A5276",
    name = paste(TRAIT, "Value")
  ) +
  labs(
    title = paste("Genotype x Environment Heatmap -", TRAIT),
    subtitle = "Darker colors indicate higher performance",
    x = "Environment",
    y = "Genotype"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
    axis.text.y = element_text(size = 7),
    axis.text.x = element_text(angle = 0, face = "bold")
  )

ggsave(paste0("Rplot ", TRAIT, " FA environment heatmap.png"), p_heatmap, 
       width = 8, height = 12, dpi = 300)
cat("✓ Saved: Rplot", TRAIT, "FA environment heatmap.png\n")

# ============================================================================
# PLOT 7: Communality & Uniqueness
# ============================================================================
comm_data <- loadings_df %>%
  select(Env, Communality, Uniqueness) %>%
  pivot_longer(cols = c(Communality, Uniqueness), 
               names_to = "Metric", values_to = "Value")

p_comm <- ggplot(comm_data, aes(x = reorder(Env, Value), y = Value, fill = Metric)) +
  geom_col(position = "stack", alpha = 0.9, width = 0.6) +
  geom_text(aes(label = round(Value, 3)), 
            position = position_stack(vjust = 0.5), 
            size = 3.5, color = "white", fontface = "bold") +
  scale_fill_manual(values = c("Communality" = "#27AE60", "Uniqueness" = "#E74C3C")) +
  labs(
    title = paste("Communality vs Uniqueness -", TRAIT),
    subtitle = "High communality = environment well-explained by the factor",
    x = "Environment",
    y = "Proportion",
    fill = "Component"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
    legend.position = "top"
  )

ggsave(paste0("Rplot ", TRAIT, " FA communality plot.png"), p_comm, 
       width = 8, height = 6, dpi = 300)
cat("✓ Saved: Rplot", TRAIT, "FA communality plot.png\n")

# ============================================================================
# PLOT 8: Variance Explained
# ============================================================================
eigen_df$Variance_num <- as.numeric(eigen_df$Variance)
eigen_df$Cumul_var_num <- as.numeric(eigen_df$Cumul_var)

p_var <- ggplot(eigen_df, aes(x = PCA, y = Variance_num)) +
  geom_col(aes(fill = Variance_num > 50), alpha = 0.85, width = 0.6) +
  geom_line(aes(group = 1, y = Cumul_var_num), color = "darkorange", linewidth = 1.2) +
  geom_point(aes(y = Cumul_var_num), color = "darkorange", size = 3) +
  geom_text(aes(label = paste0(Variance_num, "%")), vjust = -0.8, 
            size = 3.5, fontface = "bold") +
  geom_text(aes(y = Cumul_var_num, label = paste0(Cumul_var_num, "%")), 
            vjust = -0.8, color = "darkorange", size = 3, fontface = "bold") +
  scale_fill_manual(values = c("TRUE" = "#27AE60", "FALSE" = "steelblue"), guide = "none") +
  labs(
    title = paste("Variance Explained -", TRAIT),
    subtitle = "Bars = individual PC variance | Orange line = cumulative variance",
    x = "Principal Component",
    y = "Variance Explained (%)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40")
  )

ggsave(paste0("Rplot ", TRAIT, " FA variance explained.png"), p_var, 
       width = 9, height = 6, dpi = 300)
cat("✓ Saved: Rplot", TRAIT, "FA variance explained.png\n")

# ============================================================================
# SUMMARY TABLE
# ============================================================================
cat("\n========================================\n")
cat("SUMMARY TABLE FOR", TRAIT, "\n")
cat("========================================\n")

summary_table <- env_strat %>%
  left_join(loadings_df, by = "Env") %>%
  select(Env, Mean, Min, Max, CV, FA1, Communality, Uniqueness) %>%
  arrange(desc(Communality))

print(summary_table, row.names = FALSE)

cat("\n========================================\n")
cat("ALL PLOTS GENERATED SUCCESSFULLY!\n")
cat("========================================\n")


# Or force 2 factors to get scores:
fact19_fix <- ge_factanal(MET, ENVIRONMENT, GENOTYPE, REPLICATION, GYP, mineval = 0.7)
plot(fact19_fix)

################ wrap stab parameters ####################
# 1. DFF
stabp1 <- ge_stats(MET, ENVIRONMENT, GENOTYPE, REPLICATION, DFF)
stabp1
stabDFF <- stabp1$DFF
stabDFF
View(stabDFF)
write_xlsx(stabDFF,"stabDFF.xlsx")
data1 <- get_model_data(stabp1)
View(data1)
write_xlsx(data1, "data1.xlsx")
View(stabp1$DFF)
write_xlsx(stabp1$DFF, "stabp1$DFF.xlsx")

ranksp1 <- get_model_data(stabp1, "ranks")
View(ranksp1)
write_xlsx(ranksp1, "ranksp1.xlsx")
# 2. PH
stabp2 <- ge_stats(MET, ENVIRONMENT, GENOTYPE, REPLICATION, PH)
data2 <- get_model_data(stabp2)
View(data2)
write_xlsx(data2, "data2.xlsx")
View(stabp2$PH)
write_xlsx(stabp2$PH, "stabp2$PH.xlsx")

rankssp2 <- get_model_data(stabp2, "ranks")
View(rankssp2)
write_xlsx(rankssp2, "rankssp2.xlsx")
# 3. LL
stabp3 <- ge_stats(MET, ENVIRONMENT, GENOTYPE, REPLICATION, LL)
data3 <- get_model_data(stabp3)
View(data3)
write_xlsx(data3, "data3.xlsx")
View(stabp3$LL)
write_xlsx(stabp3$LL, "stabp3$LL.xlsx")

rankssp3 <- get_model_data(stabp3, "ranks")
View(rankssp3)
write_xlsx(rankssp3, "rankssp3.xlsx")
# 4. LW
stabpLW <- ge_stats(MET, ENVIRONMENT, GENOTYPE, BLOCK, LW)
data4 <- get_model_data(stabpLW)
View(data4)
write_xlsx(data4, "data4.xlsx")
View(stabpLW$LW)
write_xlsx(stabpLW$LW, "stabpLW$LW.xlsx")

rankssp4 <- get_model_data(stabpLW, "ranks")
View(rankssp4)
write_xlsx(rankssp4, "rankssp4.xlsx")
# 5. LMT
stabp5 <- ge_stats(MET, ENVIRONMENT, GENOTYPE, REPLICATION, LMT)
data5 <- get_model_data(stabp5)
View(data5)
write_xlsx(data5, "data5.xlsx")
View(stabp5$LMT)
write_xlsx(stabp5$LMT, "stabp5$LMT.xlsx")

rankssp5 <- get_model_data(stabp5, "ranks")
View(rankssp5)
write_xlsx(rankssp5, "rankssp5.xlsx")
# 6. TP
stabp6 <- ge_stats(MET, ENVIRONMENT, GENOTYPE, BLOCK, TP)
data6 <- get_model_data(stabp6)
View(data6)
write_xlsx(data6, "data6.xlsx")
View(stabp6$TP)
write_xlsx(stabp6$TP, "stabp6$TP.xlsx")

rankssp6 <- get_model_data(stabp6, "ranks")
View(rankssp6)
write_xlsx(rankssp6, "rankssp6.xlsx")
# 7. PTP
stabp7 <- ge_stats(MET, ENVIRONMENT, GENOTYPE, BLOCK, PTP)
data7 <- get_model_data(stabp7)
View(data7)
write_xlsx(data7, "data7.xlsx")
View(stabp7$PTP)
write_xlsx(stabp7$PTP, "stabp7$PTP.xlsx")

rankssp7 <- get_model_data(stabp7, "ranks")
View(rankssp7)
write_xlsx(rankssp7, "rankssp7.xlsx")
# 8. EL
stabp8 <- ge_stats(MET, ENVIRONMENT, GENOTYPE, BLOCK, EL)
data8 <- get_model_data(stabp8)
View(data8)
write_xlsx(data8, "data8.xlsx")
View(stabp8$EL)
write_xlsx(stabp8$EL, "stabp8$EL.xlsx")

rankssp8 <- get_model_data(stabp8, "ranks")
View(rankssp8)
write_xlsx(rankssp8, "rankssp8.xlsx")
# 9. FL
stabp9 <- ge_stats(MET, ENVIRONMENT, GENOTYPE, REPLICATION, FL)
data9 <- get_model_data(stabp9)
View(data9)
write_xlsx(data9, "data9.xlsx")
View(stabp9$FL)
write_xlsx(stabp9$FL, "stabp9$FL.xlsx")

rankssp9 <- get_model_data(stabp9, "ranks")
View(rankssp9)
write_xlsx(rankssp9, "rankssp9.xlsx")
# 10. NFE
stabp10 <- ge_stats(MET, ENVIRONMENT, GENOTYPE, REPLICATION, NFE)
data10 <- get_model_data(stabp10)
View(data10)
write_xlsx(data10, "data10.xlsx")
View(stabp10$NFE)
write_xlsx(stabp10$NFE, "stabp10$NFE.xlsx")

rankssp10 <- get_model_data(stabp10, "ranks")
View(rankssp10)
write_xlsx(rankssp10, "rankssp10.xlsx")
# 11. FW
stabp11 <- ge_stats(MET, ENVIRONMENT, GENOTYPE, REPLICATION, FW)
data11 <- get_model_data(stabp11)
View(data11)
write_xlsx(data11, "data11.xlsx")
View(stabp11$FW)
write_xlsx(stabp11$FW, "stabp11$FW.xlsx")

rankssp11 <- get_model_data(stabp11, "ranks")
View(rankssp11)
write_xlsx(rankssp11, "rankssp11.xlsx")
# 12. DM
stabp12 <- ge_stats(MET, ENVIRONMENT, GENOTYPE, REPLICATION, DM)
data12 <- get_model_data(stabp12)
View(data12)
write_xlsx(data12, "data12.xlsx")
View(stabp12$DM)
write_xlsx(stabp12$DM, "stabp12$DM.xlsx")

rankssp12 <- get_model_data(stabp12, "ranks")
View(rankssp12)
write_xlsx(rankssp12, "rankssp12.xlsx")
# 13. TSW
stabp13 <- ge_stats(MET, ENVIRONMENT, GENOTYPE, REPLICATION, TSW)
data13 <- get_model_data(stabp13)
View(data13)
write_xlsx(data13, "data13.xlsx")
View(stabp13$TSW)
write_xlsx(stabp13$TSW, "stabp13$TSW.xlsx")

rankssp13 <- get_model_data(stabp13, "ranks")
View(rankssp13)
write_xlsx(rankssp13, "rankssp13.xlsx")
# 14. FE
stabp14 <- ge_stats(MET, ENVIRONMENT, GENOTYPE, REPLICATION, FE)
data14 <- get_model_data(stabp14)
View(data14)
write_xlsx(data14, "data14.xlsx")
View(stabp14$FE)
write_xlsx(stabp14$FE, "stabp14$FE.xlsx")

rankssp14 <- get_model_data(stabp14, "ranks")
View(rankssp14)
write_xlsx(rankssp14, "rankssp14.xlsx")
# 15. ZN
stabp15 <- ge_stats(MET, ENVIRONMENT, GENOTYPE, REPLICATION, ZN)
data15 <- get_model_data(stabp15)
View(data15)
write_xlsx(data15, "data15.xlsx")
View(stabp15$ZN)
write_xlsx(stabp15$ZN, "stabp15$ZN.xlsx")

rankssp15 <- get_model_data(stabp15, "ranks")
View(rankssp15)
write_xlsx(rankssp15, "rankssp15.xlsx")
# 16. MN
stabp16 <- ge_stats(MET, ENVIRONMENT, GENOTYPE, REPLICATION, MN)
data16 <- get_model_data(stabp16)
View(data16)
write_xlsx(data16, "data16.xlsx")
View(stabp16$MN)
write_xlsx(stabp16$MN, "stabp16$MN.xlsx")

rankssp16 <- get_model_data(stabp16, "ranks")
View(rankssp16)
write_xlsx(rankssp16, "rankssp16.xlsx")
# 17. CU
stabp17 <- ge_stats(MET, ENVIRONMENT, GENOTYPE, REPLICATION, CU)
data17 <- get_model_data(stabp17)
View(data17)
write_xlsx(data17, "data17.xlsx")
View(stabp17$CU)
write_xlsx(stabp17$CU, "stabp17$CU.xlsx")

rankssp17 <- get_model_data(stabp17, "ranks")
View(rankssp17)
write_xlsx(rankssp17, "rankssp17.xlsx")
# 18. CA
stabp18 <- ge_stats(MET, ENVIRONMENT, GENOTYPE, BLOCK, CA)
data18 <- get_model_data(stabp18)
View(data18)
write_xlsx(data18, "data18.xlsx")
View(stabp18$CA)
write_xlsx(stabp18$CA, "stabp18$CA.xlsx")

rankssp18 <- get_model_data(stabp18, "ranks")
View(rankssp18)
write_xlsx(rankssp18, "rankssp18.xlsx")
# 19. GYP
stabp19 <- ge_stats(MET, ENVIRONMENT, GENOTYPE, REPLICATION, GYP)
data19 <- get_model_data(stabp19)
View(data19)
write_xlsx(data19, "data19.xlsx")
View(stabp19$GYP)
write_xlsx(stabp19$GYP, "stabp19$GYP.xlsx")

rankssp19 <- get_model_data(stabp19, "ranks")
View(rankssp19)
write_xlsx(rankssp19, "rankssp19.xlsx")

#PLOTS FOR WRAP-GYP

### HEATMAP OF RANKS (BEST PUBLICATION PLOT)
library(ggplot2)
library(tidyr)
library(dplyr)

GEN,Y_R,CV_R,ACV_R,POLAR_R,Var_R,Shukla_R,Wi_g_R,Wi_f_R,Wi_u_R,Ecoval_R,Sij_R,R2_R,ASTAB_R,	
ASI_R,ASV_R,AVAMGE_R,DA_R,DZ_R,EV_R,FA_R,MASI_R,MASV_R,SIPC_R,ZA_R,WAAS_R,WAASB_R,HMGV_R,	
RPGV_R,HMRPGV_R,Pi_a_R,Pi_f_R,Pi_u_R,Gai_R,S1_R,S2_R,S3_R,S6_R,N1_R,N2_R,N3_R,N4_R



rank_long <- rankssp19 %>%
  
  select(GEN,Y_R,CV_R,ACV_R,POLAR_R,Var_R,Shukla_R,Wi_g_R,Wi_f_R,Wi_u_R,Ecoval_R,Sij_R,R2_R,ASTAB_R,	
         ASI_R,ASV_R,AVAMGE_R,DA_R,DZ_R,EV_R,FA_R,MASI_R,MASV_R,SIPC_R,ZA_R,WAAS_R,WAASB_R,HMGV_R,	
         RPGV_R,HMRPGV_R,Pi_a_R,Pi_f_R,Pi_u_R,Gai_R,S1_R,S2_R,S3_R,S6_R,N1_R,N2_R,N3_R,N4_R) %>%
  
  pivot_longer(cols = -GEN,
               names_to = "Method",
               values_to = "Rank")

ggplot(rank_long,
       aes(x = Method,
           y = reorder(GEN, Rank),
           fill = Rank)) +
  
  geom_tile(color = "white") +
  
  geom_text(aes(label = Rank),
            size = 3.5) +
  
  scale_fill_gradient(low = "darkgreen",
                      high = "red") +
  
  labs(title = "HEATMAP OF STABILITY RANKS",
       x = "STABILITY PARAMETER",
       y = "GENOTYPE") +
  
  theme_bw(base_size = 14) +
  
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1))


########## MEAN RANK PLOT

#(Very important)

rank_mean <- rankssp19 %>%
  
  mutate(MeanRank =
           rowMeans(select(.,
                           ends_with("_R")),
                    na.rm = TRUE))

ggplot(rank_mean,
       aes(x = reorder(GEN, MeanRank),
           y = MeanRank)) +
  
  geom_col(fill = "steelblue",
           width = 0.7) +
  
  coord_flip() +
  
  geom_text(aes(label = round(MeanRank,1)),
            hjust = -0.2,
            size = 4) +
  
  labs(title = "Overall Mean Rank of Genotypes",
       x = "GENOTYPE",
       y = "MEAN RANK") +
  
  theme_bw(base_size = 15)
#Lower mean rank = better genotype.

########## YIELD vs STABILITY PLOT


rank_mean <- rank_mean %>%
  
  mutate(Stability = WAASB_R)

ggplot(rank_mean,
       aes(x = Stability,
           y = Y_R,
           label = GEN)) +
  
  geom_point(size = 4,
             color = "darkred") +
  
  geom_text(check_overlap = TRUE,
            vjust = -1,
            size = 4) +
  
  labs(title = "Yield Rank vs Stability Rank",
       x = "WAASB RANK",
       y = "YIELD RANK") +
  
  theme_classic(base_size = 15)
#Ideal genotypes:low yield rank, low stability rank

######## TOP 10 STABLE GENOTYPES

toprank <- rank_mean %>%
  
  arrange(MeanRank) %>%
  
  slice(1:10)

ggplot(toprank,
       aes(x = reorder(GEN, MeanRank),
           y = MeanRank,
           fill = MeanRank)) +
  
  geom_col(width = 0.7) +
  
  coord_flip() +
  
  labs(title = "Top Stable Genotypes",
       x = "GENOTYPE",
       y = "MEAN RANK") +
  
  theme_bw(base_size = 15)

######### RADAR CHART

#(Excellent visual summary)

library(fmsb)

radar_data <- rankssp19 %>%
  
  select(GEN,Y_R,CV_R,ACV_R,POLAR_R,Var_R,Shukla_R,Wi_g_R,Wi_f_R,Wi_u_R,Ecoval_R,Sij_R,R2_R,ASTAB_R,	
         ASI_R,ASV_R,AVAMGE_R,DA_R,DZ_R,EV_R,FA_R,MASI_R,MASV_R,SIPC_R,ZA_R,WAAS_R,WAASB_R,HMGV_R,	
         RPGV_R,HMRPGV_R,Pi_a_R,Pi_f_R,Pi_u_R,Gai_R,S1_R,S2_R,S3_R,S6_R,N1_R,N2_R,N3_R,N4_R)

top5 <- radar_data %>%
  
  slice(1:5)

rad <- as.data.frame(top5[,-1])

rownames(rad) <- top5$GEN

rad <- rbind(rep(max(rad)+2, ncol(rad)),
             rep(0, ncol(rad)),
             rad)

radarchart(rad)

#### CLUSTERED HEATMAP

library(pheatmap)

heat_data <- rankssp19 %>%
  
  select(GEN,Y_R,CV_R,ACV_R,POLAR_R,Var_R,Shukla_R,Wi_g_R,Wi_f_R,Wi_u_R,Ecoval_R,Sij_R,R2_R,ASTAB_R,	
         ASI_R,ASV_R,AVAMGE_R,DA_R,DZ_R,EV_R,FA_R,MASI_R,MASV_R,SIPC_R,ZA_R,WAAS_R,WAASB_R,HMGV_R,	
         RPGV_R,HMRPGV_R,Pi_a_R,Pi_f_R,Pi_u_R,Gai_R,S1_R,S2_R,S3_R,S6_R,N1_R,N2_R,N3_R,N4_R)

heat_mat <- as.matrix(heat_data[,-1])

rownames(heat_mat) <- heat_data$GEN

pheatmap(heat_mat,
         cluster_rows = TRUE,
         cluster_cols = TRUE,
         display_numbers = TRUE)

###### CONSENSUS RANK PLOT

rank_mean <- rank_mean %>%
  
  arrange(MeanRank)

ggplot(rank_mean,
       aes(x = reorder(GEN, MeanRank),
           y = MeanRank,
           group = 1)) +
  
  geom_line(color = "blue",
            linewidth = 1) +
  
  geom_point(size = 4,
             color = "red") +
  
  coord_flip() +
  
  labs(title = "CONSENSUS STABILITY RANKING",
       x = "GENOTYPE",
       y = "AVERAGE RANK") +
  
  theme_bw(base_size = 15)

# In this analysis, the average rank of each genotype was computed by 
#combining ranks obtained from multiple stability statistics such as 
#WAASB, ASV, Shukla’s variance, 
#ecovalence, superiority indices, adaptability measures, and yield rank.

### In the plot, genotypes positioned at the lower end of the 
#average rank scale are regarded as highly desirable because they 
#combine high yield potential with stable performance across environments. 
#These genotypes are considered broadly adapted and suitable for recommendation 
#in multi-environment cultivation programs.

#################### CORRELATION BTW STABILITY PARAMETERS ######################
# 1.DFF
csi1 <- corr_stab_ind(stabp1)
print(csi1)
write_xlsx(csi1,"csi1.xlsx")
dffcorr1 <- csi1$corr
View(dffcorr1)
write_xlsx(csi1$corr,"csi1$corr.xlsx")
dffcorr2 <- csi1$pval
View(dffcorr2)
write_xlsx(dffcorr2,"dffcorr2.xlsx")
# 2.PH
csi2 <- corr_stab_ind(stabp2)
csi2
View(csi2$corr)
View(csi2$pval)
# 3.LL
csi3 <- corr_stab_ind(stabp3)
csi3
View(csi3$corr)
View(csi3$pval)
# 4.LW
csi4 <- corr_stab_ind(stabpLW)
csi4
View(csi4$corr)
View(csi4$pva1)
# 5. LMT
csi5 <- corr_stab_ind(stabp5)
csi5
View(csi5$corr)
View(csi5$pva1)
# 6. TP
csi6 <- corr_stab_ind(stabp6)
csi6
View(csi6$corr)
View(csi6$pva1)
# 7. PTP
csi7 <- corr_stab_ind(stabp7)
csi7
View(csi7$corr)
View(csi7$pva1)
# 8. EL
csi8 <- corr_stab_ind(stabp8)
csi8
View(csi8$corr)
View(csi8$pva1)
# 9. FL
csi9 <- corr_stab_ind(stabp9)
csi9
View(csi9$corr)
View(csi9$pva1)
# 10. NFE
csi10 <- corr_stab_ind(stabp10)
csi10
View(csi10$corr)
View(csi10$pva1)
# 11. FW
csi11 <- corr_stab_ind(stabp11)
csi11
View(csi11$corr)
View(csi11$pva1)
# 12. DM
csi12 <- corr_stab_ind(stabp12)
csi12
View(csi12$corr)
View(csi12$pva1)
# 13. TSW
csi13 <- corr_stab_ind(stabp13)
csi13
View(csi13$corr)
View(csi13$pva1)
# 14. IRON
csi14 <-corr_stab_ind(stabp14)
csi14
View(csi14$corr)
View(csi14$pva1)
# 15. ZINC
csi15 <- corr_stab_ind(stabp15)
csi15
View(csi15$corr)
View(csi15$pva1)
# 16. MANGANESE
csi16 <-corr_stab_ind(stabp16)
csi16
View(csi16$corr)
View(csi16$pva1)
# 17. COPPER
csi17 <-corr_stab_ind(stabp16)
csi17
View(csi17$corr)
View(csi17$pva1)
# 18. CALCIUM
csi18 <- corr_stab_ind(stabp17)
csi18
View(csi18$corr)
View(csi18$pva1)
# 19. YIELD
csi19 <-corr_stab_ind(stabp19)
csi19
View(csi19$corr)
View(csi19$pva1)
#######################STABILITY ANALYSIS AMMI###########################################
library(metan)
attach(MET)
show(MET)
tail(MET)
AMMI_model1 <- performs_ammi(MET, ENVIRONMENT, GENOTYPE, REPLICATION)
AMMI_model2 <- waas(MET, ENVIRONMENT, GENOTYPE, REPLICATION, verbose = FALSE)
options(tibble.print_max=1000)
print(AMMI_model1, digits = 2)
print(AMMI_model2, digits = 2)

####### DFF
amod1 <- performs_ammi(MET, ENVIRONMENT, GENOTYPE, REPLICATION, DFF, BLOCK)
options(tibble.print_max=38)
print(amod1)
plot(amod1)
amod1f <- waas(MET, ENVIRONMENT, GENOTYPE, REPLICATION, DFF, BLOCK, verbose = FALSE)
options(tibble.print_max=38)
print(amod1f)
plot(amod1f)
View(amod1$DFF$ANOVA)
View(amod1f$DFF$model)
View(amod1f$DFF$Details)
View(amod1f$DFF$MeansGxE)
write_xlsx(amod1$DFF$ANOVA, "amod1$DFF$ANOVA.xlsx")
write_xlsx(amod1f$DFF$model, "amod1f$DFF$model.xlsx")
write_xlsx(amod1f$DFF$Details,"amod1f$DFF$Details.xlsx")
write_xlsx(amod1f$DFF$MeansGxE,"amod1f$DFF$MeansGxE.xlsx")
########## PH
amod2 <- performs_ammi(MET, ENVIRONMENT, GENOTYPE, REPLICATION, PH, BLOCK)
options(tibble.print_max=38)
print(amod2)
plot(amod2)
amod22 <- waas(MET, ENVIRONMENT, GENOTYPE, REPLICATION, PH, BLOCK, verbose = FALSE)
options(tibble.print_max=38)
print(amod22)
plot(amod22)
View(amod2$PH$ANOVA)
View(amod22$PH$model)
View(amod22$PH$Details)
View(amod22$PH$MeansGxE)
write_xlsx(amod2$PH$ANOVA, "amod2$PH$ANOVA.xlsx")
write_xlsx(amod22$PH$model, "amod22$PH$model.xlsx")
write_xlsx(amod22$PH$Details,"amod22$PH$Details.xlsx")
write_xlsx(amod22$PH$MeansGxE,"amod22$PH$MeansGxE.xlsx")
########## LL
amod3 <- performs_ammi(MET, ENVIRONMENT, GENOTYPE, REPLICATION, LL, BLOCK)
options(tibble.print_max=38)
print(amod3)
plot(amod3)
amod33 <- waas(MET, ENVIRONMENT, GENOTYPE, REPLICATION, LL, BLOCK, verbose = FALSE)
options(tibble.print_max=38)
print(amod33)
plot(amod33)
View(amod3$LL$ANOVA)
View(amod33$LL$model)
View(amod33$LL$Details)
View(amod33$LL$MeansGxE)
write_xlsx(amod3$LL$ANOVA, "amod3$LL$ANOVA.xlsx")
write_xlsx(amod33$LL$model, "amod33$LL$model.xlsx")
write_xlsx(amod33$LL$Details,"amod33$LL$Details.xlsx")
write_xlsx(amod33$LL$MeansGxE,"amod33$LL$MeansGxE.xlsx")
###### LW
amod4 <- performs_ammi(MET, ENVIRONMENT, GENOTYPE, REPLICATION, LW, BLOCK)
options(tibble.print_max=38)
print(amod4)
plot(amod4)
amod44 <- waas(MET, ENVIRONMENT, GENOTYPE, REPLICATION, LW, BLOCK, verbose = FALSE)
options(tibble.print_max=38)
print(amod44)
plot(amod44)
View(amod4$LW$ANOVA)
View(amod44$LW$model)
View(amod44$LW$Details)
View(amod44$LW$MeansGxE)
write_xlsx(amod4$LW$ANOVA, "amod4$LW$ANOVA.xlsx")
write_xlsx(amod44$LW$model, "amod44$LW$model.xlsx")
write_xlsx(amod44$LW$Details,"amod44$LW$Details.xlsx")
write_xlsx(amod44$LW$MeansGxE,"amod44$LW$MeansGxE.xlsx")
######### LMT
amod5 <- performs_ammi(MET, ENVIRONMENT, GENOTYPE, REPLICATION, LMT, BLOCK)
options(tibble.print_max=38)
print(amod5)
plot(amod5)
amod55 <- waas(MET, ENVIRONMENT, GENOTYPE, REPLICATION, LMT, BLOCK, verbose = FALSE)
options(tibble.print_max=38)
print(amod55)
plot(amod55)
View(amod5$LMT$ANOVA)
View(amod55$LMT$model)
View(amod55$LMT$Details)
View(amod55$LMT$MeansGxE)
write_xlsx(amod5$LMT$ANOVA, "amod5$LMT$ANOVA.xlsx")
write_xlsx(amod55$LMT$model, "amod55$LMT$model.xlsx")
write_xlsx(amod55$LMT$Details,"amod55$LMT$Details.xlsx")
write_xlsx(amod55$LMT$MeansGxE,"amod55$LMT$MeansGxE.xlsx")
######### TP
amod6 <- performs_ammi(MET, ENVIRONMENT, GENOTYPE, REPLICATION, TP, BLOCK)
options(tibble.print_max=38)
print(amod6)
plot(amod6)
amod66 <- waas(MET, ENVIRONMENT, GENOTYPE, REPLICATION, TP, BLOCK, verbose = FALSE)
options(tibble.print_max=38)
print(amod66)
plot(amod66)
View(amod6$TP$ANOVA)
View(amod66$TP$model)
View(amod66$TP$Details)
View(amod66$TP$MeansGxE)
write_xlsx(amod6$TP$ANOVA, "amod6$TP$ANOVA.xlsx")
write_xlsx(amod66$TP$model, "amod66$TP$model.xlsx")
write_xlsx(amod66$TP$Details,"amod66$TP$Details.xlsx")
write_xlsx(amod66$TP$MeansGxE,"amod66$TP$MeansGxE.xlsx")
########## PTP
amod7 <- performs_ammi(MET, ENVIRONMENT, GENOTYPE, REPLICATION, PTP, BLOCK)
options(tibble.print_max=38)
print(amod7)
plot(amod7)
amod77 <- waas(MET, ENVIRONMENT, GENOTYPE, REPLICATION, PTP, BLOCK, verbose = FALSE)
options(tibble.print_max=38)
print(amod77)
plot(amod77)
View(amod7$PTP$ANOVA)
View(amod77$PTP$model)
View(amod77$PTP$Details)
View(amod77$PTP$MeansGxE)
write_xlsx(amod7$PTP$ANOVA, "amod7$PTP$ANOVA.xlsx")
write_xlsx(amod77$PTP$model, "amod77$PTP$model.xlsx")
write_xlsx(amod77$PTP$Details,"amod77$PTP$Details.xlsx")
write_xlsx(amod77$PTP$MeansGxE,"amod77$PTP$MeansGxE.xlsx")
######### EL
amod8 <- performs_ammi(MET, ENVIRONMENT, GENOTYPE, REPLICATION, EL, BLOCK)
options(tibble.print_max=38)
print(amod8)
plot(amod8)
amod88 <- waas(MET, ENVIRONMENT, GENOTYPE, REPLICATION, EL, BLOCK, verbose = FALSE)
options(tibble.print_max=38)
print(amod88)
plot(amod88)
View(amod8$EL$ANOVA)
View(amod88$EL$model)
View(amod88$EL$Details)
View(amod88$EL$MeansGxE)
write_xlsx(amod8$EL$ANOVA, "amod8$EL$ANOVA.xlsx")
write_xlsx(amod88$EL$model, "amod88$EL$model.xlsx")
write_xlsx(amod88$EL$Details,"amod88$EL$Details.xlsx")
write_xlsx(amod88$EL$MeansGxE,"amod88$EL$MeansGxE.xlsx")
######## FL
amod9 <- performs_ammi(MET, ENVIRONMENT, GENOTYPE, REPLICATION, FL, BLOCK)
options(tibble.print_max=38)
print(amod9)
plot(amod9)
amod99 <- waas(MET, ENVIRONMENT, GENOTYPE, REPLICATION, FL, BLOCK, verbose = FALSE)
options(tibble.print_max=38)
print(amod99)
plot(amod99)
View(amod9$FL$ANOVA)
View(amod99$FL$model)
View(amod99$FL$Details)
View(amod99$FL$MeansGxE)
write_xlsx(amod9$FL$ANOVA, "amod9$FL$ANOVA.xlsx")
write_xlsx(amod99$FL$model, "amod99$FL$model.xlsx")
write_xlsx(amod99$FL$Details,"amod99$FL$Details.xlsx")
write_xlsx(amod99$FL$MeansGxE,"amod99$FL$MeansGxE.xlsx")
########## NFE
amod10 <- performs_ammi(MET, ENVIRONMENT, GENOTYPE, REPLICATION, NFE, BLOCK)
options(tibble.print_max=38)
print(amod10)
plot(amod10)
amod1010 <- waas(MET, ENVIRONMENT, GENOTYPE, REPLICATION, NFE, BLOCK, verbose = FALSE)
options(tibble.print_max=38)
print(amod1010)
plot(amod1010)
View(amod10$NFE$ANOVA)
View(amod1010$NFE$model)
View(amod1010$NFE$Details)
View(amod1010$NFE$MeansGxE)
write_xlsx(amod10$NFE$ANOVA, "amod10$NFE$ANOVA.xlsx")
write_xlsx(amod1010$NFE$model, "amod1010$NFE$model.xlsx")
write_xlsx(amod1010$NFE$Details,"amod1010$NFE$Details.xlsx")
write_xlsx(amod1010$NFE$MeansGxE,"amod1010$NFE$MeansGxE.xlsx")
####### FW
amod11 <- performs_ammi(MET, ENVIRONMENT, GENOTYPE, REPLICATION, FW, BLOCK)
options(tibble.print_max=38)
print(amod11)
plot(amod11)
amod1111 <- waas(MET, ENVIRONMENT, GENOTYPE, REPLICATION, FW, BLOCK, verbose = FALSE)
options(tibble.print_max=38)
print(amod1111)
plot(amod1111)
View(amod11$FW$ANOVA)
View(amod1111$FW$model)
View(amod1111$FW$Details)
View(amod1111$FW$MeansGxE)
write_xlsx(amod11$FW$ANOVA, "amod11$FW$ANOVA.xlsx")
write_xlsx(amod1111$FW$model, "amod1111$FW$model.xlsx")
write_xlsx(amod1111$FW$Details,"amod1111$FW$Details.xlsx")
write_xlsx(amod1111$FW$MeansGxE,"amod1111$FW$MeansGxE.xlsx")
######### DM
amod12 <- performs_ammi(MET, ENVIRONMENT, GENOTYPE, REPLICATION, DM, BLOCK)
options(tibble.print_max=38)
print(amod12)
plot(amod12)
amod1212 <- waas(MET, ENVIRONMENT, GENOTYPE, REPLICATION, DM, BLOCK, verbose = FALSE)
options(tibble.print_max=38)
print(amod1212)
plot(amod1212)
View(amod12$DM$ANOVA)
View(amod1212$DM$model)
View(amod1212$DM$Details)
View(amod1212$DM$MeansGxE)
write_xlsx(amod12$DM$ANOVA, "amod12$DM$ANOVA.xlsx")
write_xlsx(amod1212$DM$model, "amod1212$DM$model.xlsx")
write_xlsx(amod1212$DM$Details,"amod1212$DM$Details.xlsx")
write_xlsx(amod1212$DM$MeansGxE,"amod1212$DM$MeansGxE.xlsx")
##### TSW
amod13 <- performs_ammi(MET, ENVIRONMENT, GENOTYPE, REPLICATION, TSW, BLOCK)
options(tibble.print_max=38)
print(amod13)
plot(amod13)
amod1313 <- waas(MET, ENVIRONMENT, GENOTYPE, REPLICATION, TSW, BLOCK, verbose = FALSE)
options(tibble.print_max=38)
print(amod1313)
plot(amod1313)
View(amod13$TSW$ANOVA)
View(amod1313$TSW$model)
View(amod1313$TSW$Details)
View(amod1313$TSW$MeansGxE)
write_xlsx(amod13$TSW$ANOVA, "amod13$TSW$ANOVA.xlsx")
write_xlsx(amod1313$TSW$model, "amod1313$TSW$model.xlsx")
write_xlsx(amod1313$TSW$Details,"amod1313$TSW$Details.xlsx")
write_xlsx(amod1313$TSW$MeansGxE,"amod1313$TSW$MeansGxE.xlsx")
######### FE
amod14 <- performs_ammi(MET, ENVIRONMENT, GENOTYPE, REPLICATION, FE, BLOCK)
options(tibble.print_max=38)
print(amod14)
plot(amod14)
amod1414 <- waas(MET, ENVIRONMENT, GENOTYPE, REPLICATION, FE, BLOCK, verbose = FALSE)
options(tibble.print_max=38)
print(amod1414)
plot(amod1414)
View(amod14$FE$ANOVA)
View(amod1414$FE$model)
View(amod1414$FE$Details)
View(amod1414$FE$MeansGxE)
write_xlsx(amod14$FE$ANOVA, "amod14$FE$ANOVA.xlsx")
write_xlsx(amod1414$FE$model, "amod1414$FE$model.xlsx")
write_xlsx(amod1414$FE$Details,"amod1414$FE$Detail.xlsx")
write_xlsx(amod1414$FE$MeansGxE,"amod1414$FE$MeansGxE.xlsx")
########## ZN
amod15 <- performs_ammi(MET, ENVIRONMENT, GENOTYPE, REPLICATION, ZN, BLOCK)
options(tibble.print_max=38)
print(amod15)
plot(amod15)
amod1515 <- waas(MET, ENVIRONMENT, GENOTYPE, REPLICATION, ZN, BLOCK, verbose = FALSE)
options(tibble.print_max=38)
print(amod1515)
plot(amod1515)
View(amod15$ZN$ANOVA)
View(amod1515$ZN$model)
View(amod1515$ZN$Details)
View(amod1515$ZN$MeansGxE)
write_xlsx(amod15$ZN$ANOVA, "amod15$ZN$ANOVA.xlsx")
write_xlsx(amod1515$ZN$model, "amod1515$ZN$model.xlsx")
write_xlsx(amod1515$ZN$Details,"amod1515$ZN$Details.xlsx")
write_xlsx(amod1515$ZN$MeansGxE,"amod1515$ZN$MeansGxE.xlsx")
##### MN
amod16 <- performs_ammi(MET, ENVIRONMENT, GENOTYPE, REPLICATION, MN, BLOCK)
options(tibble.print_max=38)
print(amod16)
plot(amod16)
amod1616 <- waas(MET, ENVIRONMENT, GENOTYPE, REPLICATION, MN, BLOCK, verbose = FALSE)
options(tibble.print_max=38)
print(amod1616)
plot(amod1616)
View(amod16$MN$ANOVA)
View(amod1616$MN$model)
View(amod1616$MN$Details)
View(amod1616$MN$MeansGxE)
write_xlsx(amod16$MN$ANOVA, "amod16$MN$ANOVA.xlsx")
write_xlsx(amod1616$MN$model, "amod1616$MN$model.xlsx")
write_xlsx(amod1616$MN$Details,"amod1616$MN$Details.xlsx")
write_xlsx(amod1616$MN$MeansGxE,"amod1616$MN$MeansGxE.xlsx")
######### CU
amod17 <- performs_ammi(MET, ENVIRONMENT, GENOTYPE, REPLICATION, CU, BLOCK)
options(tibble.print_max=38)
print(amod17)
plot(amod17)
amod1717 <- waas(MET, ENVIRONMENT, GENOTYPE, REPLICATION, CU, BLOCK, verbose = FALSE)
options(tibble.print_max=38)
print(amod1717)
plot(amod1717)
View(amod17$CU$ANOVA)
View(amod1717$CU$model)
View(amod1717$CU$Details)
View(amod1717$CU$MeansGxE)
write_xlsx(amod17$CU$ANOVA, "amod17$CU$ANOVA.xlsx")
write_xlsx(amod1717$CU$model, "amod1717$CU$model.xlsx")
write_xlsx(amod1717$CU$Details,"amod1717$CU$Details.xlsx")
write_xlsx(amod1717$CU$MeansGxE,"amod1717$CU$MeansGxE.xlsx")
####### CA
amod18 <- performs_ammi(MET, ENVIRONMENT, GENOTYPE, REPLICATION, CA, BLOCK)
options(tibble.print_max=38)
print(amod18)
plot(amod18)
amod1818 <- waas(MET, ENVIRONMENT, GENOTYPE, REPLICATION, CA, BLOCK, verbose = FALSE)
options(tibble.print_max=38)
print(amod1818)
plot(amod1818)
View(amod18$CA$ANOVA)
View(amod1818$CA$model)
View(amod1818$CA$Details)
View(amod1818$CA$MeansGxE)
write_xlsx(amod18$CA$ANOVA, "amod18$CA$ANOVA.xlsx")
write_xlsx(amod1818$CA$model, "amod1818$CA$model.xlsx")
write_xlsx(amod1818$CA$Details,"amod1818$CA$Details.xlsx")
write_xlsx(amod1818$CA$MeansGxE,"amod1818$CA$MeansGxE.xlsx")
####### YIELD
amod19 <- performs_ammi(MET, ENVIRONMENT, GENOTYPE, REPLICATION, GYP, BLOCK)
options(tibble.print_max=1000)
print(amod19)
plot(amod19)
amod1919 <- waas(MET, ENVIRONMENT, GENOTYPE, REPLICATION, GYP, BLOCK, verbose = FALSE)
options(tibble.print_max=1000)
print(amod1919)
plot(amod1919)
View(amod19$GYP$ANOVA)
View(amod1919$GYP$model)
View(amod1919$GYP$Details)
View(amod1919$GYP$MeansGxE)
write_xlsx(amod19$GYP$ANOVA, "amod19$GYP$ANOVA.xlsx")
write_xlsx(amod1919$GYP$model, "amod1919$GYP$model.xlsx")
write_xlsx(amod1919$GYP$Details,"amod1919$GYP$Details.xlsx")
write_xlsx(amod1919$GYP$MeansGxE,"amod1919$GYP$MeansGxE.xlsx")
############### significance of ipca #############################
get_model_data(amod1, "ipca_pval")
get_model_data(amod2, "ipca_pval")
get_model_data(amod3, "ipca_pval")
get_model_data(amod4, "ipca_pval")
get_model_data(amod5, "ipca_pval")
get_model_data(amod6, "ipca_pval")
get_model_data(amod7, "ipca_pval")
get_model_data(amod8, "ipca_pval")
get_model_data(amod9, "ipca_pval")
get_model_data(amod10, "ipca_pval")
get_model_data(amod11, "ipca_pval")
get_model_data(amod12, "ipca_pval")
get_model_data(amod13, "ipca_pval")
get_model_data(amod14, "ipca_pval")
get_model_data(amod15, "ipca_pval")
get_model_data(amod16, "ipca_pval")
get_model_data(amod17, "ipca_pval")
get_model_data(amod18, "ipca_pval")
get_model_data(amod19, "ipca_pval")
######################## ammi biplots #######################
######### ammi biplot for days to 50% flowering  ###########
DF1 <- plot_scores(amod1)
plot(DF1)
DF1 <- plot_scores(amod1, x.lab = "DAYS TO 50% FLOWERING")
DF1

DFF2 <- plot_scores(amod1, type = 2)
plot(DFF2)
DFF2 <- plot_scores(amod1, type = 2, polygon = TRUE)
DFF2
DFF2 <- plot_scores(amod1,
                    type = 2,
                    col.env = "blue",
                    col.gen = transparent_color(),
                    col.segm.env = "orange",
                    highlight = c("G20" , "G35"),
                    col.highlight = "darkcyan",
                    axis.expand = 1.5)
DFF2

DFF3 <- plot_scores(amod1, type = 4)
DFF3
DFF3 <- plot_scores(amod1, type = 4, repulsion = 2)
DFF3
DFF3 <- plot_scores(amod1, type = 4,
                    size.tex.gen = 2,
                    x.lab = "PC1 of E1",
                    y.lab = "Nominal Height",
                    title = FALSE,
                    col.alpha.gen = 0)
DFF3
######### ammi biplot for plant height ###########
a <- plot_scores(amod2)
plot(a)
a <- plot_scores(amod2, x.lab = "PLANT HEIGHT")
a

E <- plot_scores(amod2, type = 2)
plot(E)
E <- plot_scores(amod2, type = 2, polygon = TRUE)
E
E <- plot_scores(amod2,
                 type = 2,
                 col.env = "blue",
                 col.gen = transparent_color(),
                 col.segm.env = "orange",
                 highlight = c("G1", "G2"),
                 col.highlight = "darkcyan",
                 axis.expand = 1.5)
E

c <- plot_scores(amod2, type = 4)
c
c <- plot_scores(amod2, type = 4, repulsion = 2)
c
c <- plot_scores(amod2, type = 4,
                 size.tex.gen = 2,
                 x.lab = "PC1 of E1",
                 y.lab = "Nominal Height",
                 title = FALSE,
                 col.alpha.gen = 0)
c
############### AMMI biplots for FLAG LEAF LENGTH  ########
LL <- plot_scores(amod3)
plot(LL)
LL <- plot_scores(amod3, x.lab = "FLAG LEAF LENGTH")
LL

LL2 <- plot_scores(amod3, type = 2)
plot(LL2)
LL2 <- plot_scores(amod3, type = 2, polygon = TRUE)
LL2
LL2 <- plot_scores(amod3,
                   type = 2,
                   col.env = "blue",
                   col.gen = transparent_color(),
                   col.segm.env = "orange",
                   highlight = c("G1", "G2"),
                   col.highlight = "darkcyan",
                   axis.expand = 1.5)
LL2

LL3 <- plot_scores(amod3, type = 4)
LL3
LL3 <- plot_scores(amod3, type = 4, repulsion = 2)
LL3
LL3 <- plot_scores(amod3, type = 4,
                   size.tex.gen = 2,
                   x.lab = "PC1 of E1",
                   y.lab = "Nominal Height",
                   title = FALSE,
                   col.alpha.gen = 0)
LL3
############### AMMI biplots for FLAG LEAF WIDTH ####
LLW <- plot_scores(amod4)
plot(LLW)
LLW <- plot_scores(amod4, x.lab = "FLAG LEAF WIDTH")
LLW

LLW2 <- plot_scores(amod4, type = 2)
plot(LLW2)
LLW2 <- plot_scores(amod4, type = 2, polygon = TRUE)
LLW2
LLW2 <- plot_scores(amod4,
                    type = 2,
                    col.env = "blue",
                    col.gen = transparent_color(),
                    col.segm.env = "orange",
                    highlight = c("G1", "G2"),
                    col.highlight = "darkcyan",
                    axis.expand = 1.5)
LLW2

LLW3 <- plot_scores(amod4, type = 4)
LLW3
LLW3 <- plot_scores(amod4, type = 4, repulsion = 2)
LLW3
LLW3 <- plot_scores(amod4, type = 4,
                    size.tex.gen = 2,
                    x.lab = "PC1 of E1",
                    y.lab = "Nominal Height",
                    title = FALSE,
                    col.alpha.gen = 0)
LLW3
############### AMMI biplots for leaves per main tiller ####
LM <- plot_scores(amod5)
plot(LM)
LM <- plot_scores(amod5, x.lab = "LEAVES PER MAIN TILLER")
LM

LM2 <- plot_scores(amod5, type = 2)
plot(LM2)
LM2 <- plot_scores(amod5, type = 2, polygon = TRUE)
LM2
LM2 <- plot_scores(amod5,
                   type = 2,
                   col.env = "blue",
                   col.gen = transparent_color(),
                   col.segm.env = "orange",
                   highlight = c("G1", "G2"),
                   col.highlight = "darkcyan",
                   axis.expand = 1.5)
LM2

LM3 <- plot_scores(amod5, type = 4)
LM3
LM3 <- plot_scores(amod5, type = 4, repulsion = 2)
LM3
LM3 <- plot_scores(amod5, type = 4,
                   size.tex.gen = 2,
                   x.lab = "PC1 of E1",
                   y.lab = "Nominal Height",
                   title = FALSE,
                   col.alpha.gen = 0)
LM3
############### AMMI biplots for TILLERS PER PLANT ####
TP <- plot_scores(amod6)
plot(TP)
TP <- plot_scores(amod6, x.lab = "TILLERS PER PLANT")
TP

TP2 <- plot_scores(amod6, type = 2)
plot(TP2)
TP2 <- plot_scores(amod6, type = 2, polygon = TRUE)
TP2
TP2 <- plot_scores(amod6,
                   type = 2,
                   col.env = "blue",
                   col.gen = transparent_color(),
                   col.segm.env = "orange",
                   highlight = c("G1", "G2"),
                   col.highlight = "darkcyan",
                   axis.expand = 1.5)
TP2

TP3 <- plot_scores(amod6, type = 4)
TP3
TP3 <- plot_scores(amod6, type = 4, repulsion = 2)
TP3
TP3 <- plot_scores(amod6, type = 4,
                   size.tex.gen = 2,
                   x.lab = "PC1 of E1",
                   y.lab = "Nominal Height",
                   title = FALSE,
                   col.alpha.gen = 0)
TP3
############### AMMI biplots for PRODUCTIVE TILLERS PER PLANT ####
PT <- plot_scores(amod7)
plot(PT)
PT <- plot_scores(amod7, x.lab = "PRODUCTIVE TILLERS PER PLANT")
PT

PT2 <- plot_scores(amod7, type = 2)
plot(PT2)
PT2 <- plot_scores(amod7, type = 2, polygon = TRUE)
PT2
PT2 <- plot_scores(amod7,
                   type = 2,
                   col.env = "blue",
                   col.gen = transparent_color(),
                   col.segm.env = "orange",
                   highlight = c("G1", "G2"),
                   col.highlight = "darkcyan",
                   axis.expand = 1.5)
PT2

PT3 <- plot_scores(amod7, type = 4)
PT3
PT3 <- plot_scores(amod7, type = 4, repulsion = 2)
PT3
PT3 <- plot_scores(amod7, type = 4,
                   size.tex.gen = 2,
                   x.lab = "PC1 of E1",
                   y.lab = "Nominal Height",
                   title = FALSE,
                   col.alpha.gen = 0)
PT3
############### AMMI biplots for EAR LENGTH ####
EL <- plot_scores(amod8)
plot(EL)
EL <- plot_scores(amod8, x.lab = "EAR LENGTH")
EL

EL2 <- plot_scores(amod8, type = 2)
plot(EL2)
EL2 <- plot_scores(amod8, type = 2, polygon = TRUE)
EL2
EL2 <- plot_scores(amod8,
                   type = 2,
                   col.env = "blue",
                   col.gen = transparent_color(),
                   col.segm.env = "orange",
                   highlight = c("G1", "G2"),
                   col.highlight = "darkcyan",
                   axis.expand = 1.5)
EL2

EL3 <- plot_scores(amod8, type = 4)
EL3
EL3 <- plot_scores(amod8, type = 4, repulsion = 2)
EL3
EL3 <- plot_scores(amod8, type = 4,
                   size.tex.gen = 2,
                   x.lab = "PC1 of E1",
                   y.lab = "Nominal Height",
                   title = FALSE,
                   col.alpha.gen = 0)
EL3
############### AMMI biplots for FINGER LENGTH ####
FL <- plot_scores(amod9)
plot(FL)
FL <- plot_scores(amod9, x.lab = "FINGER LENGTH")
FL

FL2 <- plot_scores(amod9, type = 2)
plot(FL2)
FL2 <- plot_scores(amod9, type = 2, polygon = TRUE)
FL2
FL2 <- plot_scores(amod9,
                   type = 2,
                   col.env = "blue",
                   col.gen = transparent_color(),
                   col.segm.env = "orange",
                   highlight = c("G1", "G2"),
                   col.highlight = "darkcyan",
                   axis.expand = 1.5)
FL2

FL3 <- plot_scores(amod9, type = 4)
FL3
FL3 <- plot_scores(amod9, type = 4, repulsion = 2)
FL3
FL3 <- plot_scores(amod9, type = 4,
                   size.tex.gen = 2,
                   x.lab = "PC1 of E1",
                   y.lab = "Nominal Height",
                   title = FALSE,
                   col.alpha.gen = 0)
FL3
############### AMMI biplots for NO.OF FINGERS PER EAR ####
NF <- plot_scores(amod10)
plot(NF)
NF <- plot_scores(amod10, x.lab = "FINGERS PER EAR")
NF

NF2 <- plot_scores(amod10, type = 2)
plot(NF2)
NF2 <- plot_scores(amod10, type = 2, polygon = TRUE)
NF2
NF2 <- plot_scores(amod10,
                   type = 2,
                   col.env = "blue",
                   col.gen = transparent_color(),
                   col.segm.env = "orange",
                   highlight = c("G20" , "G35"),
                   col.highlight = "darkcyan",
                   axis.expand = 1.5)
NF2
NF3 <- plot_scores(amod10, type = 4)
NF3
NF3 <- plot_scores(amod10, type = 4, repulsion = 2)
NF3
NF3 <- plot_scores(amod10, type = 4,
                   size.tex.gen = 2,
                   x.lab = "PC1 of E1",
                   y.lab = "Nominal Height",
                   title = FALSE,
                   col.alpha.gen = 0)
NF3
############### AMMI biplots for FINGER WIDTH ####
FW <- plot_scores(amod11)
plot(FW)
FW <- plot_scores(amod11, x.lab = "FINGER WIDTH")
FW

FW2 <- plot_scores(amod11, type = 2)
plot(FW2)
FW2 <- plot_scores(amod11, type = 2, polygon = TRUE)
FW2
FW2 <- plot_scores(amod11,
                   type = 2,
                   col.env = "blue",
                   col.gen = transparent_color(),
                   col.segm.env = "orange",
                   highlight = c("G20" , "G35"),
                   col.highlight = "darkcyan",
                   axis.expand = 1.5)
FW2
FW3 <- plot_scores(amod11, type = 4)
FW3
FW3 <- plot_scores(amod11, type = 4, repulsion = 2)
FW3
FW3 <- plot_scores(amod11, type = 4,
                   size.tex.gen = 2,
                   x.lab = "PC1 of E1",
                   y.lab = "Nominal Height",
                   title = FALSE,
                   col.alpha.gen = 0)
FW3
############### AMMI biplots for DAYS TO MATURITY ####
DM1 <- plot_scores(amod12)
plot(DM1)
DM1 <- plot_scores(amod12, x.lab = "DAYS TO MATURITY")
DM1

DM2 <- plot_scores(amod12, type = 2)
plot(DM2)
DM2 <- plot_scores(amod12, type = 2, polygon = TRUE)
DM2
DM2 <- plot_scores(amod12,
                   type = 2,
                   col.env = "blue",
                   col.gen = transparent_color(),
                   col.segm.env = "orange",
                   highlight = c("G19", "G35"),
                   col.highlight = "darkcyan",
                   axis.expand = 1.5)
DM2
DM3 <- plot_scores(amod12, type = 4)
DM3
DM3 <- plot_scores(amod12, type = 4, repulsion = 2)
DM3
DM3 <- plot_scores(amod12, type = 4,
                   size.tex.gen = 2,
                   x.lab = "PC1 of E1",
                   y.lab = "Nominal Height",
                   title = FALSE,
                   col.alpha.gen = 0)
DM3
############### AMMI biplots for TEST WEIGHT ####
TW1 <- plot_scores(amod13)
plot(TW1)
TW1 <- plot_scores(amod13, x.lab = "THOUSAND GRAIN WEIGHT")
TW1

TW2 <- plot_scores(amod13, type = 2)
plot(TW2)
TW2 <- plot_scores(amod13, type = 2, polygon = TRUE)
TW2
TW2 <- plot_scores(amod13,
                   type = 2,
                   col.env = "blue",
                   col.gen = transparent_color(),
                   col.segm.env = "orange",
                   highlight = c("G20" , "G35"),
                   col.highlight = "darkcyan",
                   axis.expand = 1.5)
TW2
TW2 <- plot_scores(amod13, type = 4)
TW2
TW2 <- plot_scores(amod13, type = 4, repulsion = 2)
TW2
TW2 <- plot_scores(amod13, type = 4,
                   size.tex.gen = 2,
                   x.lab = "PC1 of E1",
                   y.lab = "Nominal Height",
                   title = FALSE,
                   col.alpha.gen = 0)
TW2
############### AMMI biplots for Grain Iron content ####
I <- plot_scores(amod14)
plot(I)
I <- plot_scores(amod14, x.lab = "GRAIN IRON CONTENT")
I

FE <- plot_scores(amod14, type = 2)
plot(FE)
FE <- plot_scores(amod14, type = 2, polygon = TRUE)
FE
FE <- plot_scores(amod14,
                  type = 2,
                  col.env = "blue",
                  col.gen = transparent_color(),
                  col.segm.env = "orange",
                  highlight = c("G20" , "G35"),
                  col.highlight = "darkcyan",
                  axis.expand = 1.5)
FE
FEC <- plot_scores(amod14, type = 4)
FEC
FEC <- plot_scores(amod14, type = 4, repulsion = 2)
FEC
FEC <- plot_scores(amod14, type = 4,
                   size.tex.gen = 2,
                   x.lab = "PC1 of E1",
                   y.lab = "Nominal Height",
                   title = FALSE,
                   col.alpha.gen = 0)
FEC
############### AMMI biplots for Grain ZINC content ####
Z <- plot_scores(amod15)
plot(Z)
Z <- plot_scores(amod15, x.lab = "GRAIN ZINC CONTENT")
Z

ZN <- plot_scores(amod15, type = 2)
plot(ZN)
ZN <- plot_scores(amod15, type = 2, polygon = TRUE)
ZN
ZN <- plot_scores(amod15,
                  type = 2,
                  col.env = "blue",
                  col.gen = transparent_color(),
                  col.segm.env = "orange",
                  highlight = c("G19", "G35"),
                  col.highlight = "darkcyan",
                  axis.expand = 1.5)
ZN
ZNC <- plot_scores(amod15, type = 4)
ZNC
ZNC <- plot_scores(amod15, type = 4, repulsion = 2)
ZNC
ZNC <- plot_scores(amod15, type = 4,
                   size.tex.gen = 2,
                   x.lab = "PC1 of E1",
                   y.lab = "Nominal Height",
                   title = FALSE,
                   col.alpha.gen = 0)
ZNC
############### AMMI biplots for Grain MANGANESE content ####
M <- plot_scores(amod16)
plot(M)
M <- plot_scores(amod16, x.lab = "GRAIN MANGANESE CONTENT")
M

MN <- plot_scores(amod16, type = 2)
plot(MN)
MN <- plot_scores(amod16, type = 2, polygon = TRUE)
MN
MN <- plot_scores(amod16,
                  type = 2,
                  col.env = "blue",
                  col.gen = transparent_color(),
                  col.segm.env = "orange",
                  highlight = c("G11", "G26"),
                  col.highlight = "darkcyan",
                  axis.expand = 1.5)
MN
MNC <- plot_scores(amod16, type = 4)
MNC
MNC <- plot_scores(amod16, type = 4, repulsion = 2)
MNC
MNC <- plot_scores(amod16, type = 4,
                   size.tex.gen = 2,
                   x.lab = "PC1 of E1",
                   y.lab = "Nominal Height",
                   title = FALSE,
                   col.alpha.gen = 0)
MNC
############### AMMI biplots for Grain COPPER content ####
CO <- plot_scores(amod17)
plot(CO)
CO <- plot_scores(amod17, x.lab = "GRAIN COPPER CONTENT")
CO

CO <- plot_scores(amod17, type = 2)
plot(CO)
CO <- plot_scores(amod17, type = 2, polygon = TRUE)
CO
CO <- plot_scores(amod17,
                  type = 2,
                  col.env = "blue",
                  col.gen = transparent_color(),
                  col.segm.env = "orange",
                  highlight = c("G8", "G9"),
                  col.highlight = "darkcyan",
                  axis.expand = 1.5)
CO
COC <- plot_scores(amod17, type = 4)
COC
COC <- plot_scores(amod17, type = 4, repulsion = 2)
COC
COC <- plot_scores(amod17, type = 4,
                   size.tex.gen = 2,
                   x.lab = "PC1 of E1",
                   y.lab = "Nominal Height",
                   title = FALSE,
                   col.alpha.gen = 0)
COC
############### AMMI biplots for Grain CALCIUM content ####
CA <- plot_scores(amod18)
plot(CA)
CA <- plot_scores(amod18, x.lab = "GRAIN CALCIUM CONTENT")
CA

CA <- plot_scores(amod18, type = 2)
plot(CA)
CA <- plot_scores(amod18, type = 2, polygon = TRUE)
CA
CA <- plot_scores(amod18,
                  type = 2,
                  col.env = "blue",
                  col.gen = transparent_color(),
                  col.segm.env = "orange",
                  highlight = c("G10", "G7"),
                  col.highlight = "darkcyan",
                  axis.expand = 1.5)
CA
CAC <- plot_scores(amod18, type = 4)
CAC
CAC <- plot_scores(amod18, type = 4, repulsion = 2)
CAC
CAC <- plot_scores(amod18, type = 4,
                   size.tex.gen = 2,
                   x.lab = "PC1 of E1",
                   y.lab = "Nominal Height",
                   title = FALSE,
                   col.alpha.gen = 0)
CAC
############### AMMI biplots for Grain Yield per plant ####
GY <- plot_scores(amod19)
plot(GY)
GY <- plot_scores(amod19, x.lab = "GRAIN YIELD PER PLANT")
GY

GY <- plot_scores(amod19, type = 2)
plot(GY)
GY <- plot_scores(amod19, type = 2, polygon = TRUE)
GY
GY <- plot_scores(amod19,
                  type = 2,
                  col.env = "blue",
                  col.gen = transparent_color(),
                  col.segm.env = "orange",
                  highlight = c("G5", "G13"),
                  col.highlight = "darkcyan",
                  axis.expand = 1.5)
GY
GYC <- plot_scores(amod19, type = 4)
GYC
GYC <- plot_scores(amod19, type = 4, repulsion = 2)
GYC
GYC <- plot_scores(amod19, type = 4,
                   size.tex.gen = 2,
                   x.lab = "PC1 of E1",
                   y.lab = "Nominal Height",
                   title = FALSE,
                   col.alpha.gen = 0)
GYC
################# ammi based statistics ##########################
##DFF
abs1 <- AMMI_indexes(amod1)
print(abs1)
View(abs1$DFF)
write_xlsx(abs1$DFF, "abs1$DFF.xlsx")
##PH
abs2 <- AMMI_indexes(amod2)
print(abs2)
View(abs2$PH)
write_xlsx(abs2$PH, "abs2$PH.xlsx")
##FLL
abs3 <- AMMI_indexes(amod3)
print(abs3)
View(abs3$LL)
write_xlsx(abs3$LL, "abs3$LL.xlsx")
##FLW
abs4 <- AMMI_indexes(amod4)
print(abs4)
View(abs4$LW)
write_xlsx(abs4$LW, "abs4$LW.xlsx")
## LMT
abs5 <- AMMI_indexes(amod5)
print(abs5)
View(abs5$LMT)
write_xlsx(abs5$LMT, "abs5$LMT.xlsx")
## TP
abs6 <- AMMI_indexes(amod6)
print(abs6)
View(abs6$TP)
write_xlsx(abs6$TP, "abs6$TP.xlsx")
## PTP
abs7 <- AMMI_indexes(amod7)
print(abs7)
View(abs7$PTP)
write_xlsx(abs7$PTP, "abs7$PTP.xlsx")
## EL
abs8 <- AMMI_indexes(amod8)
print(abs8)
View(abs8$EL)
write_xlsx(abs8$EL, "abs8$EL.xlsx")
## FL
abs9 <- AMMI_indexes(amod9)
print(abs9)
View(abs9$FL)
write_xlsx(abs9$FL, "abs9$FL.xlsx")
##NFE
abs10 <- AMMI_indexes(amod10)
print(abs10)
View(abs10$NFE)
write_xlsx(abs10$NFE, "abs10$NFE.xlsx")
##FW
abs11 <- AMMI_indexes(amod11)
print(abs11)
View(abs11$FW)
write_xlsx(abs11$FW, "abs11$FW.xlsx")
##DM
abs12 <- AMMI_indexes(amod12)
print(abs12)
View(abs12$DM)
write_xlsx(abs12$DM, "abs12$DM.xlsx")
##TSW
abs13 <- AMMI_indexes(amod13)
print(abs13)
View(abs13$TSW)
write_xlsx(abs13$TSW, "abs13$TSW.xlsx")
##FE
abs14 <- AMMI_indexes(amod14)
print(abs14)
View(abs14$FE)
write_xlsx(abs14$FE, "abs14$FE.xlsx")
## ZN
abs15 <- AMMI_indexes(amod15)
print(abs15)
View(abs15$ZN)
write_xlsx(abs15$ZN, "abs15$ZN.xlsx")
## MN
abs16 <- AMMI_indexes(amod16)
print(abs16)
View(abs16$MN)
write_xlsx(abs16$MN, "abs16$MN.xlsx")
##CU
abs17 <- AMMI_indexes(amod17)
print(abs17)
View(abs17$CU)
write_xlsx(abs17$CU, "abs17$CU.xlsx")
## CA
abs18 <- AMMI_indexes(amod18)
print(abs18)
View(abs18$CA)
write_xlsx(abs18$CA, "abs18$CA.xlsx")
## GYP
abs19 <- ammi_indexes(amod19)
print(abs19)
View(abs19$GYP)
write_xlsx(abs19$GYP, "abs19$GYP.xlsx")
################ ammi based on waas #####################
### DFF
waas1 <- waas(MET, ENVIRONMENT, GENOTYPE, REPLICATION, DFF, BLOCK)
waas1
print(waas1)

wp1_3 <- plot_scores(waas1, type = 3)
wp1_3

wp1_2 <- plot_scores(waas1, type = 2, polygon = TRUE)
wp1_2
## PH
waas2 <- waas(MET, ENVIRONMENT, GENOTYPE, REPLICATION, PH, BLOCK)
waas2
print(waas2)

wp2_3 <- plot_scores(waas2, type = 3)
wp2_3

wp2_2 <- plot_scores(waas2, type = 2, polygon = TRUE)
wp2_2
## FLL
waas3 <- waas(MET, ENVIRONMENT, GENOTYPE, REPLICATION, LL, BLOCK)
waas3
print(waas3)

wp3_3 <- plot_scores(waas3, type = 3)
wp3_3

wp3_2 <- plot_scores(waas3, type = 2, polygon = TRUE)
wp3_2
## FLW
waas4 <- waas(MET, ENVIRONMENT, GENOTYPE, REPLICATION, LW, BLOCK)
waas4
print(waas4)

wp4_3 <- plot_scores(waas4, type = 3)
wp4_3

wp4_2 <- plot_scores(waas4, type = 2, polygon = TRUE)
wp4_2
## LMT
waas5 <- waas(MET, ENVIRONMENT, GENOTYPE, REPLICATION, LMT, BLOCK)
waas5
print(waas5)

wp5_3 <- plot_scores(waas5, type = 3)
wp5_3

wp5_2 <- plot_scores(waas5, type = 2, polygon = TRUE)
wp5_2
## TP
waas6 <- waas(MET, ENVIRONMENT, GENOTYPE, REPLICATION, TP, BLOCK)
waas6
print(waas6)

wp6_3 <- plot_scores(waas6, type = 3)
wp6_3

wp6_2 <- plot_scores(waas6, type = 2, polygon = TRUE)
wp6_2
## PTP
waas7 <- waas(MET, ENVIRONMENT, GENOTYPE, REPLICATION, PTP, BLOCK)
waas7
print(waas7)

wp7_3 <- plot_scores(waas7, type = 3)
wp7_3

wp7_2 <- plot_scores(waas7, type = 2, polygon = TRUE)
wp7_2
## EL
waas8 <- waas(MET, ENVIRONMENT, GENOTYPE, REPLICATION, EL, BLOCK)
waas8
print(waas8)

wp8_3 <- plot_scores(waas8, type = 3)
wp8_3

wp8_2 <- plot_scores(waas8, type = 2, polygon = TRUE)
wp8_2
## FL
waas9 <- waas(MET, ENVIRONMENT, GENOTYPE, REPLICATION, FL, BLOCK)
waas9
print(waas9)

wp9_3 <- plot_scores(waas9, type = 3)
wp9_3

wp9_2 <- plot_scores(waas9, type = 2, polygon = TRUE)
wp9_2
## NFE
waas10 <- waas(MET, ENVIRONMENT, GENOTYPE, REPLICATION, NFE, BLOCK)
waas10
print(waas10)

wp10_3 <- plot_scores(waas10, type = 3)
wp10_3

wp10_2 <- plot_scores(waas10, type = 2, polygon = TRUE)
wp10_2
## FW
waas11 <- waas(MET, ENVIRONMENT, GENOTYPE, REPLICATION, FW, BLOCK)
waas11
print(waas11)

wp11_3 <- plot_scores(waas11, type = 3)
wp11_3

wp11_2 <- plot_scores(waas11, type = 2, polygon = TRUE)
wp11_2
## DM
waas12 <- waas(MET, ENVIRONMENT, GENOTYPE, REPLICATION, DM, BLOCK)
waas12
print(waas12)

wp12_3 <- plot_scores(waas12, type = 3)
wp12_3

wp12_2 <- plot_scores(waas12, type = 2, polygon = TRUE)
wp12_2
## TSW
waas13 <- waas(MET, ENVIRONMENT, GENOTYPE, REPLICATION, TSW, BLOCK)
waas13
print(waas13)

wp13_3 <- plot_scores(waas13, type = 3)
wp13_3

wp13_2 <- plot_scores(waas13, type = 2, polygon = TRUE)
wp13_2
## FE
waas14 <- waas(MET, ENVIRONMENT, GENOTYPE, REPLICATION, FE, BLOCK)
waas14
print(waas14)

wp14_3 <- plot_scores(waas14, type = 3)
wp14_3

wp14_2 <- plot_scores(waas14, type = 2, polygon = TRUE)
wp14_2
## ZN
waas15 <- waas(MET, ENVIRONMENT, GENOTYPE, REPLICATION, ZN, BLOCK)
waas15
print(waas15)

wp15_3 <- plot_scores(waas15, type = 3)
wp15_3

wp15_2 <- plot_scores(waas15, type = 2, polygon = TRUE)
wp15_2
## MN
waas16 <- waas(MET, ENVIRONMENT, GENOTYPE, REPLICATION, MN, BLOCK)
waas16
print(waas16)

wp16_3 <- plot_scores(waas16, type = 3)
wp16_3

wp16_2 <- plot_scores(waas16, type = 2, polygon = TRUE)
wp16_2
## CU
waas17 <- waas(MET, ENVIRONMENT, GENOTYPE, REPLICATION, CU, BLOCK)
waas17
print(waas17)

wp17_3 <- plot_scores(waas17, type = 3)
wp17_3

wp17_2 <- plot_scores(waas17, type = 2, polygon = TRUE)
wp17_2
## CA
waas18 <- waas(MET, ENVIRONMENT, GENOTYPE, REPLICATION, CA, BLOCK)
waas18
print(waas18)

wp18_3 <- plot_scores(waas18, type = 3)
wp18_3

wp18_2 <- plot_scores(waas18, type = 2, polygon = TRUE)
wp18_2
## GYP
waas19 <- waas(MET, ENVIRONMENT, GENOTYPE, REPLICATION, GYP, BLOCK)
waas19
print(waas19)

wp19_3 <- plot_scores(waas19, type = 3)
wp19_3

wp19_2 <- plot_scores(waas19, type = 2, polygon = TRUE)
wp19_2
############### waas based stats ##############################
was_ind <- waas(MET, ENVIRONMENT, GENOTYPE, REPLICATION, resp = everything(), BLOCK)
## DFF
wabs1 <- AMMI_indexes(waas1)
View(wabs1$DFF)
print(wabs1$DFF)
write_xlsx(wabs1$DFF, "wabs1$DFF.xlsx")
## PH
wabs2 <- AMMI_indexes(waas2)
View(wabs2$PH)
print(wabs2$PH)
write_xlsx(wabs2$PH, "wabs2$PH.xlsx")
## FLL
wabs3 <- AMMI_indexes(waas3)
View(wabs3$LL)
print(wabs3$LL)
write_xlsx(wabs3$LL, "wabs3$LL.xlsx")
## FLW
wabs4 <- AMMI_indexes(waas4)
View(wabs4$LW)
print(wabs4$LW)
write_xlsx(wabs4$LW, "wabs4$LW.xlsx")
## LMT
wabs5 <- AMMI_indexes(waas5)
View(wabs5$LMT)
print(wabs5$LMT)
write_xlsx(wabs5$LMT, "wabs5$LMT.xlsx")
## TP
wabs6 <- AMMI_indexes(waas6)
View(wabs6$TP)
print(wabs6$TP)
write_xlsx(wabs6$TP, "wabs6$TP.xlsx")
## PTP
wabs7 <- AMMI_indexes(waas7)
View(wabs7$PTP)
print(wabs7$PTP)
write_xlsx(wabs7$PTP, "wabs7$PTP.xlsx")
## EL
wabs8 <- AMMI_indexes(waas8)
View(wabs8$EL)
print(wabs8$EL)
write_xlsx(wabs8$EL, "wabs8$EL.xlsx")
## FL
wabs9 <- AMMI_indexes(waas9)
View(wabs9$FL)
print(wabs9$FL)
write_xlsx(wabs9$FL, "wabs9$FL.xlsx")
## NFE
wabs10 <- AMMI_indexes(waas10)
View(wabs10$NFE)
print(wabs10$NFE)
write_xlsx(wabs10$NFE, "wabs10$NFE.xlsx")
## FW
wabs11 <- AMMI_indexes(waas11)
View(wabs11$FW)
print(wabs11$FW)
write_xlsx(wabs11$FW, "wabs11$FW.xlsx")
## DM
wabs12 <- AMMI_indexes(waas12)
View(wabs12$DM)
print(wabs12$DM)
write_xlsx(wabs12$DM, "wabs12$DM.xlsx")
## TSW
wabs13 <- AMMI_indexes(waas13)
View(wabs13$TSW)
print(wabs13$TSW)
write_xlsx(wabs13$TSW, "wabs13$TSW.xlsx")
## FE
wabs14 <- AMMI_indexes(waas14)
View(wabs14$FE)
print(wabs14$FE)
write_xlsx(wabs14$FE, "wabs14$FE.xlsx")
## ZN
wabs15 <- AMMI_indexes(waas15)
View(wabs15$ZN)
print(wabs15$ZN)
write_xlsx(wabs15$ZN, "wabs15$ZN.xlsx")
## MN
wabs16 <- AMMI_indexes(waas16)
View(wabs16$MN)
print(wabs16$MN)
write_xlsx(wabs16$MN, "wabs16$MN.xlsx")
## CU
wabs17 <- AMMI_indexes(waas17)
View(wabs17$CU)
print(wabs17$CU)
write_xlsx(wabs17$CU, "wabs17$CU.xlsx")
## CA
wabs18 <- AMMI_indexes(waas18)
View(wabs18$CA)
print(wabs18$CA)
write_xlsx(wabs18$CA, "wabs18$CA.xlsx")
## GYP
wabs19 <- ammi_indexes(waas19)
View(wabs19$GYP)
print(wabs19$GYP)
write_xlsx(wabs19$GYP, "wabs19$GYP.xlsx")
########## blup indexes ############
library(metan)
res_ind <- waasb(MET, ENVIRONMENT, GENOTYPE, REPLICATION, resp = everything(), BLOCK)
## DFF
res_ind <- gamem_met(MET, ENVIRONMENT, GENOTYPE, REPLICATION, DFF, BLOCK)
res_ind1 <- waasb(MET, ENVIRONMENT, GENOTYPE, REPLICATION, DFF, BLOCK)
model_indexes1 <- blup_indexes(res_ind1)
Q1 <- gmd(model_indexes1)
Q1
print(Q1, n=50)
## PH
res_ind2 <- waasb(MET, ENVIRONMENT, GENOTYPE, REPLICATION, PH, BLOCK)
model_indexes2 <- blup_indexes(res_ind2)
Q2 <- gmd(model_indexes2)
Q2
print(Q2, n=50)
## FLL
res_ind3 <- waasb(MET, ENVIRONMENT, GENOTYPE, REPLICATION, LL, BLOCK)
model_indexes3 <- blup_indexes(res_ind3)
Q3 <- gmd(model_indexes3)
Q3
print(Q3, n=50)
## FLW
res_ind4 <- waasb(MET, ENVIRONMENT, GENOTYPE, REPLICATION, LW, BLOCK)
model_indexes4 <- blup_indexes(res_ind4)
Q4 <- gmd(model_indexes4)
Q4
print(Q4, n=50)
## LMT
res_ind5 <- waasb(MET, ENVIRONMENT, GENOTYPE, REPLICATION, LMT, BLOCK)
model_indexes5 <- blup_indexes(res_ind5)
Q5 <- gmd(model_indexes5)
Q5
print(Q5, n=50)
## TP
res_ind6 <- waasb(MET, ENVIRONMENT, GENOTYPE, REPLICATION, TP, BLOCK)
model_indexes6 <- blup_indexes(res_ind6)
Q6 <- gmd(model_indexes6)
Q6
print(Q6, n=50)
## PTP
res_ind7 <- waasb(MET, ENVIRONMENT, GENOTYPE, REPLICATION, PTP, BLOCK)
model_indexes7 <- blup_indexes(res_ind7)
Q7 <- gmd(model_indexes7)
Q7
print(Q7, n=50)
## EL
res_ind8 <- waasb(MET, ENVIRONMENT, GENOTYPE, REPLICATION, EL, BLOCK)
model_indexes8 <- blup_indexes(res_ind8)
Q8 <- gmd(model_indexes8)
Q8
print(Q8, n=50)
## FL
res_ind9 <- waasb(MET, ENVIRONMENT, GENOTYPE, REPLICATION, FL, BLOCK)
model_indexes9 <- blup_indexes(res_ind9)
Q9 <- gmd(model_indexes9)
Q9
print(Q9, n=50)
## NFE
res_ind10 <- waasb(MET, ENVIRONMENT, GENOTYPE, REPLICATION, NFE, BLOCK)
model_indexes10 <- blup_indexes(res_ind10)
Q10 <- gmd(model_indexes10)
Q10
print(Q10, n=50)
## FW
res_ind11 <- waasb(MET, ENVIRONMENT, GENOTYPE, REPLICATION, FW, BLOCK)
model_indexes11 <- blup_indexes(res_ind11)
Q11 <- gmd(model_indexes11)
Q11
print(Q11, n=50)
## DM
res_ind12 <- waasb(MET, ENVIRONMENT, GENOTYPE, REPLICATION, DM, BLOCK)
model_indexes12 <- blup_indexes(res_ind12)
Q12 <- gmd(model_indexes12)
Q12
print(Q12, n=50)
## TSW
res_ind13 <- waasb(MET, ENVIRONMENT, GENOTYPE, REPLICATION, TSW, BLOCK)
model_indexes13 <- blup_indexes(res_ind13)
Q13 <- gmd(model_indexes13)
Q13
print(Q13, n=50)
## FE
res_ind14 <- waasb(MET, ENVIRONMENT, GENOTYPE, REPLICATION, FE, BLOCK)
model_indexes14 <- blup_indexes(res_ind14)
Q14 <- gmd(model_indexes14)
Q14
print(Q14, n=50)
## ZN
res_ind15 <- waasb(MET, ENVIRONMENT, GENOTYPE, REPLICATION, ZN, BLOCK)
model_indexes15 <- blup_indexes(res_ind15)
Q15 <- gmd(model_indexes15)
Q15
print(Q15, n=50)
## MN
res_ind16 <- waasb(MET, ENVIRONMENT, GENOTYPE, REPLICATION, MN, BLOCK)
model_indexes16 <- blup_indexes(res_ind16)
Q16 <- gmd(model_indexes16)
Q16
print(Q16, n=50)
## CU
res_ind17 <- waasb(MET, ENVIRONMENT, GENOTYPE, REPLICATION, CU, BLOCK)
model_indexes17 <- blup_indexes(res_ind17)
Q17 <- gmd(model_indexes17)
Q17
print(Q17, n=50)
## CA
res_ind18 <- waasb(MET, ENVIRONMENT, GENOTYPE, REPLICATION, CA, BLOCK)
model_indexes18 <- blup_indexes(res_ind18)
Q18 <- gmd(model_indexes18)
Q18
print(Q18, n=50)
## GYP
res_ind19 <- waasb(
  MET,
  ENVIRONMENT,
  GENOTYPE,
  REPLICATION,
  GYP
)
print(res_ind19)
model_indexes19 <- blup_indexes(res_ind19)
Q19 <- gmd(model_indexes19)
Q19
library(ggplot2)
plot_blup(res_ind19)

#################### 1 GGE Model################################
## DFF
gge_model1 <- gge(MET, ENVIRONMENT, GENOTYPE, DFF)
predict(gge_model1)
pgge1 <- predict(gge_model1)
View(pgge1$DFF)
## PH
gge_model2 <- gge(MET, ENVIRONMENT, GENOTYPE, PH)
predict(gge_model2)
pgge2 <- predict(gge_model2)
View(pgge2$PH)
## FLL
gge_model3 <- gge(MET, ENVIRONMENT, GENOTYPE, LL)
predict(gge_model3)
pgge3 <- predict(gge_model3)
View(pgge3$LL)
## FLW
gge_model4 <- gge(MET, ENVIRONMENT, GENOTYPE, LW)
predict(gge_model4)
pgge4 <- predict(gge_model4)
View(pgge4$LW)
##LMT
gge_model5 <- gge(MET, ENVIRONMENT, GENOTYPE, LMT)
predict(gge_model5)
pgge5 <- predict(gge_model5)
View(pgge5$LMT)
##TP
gge_model6 <- gge(MET, ENVIRONMENT, GENOTYPE, TP)
predict(gge_model6)
pgge6 <- predict(gge_model6)
View(pgge6$TP)
## PTP
gge_model7 <- gge(MET, ENVIRONMENT, GENOTYPE, PTP)
predict(gge_model7)
pgge7 <- predict(gge_model7)
View(pgge7$PTP)
## EL
gge_model8 <- gge(MET, ENVIRONMENT, GENOTYPE, EL)
predict(gge_model8)
pgge8 <- predict(gge_model8)
View(pgge8$EL)
##FL
gge_model9 <- gge(MET, ENVIRONMENT, GENOTYPE, FL)
predict(gge_model9)
pgge9 <- predict(gge_model9)
View(pgge9$FL)
##NFE
gge_model10 <- gge(MET, ENVIRONMENT, GENOTYPE, NFE)
predict(gge_model10)
pgge10 <- predict(gge_model10)
View(pgge10$NFE)
##FW
gge_model11 <- gge(MET, ENVIRONMENT, GENOTYPE, FW)
predict(gge_model11)
pgge11 <- predict(gge_model11)
View(pgge11$FW)
##DM
gge_model12 <- gge(MET, ENVIRONMENT, GENOTYPE, DM)
predict(gge_model12)
pgge12 <- predict(gge_model12)
View(pgge12$DM)
##TSW
gge_model13 <- gge(MET, ENVIRONMENT, GENOTYPE, TSW)
predict(gge_model13)
pgge13 <- predict(gge_model13)
View(pgge13$TSW)
##FE
gge_model14 <- gge(MET, ENVIRONMENT, GENOTYPE, FE)
predict(gge_model14)
pgge14 <- predict(gge_model14)
View(pgge14$FE)
##ZN
gge_model15 <- gge(MET, ENVIRONMENT, GENOTYPE, ZN)
predict(gge_model15)
pgge15 <- predict(gge_model15)
View(pgge15$ZN)
##MN
gge_model16 <- gge(MET, ENVIRONMENT, GENOTYPE, MN)
predict(gge_model16)
pgge16 <- predict(gge_model16)
View(pgge16$MN)
##CU
gge_model17 <- gge(MET, ENVIRONMENT, GENOTYPE, CU)
predict(gge_model17)
pgge17 <- predict(gge_model17)
View(pgge17$CU)
##CA
gge_model18 <- gge(MET, ENVIRONMENT, GENOTYPE, CA)
predict(gge_model18)
pgge18 <- predict(gge_model18)
View(pgge18$CA)
##GYP
gge_model19 <- gge(MET, ENVIRONMENT, GENOTYPE, GYP)
gge_model19
predict(gge_model19)
pgge19 <- predict(gge_model19)
View(pgge19$GYP)
##################basic biplot#####################
## DFF
bbp1 <- plot(gge_model1)
bbp1
bbp1 <- plot(gge_model1, col.gen = "red")
bbp1
## PH
bbp2 <- plot(gge_model2)
bbp2
bbp2 <- plot(gge_model2, col.gen = "red")
bbp2
## FLL
bbp3 <- plot(gge_model3)
bbp3
bbp3 <- plot(gge_model3, col.gen = "red")
bbp3
##FLW
bbp4 <- plot(gge_model4)
bbp4
bbp4 <- plot(gge_model4, col.gen = "red")
bbp4
##LMT
bbp5 <- plot(gge_model5)
bbp5
bbp5 <- plot(gge_model5, col.gen = "red")
bbp5
##TP
bbp6 <- plot(gge_model6)
bbp6
bbp6 <- plot(gge_model6, col.gen = "red")
bbp6
##PTP
bbp7 <- plot(gge_model7)
bbp7
bbp7 <- plot(gge_model7, col.gen = "red")
bbp7
##EL
bbp8 <- plot(gge_model8)
bbp8
bbp8 <- plot(gge_model8, col.gen = "red")
bbp8
##FL
bbp9 <- plot(gge_model9)
bbp9
bbp9 <- plot(gge_model9, col.gen = "red")
bbp9
##NFE
bbp10 <- plot(gge_model10)
bbp10
bbp10 <- plot(gge_model10, col.gen = "red")
bbp10
##FW
bbp11 <- plot(gge_model11)
bbp11
bbp11 <- plot(gge_model11, col.gen = "red")
bbp11
##DM
bbp12 <- plot(gge_model12)
bbp12
bbp12 <- plot(gge_model12, col.gen = "red")
bbp12
##TSW
bbp13 <- plot(gge_model13)
bbp13
bbp13 <- plot(gge_model13, col.gen = "red")
bbp13
##FE
bbp14 <- plot(gge_model14)
bbp14
bbp14 <- plot(gge_model14, col.gen = "red")
bbp14
##ZN
bbp15 <- plot(gge_model15)
bbp15
bbp15 <- plot(gge_model15, col.gen = "red")
bbp15
##MN
bbp16 <- plot(gge_model16)
bbp16
bbp16 <- plot(gge_model16, col.gen = "red")
bbp16
##CU
bbp17 <- plot(gge_model17)
bbp17
bbp17 <- plot(gge_model17, col.gen = "red")
bbp17
##CA
bbp18 <- plot(gge_model18)
bbp18
bbp18 <- plot(gge_model18, col.gen = "red")
bbp18
##GYP
bbp19 <- plot(gge_model19)
bbp19
bbp19 <- plot(gge_model19, col.gen = "blue")
bbp19
###################### 2 DIscriminativeness vs representativeness ####################
## DFF
dvr1 <- plot(gge_model1, type = 4)
dvr1
## PH
dvr2 <- plot(gge_model2, type = 4)
dvr2
##FLL
dvr3 <- plot(gge_model3, type = 4)
dvr3
## FLW
dvr4 <- plot(gge_model4, type = 4)
dvr4
## LMT
dvr5 <- plot(gge_model5, type = 4)
dvr5
##TP
dvr6 <- plot(gge_model6, type = 4)
dvr6
## PTP
dvr7 <- plot(gge_model7, type = 4)
dvr7
##EL
dvr8 <- plot(gge_model8, type = 4)
dvr8
## FL
dvr9 <- plot(gge_model9, type = 4)
dvr9
## NFE
dvr10 <- plot(gge_model10, type = 4)
dvr10
## FW
dvr11 <- plot(gge_model11, type = 4)
dvr11
##DM
dvr12 <- plot(gge_model12, type = 4)
dvr12
##TSW
dvr13 <- plot(gge_model13, type = 4)
dvr13
##FE
dvr14 <- plot(gge_model14, type = 4)
dvr14
##ZN
dvr15 <- plot(gge_model15, type = 4)
dvr15
##MN
dvr16 <- plot(gge_model16, type = 4)
dvr16
##CU
dvr17 <- plot(gge_model17, type = 4)
dvr17
##CA
dvr18 <- plot(gge_model18, type = 4)
dvr18
##GYP
dvr19 <- plot(gge_model19, type = 4)
dvr19
#################### 3 ranks environments#######################
## DFF
re1 <- plot(gge_model1, type = 6)
re1
## PH
re2 <- plot(gge_model2, type = 6)
re2
##FLL
re3 <- plot(gge_model3, type = 6)
re3
##FLW
re4 <- plot(gge_model4, type = 6)
re4
## LMT
re5 <- plot(gge_model5, type = 6)
re5
## TP
re6 <- plot(gge_model6, type = 6)
re6
##PTP
re7 <- plot(gge_model7, type = 6)
re7
##EL
re8 <- plot(gge_model8, type = 6)
re8
##FL
re9 <- plot(gge_model9, type = 6)
re9
##NFE
re10 <- plot(gge_model10, type = 6)
re10
##FW
re11 <- plot(gge_model11, type = 6)
re11
##DM
re12 <- plot(gge_model12, type = 6)
re12
##TSW
re13 <- plot(gge_model13, type = 6)
re13
##FE
re14 <- plot(gge_model14, type = 6)
re14
##ZN
re15 <- plot(gge_model15, type = 6)
re15
##MN
re16 <- plot(gge_model16, type = 6)
re16
##CU
re17 <- plot(gge_model17, type = 6)
re17
##CA
re18 <- plot(gge_model18, type = 6)
re18
##GYP
re19 <- plot(gge_model19, type = 6)
re19
######################## 4 relationship among environments ###########
## DFF
rae1 <- plot(gge_model1, type = 10)
rae1
##PH
rae2 <- plot(gge_model2, type = 10)
rae2
##FLL
rae3 <- plot(gge_model3, type = 10)
rae3
##FLW
rae4 <- plot(gge_model4, type = 10)
rae4
##LMT
rae5 <- plot(gge_model5, type = 10)
rae5
##TP
rae6 <- plot(gge_model6, type = 10)
rae6
##PTP
rae7 <- plot(gge_model7, type = 10)
rae7
##EL
rae8 <- plot(gge_model8, type = 10)
rae8
##FL
rae9 <- plot(gge_model9, type = 10)
rae9
##NFE
rae10 <- plot(gge_model10, type = 10)
rae10
##FW
rae11 <- plot(gge_model11, type = 10)
rae11
##DM
rae12 <- plot(gge_model12, type = 10)
rae12
##TSW
rae13 <- plot(gge_model13, type = 10)
rae13
##FE
rae14 <- plot(gge_model14, type = 10)
rae14
##ZN
rae15 <- plot(gge_model15, type = 10)
rae15
##MN
rae16 <- plot(gge_model16, type = 10)
rae16
##CU
rae17 <- plot(gge_model17, type = 10)
rae17
##CA
rae18 <- plot(gge_model18, type = 10)
rae18
##GYP
rae19 <- plot(gge_model19, type = 10)
rae19
################# svp = genotype ########
##DFF
gpg1 <- gge(MET, ENVIRONMENT, GENOTYPE, DFF, svp = "genotype")
pgpg1 <- predict(gpg1)
View(pgpg1$DFF)
g1 <- pgpg1$DFF
g1
write_xlsx(g1, "g1.xlsx")
##PH
gpg2 <- gge(MET, ENVIRONMENT, GENOTYPE, PH, svp = "genotype")
pgpg2 <- predict(gpg2)
View(pgpg2$PH)
g2 <- pgpg2$PH
g2
write_xlsx(g2, "g2.xlsx")
##FLL
gpg3 <- gge(MET, ENVIRONMENT, GENOTYPE, LL, svp = "genotype")
pgpg3 <- predict(gpg3)
View(pgpg3$LL)
g3 <- pgpg3$LL
g3
write_xlsx(g3, "g3.xlsx")
##FLW
gpg4 <- gge(MET, ENVIRONMENT, GENOTYPE, LW, svp = "genotype")
pgpg4 <- predict(gpg4)
View(pgpg4$LW)
g4 <- pgpg3$LW
g4
write_xlsx(g4, "g4.xlsx")
##LMT
gpg5 <- gge(MET, ENVIRONMENT, GENOTYPE, LMT, svp = "genotype")
pgpg5 <- predict(gpg5)
View(pgpg5$LMT)
g5 <- pgpg5$LMT
g5
write_xlsx(g5, "g5.xlsx")
##TP
gpg6 <- gge(MET, ENVIRONMENT, GENOTYPE, TP, svp = "genotype")
pgpg6 <- predict(gpg6)
View(pgpg6$TP)
g6 <- pgpg6$TP
g6
write_xlsx(g6, "g6.xlsx")
##PTP
gpg7 <- gge(MET, ENVIRONMENT, GENOTYPE, PTP, svp = "genotype")
pgpg7 <- predict(gpg7)
View(pgpg7$PTP)
g7 <- pgpg7$PTP
g7
write_xlsx(g7, "g7.xlsx")
##EL
gpg8 <- gge(MET, ENVIRONMENT, GENOTYPE, EL, svp = "genotype")
pgpg8 <- predict(gpg8)
View(pgpg8$EL)
g8 <- pgpg8$EL
g8
write_xlsx(g8, "g8.xlsx")
##FL
gpg9 <- gge(MET, ENVIRONMENT, GENOTYPE, FL, svp = "genotype")
pgpg9 <- predict(gpg9)
View(pgpg9$FL)
g9 <- pgpg9$FL
g9
write_xlsx(g9, "g9.xlsx")
##NFE
gpg10 <- gge(MET, ENVIRONMENT, GENOTYPE, NFE, svp = "genotype")
pgpg10 <- predict(gpg10)
View(pgpg10$NFE)
g10 <- pgpg10$NFE
g10
write_xlsx(g10, "g10.xlsx")
##FW
gpg11 <- gge(MET, ENVIRONMENT, GENOTYPE, FW, svp = "genotype")
pgpg11 <- predict(gpg11)
View(pgpg11$FW)
g11 <- pgpg11$FW
g11
write_xlsx(g11, "g11.xlsx")
##DM
gpg12 <- gge(MET, ENVIRONMENT, GENOTYPE, DM, svp = "genotype")
pgpg12 <- predict(gpg12)
View(pgpg12$DM)
g12 <- pgpg12$DM
g12
write_xlsx(g12, "g12.xlsx")
##TSW
gpg13 <- gge(MET, ENVIRONMENT, GENOTYPE, TSW, svp = "genotype")
pgpg13 <- predict(gpg13)
View(pgpg13$TSW)
g13 <- pgpg13$TSW
g13
write_xlsx(g13, "g13.xlsx")
##FE
gpg14 <- gge(MET, ENVIRONMENT, GENOTYPE, FE, svp = "genotype")
pgpg14 <- predict(gpg14)
View(pgpg14$FE)
g14 <- pgpg14$FE
g14
write_xlsx(g14, "g14.xlsx")
##ZN
gpg15 <- gge(MET, ENVIRONMENT, GENOTYPE, ZN, svp = "genotype")
pgpg15 <- predict(gpg15)
View(pgpg15$ZN)
g15 <- pgpg15$ZN
g15
write_xlsx(g15, "g15.xlsx")
##MN
gpg16 <- gge(MET, ENVIRONMENT, GENOTYPE, MN, svp = "genotype")
pgpg16 <- predict(gpg16)
View(pgpg16$MN)
g16 <- pgpg16$MN
g16
write_xlsx(g16, "g16.xlsx")
##CU
gpg17 <- gge(MET, ENVIRONMENT, GENOTYPE, CU, svp = "genotype")
pgpg17 <- predict(gpg17)
View(pgpg17$CU)
g17 <- pgpg17$CU
g17
write_xlsx(g17, "g17.xlsx")
##CA
gpg18 <- gge(MET, ENVIRONMENT, GENOTYPE, CA, svp = "genotype")
pgpg18 <- predict(gpg18)
View(pgpg18$CA)
g18 <- pgpg18$CA
g18
write_xlsx(g18, "g18.xlsx")
##GYP
gpg19 <- gge(MET, ENVIRONMENT, GENOTYPE, GYP, svp = "genotype")
pgpg19 <- predict(gpg19)
View(pgpg19$GYP)
g19 <- pgpg19$GYP
g19
write_xlsx(g19, "g19.xlsx")
################# 5 mean perfomance vs stability ###############
##DFF
mvs1 <- plot(gpg1, type = 2)
mvs1
##PH
mvs2 <- plot(gpg2, type = 2)
mvs2
##FLL
mvs3 <- plot(gpg3, type = 2)
mvs3
##FLW
mvs4 <- plot(gpg4, type = 2)
mvs4
##LMT
mvs5 <- plot(gpg5, type = 2)
mvs5
##TP
mvs6 <- plot(gpg6, type = 2)
mvs6
##PTP
mvs7 <- plot(gpg7, type = 2)
mvs7
##EL
mvs8 <- plot(gpg8, type = 2)
mvs8
##FL
mvs9 <- plot(gpg9, type = 2)
mvs9
##NFE
mvs10 <- plot(gpg10, type = 2)
mvs10
##FW
mvs11 <- plot(gpg11, type = 2)
mvs11
##DM
mvs12 <- plot(gpg12, type = 2)
mvs12
##TSW
mvs13 <- plot(gpg13, type = 2)
mvs13
##FE
mvs14 <- plot(gpg14, type = 2)
mvs14
##ZN
mvs15 <- plot(gpg15, type = 2)
mvs15
##MN
mvs16 <- plot(gpg16, type = 2)
mvs16
##CU
mvs17 <- plot(gpg17, type = 2)
mvs17
##CA
mvs18 <- plot(gpg18, type = 2)
mvs18
##GYP
mvs19 <- plot(gpg19, type = 2)
mvs19
################ 6 examine a genotype ################
##DFF
eg1 <- plot(gpg1, type = 7, sel_gen = "G6")
eg1
##PH
eg2 <- plot(gpg2, type = 7, sel_gen = "G6")
eg2
##FLL
eg3 <- plot(gpg3, type = 7, sel_gen = "G6")
eg3
##FLW
eg4 <- plot(gpg4, type = 7, sel_gen = "G6")
eg4
##LMT
eg5 <- plot(gpg5, type = 7, sel_gen = "G6")
eg5
##TP
eg6 <- plot(gpg6, type = 7, sel_gen = "G6")
eg6
##PTP
eg7 <- plot(gpg7, type = 7, sel_gen = "G6")
eg7
##EL
eg8 <- plot(gpg8, type = 7, sel_gen = "G6")
eg8
##FL
eg9 <- plot(gpg9, type = 7, sel_gen = "G6")
eg9
##NFE
eg10 <- plot(gpg10, type = 7, sel_gen = "G6")
eg10
##FW
eg11 <- plot(gpg11, type = 7, sel_gen = "G6")
eg11
##DM
eg12 <- plot(gpg12, type = 7, sel_gen = "G6")
eg12
##TSW
eg13 <- plot(gpg13, type = 7, sel_gen = "G6")
eg13
##FE
eg14 <- plot(gpg14, type = 7, sel_gen = "G6")
eg14
##ZN
eg15 <- plot(gpg15, type = 7, sel_gen = "G6")
eg15
##MN
eg16 <- plot(gpg16, type = 7, sel_gen = "G6")
eg16
##CU
eg17 <- plot(gpg17, type = 7, sel_gen = "G6")
eg17
##CA
eg18 <- plot(gpg18, type = 7, sel_gen = "G6")
eg18
##GYP
eg19 <- plot(gpg19, type = 7, sel_gen = "G9")
eg19
############### 7 ranking genotypes ###############
##DFF
rg1 <- plot(gpg1, type = 8)
rg1
## PH
rg2 <- plot(gpg2, type = 8)
rg2
##FLL
rg3 <- plot(gpg3, type = 8)
rg3
##FLW
rg4 <- plot(gpg4, type = 8)
rg4
##LMT
rg5 <- plot(gpg5, type = 8)
rg5
##TP
rg6 <- plot(gpg6, type = 8)
rg6
##PTP
rg7 <- plot(gpg7, type = 8)
rg7
##EL
rg8 <- plot(gpg8, type = 8)
rg8
##FL
rg9 <- plot(gpg9, type = 8)
rg9
##NFE
rg10 <- plot(gpg10, type = 8)
rg10
##FW
rg11 <- plot(gpg11, type = 8)
rg11
##DM
rg12 <- plot(gpg12, type = 8)
rg12
##TSW
rg13 <- plot(gpg13, type = 8)
rg13
##FE
rg14 <- plot(gpg14, type = 8)
rg14
##ZN
rg15 <- plot(gpg15, type = 8)
rg15
##MN
rg16 <- plot(gpg16, type = 8)
rg16
##CU
rg17 <- plot(gpg17, type = 8)
rg17
##CA
rg18 <- plot(gpg18, type = 8)
rg18
##GYP
rg19 <- plot(gpg19, type = 8)
rg19
################ svp=symmetrical ##################
##DFF
gps1 <- gge(MET, ENVIRONMENT, GENOTYPE, DFF, svp = "symmetrical")
pgps1<- predict(gps1)
View(pgps1$DFF)
SVP1 <- (pgps1$DFF)
SVP1
write_xlsx(SVP1, "SVP1.xlsx")
##PH
gps2 <- gge(MET, ENVIRONMENT, GENOTYPE, PH, svp = "symmetrical")
pgps2<- predict(gps2)
View(pgps2$PH)
SVP2 <- (pgps2$PH)
SVP2
write_xlsx(SVP2, "SVP2.xlsx")
##FLL
gps3 <- gge(MET, ENVIRONMENT, GENOTYPE, LL, svp = "symmetrical")
pgps3<- predict(gps3)
View(pgps3$LL)
SVP3 <- (pgps3$LL)
SVP3
write_xlsx(SVP3, "SVP3.xlsx")
##FLW
gps4 <- gge(MET, ENVIRONMENT, GENOTYPE, LW, svp = "symmetrical")
pgps4<- predict(gps4)
View(pgps4$LW)
SVP4 <- (pgps4$LW)
SVP4
write_xlsx(SVP4, "SVP4.xlsx")
##LMT
gps5 <- gge(MET, ENVIRONMENT, GENOTYPE, LMT, svp = "symmetrical")
pgps5<- predict(gps5)
View(pgps5$LMT)
SVP5 <- (pgps5$LMT)
SVP5
write_xlsx(SVP5, "SVP5.xlsx")
##TP
gps6 <- gge(MET, ENVIRONMENT, GENOTYPE, TP, svp = "symmetrical")
pgps6<- predict(gps6)
View(pgps6$TP)
SVP6 <- (pgps6$TP)
SVP6
write_xlsx(SVP6, "SVP6.xlsx")
##PTP
gps7 <- gge(MET, ENVIRONMENT, GENOTYPE, PTP, svp = "symmetrical")
pgps7<- predict(gps7)
View(pgps7$PTP)
SVP7 <- (pgps7$PTP)
SVP7
write_xlsx(SVP7, "SVP7.xlsx")
##EL
gps8 <- gge(MET, ENVIRONMENT, GENOTYPE, EL, svp = "symmetrical")
pgps8<- predict(gps8)
View(pgps8$EL)
SVP8 <- (pgps8$EL)
SVP8
write_xlsx(SVP8, "SVP8.xlsx")
##FL
gps9 <- gge(MET, ENVIRONMENT, GENOTYPE, FL, svp = "symmetrical")
pgps9<- predict(gps9)
View(pgps9$FL)
SVP9 <- (pgps9$FL)
SVP9
write_xlsx(SVP9, "SVP9.xlsx")
##NFE
gps10 <- gge(MET, ENVIRONMENT, GENOTYPE, NFE, svp = "symmetrical")
pgps10<- predict(gps10)
View(pgps10$NFE)
SVP10 <- (pgps10$NFE)
SVP10
write_xlsx(SVP10, "SVP10.xlsx")
##FW
gps11 <- gge(MET, ENVIRONMENT, GENOTYPE, FW, svp = "symmetrical")
pgps11<- predict(gps11)
View(pgps11$FW)
SVP11 <- (pgps11$FW)
SVP11
write_xlsx(SVP11, "SVP11.xlsx")
##DM
gps12 <- gge(MET, ENVIRONMENT, GENOTYPE, DM, svp = "symmetrical")
pgps12<- predict(gps1)
View(pgps12$DM)
SVP12 <- (pgps12$DM)
SVP12
write_xlsx(SVP12, "SVP12.xlsx")
##TSW
gps13 <- gge(MET, ENVIRONMENT, GENOTYPE, TSW, svp = "symmetrical")
pgps13<- predict(gps13)
View(pgps13$TSW)
SVP13 <- (pgps13$TSW)
SVP13
write_xlsx(SVP13, "SVP13.xlsx")
##FE
gps14 <- gge(MET, ENVIRONMENT, GENOTYPE, FE, svp = "symmetrical")
pgps14<- predict(gps14)
View(pgps14$FE)
SVP14 <- (pgps14$FE)
SVP14
write_xlsx(SVP14, "SVP14.xlsx")
##ZN
gps15 <- gge(MET, ENVIRONMENT, GENOTYPE, ZN, svp = "symmetrical")
pgps15<- predict(gps15)
View(pgps15$ZN)
SVP15 <- (pgps15$ZN)
SVP15
write_xlsx(SVP15, "SVP15.xlsx")
##MN
gps16 <- gge(MET, ENVIRONMENT, GENOTYPE, MN, svp = "symmetrical")
pgps16<- predict(gps16)
View(pgps16$MN)
SVP16 <- (pgps16$MN)
SVP16
write_xlsx(SVP16, "SVP16.xlsx")
##CU
gps17 <- gge(MET, ENVIRONMENT, GENOTYPE, CU, svp = "symmetrical")
pgps17<- predict(gps17)
View(pgps17$CU)
SVP17 <- (pgps17$CU)
SVP17
write_xlsx(SVP17, "SVP17.xlsx")
##CA
gps18 <- gge(MET, ENVIRONMENT, GENOTYPE, CA, svp = "symmetrical")
pgps18<- predict(gps18)
View(pgps18$CA)
SVP18 <- (pgps18$CA)
SVP18
write_xlsx(SVP18, "SVP18.xlsx")
##GYP
gps19 <- gge(MET, ENVIRONMENT, GENOTYPE, GYP, svp = "symmetrical")
pgps19<- predict(gps19)
View(pgps19$GYP)
SVP19 <- (pgps19$GYP)
SVP19
write_xlsx(SVP19, "SVP19.xlsx")
#################### 8 which won where ######################
##DFF
www1 <- plot(gps1, type = 3)
www1
##PH
www2 <- plot(gps2, type = 3)
www2
##FLL
www3 <- plot(gps3, type = 3)
www3
##FLW
www4 <- plot(gps4, type = 3)
www4
##LMT
www5 <- plot(gps5, type = 3)
www5
##TP
www6 <- plot(gps6, type = 3)
www6
##PTP
www7 <- plot(gps7, type = 3)
www7
##EL
www8 <- plot(gps8, type = 3)
www8
##FL
www9 <- plot(gps9, type = 3)
www9
##NFE
www10 <- plot(gps10, type = 3)
www10
##FW
www11 <- plot(gps11, type = 3)
www11
##DM
www12 <- plot(gps12, type = 3)
www12
##TSW
www13 <- plot(gps13, type = 3)
www13
##FE
www14 <- plot(gps14, type = 3)
www14
##ZN
www15 <- plot(gps15, type = 3)
www15
##MN
www16 <- plot(gps16, type = 3)
www16
##CU
www17 <- plot(gps17, type = 3)
www17
##CA
www18 <- plot(gps18, type = 3)
www18
##GYP
www19 <- plot(gps19, type = 3)
www19
############################## 9 examine environment #################
##DFF
el1 <- plot(gps1, type = 5, sel_env = "E1")
el1
##PH
el2 <- plot(gps2, type = 5, sel_env = "E1")
el2
##FLL
el3 <- plot(gps3, type = 5, sel_env = "E1")
el3
##FLW
el4 <- plot(gps4, type = 5, sel_env = "E1")
el4
##LMT
el5 <- plot(gps5, type = 5, sel_env = "E1")
el5
##TP
el6 <- plot(gps6, type = 5, sel_env = "E1")
el6
##PTP
el7 <- plot(gps7, type = 5, sel_env = "E1")
el7
##EL
el8 <- plot(gps8, type = 5, sel_env = "E1")
el8
##FL
el9 <- plot(gps9, type = 5, sel_env = "E1")
el9
##NFE
el10 <- plot(gps10, type = 5, sel_env = "E1")
el10
##FW
el11 <- plot(gps11, type = 5, sel_env = "E1")
el11
##DM
el12 <- plot(gps12, type = 5, sel_env = "E1")
el12
##TSW
el13 <- plot(gps13, type = 5, sel_env = "E1")
el13
##FE
el14 <- plot(gps14, type = 5, sel_env = "E1")
el14
##ZN
el15 <- plot(gps15, type = 5, sel_env = "E1")
el15
##MN
el16 <- plot(gps16, type = 5, sel_env = "E1")
el16
##CU
el17 <- plot(gps17, type = 5, sel_env = "E1")
el17
##CA
el18 <- plot(gps18, type = 5, sel_env = "E1")
el18
##GYP
el19 <- plot(gps19, type = 5, sel_env = "E1")
el19
####################### 10 comparison between two genotypes ################
##DFF
CG1 <- plot(gps1, type = 9, sel_gen1 = "G2", sel_gen2 = "G3")
CG1
##PH
CG2 <- plot(gps2, type = 9, sel_gen1 = "G2", sel_gen2 = "G3")
CG2
##FLL
CG3 <- plot(gps3, type = 9, sel_gen1 = "G2", sel_gen2 = "G3")
CG3
##FLW
CG4 <- plot(gps4, type = 9, sel_gen1 = "G2", sel_gen2 = "G3")
CG4
##LMT
CG5 <- plot(gps5, type = 9, sel_gen1 = "G2", sel_gen2 = "G3")
CG5
##TP
CG6 <- plot(gps6, type = 9, sel_gen1 = "G2", sel_gen2 = "G3")
CG6
##PTP
CG7 <- plot(gps7, type = 9, sel_gen1 = "G2", sel_gen2 = "G3")
CG7
##EL
CG8 <- plot(gps8, type = 9, sel_gen1 = "G2", sel_gen2 = "G3")
CG8
##FL
CG9 <- plot(gps9, type = 9, sel_gen1 = "G2", sel_gen2 = "G3")
CG9
##NFE
CG10 <- plot(gps10, type = 9, sel_gen1 = "G2", sel_gen2 = "G3")
CG10
##FW
CG11 <- plot(gps11, type = 9, sel_gen1 = "G2", sel_gen2 = "G3")
CG11
##DM
CG12 <- plot(gps12, type = 9, sel_gen1 = "G2", sel_gen2 = "G3")
CG12
##TSW
CG13 <- plot(gps13, type = 9, sel_gen1 = "G2", sel_gen2 = "G3")
CG13
##FE
CG14 <- plot(gps14, type = 9, sel_gen1 = "G2", sel_gen2 = "G3")
CG14
##ZN
CG15 <- plot(gps15, type = 9, sel_gen1 = "G2", sel_gen2 = "G3")
CG15
##N
CG16 <- plot(gps16, type = 9, sel_gen1 = "G2", sel_gen2 = "G3")
CG16
##CU
CG17 <- plot(gps17, type = 9, sel_gen1 = "G2", sel_gen2 = "G3")
CG17
##CA
CG18 <- plot(gps18, type = 9, sel_gen1 = "G2", sel_gen2 = "G3")
CG18
##GYP
CG19 <- plot(gps19, type = 9, sel_gen1 = "G2", sel_gen2 = "G3")
CG19
############################################################################################