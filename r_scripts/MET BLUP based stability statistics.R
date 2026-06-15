########### BLUP BASED STABILITY STATISTICS ###############
library(metan)
wako <- gamem_met(MET,ENVIRONMENT,GENOTYPE,REPLICATION,GYP,verbose = FALSE)
# DIAGNOSTIC PLOT FOR RESIDUALS
plot(wako)
# TO TEST NORMALITY OF THE RANDOM EFFECTS OF GENOTYPES$ENVIRONMENT
plot(wako,type = "re")
#likelihood ratio tests
mr <- get_model_data(wako,"lrt")
print(mr,n=50)
# variance components and genetic parameters
hert=get_model_data(wako)
print(hert)
write_xlsx(hert,"hert.xlsx")
# BLUP for genotypes
print(wako$GYP$BLUPgen)
print(wako$GYP$BLUPgen,n=50)
# plotting BLUP for genotypes
library(ggplot2)
a <- plot_blup(wako)
a
b <- plot_blup(wako,col.shape = c("blue","red"),plot_theme = theme_metan(grid = "y"))+coord_flip()
arrange_ggplot(a,b,tag_levels = "a")
#BLUP for GE combination
print(wako$GYP$BLUPint)
print(wako$GYP$BLUPint,n=400)
########## ALL STABILITY PARAMETERs MODELS VALUES AND RANKS
#GYP
model_GYP <- ge_stats(MET, ENVIRONMENT, GENOTYPE, REPLICATION, GYP)
get_model_data(model_GYP)
print(model_GYP,N=1000)
write_xlsx(model_GYP,"model_GYP.xlsx")
#ph
model_ph <- ge_stats(AMMI_DATA, ENVIRONMENT, GENOTYPE, REPLICATION, PH)
get_model_data(model_ph)
print(model_ph,N=1000)
write_xlsx(model_ph,"model_ph.xlsx")
#FLL
model_fll <- ge_stats(AMMI_DATA, ENVIRONMENT, GENOTYPE, REPLICATION, LL)
get_model_data(model_fll)
print(model_fll,N=1000)
write_xlsx(model_fll,"model_fll.xlsx")
#FLW
model_flw <- ge_stats(AMMI_DATA, ENVIRONMENT, GENOTYPE, REPLICATION, LW)
get_model_data(model_flw)
print(model_flw,N=1000)
write_xlsx(model_flw,"model_flw.xlsx")
#LMT
model_lmt <- ge_stats(AMMI_DATA, ENVIRONMENT, GENOTYPE, REPLICATION, LMT)
get_model_data(model_lmt)
print(model_lmt,N=1000)
write_xlsx(model_lmt,"model_lmt.xlsx")
#EL
model_el <- ge_stats(AMMI_DATA, ENVIRONMENT, GENOTYPE, REPLICATION, EL)
get_model_data(model_el)
print(model_el,N=1000)
write_xlsx(model_el,"model_lmt.xlsx")