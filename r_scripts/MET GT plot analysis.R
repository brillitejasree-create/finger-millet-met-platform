################ Genotype by trait biplot ##########################
library(metan)
library(readxl)
GT_MET <- MET
View(GT_MET)
modb <- gtb(GT_MET, GENOTYPE, resp = c('PH', 'LL', 'LW', 'LMT', 'DFF', 'TP', 'PTP', 
                                       'FL', 'EL', 'FW', 'NFE', 'TSW', 'DM', 'GYP'), centering = "trait", 
                                       scaling = "sd", svp = "trait")
print(modb)
plot(modb)

#####--------------- ###### Genotype by yield*trait biplot-------------------
mod <- gytb(GT_MET, GENOTYPE, GYP)
print(mod)
plot(mod)


plot(modb, type = 2, col.gen = "blue")
plot(modb, type = 3, col.gen = "blue")
plot(modb, type = 4, col.gen = "blue")
plot(modb, type = 6, col.gen = "blue")
plot(modb, type = 7, col.gen = "blue")
plot(modb, type = 8, col.gen = "blue")
plot(modb, type = 9, col.gen = "blue")
plot(modb, type = 10, col.gen = "blue")



