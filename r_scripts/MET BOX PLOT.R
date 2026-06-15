library(ggplot2)
library(agricolae)
library(dplyr)
######## GYP
MET$ENVIRONMENT <- as.factor(MET$ENVIRONMENT)
value_max = MET %>% group_by(ENVIRONMENT) %>% summarise(max_value = max(GYP))
hsd = HSD.test(aov(GYP~ENVIRONMENT,data = MET), trt = "ENVIRONMENT", group = T)
sig.letters <- hsd$groups[order(row.names(hsd$groups)),]
waka <- ggplot(data = MET, aes(x=ENVIRONMENT,y=GYP))+
  geom_boxplot(aes(fill=ENVIRONMENT))+geom_text(data = value_max, 
                                    aes(x=ENVIRONMENT,y=0.1+max_value,
                                      label=sig.letters$groups),vjust=0)+
                                         stat_boxplot(geom = 'errorbar', width = 0.1)+
                                            ggtitle("GRAIN YIELD PER PLANT")+xlab("ENVIRONMENT")+
                                              ylab("GRAIN YIELD PER PLANT (g)")
waka
######## CU
AMMI_DATA$ENVIRONMENT <- as.factor(AMMI_DATA$ENVIRONMENT)
value_max = AMMI_DATA %>% group_by(ENVIRONMENT) %>% summarise(max_value = max(CU))
hsd = HSD.test(aov(CU~ENVIRONMENT,data = AMMI_DATA), trt = "ENVIRONMENT", group = T)
sig.letters <- hsd$groups[order(row.names(hsd$groups)),]
waka <- ggplot(data = AMMI_DATA, aes(x=ENVIRONMENT,y=CU))+
  geom_boxplot(aes(fill=ENVIRONMENT))+geom_text(data = value_max, 
                                aes(x=ENVIRONMENT,y=0.1+max_value,
                                   label=sig.letters$groups),vjust=0)+
                                      stat_boxplot(geom = 'errorbar', width = 0.1)+
                                        ggtitle("GRAIN COPPER CONTENT ")+xlab("ENVIRONMENT")+
                                                  ylab(" GRAIN COPPER CONTENT (mg/100g)")
waka
######## CA
AMMI_DATA$ENVIRONMENT <- as.factor(AMMI_DATA$ENVIRONMENT)
value_max = AMMI_DATA %>% group_by(ENVIRONMENT) %>% summarise(max_value = max(CA))
hsd = HSD.test(aov(CA~ENVIRONMENT,data = AMMI_DATA), trt = "ENVIRONMENT", group = T)
sig.letters <- hsd$groups[order(row.names(hsd$groups)),]
waka <- ggplot(data = AMMI_DATA, aes(x=ENVIRONMENT,y=CA))+
  geom_boxplot(aes(fill=ENVIRONMENT))+geom_text(data = value_max, 
                                                aes(x=ENVIRONMENT,y=0.1+max_value,
                                                    label=sig.letters$groups),vjust=0)+
                                                       stat_boxplot(geom = 'errorbar', width = 0.1)+
                                                          ggtitle("GRAIN CALCIUM CONTENT ")+xlab("ENVIRONMENT")+
                                                               ylab(" GRAIN CALCIUM CONTENT (mg/100g)")
waka
######## MN
AMMI_DATA$ENVIRONMENT <- as.factor(AMMI_DATA$ENVIRONMENT)
value_max = AMMI_DATA %>% group_by(ENVIRONMENT) %>% summarise(max_value = max(MN))
hsd = HSD.test(aov(MN~ENVIRONMENT,data = AMMI_DATA), trt = "ENVIRONMENT", group = T)
sig.letters <- hsd$groups[order(row.names(hsd$groups)),]
waka <- ggplot(data = AMMI_DATA, aes(x=ENVIRONMENT,y=MN))+
  geom_boxplot(aes(fill=ENVIRONMENT))+geom_text(data = value_max, 
                                                aes(x=ENVIRONMENT,y=0.1+max_value,
                                                    label=sig.letters$groups),vjust=0)+
                                                      stat_boxplot(geom = 'errorbar', width = 0.1)+
                                                        ggtitle("GRAIN MANGANESE CONTENT ")+xlab("ENVIRONMENT")+
                                                             ylab(" GRAIN MANGANESE CONTENT (mg/100g)")
waka
######## ZN
AMMI_DATA$ENVIRONMENT <- as.factor(AMMI_DATA$ENVIRONMENT)
value_max = AMMI_DATA %>% group_by(ENVIRONMENT) %>% summarise(max_value = max(ZN))
hsd = HSD.test(aov(ZN~ENVIRONMENT,data = AMMI_DATA), trt = "ENVIRONMENT", group = T)
sig.letters <- hsd$groups[order(row.names(hsd$groups)),]
waka <- ggplot(data = AMMI_DATA, aes(x=ENVIRONMENT,y=ZN))+
  geom_boxplot(aes(fill=ENVIRONMENT))+geom_text(data = value_max, 
                                                aes(x=ENVIRONMENT,y=0.1+max_value,
                                                    label=sig.letters$groups),vjust=0)+
                                                      stat_boxplot(geom = 'errorbar', width = 0.1)+
                                                       ggtitle("GRAIN ZINC CONTENT ")+xlab("ENVIRONMENT")+
                                                          ylab(" GRAIN ZINC CONTENT (mg/100g)")
waka
######## FE
AMMI_DATA$ENVIRONMENT <- as.factor(AMMI_DATA$ENVIRONMENT)
value_max = AMMI_DATA %>% group_by(ENVIRONMENT) %>% summarise(max_value = max(FE))
hsd = HSD.test(aov(FE~ENVIRONMENT,data = AMMI_DATA), trt = "ENVIRONMENT", group = T)
sig.letters <- hsd$groups[order(row.names(hsd$groups)),]
waka <- ggplot(data = AMMI_DATA, aes(x=ENVIRONMENT,y=FE))+
  geom_boxplot(aes(fill=ENVIRONMENT))+geom_text(data = value_max, 
                                                aes(x=ENVIRONMENT,y=0.1+max_value,
                                                    label=sig.letters$groups),vjust=0)+
                                                      stat_boxplot(geom = 'errorbar', width = 0.1)+
                                                        ggtitle("GRAIN IRON CONTENT ")+xlab("ENVIRONMENT")+
                                                           ylab(" GRAIN IRON CONTENT (mg/100g)")
waka
######## TSW
AMMI_DATA$ENVIRONMENT <- as.factor(AMMI_DATA$ENVIRONMENT)
value_max = AMMI_DATA %>% group_by(ENVIRONMENT) %>% summarise(max_value = max(TSW))
hsd = HSD.test(aov(TSW~ENVIRONMENT,data = AMMI_DATA), trt = "ENVIRONMENT", group = T)
sig.letters <- hsd$groups[order(row.names(hsd$groups)),]
waka <- ggplot(data = AMMI_DATA, aes(x=ENVIRONMENT,y=TSW))+
  geom_boxplot(aes(fill=ENVIRONMENT))+geom_text(data = value_max, 
                                                aes(x=ENVIRONMENT,y=0.1+max_value,
                                                    label=sig.letters$groups),vjust=0)+
                                                       stat_boxplot(geom = 'errorbar', width = 0.1)+
                                                        ggtitle("THOUSAND GRAIN WEIGHT ")+xlab("ENVIRONMENT")+
                                                          ylab(" THOUSAND GRAIN WEIGHT (g)")
waka
######## DM
AMMI_DATA$ENVIRONMENT <- as.factor(AMMI_DATA$ENVIRONMENT)
value_max = AMMI_DATA %>% group_by(ENVIRONMENT) %>% summarise(max_value = max(DM))
hsd = HSD.test(aov(DM~ENVIRONMENT,data = AMMI_DATA), trt = "ENVIRONMENT", group = T)
sig.letters <- hsd$groups[order(row.names(hsd$groups)),]
waka <- ggplot(data = AMMI_DATA, aes(x=ENVIRONMENT,y=DM))+
  geom_boxplot(aes(fill=ENVIRONMENT))+geom_text(data = value_max, 
                                                aes(x=ENVIRONMENT,y=0.1+max_value,
                                                    label=sig.letters$groups),vjust=0)+
                                                      stat_boxplot(geom = 'errorbar', width = 0.1)+
                                                         ggtitle("DAYS TO MATURITY")+xlab("ENVIRONMENT")+
                                                                ylab("DAYS TO MATURITY")
waka
######## FW
AMMI_DATA$ENVIRONMENT <- as.factor(AMMI_DATA$ENVIRONMENT)
value_max = AMMI_DATA %>% group_by(ENVIRONMENT) %>% summarise(max_value = max(FW))
hsd = HSD.test(aov(FW~ENVIRONMENT,data = AMMI_DATA), trt = "ENVIRONMENT", group = T)
sig.letters <- hsd$groups[order(row.names(hsd$groups)),]
waka <- ggplot(data = AMMI_DATA, aes(x=ENVIRONMENT,y=FW))+
  geom_boxplot(aes(fill=ENVIRONMENT))+geom_text(data = value_max, 
                                                aes(x=ENVIRONMENT,y=0.1+max_value,
                                                    label=sig.letters$groups),vjust=0)+
                                                      stat_boxplot(geom = 'errorbar', width = 0.1)+
                                                        ggtitle("FINGER WIDTH")+xlab("ENVIRONMENT")+
                                                          ylab("FINGER WIDTH(cm)")
waka
######## NFE
AMMI_DATA$ENVIRONMENT <- as.factor(AMMI_DATA$ENVIRONMENT)
value_max = AMMI_DATA %>% group_by(ENVIRONMENT) %>% summarise(max_value = max(NFE))
hsd = HSD.test(aov(NFE~ENVIRONMENT,data = AMMI_DATA), trt = "ENVIRONMENT", group = T)
sig.letters <- hsd$groups[order(row.names(hsd$groups)),]
waka <- ggplot(data = AMMI_DATA, aes(x=ENVIRONMENT,y=NFE))+
  geom_boxplot(aes(fill=ENVIRONMENT))+geom_text(data = value_max, 
                                                aes(x=ENVIRONMENT,y=0.1+max_value,
                                                    label=sig.letters$groups),vjust=0)+
                                                      stat_boxplot(geom = 'errorbar', width = 0.1)+
                                                        ggtitle("FINGERS PER EAR")+xlab("ENVIRONMENT")+
                                                            ylab("FINGERS PER EAR")
waka
######## FL
AMMI_DATA$ENVIRONMENT <- as.factor(AMMI_DATA$ENVIRONMENT)
value_max = AMMI_DATA %>% group_by(ENVIRONMENT) %>% summarise(max_value = max(FL))
hsd = HSD.test(aov(FL~ENVIRONMENT,data = AMMI_DATA), trt = "ENVIRONMENT", group = T)
sig.letters <- hsd$groups[order(row.names(hsd$groups)),]
waka <- ggplot(data = AMMI_DATA, aes(x=ENVIRONMENT,y=FL))+
  geom_boxplot(aes(fill=ENVIRONMENT))+geom_text(data = value_max, 
                                                aes(x=ENVIRONMENT,y=0.1+max_value,
                                                    label=sig.letters$groups),vjust=0)+
                                                      stat_boxplot(geom = 'errorbar', width = 0.1)+
                                                        ggtitle("FINGER LENGTH")+xlab("ENVIRONMENT")+
                                                            ylab("FINGER LENGTH(cm)")
waka
######## EL
AMMI_DATA$ENVIRONMENT <- as.factor(AMMI_DATA$ENVIRONMENT)
value_max = AMMI_DATA %>% group_by(ENVIRONMENT) %>% summarise(max_value = max(EL))
hsd = HSD.test(aov(EL~ENVIRONMENT,data = AMMI_DATA), trt = "ENVIRONMENT", group = T)
sig.letters <- hsd$groups[order(row.names(hsd$groups)),]
waka <- ggplot(data = AMMI_DATA, aes(x=ENVIRONMENT,y=EL))+
  geom_boxplot(aes(fill=ENVIRONMENT))+geom_text(data = value_max, 
                                                aes(x=ENVIRONMENT,y=0.1+max_value,
                                                    label=sig.letters$groups),vjust=0)+
                                                      stat_boxplot(geom = 'errorbar', width = 0.1)+
                                                        ggtitle("EAR LENGTH")+xlab("ENVIRONMENT")+
                                                             ylab("EAR LENGTH(cm)")
waka
######## PTP
AMMI_DATA$ENVIRONMENT <- as.factor(AMMI_DATA$ENVIRONMENT)
value_max = AMMI_DATA %>% group_by(ENVIRONMENT) %>% summarise(max_value = max(PTP))
hsd = HSD.test(aov(PTP~ENVIRONMENT,data = AMMI_DATA), trt = "ENVIRONMENT", group = T)
sig.letters <- hsd$groups[order(row.names(hsd$groups)),]
waka <- ggplot(data = AMMI_DATA, aes(x=ENVIRONMENT,y=PTP))+
  geom_boxplot(aes(fill=ENVIRONMENT))+geom_text(data = value_max, 
                                                aes(x=ENVIRONMENT,y=0.1+max_value,
                                                    label=sig.letters$groups),vjust=0)+
                                                      stat_boxplot(geom = 'errorbar', width = 0.1)+
                                                        ggtitle("PRODUCTIVE TILLERS PER PLANT")+xlab("ENVIRONMENT")+
                                                            ylab("PRODUCTIVE TILLERS PER PLANT")
waka
######## TP
AMMI_DATA$ENVIRONMENT <- as.factor(AMMI_DATA$ENVIRONMENT)
value_max = AMMI_DATA %>% group_by(ENVIRONMENT) %>% summarise(max_value = max(TP))
hsd = HSD.test(aov(TP~ENVIRONMENT,data = AMMI_DATA), trt = "ENVIRONMENT", group = T)
sig.letters <- hsd$groups[order(row.names(hsd$groups)),]
waka <- ggplot(data = AMMI_DATA, aes(x=ENVIRONMENT,y=TP))+
  geom_boxplot(aes(fill=ENVIRONMENT))+geom_text(data = value_max, 
                                                aes(x=ENVIRONMENT,y=0.1+max_value,
                                                    label=sig.letters$groups),vjust=0)+
                                                       stat_boxplot(geom = 'errorbar', width = 0.1)+
                                                         ggtitle("TILLERS PER PLANT")+xlab("ENVIRONMENT")+
                                                           ylab("TILLERS PER PLANT")
waka
######## LMT
AMMI_DATA$ENVIRONMENT <- as.factor(AMMI_DATA$ENVIRONMENT)
value_max = AMMI_DATA %>% group_by(ENVIRONMENT) %>% summarise(max_value = max(LMT))
hsd = HSD.test(aov(LMT~ENVIRONMENT,data = AMMI_DATA), trt = "ENVIRONMENT", group = T)
sig.letters <- hsd$groups[order(row.names(hsd$groups)),]
waka <- ggplot(data = AMMI_DATA, aes(x=ENVIRONMENT,y=LMT))+
  geom_boxplot(aes(fill=ENVIRONMENT))+geom_text(data = value_max, 
                                                aes(x=ENVIRONMENT,y=0.1+max_value,
                                                    label=sig.letters$groups),vjust=0)+
                                                      stat_boxplot(geom = 'errorbar', width = 0.1)+
                                                        ggtitle("LEAVES PER MAIN TILLER")+xlab("ENVIRONMENT")+
                                                              ylab("LEAVES PER MAIN TILLER")
waka
######## FLW
AMMI_DATA$ENVIRONMENT <- as.factor(AMMI_DATA$ENVIRONMENT)
value_max = AMMI_DATA %>% group_by(ENVIRONMENT) %>% summarise(max_value = max(LW))
hsd = HSD.test(aov(LW~ENVIRONMENT,data = AMMI_DATA), trt = "ENVIRONMENT", group = T)
sig.letters <- hsd$groups[order(row.names(hsd$groups)),]
waka <- ggplot(data = AMMI_DATA, aes(x=ENVIRONMENT,y=LW))+
  geom_boxplot(aes(fill=ENVIRONMENT))+geom_text(data = value_max, 
                                                aes(x=ENVIRONMENT,y=0.1+max_value,
                                                    label=sig.letters$groups),vjust=0)+
                                                       stat_boxplot(geom = 'errorbar', width = 0.1)+
                                                         ggtitle("FLAG LEAF WIDTH")+xlab("ENVIRONMENT")+
                                                               ylab("FLAG LEAF WIDTH(cm)")
waka
######## FLL
AMMI_DATA$ENVIRONMENT <- as.factor(AMMI_DATA$ENVIRONMENT)
value_max = AMMI_DATA %>% group_by(ENVIRONMENT) %>% summarise(max_value = max(LL))
hsd = HSD.test(aov(LL~ENVIRONMENT,data = AMMI_DATA), trt = "ENVIRONMENT", group = T)
sig.letters <- hsd$groups[order(row.names(hsd$groups)),]
waka <- ggplot(data = AMMI_DATA, aes(x=ENVIRONMENT,y=LL))+
  geom_boxplot(aes(fill=ENVIRONMENT))+geom_text(data = value_max, 
                                                aes(x=ENVIRONMENT,y=0.1+max_value,
                                                    label=sig.letters$groups),vjust=0)+
                                                      stat_boxplot(geom = 'errorbar', width = 0.1)+
                                                        ggtitle("FLAG LEAF LENGTH")+xlab("ENVIRONMENT")+
                                                            ylab("FLAG LEAF LENGTH(cm)")
waka
######## PH
AMMI_DATA$ENVIRONMENT <- as.factor(AMMI_DATA$ENVIRONMENT)
value_max = AMMI_DATA %>% group_by(ENVIRONMENT) %>% summarise(max_value = max(PH))
hsd = HSD.test(aov(PH~ENVIRONMENT,data = AMMI_DATA), trt = "ENVIRONMENT", group = T)
sig.letters <- hsd$groups[order(row.names(hsd$groups)),]
waka <- ggplot(data = AMMI_DATA, aes(x=ENVIRONMENT,y=PH))+
  geom_boxplot(aes(fill=ENVIRONMENT))+geom_text(data = value_max, 
                                                aes(x=ENVIRONMENT,y=0.1+max_value,
                                                    label=sig.letters$groups),vjust=0)+
                                                      stat_boxplot(geom = 'errorbar', width = 0.1)+
                                                        ggtitle("PLANT HEIGHT")+xlab("ENVIRONMENT")+
                                                            ylab("PLANT HEIGHT(cm)")
waka
######## DFF
AMMI_DATA$ENVIRONMENT <- as.factor(AMMI_DATA$ENVIRONMENT)
value_max = AMMI_DATA %>% group_by(ENVIRONMENT) %>% summarise(max_value = max(DFF))
hsd = HSD.test(aov(DFF~ENVIRONMENT,data = AMMI_DATA), trt = "ENVIRONMENT", group = T)
sig.letters <- hsd$groups[order(row.names(hsd$groups)),]
waka <- ggplot(data = AMMI_DATA, aes(x=ENVIRONMENT,y=DFF))+
  geom_boxplot(aes(fill=ENVIRONMENT))+geom_text(data = value_max, 
                                                aes(x=ENVIRONMENT,y=0.1+max_value,
                                                    label=sig.letters$groups),vjust=0)+
                                                      stat_boxplot(geom = 'errorbar', width = 0.1)+
                                                          ggtitle("DAYS TO 50% FLOWERING")+xlab("ENVIRONMENT")+
                                                                  ylab("DAYS TO 50% FLOWERING")
waka
