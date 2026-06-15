library(metan)
library(ggplot2)
######### GYP
waasby <- waasb(MET,
                resp = GYP,
                gen = GENOTYPE,
                env = ENVIRONMENT,
                rep = REPLICATION)
waasby2 <- waas(MET,
                resp = GYP,
                gen = GENOTYPE,
                env = ENVIRONMENT,
                rep = REPLICATION)
plot_waasby(waasby)
plot_waasby(waasby2)+
  theme_gray()+
  theme(legend.position = "bottom",
        legend.background = element_blank(),
        legend.title = element_blank(),
        legend.direction = "horizontal")
########## CA
waasby <- waasb(AMMI_DATA,
                resp = CA,
                gen = GENOTYPE,
                env = ENVIRONMENT,
                rep = REPLICATION)
waasby2 <- waas(AMMI_DATA,
                resp = CA,
                gen = GENOTYPE,
                env = ENVIRONMENT,
                rep = REPLICATION)
plot_waasby(waasby)
plot_waasby(waasby2)+
  theme_gray()+
  theme(legend.position = "bottom",
        legend.background = element_blank(),
        legend.title = element_blank(),
        legend.direction = "horizontal")
########## CU
waasby <- waasb(AMMI_DATA,
                resp = CU,
                gen = GENOTYPE,
                env = ENVIRONMENT,
                rep = REPLICATION)
waasby2 <- waas(AMMI_DATA,
                resp = CU,
                gen = GENOTYPE,
                env = ENVIRONMENT,
                rep = REPLICATION)
plot_waasby(waasby)
plot_waasby(waasby2)+
  theme_gray()+
  theme(legend.position = "bottom",
        legend.background = element_blank(),
        legend.title = element_blank(),
        legend.direction = "horizontal")
########## MN
waasby <- waasb(AMMI_DATA,
                resp = MN,
                gen = GENOTYPE,
                env = ENVIRONMENT,
                rep = REPLICATION)
waasby2 <- waas(AMMI_DATA,
                resp = MN,
                gen = GENOTYPE,
                env = ENVIRONMENT,
                rep = REPLICATION)
plot_waasby(waasby)
plot_waasby(waasby2)+
  theme_gray()+
  theme(legend.position = "bottom",
        legend.background = element_blank(),
        legend.title = element_blank(),
        legend.direction = "horizontal")
########## ZN
waasby <- waasb(AMMI_DATA,
                resp = ZN,
                gen = GENOTYPE,
                env = ENVIRONMENT,
                rep = REPLICATION)
waasby2 <- waas(AMMI_DATA,
                resp = ZN,
                gen = GENOTYPE,
                env = ENVIRONMENT,
                rep = REPLICATION)
plot_waasby(waasby)
plot_waasby(waasby2)+
  theme_gray()+
  theme(legend.position = "bottom",
        legend.background = element_blank(),
        legend.title = element_blank(),
        legend.direction = "horizontal")
########## FE
waasby <- waasb(AMMI_DATA,
                resp = FE,
                gen = GENOTYPE,
                env = ENVIRONMENT,
                rep = REPLICATION)
waasby2 <- waas(AMMI_DATA,
                resp = FE,
                gen = GENOTYPE,
                env = ENVIRONMENT,
                rep = REPLICATION)
plot_waasby(waasby)
plot_waasby(waasby2)+
  theme_gray()+
  theme(legend.position = "bottom",
        legend.background = element_blank(),
        legend.title = element_blank(),
        legend.direction = "horizontal")
########## TSW
waasby <- waasb(AMMI_DATA,
                resp = TSW,
                gen = GENOTYPE,
                env = ENVIRONMENT,
                rep = REPLICATION)
waasby2 <- waas(AMMI_DATA,
                resp = TSW,
                gen = GENOTYPE,
                env = ENVIRONMENT,
                rep = REPLICATION)
plot_waasby(waasby)
plot_waasby(waasby2)+
  theme_gray()+
  theme(legend.position = "bottom",
        legend.background = element_blank(),
        legend.title = element_blank(),
        legend.direction = "horizontal")
########## DM
waasby <- waasb(AMMI_DATA,
                resp = DM,
                gen = GENOTYPE,
                env = ENVIRONMENT,
                rep = REPLICATION)
waasby2 <- waas(AMMI_DATA,
                resp = DM,
                gen = GENOTYPE,
                env = ENVIRONMENT,
                rep = REPLICATION)
plot_waasby(waasby)
plot_waasby(waasby2)+
  theme_gray()+
  theme(legend.position = "bottom",
        legend.background = element_blank(),
        legend.title = element_blank(),
        legend.direction = "horizontal")
########## FW
waasby <- waasb(AMMI_DATA,
                resp = FW,
                gen = GENOTYPE,
                env = ENVIRONMENT,
                rep = REPLICATION)
waasby2 <- waas(AMMI_DATA,
                resp = FW,
                gen = GENOTYPE,
                env = ENVIRONMENT,
                rep = REPLICATION)
plot_waasby(waasby)
plot_waasby(waasby2)+
  theme_gray()+
  theme(legend.position = "bottom",
        legend.background = element_blank(),
        legend.title = element_blank(),
        legend.direction = "horizontal")
########## NFE
waasby <- waasb(AMMI_DATA,
                resp = NFE,
                gen = GENOTYPE,
                env = ENVIRONMENT,
                rep = REPLICATION)
waasby2 <- waas(AMMI_DATA,
                resp = NFE,
                gen = GENOTYPE,
                env = ENVIRONMENT,
                rep = REPLICATION)
plot_waasby(waasby)
plot_waasby(waasby2)+
  theme_gray()+
  theme(legend.position = "bottom",
        legend.background = element_blank(),
        legend.title = element_blank(),
        legend.direction = "horizontal")
########## FL
waasby <- waasb(AMMI_DATA,
                resp = FL,
                gen = GENOTYPE,
                env = ENVIRONMENT,
                rep = REPLICATION)
waasby2 <- waas(AMMI_DATA,
                resp = FL,
                gen = GENOTYPE,
                env = ENVIRONMENT,
                rep = REPLICATION)
plot_waasby(waasby)
plot_waasby(waasby2)+
  theme_gray()+
  theme(legend.position = "bottom",
        legend.background = element_blank(),
        legend.title = element_blank(),
        legend.direction = "horizontal")
########## EL
waasby <- waasb(AMMI_DATA,
                resp = EL,
                gen = GENOTYPE,
                env = ENVIRONMENT,
                rep = REPLICATION)
waasby2 <- waas(AMMI_DATA,
                resp = EL,
                gen = GENOTYPE,
                env = ENVIRONMENT,
                rep = REPLICATION)
plot_waasby(waasby)
plot_waasby(waasby2)+
  theme_gray()+
  theme(legend.position = "bottom",
        legend.background = element_blank(),
        legend.title = element_blank(),
        legend.direction = "horizontal")
########## PTP
waasby <- waasb(AMMI_DATA,
                resp = PTP,
                gen = GENOTYPE,
                env = ENVIRONMENT,
                rep = REPLICATION)
waasby2 <- waas(AMMI_DATA,
                resp = PTP,
                gen = GENOTYPE,
                env = ENVIRONMENT,
                rep = REPLICATION)
plot_waasby(waasby)
plot_waasby(waasby2)+
  theme_gray()+
  theme(legend.position = "bottom",
        legend.background = element_blank(),
        legend.title = element_blank(),
        legend.direction = "horizontal")
########## TP
waasby <- waasb(AMMI_DATA,
                resp = TP,
                gen = GENOTYPE,
                env = ENVIRONMENT,
                rep = REPLICATION)
waasby2 <- waas(AMMI_DATA,
                resp = TP,
                gen = GENOTYPE,
                env = ENVIRONMENT,
                rep = REPLICATION)
plot_waasby(waasby)
plot_waasby(waasby2)+
  theme_gray()+
  theme(legend.position = "bottom",
        legend.background = element_blank(),
        legend.title = element_blank(),
        legend.direction = "horizontal")
########## LMT
waasby <- waasb(AMMI_DATA,
                resp = LMT,
                gen = GENOTYPE,
                env = ENVIRONMENT,
                rep = REPLICATION)
waasby2 <- waas(AMMI_DATA,
                resp = LMT,
                gen = GENOTYPE,
                env = ENVIRONMENT,
                rep = REPLICATION)
plot_waasby(waasby)
plot_waasby(waasby2)+
  theme_gray()+
  theme(legend.position = "bottom",
        legend.background = element_blank(),
        legend.title = element_blank(),
        legend.direction = "horizontal")
########## FLW
waasby <- waasb(AMMI_DATA,
                resp = LW,
                gen = GENOTYPE,
                env = ENVIRONMENT,
                rep = REPLICATION)
waasby2 <- waas(AMMI_DATA,
                resp = LW,
                gen = GENOTYPE,
                env = ENVIRONMENT,
                rep = REPLICATION)
plot_waasby(waasby)
plot_waasby(waasby2)+
  theme_gray()+
  theme(legend.position = "bottom",
        legend.background = element_blank(),
        legend.title = element_blank(),
        legend.direction = "horizontal")
########## LL
waasby <- waasb(AMMI_DATA,
                resp = LL,
                gen = GENOTYPE,
                env = ENVIRONMENT,
                rep = REPLICATION)
waasby2 <- waas(AMMI_DATA,
                resp = LL,
                gen = GENOTYPE,
                env = ENVIRONMENT,
                rep = REPLICATION)
plot_waasby(waasby)
plot_waasby(waasby2)+
  theme_gray()+
  theme(legend.position = "bottom",
        legend.background = element_blank(),
        legend.title = element_blank(),
        legend.direction = "horizontal")
########## PH
waasby <- waasb(AMMI_DATA,
                resp = PH,
                gen = GENOTYPE,
                env = ENVIRONMENT,
                rep = REPLICATION)
waasby2 <- waas(AMMI_DATA,
                resp = PH,
                gen = GENOTYPE,
                env = ENVIRONMENT,
                rep = REPLICATION)
plot_waasby(waasby)
plot_waasby(waasby2)+
  theme_gray()+
  theme(legend.position = "bottom",
        legend.background = element_blank(),
        legend.title = element_blank(),
        legend.direction = "horizontal")
########## DFF
waasby <- waasb(AMMI_DATA,
                resp = DFF,
                gen = GENOTYPE,
                env = ENVIRONMENT,
                rep = REPLICATION)
waasby2 <- waas(AMMI_DATA,
                resp = DFF,
                gen = GENOTYPE,
                env = ENVIRONMENT,
                rep = REPLICATION)
plot_waasby(waasby)
plot_waasby(waasby2)+
  theme_gray()+
  theme(legend.position = "bottom",
        legend.background = element_blank(),
        legend.title = element_blank(),
        legend.direction = "horizontal")
