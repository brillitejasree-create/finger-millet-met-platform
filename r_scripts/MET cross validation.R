############# cross validation #################

# Ensure the 'metan' and 'dplyr' libraries are loaded
library(metan)
library(dplyr)

# Convert all relevant columns to factors
MET$ENVIRONMENT <- as.factor(MET$ENVIRONMENT)
MET$GENOTYPE <- as.factor(MET$GENOTYPE)
MET$REPLICATION <- as.factor(MET$REPLICATION)
MET$BLOCK <- as.factor(MET$BLOCK)

# Run the cv_blup function including blocks
model1 <- cv_ammi(MET,
                 env = ENVIRONMENT,
                 gen = GENOTYPE,
                 rep = REPLICATION,
                 resp = GYP,
                 block = BLOCK,    # Include block for alpha-lattice design
                 nboot = 200,
                 naxis = 2)
plot(model1)
model2 <- cv_ammif(MET,
                  env = ENVIRONMENT,
                  gen = GENOTYPE,
                  rep = REPLICATION,
                  resp = GYP,
                  block = BLOCK,
                  nboot = 200)
plot(model2)
model3 <- cv_blup(MET,
                 env = ENVIRONMENT,
                 gen = GENOTYPE,
                 rep = REPLICATION,
                 resp = GYP,
                 block = BLOCK,    # Include block for alpha-lattice design
                 nboot = 200,
                 random = "all")   # Treat all effects as random

plot(model3)


bind_means <- bind_cv(model2, model1,model3,bind = "means")
print(bind_means$RMSPD)
bind_ammi_blup <- bind_cv(model2, model1,model3)
bind_ammi_blup
plot(bind_ammi_blup)
