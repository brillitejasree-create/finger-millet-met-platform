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


