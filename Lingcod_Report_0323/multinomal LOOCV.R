###############################################################################
# Leave One Out Cross Validation
###############################################################################

library(magrittr)
library(ggplot2)
source("./Lingcod_Report_0323/functions.R")

int_boat <- readRDS("./Lingcod_Report_0323/Spatial Comps/int_boat.rds")

int_area <- 
  int_boat %>%
  dplyr::group_by(port, year, fleet, area) %>%
  dplyr::summarize(lH = sum(lH), E = sum(E)) %>%
  dplyr::ungroup()
dat_Homer <- lapply(2003:2023, function(x) make_jagsdatpred(int_area, "Homer", "lH", x))
dat_Kodiak <- lapply(2003:2023, function(x) make_jagsdatpred(int_area, "Kodiak", "lH", x))
dat_Seward <- lapply(2003:2023, function(x) make_jagsdatpred(int_area, "Seward", "lH", x))
dat_Valdez <- lapply(2003:2023, function(x) make_jagsdatpred(int_area, "Valdez", "lH", x))
dat_Whittier <- lapply(2003:2023, function(x) make_jagsdatpred(int_area, "Whittier", "lH", x))



#jags w overdisersion on each observation
#area mean
writeLines("
model{
  # priors:
  alpha[1] <- 0 #area baseline
  for (a in 2:A){alpha[a] ~ dnorm(0, 0.0001)}
  
  tau ~ dgamma(0.001, 0.001) # prior for mixed effect precision
  sd <- sqrt(1/tau)

  for(f in 1:2){ #loop around fleet
    for(y in 1:Y){ # loop around years
      count[y,f,1:A] ~ dmulti(q[y,f,1:A], M[y,f])
      for(a in 1:A){
        re[y,f,a] ~ dnorm(0, tau)
        q[y,f,a] <- phi[y,f,a]/sum(phi[y,f,])
        log(phi[y,f,a]) <- alpha[a] + re[y,f,a]
      }
    }
  }
  for(f in 1:2){ #loop around fleet
    pred_count[f,1:A] ~ dmulti(q_pred[f,1:A], pred_M[f])
      for(a in 1:A){
        re_pred[f,a] ~ dnorm(0, tau)
        q_pred[f,a] <- phi_pred[f,a]/sum(phi_pred[f,])
        log(phi_pred[f,a]) <- alpha[a] + re_pred[f,a]
      }
  }
}
", con="model_alpha_pred.txt")
modfile_alpha_pred <- 'model_alpha_pred.txt'

parameters_alpha_pred <- c("alpha", "sd", "q_pred", "pred")
ni <- 5E4; nb <- ni/2; nc <- 3; nt <- 100
'Hpost_alpha_pred_Homer' <- 
  lapply(dat_Homer, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_alpha_pred, 
                                     model.file = modfile_alpha_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
saveRDS(Hpost_alpha_pred_Homer, "./Lingcod_Report_0323/Interview post/Hpost_alpha_pred_Homer.rds")
Hpost_alpha_pred_Kodiak <- 
  lapply(dat_Kodiak, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_alpha_pred, 
                                     model.file = modfile_alpha_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
saveRDS(Hpost_alpha_pred_Kodiak, "./Lingcod_Report_0323/Interview post/Hpost_alpha_pred_Kodiak.rds")

Hpost_alpha_pred_Seward <- 
  lapply(dat_Seward, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_alpha_pred, 
                                     model.file = modfile_alpha_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
saveRDS(Hpost_alpha_pred_Seward, "./Lingcod_Report_0323/Interview post/Hpost_alpha_pred_Seward.rds")

Hpost_alpha_pred_Valdez <- 
  lapply(dat_Valdez, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_alpha_pred, 
                                     model.file = modfile_alpha_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
saveRDS(Hpost_alpha_pred_Valdez, "./Lingcod_Report_0323/Interview post/Hpost_alpha_pred_Valdez.rds")

Hpost_alpha_pred_Whittier <- 
  lapply(dat_Whittier, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_alpha_pred, 
                                     model.file = modfile_alpha_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
saveRDS(Hpost_alpha_pred_Whittier, "./Lingcod_Report_0323/Interview post/Hpost_alpha_pred_Whittier.rds")



#fleet/area mean
writeLines("
model{
  # priors:
  alpha[1] <- 0 #area baseline
  for (a in 2:A){alpha[a] ~ dnorm(0, 0.0001)}
  
  for (a in 1:A){beta[1,a] <- 0 } #area baseline
  beta[2, 1] <- 0 ; #fleet baseline
  for (a in 2:A){ beta[2,a] ~ dnorm(0, 0.0001)}
  
  tau ~ dgamma(0.001, 0.001) # prior for mixed effect precision
  sd <- sqrt(1/tau)

  for(f in 1:2){ #loop around fleet
    for(y in 1:Y){ # loop around years
      count[y,f,1:A] ~ dmulti(q[y,f,1:A], M[y,f])
      for(a in 1:A){
        re[y,f,a] ~ dnorm(0, tau)
        q[y,f,a] <- phi[y,f,a]/sum(phi[y,f,])
        log(phi[y,f,a]) <- alpha[a] + beta[f,a] + re[y,f,a]
      }
    }
  }
  for(f in 1:2){ #loop around fleet
    pred_count[f,1:A] ~ dmulti(q_pred[f,1:A], pred_M[f])
      for(a in 1:A){
        re_pred[f,a] ~ dnorm(0, tau)
        q_pred[f,a] <- phi_pred[f,a]/sum(phi_pred[f,])
        log(phi_pred[f,a]) <- alpha[a] + beta[pred_fleet[f] + 1,a] + re_pred[f,a]
      }
  }
}
", con="model_beta_pred.txt")
modfile_beta_pred <- 'model_beta_pred.txt'

parameters_beta_pred <- c("alpha", "beta", "sd", "q_pred", "pred")
Hpost_beta_pred_Homer <- 
  lapply(dat_Homer, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_beta_pred, 
                                     model.file = modfile_beta_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
saveRDS(Hpost_beta_pred_Homer, "./Lingcod_Report_0323/Interview post/Hpost_beta_pred_Homer.rds")

Hpost_beta_pred_Kodiak <- 
  lapply(dat_Kodiak, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_beta_pred, 
                                     model.file = modfile_beta_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
saveRDS(Hpost_beta_pred_Kodiak, "./Lingcod_Report_0323/Interview post/Hpost_beta_pred_Kodiak.rds")

Hpost_beta_pred_Seward <- 
  lapply(dat_Seward, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_beta_pred, 
                                     model.file = modfile_beta_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
saveRDS(Hpost_beta_pred_Seward, "./Lingcod_Report_0323/Interview post/Hpost_beta_pred_Seward.rds")

Hpost_beta_pred_Valdez <- 
  lapply(dat_Valdez, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_beta_pred, 
                                     model.file = modfile_beta_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
saveRDS(Hpost_beta_pred_Valdez, "./Lingcod_Report_0323/Interview post/Hpost_beta_pred_Valdez.rds")

Hpost_beta_pred_Whittier <- 
  lapply(dat_Whittier, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_beta_pred, 
                                     model.file = modfile_beta_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
saveRDS(Hpost_beta_pred_Whittier, "./Lingcod_Report_0323/Interview post/Hpost_beta_pred_Whittier.rds")





#area slopes
writeLines("
model{
  # priors:
  alpha[1] <- 0 #area baseline
  for (a in 2:A){alpha[a] ~ dnorm(0, 0.0001)}
  
  for (a in 1:A){beta[1,a] <- 0 } #area baseline
  beta[2, 1] <- 0 ; #fleet baseline
  for (a in 2:A){ beta[2,a] ~ dnorm(0, 0.0001)}

  epsilon[1] <- 0 #area baseline
  for (a in 2:A){epsilon[a] ~ dnorm(0, 0.0001)}
  
  tau ~ dgamma(0.001, 0.001) # prior for mixed effect precision
  sd <- sqrt(1/tau)

  for(f in 1:2){ #loop around fleet
    for(y in 1:Y){ # loop around years
      count[y,f,1:A] ~ dmulti(q[y,f,1:A], M[y,f])
      for(a in 1:A){
        re[y,f,a] ~ dnorm(0, tau)
        q[y,f,a] <- phi[y,f,a]/sum(phi[y,f,])
        log(phi[y,f,a]) <- alpha[a] + beta[f,a] + epsilon[a]*yearc[y] + re[y,f,a]
      }
    }
  }
  for(f in 1:2){ #loop around fleet
    pred_count[f,1:A] ~ dmulti(q_pred[f,1:A], pred_M[f])
      for(a in 1:A){
        re_pred[f,a] ~ dnorm(0, tau)
        q_pred[f,a] <- phi_pred[f,a]/sum(phi_pred[f,])
        log(phi_pred[f,a]) <- alpha[a] + beta[pred_fleet[f] + 1,a] + epsilon[a]*pred_year[f] + re_pred[f,a]
      }
  }
}
", con="model_epsilon_pred.txt")
modfile_epsilon_pred <- 'model_epsilon_pred.txt'

parameters_epsilon_pred <- c("alpha", "beta", "epsilon", "sd", "q_pred", "pred")
Hpost_epsilon_pred_Homer <- 
  lapply(dat_Homer, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_epsilon_pred, 
                                     model.file = modfile_epsilon_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
saveRDS(Hpost_epsilon_pred_Homer, "./Lingcod_Report_0323/Interview post/Hpost_epsilon_pred_Homer.rds")

Hpost_epsilon_pred_Kodiak <- 
  lapply(dat_Kodiak, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_epsilon_pred, 
                                     model.file = modfile_epsilon_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
saveRDS(Hpost_epsilon_pred_Kodiak, "./Lingcod_Report_0323/Interview post/Hpost_epsilon_pred_Kodiak.rds")

Hpost_epsilon_pred_Seward <- 
  lapply(dat_Seward, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_epsilon_pred, 
                                     model.file = modfile_epsilon_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
saveRDS(Hpost_epsilon_pred_Seward, "./Lingcod_Report_0323/Interview post/Hpost_epsilon_pred_Seward.rds")

Hpost_epsilon_pred_Valdez <- 
  lapply(dat_Valdez, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_epsilon_pred, 
                                     model.file = modfile_epsilon_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
saveRDS(Hpost_epsilon_pred_Valdez, "./Lingcod_Report_0323/Interview post/Hpost_epsilon_pred_Valdez.rds")

Hpost_epsilon_pred_Whittier <- 
  lapply(dat_Whittier, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_epsilon_pred, 
                                     model.file = modfile_epsilon_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
saveRDS(Hpost_epsilon_pred_Whittier, "./Lingcod_Report_0323/Interview post/Hpost_epsilon_pred_Whittier.rds")



writeLines("
model{
  # priors:
  alpha[1] <- 0 #area baseline
  for (a in 2:A){alpha[a] ~ dnorm(0, 0.0001)}
  
  for (a in 1:A){beta[1,a] <- 0 } #area baseline
  beta[2, 1] <- 0 ; #fleet baseline
  for (a in 2:A){ beta[2,a] ~ dnorm(0, 0.0001)}

  # gamma[1] <- 0 #area baseline
  # for (a in 2:A){gamma[a] ~ dnorm(0, 0.0001)}
  
  epsilon[1] <- 0 #area baseline
  for (a in 2:A){epsilon[a] ~ dnorm(0, 0.0001)}
  
  for (a in 1:A){gamma[1,a] <- 0 } #area baseline
  gamma[2,1] <- 0 ; #fleet baseline
  for (a in 2:A){ gamma[2,a] ~ dnorm(0, 0.0001)}
  
  tau ~ dgamma(0.001, 0.001) # prior for mixed effect precision
  sd <- sqrt(1/tau)

  for(f in 1:2){ #loop around fleet
    for(y in 1:Y){ # loop around years
      count[y,f,1:A] ~ dmulti(q[y,f,1:A], M[y,f])
      for(a in 1:A){
        re[y,f,a] ~ dnorm(0, tau)
        q[y,f,a] <- phi[y,f,a]/sum(phi[y,f,])
        log(phi[y,f,a]) <- alpha[a] + beta[f,a] + epsilon[a]*yearc[y] + gamma[f,a]*yearc[y] + re[y,f,a]
      }
    }
  }
  for(f in 1:2){ #loop around fleet
    pred_count[f,1:A] ~ dmulti(q_pred[f,1:A], pred_M[f])
      for(a in 1:A){
        re_pred[f,a] ~ dnorm(0, tau)
        q_pred[f,a] <- phi_pred[f,a]/sum(phi_pred[f,])
        log(phi_pred[f,a]) <- alpha[a] + beta[pred_fleet[f] + 1,a] + epsilon[a]*pred_year[f] + gamma[pred_fleet[f] + 1,a]*pred_year[f] + re_pred[f,a]
      }
  }
}
", con="model_gamma_pred.txt")
modfile_gamma_pred <- 'model_gamma_pred.txt'

parameters_gamma_pred <- c("alpha", "beta", "epsilon", "gamma", "sd", "q_pred", "pred")
Hpost_gamma_pred_Homer <- 
  lapply(dat_Homer, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_gamma_pred, 
                                     model.file = modfile_gamma_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
saveRDS(Hpost_gamma_pred_Homer, "./Lingcod_Report_0323/Interview post/Hpost_gamma_pred_Homer.rds")

Hpost_gamma_pred_Kodiak <- 
  lapply(dat_Kodiak, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_gamma_pred, 
                                     model.file = modfile_gamma_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
saveRDS(Hpost_gamma_pred_Kodiak, "./Lingcod_Report_0323/Interview post/Hpost_gamma_pred_Kodiak.rds")

Hpost_gamma_pred_Seward <- 
  lapply(dat_Seward, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_gamma_pred, 
                                     model.file = modfile_gamma_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
saveRDS(Hpost_gamma_pred_Seward, "./Lingcod_Report_0323/Interview post/Hpost_gamma_pred_Seward.rds")

Hpost_gamma_pred_Valdez <- 
  lapply(dat_Valdez, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_gamma_pred, 
                                     model.file = modfile_gamma_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
saveRDS(Hpost_gamma_pred_Valdez, "./Lingcod_Report_0323/Interview post/Hpost_gamma_pred_Valdez.rds")

Hpost_gamma_pred_Whittier <- 
  lapply(dat_Whittier, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_gamma_pred, 
                                     model.file = modfile_gamma_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
saveRDS(Hpost_gamma_pred_Whittier, "./Lingcod_Report_0323/Interview post/Hpost_gamma_pred_Whittier.rds")




### 
# Model Selection 
Hpost_alpha_pred_Homer <- readRDS("./Lingcod_Report_0323/Interview post/Hpost_alpha_pred_Homer.rds")
Hpost_alpha_pred_Kodiak <- readRDS("./Lingcod_Report_0323/Interview post/Hpost_alpha_pred_Kodiak.rds")
Hpost_alpha_pred_Seward <- readRDS("./Lingcod_Report_0323/Interview post/Hpost_alpha_pred_Seward.rds")
Hpost_alpha_pred_Valdez <- readRDS("./Lingcod_Report_0323/Interview post/Hpost_alpha_pred_Valdez.rds")
Hpost_alpha_pred_Whittier <- readRDS("./Lingcod_Report_0323/Interview post/Hpost_alpha_pred_Whittier.rds")
Hpost_beta_pred_Homer <- readRDS("./Lingcod_Report_0323/Interview post/Hpost_beta_pred_Homer.rds")
Hpost_beta_pred_Kodiak <- readRDS("./Lingcod_Report_0323/Interview post/Hpost_beta_pred_Kodiak.rds")
Hpost_beta_pred_Seward <- readRDS("./Lingcod_Report_0323/Interview post/Hpost_beta_pred_Seward.rds")
Hpost_beta_pred_Valdez <- readRDS("./Lingcod_Report_0323/Interview post/Hpost_beta_pred_Valdez.rds")
Hpost_beta_pred_Whittier <- readRDS("./Lingcod_Report_0323/Interview post/Hpost_beta_pred_Whittier.rds")
Hpost_epsilon_pred_Homer <- readRDS("./Lingcod_Report_0323/Interview post/Hpost_epsilon_pred_Homer.rds")
Hpost_epsilon_pred_Kodiak <- readRDS("./Lingcod_Report_0323/Interview post/Hpost_epsilon_pred_Kodiak.rds")
Hpost_epsilon_pred_Seward <- readRDS("./Lingcod_Report_0323/Interview post/Hpost_epsilon_pred_Seward.rds")
Hpost_epsilon_pred_Valdez <- readRDS("./Lingcod_Report_0323/Interview post/Hpost_epsilon_pred_Valdez.rds")
Hpost_epsilon_pred_Whittier <- readRDS("./Lingcod_Report_0323/Interview post/Hpost_epsilon_pred_Whittier.rds")
Hpost_gamma_pred_Homer <- readRDS("./Lingcod_Report_0323/Interview post/Hpost_gamma_pred_Homer.rds")
Hpost_gamma_pred_Kodiak <- readRDS("./Lingcod_Report_0323/Interview post/Hpost_gamma_pred_Kodiak.rds")
Hpost_gamma_pred_Seward <- readRDS("./Lingcod_Report_0323/Interview post/Hpost_gamma_pred_Seward.rds")
Hpost_gamma_pred_Valdez <- readRDS("./Lingcod_Report_0323/Interview post/Hpost_gamma_pred_Valdez.rds")
Hpost_gamma_pred_Whittier <- readRDS("./Lingcod_Report_0323/Interview post/Hpost_gamma_pred_Whittier.rds")


#  * ll calcs -------------------------------------------------------------


Hll_Homer <- 
  list(alpha = mapply(get_llpred, pred = Hpost_alpha_pred_Homer, obs = dat_Homer),
       beta = mapply(get_llpred, pred = Hpost_beta_pred_Homer, obs = dat_Homer),
       epsilon = mapply(get_llpred, pred = Hpost_epsilon_pred_Homer, obs = dat_Homer),
       gamma = mapply(get_llpred, pred = Hpost_gamma_pred_Homer, obs = dat_Homer))

Hll_Kodiak <- 
  list(alpha = mapply(get_llpred, pred = Hpost_alpha_pred_Kodiak, obs = dat_Kodiak),
       beta = mapply(get_llpred, pred = Hpost_beta_pred_Kodiak, obs = dat_Kodiak),
       epsilon = mapply(get_llpred, pred = Hpost_epsilon_pred_Kodiak, obs = dat_Kodiak),
       gamma = mapply(get_llpred, pred = Hpost_gamma_pred_Kodiak, obs = dat_Kodiak))

Hll_Seward <- 
  list(alpha = mapply(get_llpred, pred = Hpost_alpha_pred_Seward, obs = dat_Seward),
       beta = mapply(get_llpred, pred = Hpost_beta_pred_Seward, obs = dat_Seward),
       epsilon = mapply(get_llpred, pred = Hpost_epsilon_pred_Seward, obs = dat_Seward),
       gamma = mapply(get_llpred, pred = Hpost_gamma_pred_Seward, obs = dat_Seward))

Hll_Valdez <- 
  list(alpha = mapply(get_llpred, pred = Hpost_alpha_pred_Valdez, obs = dat_Valdez),
       beta = mapply(get_llpred, pred = Hpost_beta_pred_Valdez, obs = dat_Valdez),
       epsilon = mapply(get_llpred, pred = Hpost_epsilon_pred_Valdez, obs = dat_Valdez),
       gamma = mapply(get_llpred, pred = Hpost_gamma_pred_Valdez, obs = dat_Valdez))

Hll_Whittier <- 
  list(alpha = mapply(get_llpred, pred = Hpost_alpha_pred_Whittier, obs = dat_Whittier),
       beta = mapply(get_llpred, pred = Hpost_beta_pred_Whittier, obs = dat_Whittier),
       epsilon = mapply(get_llpred, pred = Hpost_epsilon_pred_Whittier, obs = dat_Whittier),
       gamma = mapply(get_llpred, pred = Hpost_gamma_pred_Whittier, obs = dat_Whittier))

ll_H <- list(Hll_Homer, Hll_Kodiak, Hll_Seward, Hll_Valdez, Hll_Whittier)
saveRDS(ll_H, "./Lingcod_Report_0323/Data/ll_H.rds")



#high sd predictions are BAD!
as.data.frame(Hpost_alpha_pred_Homer[[1]]$sims.list$q_pred) %>%
  tidyr::pivot_longer(cols = where(is.numeric)) %>%
  ggplot(aes(x = value)) + geom_histogram() + facet_grid(.~name)
#In those situations the median wi better than the mean
apply(Hpost_alpha_pred_Homer[[1]]$sims.list$q_pred, 2, median)
apply(Hpost_alpha_pred_Homer[[1]]$sims.list$q_pred, 2, mean)

#Better if estiamte is near edge of support.
as.data.frame(Hpost_beta_pred_Kodiak[[1]]$sims.list$q_pred) %>%
  tidyr::pivot_longer(cols = where(is.numeric)) %>%
  ggplot(aes(x = value)) + geom_histogram() + facet_grid(.~name)
#In those situations the median wi better than the mean
apply(Hpost_beta_pred_Kodiak[[1]]$sims.list$q_pred, c(2, 3), median)
apply(Hpost_beta_pred_Kodiak[[1]]$sims.list$q_pred, c(2, 3), mean)

#Good w low sd.
as.data.frame(Hpost_epsilon_pred_Valdez[[10]]$sims.list$q_pred) %>%
  tidyr::pivot_longer(cols = where(is.numeric)) %>%
  ggplot(aes(x = value)) + geom_histogram() + facet_grid(.~name)
#In those situations the median wi better than the mean
apply(Hpost_epsilon_pred_Valdez[[10]]$sims.list$q_pred, c(2, 3), median)
apply(Hpost_epsilon_pred_Valdez[[10]]$sims.list$q_pred, c(2, 3), mean)

#Medium-high sd predictions highly influenced by support.
as.data.frame(Hpost_epsilon_pred_Seward[[10]]$sims.list$q_pred) %>%
  tidyr::pivot_longer(cols = where(is.numeric)) %>%
  ggplot(aes(x = value)) + geom_histogram() + facet_grid(.~name)
#In those situations the median wi better than the mean
apply(Hpost_epsilon_pred_Seward[[10]]$sims.list$q_pred, c(2, 3), median)
apply(Hpost_epsilon_pred_Seward[[10]]$sims.list$q_pred, c(2, 3), mean)


################################################################################
# Effort
#
# Selecting non-Halibut groundfish harvest
# Limited to only the open lingcod season (Starting July 1)
################################################################################
int_boat <- readRDS("./Lingcod_Report_0323/int_boat.rds")
source("./Lingcod_Report_0323/functions.R")
library(magrittr)
library(ggplot2)

int_area_e <- 
  int_boat %>%
  dplyr::filter(month >= 7, target %in% c("Rockfish", "Bottomfish", "Bottomfish & Salmon", "Lingcod")) %>% 
  dplyr::group_by(port, year, fleet, area) %>%
  dplyr::summarize(lH = sum(lH), E = sum(E)) %>%
  dplyr::ungroup()
dat_Homer_e <- lapply(2003:2023, function(x) make_jagsdatpred(int_area_e, "Homer", "E", x))
dat_Kodiak_e <- lapply(2003:2023, function(x) make_jagsdatpred(int_area_e, "Kodiak", "E", x))
dat_Seward_e <- lapply(2003:2023, function(x) make_jagsdatpred(int_area_e, "Seward", "E", x))
dat_Valdez_e <- lapply(2003:2023, function(x) make_jagsdatpred(int_area_e, "Valdez", "E", x))
dat_Whittier_e <- lapply(2003:2023, function(x) make_jagsdatpred(int_area_e, "Whittier", "lH", x))



#jags w overdisersion on each observation
#area mean
writeLines("
model{
  # priors:
  alpha[1] <- 0 #area baseline
  for (a in 2:A){alpha[a] ~ dnorm(0, 0.0001)}
  
  tau ~ dgamma(0.001, 0.001) # prior for mixed effect precision
  sd <- sqrt(1/tau)

  for(f in 1:2){ #loop around fleet
    for(y in 1:Y){ # loop around years
      count[y,f,1:A] ~ dmulti(q[y,f,1:A], M[y,f])
      for(a in 1:A){
        re[y,f,a] ~ dnorm(0, tau)
        q[y,f,a] <- phi[y,f,a]/sum(phi[y,f,])
        log(phi[y,f,a]) <- alpha[a] + re[y,f,a]
      }
    }
  }
  for(f in 1:2){ #loop around fleet
    pred_count[f,1:A] ~ dmulti(q_pred[f,1:A], pred_M[f])
      for(a in 1:A){
        re_pred[f,a] ~ dnorm(0, tau)
        q_pred[f,a] <- phi_pred[f,a]/sum(phi_pred[f,])
        log(phi_pred[f,a]) <- alpha[a] + re_pred[f,a]
      }
  }
}
", con="model_alpha_pred.txt")
modfile_alpha_pred <- 'model_alpha_pred.txt'

parameters_alpha_pred <- c("alpha", "sd", "q_pred", "pred")
ni <- 5E4; nb <- ni/2; nc <- 3; nt <- 100
'Epost_alpha_pred_Homer' <- 
  lapply(dat_Homer_e, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_alpha_pred, 
                                     model.file = modfile_alpha_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
saveRDS(Epost_alpha_pred_Homer, "./Lingcod_Report_0323/Interview post/Epost_alpha_pred_Homer.rds")
Epost_alpha_pred_Kodiak <- 
  lapply(dat_Kodiak_e, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_alpha_pred, 
                                     model.file = modfile_alpha_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
saveRDS(Epost_alpha_pred_Kodiak, "./Lingcod_Report_0323/Interview post/Epost_alpha_pred_Kodiak.rds")

Epost_alpha_pred_Seward <- 
  lapply(dat_Seward_e, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_alpha_pred, 
                                     model.file = modfile_alpha_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
saveRDS(Epost_alpha_pred_Seward, "./Lingcod_Report_0323/Interview post/Epost_alpha_pred_Seward.rds")

Epost_alpha_pred_Valdez <- 
  lapply(dat_Valdez_e, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_alpha_pred, 
                                     model.file = modfile_alpha_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
saveRDS(Epost_alpha_pred_Valdez, "./Lingcod_Report_0323/Interview post/Epost_alpha_pred_Valdez.rds")

Epost_alpha_pred_Whittier <- 
  lapply(dat_Whittier_e, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_alpha_pred, 
                                     model.file = modfile_alpha_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
saveRDS(Epost_alpha_pred_Whittier, "./Lingcod_Report_0323/Interview post/Epost_alpha_pred_Whittier.rds")



#fleet/area mean
writeLines("
model{
  # priors:
  alpha[1] <- 0 #area baseline
  for (a in 2:A){alpha[a] ~ dnorm(0, 0.0001)}
  
  for (a in 1:A){beta[1,a] <- 0 } #area baseline
  beta[2, 1] <- 0 ; #fleet baseline
  for (a in 2:A){ beta[2,a] ~ dnorm(0, 0.0001)}
  
  tau ~ dgamma(0.001, 0.001) # prior for mixed effect precision
  sd <- sqrt(1/tau)

  for(f in 1:2){ #loop around fleet
    for(y in 1:Y){ # loop around years
      count[y,f,1:A] ~ dmulti(q[y,f,1:A], M[y,f])
      for(a in 1:A){
        re[y,f,a] ~ dnorm(0, tau)
        q[y,f,a] <- phi[y,f,a]/sum(phi[y,f,])
        log(phi[y,f,a]) <- alpha[a] + beta[f,a] + re[y,f,a]
      }
    }
  }
  for(f in 1:2){ #loop around fleet
    pred_count[f,1:A] ~ dmulti(q_pred[f,1:A], pred_M[f])
      for(a in 1:A){
        re_pred[f,a] ~ dnorm(0, tau)
        q_pred[f,a] <- phi_pred[f,a]/sum(phi_pred[f,])
        log(phi_pred[f,a]) <- alpha[a] + beta[pred_fleet[f] + 1,a] + re_pred[f,a]
      }
  }
}
", con="model_beta_pred.txt")
modfile_beta_pred <- 'model_beta_pred.txt'

parameters_beta_pred <- c("alpha", "beta", "sd", "q_pred", "pred")
Epost_beta_pred_Homer <- 
  lapply(dat_Homer_e, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_beta_pred, 
                                     model.file = modfile_beta_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
saveRDS(Epost_beta_pred_Homer, "./Lingcod_Report_0323/Interview post/Epost_beta_pred_Homer.rds")

Epost_beta_pred_Kodiak <- 
  lapply(dat_Kodiak_e, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_beta_pred, 
                                     model.file = modfile_beta_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
saveRDS(Epost_beta_pred_Kodiak, "./Lingcod_Report_0323/Interview post/Epost_beta_pred_Kodiak.rds")

Epost_beta_pred_Seward <- 
  lapply(dat_Seward_e, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_beta_pred, 
                                     model.file = modfile_beta_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
saveRDS(Epost_beta_pred_Seward, "./Lingcod_Report_0323/Interview post/Epost_beta_pred_Seward.rds")

Epost_beta_pred_Valdez <- 
  lapply(dat_Valdez_e, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_beta_pred, 
                                     model.file = modfile_beta_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
saveRDS(Epost_beta_pred_Valdez, "./Lingcod_Report_0323/Interview post/Epost_beta_pred_Valdez.rds")

Epost_beta_pred_Whittier <- 
  lapply(dat_Whittier_e, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_beta_pred, 
                                     model.file = modfile_beta_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
saveRDS(Epost_beta_pred_Whittier, "./Lingcod_Report_0323/Interview post/Epost_beta_pred_Whittier.rds")





#area slopes
writeLines("
model{
  # priors:
  alpha[1] <- 0 #area baseline
  for (a in 2:A){alpha[a] ~ dnorm(0, 0.0001)}
  
  for (a in 1:A){beta[1,a] <- 0 } #area baseline
  beta[2, 1] <- 0 ; #fleet baseline
  for (a in 2:A){ beta[2,a] ~ dnorm(0, 0.0001)}

  epsilon[1] <- 0 #area baseline
  for (a in 2:A){epsilon[a] ~ dnorm(0, 0.0001)}
  
  tau ~ dgamma(0.001, 0.001) # prior for mixed effect precision
  sd <- sqrt(1/tau)

  for(f in 1:2){ #loop around fleet
    for(y in 1:Y){ # loop around years
      count[y,f,1:A] ~ dmulti(q[y,f,1:A], M[y,f])
      for(a in 1:A){
        re[y,f,a] ~ dnorm(0, tau)
        q[y,f,a] <- phi[y,f,a]/sum(phi[y,f,])
        log(phi[y,f,a]) <- alpha[a] + beta[f,a] + epsilon[a]*yearc[y] + re[y,f,a]
      }
    }
  }
  for(f in 1:2){ #loop around fleet
    pred_count[f,1:A] ~ dmulti(q_pred[f,1:A], pred_M[f])
      for(a in 1:A){
        re_pred[f,a] ~ dnorm(0, tau)
        q_pred[f,a] <- phi_pred[f,a]/sum(phi_pred[f,])
        log(phi_pred[f,a]) <- alpha[a] + beta[pred_fleet[f] + 1,a] + epsilon[a]*pred_year[f] + re_pred[f,a]
      }
  }
}
", con="model_epsilon_pred.txt")
modfile_epsilon_pred <- 'model_epsilon_pred.txt'

parameters_epsilon_pred <- c("alpha", "beta", "epsilon", "sd", "q_pred", "pred")
Epost_epsilon_pred_Homer <- 
  lapply(dat_Homer_e, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_epsilon_pred, 
                                     model.file = modfile_epsilon_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
saveRDS(Epost_epsilon_pred_Homer, "./Lingcod_Report_0323/Interview post/Epost_epsilon_pred_Homer.rds")

Epost_epsilon_pred_Kodiak <- 
  lapply(dat_Kodiak_e, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_epsilon_pred, 
                                     model.file = modfile_epsilon_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
saveRDS(Epost_epsilon_pred_Kodiak, "./Lingcod_Report_0323/Interview post/Epost_epsilon_pred_Kodiak.rds")

Epost_epsilon_pred_Seward <- 
  lapply(dat_Seward_e, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_epsilon_pred, 
                                     model.file = modfile_epsilon_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
saveRDS(Epost_epsilon_pred_Seward, "./Lingcod_Report_0323/Interview post/Epost_epsilon_pred_Seward.rds")

Epost_epsilon_pred_Valdez <- 
  lapply(dat_Valdez_e, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_epsilon_pred, 
                                     model.file = modfile_epsilon_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
saveRDS(Epost_epsilon_pred_Valdez, "./Lingcod_Report_0323/Interview post/Epost_epsilon_pred_Valdez.rds")

Epost_epsilon_pred_Whittier <- 
  lapply(dat_Whittier_e, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_epsilon_pred, 
                                     model.file = modfile_epsilon_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
saveRDS(Epost_epsilon_pred_Whittier, "./Lingcod_Report_0323/Interview post/Epost_epsilon_pred_Whittier.rds")



writeLines("
model{
  # priors:
  alpha[1] <- 0 #area baseline
  for (a in 2:A){alpha[a] ~ dnorm(0, 0.0001)}
  
  for (a in 1:A){beta[1,a] <- 0 } #area baseline
  beta[2, 1] <- 0 ; #fleet baseline
  for (a in 2:A){ beta[2,a] ~ dnorm(0, 0.0001)}

  # gamma[1] <- 0 #area baseline
  # for (a in 2:A){gamma[a] ~ dnorm(0, 0.0001)}
  
  epsilon[1] <- 0 #area baseline
  for (a in 2:A){epsilon[a] ~ dnorm(0, 0.0001)}
  
  for (a in 1:A){gamma[1,a] <- 0 } #area baseline
  gamma[2,1] <- 0 ; #fleet baseline
  for (a in 2:A){ gamma[2,a] ~ dnorm(0, 0.0001)}
  
  tau ~ dgamma(0.001, 0.001) # prior for mixed effect precision
  sd <- sqrt(1/tau)

  for(f in 1:2){ #loop around fleet
    for(y in 1:Y){ # loop around years
      count[y,f,1:A] ~ dmulti(q[y,f,1:A], M[y,f])
      for(a in 1:A){
        re[y,f,a] ~ dnorm(0, tau)
        q[y,f,a] <- phi[y,f,a]/sum(phi[y,f,])
        log(phi[y,f,a]) <- alpha[a] + beta[f,a] + epsilon[a]*yearc[y] + gamma[f,a]*yearc[y] + re[y,f,a]
      }
    }
  }
  for(f in 1:2){ #loop around fleet
    pred_count[f,1:A] ~ dmulti(q_pred[f,1:A], pred_M[f])
      for(a in 1:A){
        re_pred[f,a] ~ dnorm(0, tau)
        q_pred[f,a] <- phi_pred[f,a]/sum(phi_pred[f,])
        log(phi_pred[f,a]) <- alpha[a] + beta[pred_fleet[f] + 1,a] + epsilon[a]*pred_year[f] + gamma[pred_fleet[f] + 1,a]*pred_year[f] + re_pred[f,a]
      }
  }
}
", con="model_gamma_pred.txt")
modfile_gamma_pred <- 'model_gamma_pred.txt'

parameters_gamma_pred <- c("alpha", "beta", "epsilon", "gamma", "sd", "q_pred", "pred")
Epost_gamma_pred_Homer <- 
  lapply(dat_Homer_e, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_gamma_pred, 
                                     model.file = modfile_gamma_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
saveRDS(Epost_gamma_pred_Homer, "./Lingcod_Report_0323/Interview post/Epost_gamma_pred_Homer.rds")

Epost_gamma_pred_Kodiak <- 
  lapply(dat_Kodiak_e, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_gamma_pred, 
                                     model.file = modfile_gamma_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
saveRDS(Epost_gamma_pred_Kodiak, "./Lingcod_Report_0323/Interview post/Epost_gamma_pred_Kodiak.rds")

Epost_gamma_pred_Seward <- 
  lapply(dat_Seward_e, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_gamma_pred, 
                                     model.file = modfile_gamma_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
saveRDS(Epost_gamma_pred_Seward, "./Lingcod_Report_0323/Interview post/Epost_gamma_pred_Seward.rds")

Epost_gamma_pred_Valdez <- 
  lapply(dat_Valdez_e, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_gamma_pred, 
                                     model.file = modfile_gamma_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
saveRDS(Epost_gamma_pred_Valdez, "./Lingcod_Report_0323/Interview post/Epost_gamma_pred_Valdez.rds")

Epost_gamma_pred_Whittier <- 
  lapply(dat_Whittier_e, 
         function(x){x2 <- x[names(x) != "pred_area"]
         OLREjags_re <- jagsUI::jags(x2, 
                                     parameters.to.save = parameters_gamma_pred, 
                                     model.file = modfile_gamma_pred, 
                                     n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                                     parallel = TRUE)})
saveRDS(Epost_gamma_pred_Whittier, "./Lingcod_Report_0323/Interview post/Epost_gamma_pred_Whittier.rds")


### 
# Model Selection
Epost_alpha_pred_Homer <- readRDS("./Lingcod_Report_0323/Interview post/Epost_alpha_pred_Homer.rds")
Epost_alpha_pred_Kodiak <- readRDS("./Lingcod_Report_0323/Interview post/Epost_alpha_pred_Kodiak.rds")
Epost_alpha_pred_Seward <- readRDS("./Lingcod_Report_0323/Interview post/Epost_alpha_pred_Seward.rds")
Epost_alpha_pred_Valdez <- readRDS("./Lingcod_Report_0323/Interview post/Epost_alpha_pred_Valdez.rds")
Epost_alpha_pred_Whittier <- readRDS("./Lingcod_Report_0323/Interview post/Epost_alpha_pred_Whittier.rds")
Epost_beta_pred_Homer <- readRDS("./Lingcod_Report_0323/Interview post/Epost_beta_pred_Homer.rds")
Epost_beta_pred_Kodiak <- readRDS("./Lingcod_Report_0323/Interview post/Epost_beta_pred_Kodiak.rds")
Epost_beta_pred_Seward <- readRDS("./Lingcod_Report_0323/Interview post/Epost_beta_pred_Seward.rds")
Epost_beta_pred_Valdez <- readRDS("./Lingcod_Report_0323/Interview post/Epost_beta_pred_Valdez.rds")
Epost_beta_pred_Whittier <- readRDS("./Lingcod_Report_0323/Interview post/Epost_beta_pred_Whittier.rds")
Epost_epsilon_pred_Homer <- readRDS("./Lingcod_Report_0323/Interview post/Epost_epsilon_pred_Homer.rds")
Epost_epsilon_pred_Kodiak <- readRDS("./Lingcod_Report_0323/Interview post/Epost_epsilon_pred_Kodiak.rds")
Epost_epsilon_pred_Seward <- readRDS("./Lingcod_Report_0323/Interview post/Epost_epsilon_pred_Seward.rds")
Epost_epsilon_pred_Valdez <- readRDS("./Lingcod_Report_0323/Interview post/Epost_epsilon_pred_Valdez.rds")
Epost_epsilon_pred_Whittier <- readRDS("./Lingcod_Report_0323/Interview post/Epost_epsilon_pred_Whittier.rds")
Epost_gamma_pred_Homer <- readRDS("./Lingcod_Report_0323/Interview post/Epost_gamma_pred_Homer.rds")
Epost_gamma_pred_Kodiak <- readRDS("./Lingcod_Report_0323/Interview post/Epost_gamma_pred_Kodiak.rds")
Epost_gamma_pred_Seward <- readRDS("./Lingcod_Report_0323/Interview post/Epost_gamma_pred_Seward.rds")
Epost_gamma_pred_Valdez <- readRDS("./Lingcod_Report_0323/Interview post/Epost_gamma_pred_Valdez.rds")
Epost_gamma_pred_Whittier <- readRDS("./Lingcod_Report_0323/Interview post/Epost_gamma_pred_Whittier.rds")



#  * ll calcs -------------------------------------------------------------
#Not sure how to evaluate cross-validated predictions
#Maybe the most direct route is just to calcualte the log-liklihood for the left our observations given the modeled probabilities
#Seems like the best choice

Ell_Homer <- 
  list(alpha = mapply(get_llpred, pred = Epost_alpha_pred_Homer, obs = dat_Homer_e),
       beta = mapply(get_llpred, pred = Epost_beta_pred_Homer, obs = dat_Homer_e),
       epsilon = mapply(get_llpred, pred = Epost_epsilon_pred_Homer, obs = dat_Homer_e),
       gamma = mapply(get_llpred, pred = Epost_gamma_pred_Homer, obs = dat_Homer_e))

Ell_Kodiak <- 
  list(alpha = mapply(get_llpred, pred = Epost_alpha_pred_Kodiak, obs = dat_Kodiak_e),
       beta = mapply(get_llpred, pred = Epost_beta_pred_Kodiak, obs = dat_Kodiak_e),
       epsilon = mapply(get_llpred, pred = Epost_epsilon_pred_Kodiak, obs = dat_Kodiak_e),
       gamma = mapply(get_llpred, pred = Epost_gamma_pred_Kodiak, obs = dat_Kodiak_e))

Ell_Seward <- 
  list(alpha = mapply(get_llpred, pred = Epost_alpha_pred_Seward, obs = dat_Seward_e),
       beta = mapply(get_llpred, pred = Epost_beta_pred_Seward, obs = dat_Seward_e),
       epsilon = mapply(get_llpred, pred = Epost_epsilon_pred_Seward, obs = dat_Seward_e),
       gamma = mapply(get_llpred, pred = Epost_gamma_pred_Seward, obs = dat_Seward_e))

Ell_Valdez <- 
  list(alpha = mapply(get_llpred, pred = Epost_alpha_pred_Valdez, obs = dat_Valdez_e),
       beta = mapply(get_llpred, pred = Epost_beta_pred_Valdez, obs = dat_Valdez_e),
       epsilon = mapply(get_llpred, pred = Epost_epsilon_pred_Valdez, obs = dat_Valdez_e),
       gamma = mapply(get_llpred, pred = Epost_gamma_pred_Valdez, obs = dat_Valdez_e))

Ell_Whittier <- 
  list(alpha = mapply(get_llpred, pred = Epost_alpha_pred_Whittier, obs = dat_Whittier_e),
       beta = mapply(get_llpred, pred = Epost_beta_pred_Whittier, obs = dat_Whittier_e),
       epsilon = mapply(get_llpred, pred = Epost_epsilon_pred_Whittier, obs = dat_Whittier_e),
       gamma = mapply(get_llpred, pred = Epost_gamma_pred_Whittier, obs = dat_Whittier_e))


ll_E <- list(Ell_Homer, Ell_Kodiak, Ell_Seward, Ell_Valdez, Ell_Whittier)
saveRDS(ll_E, "./Lingcod_Report_0323/Data/ll_E.rds")

#Model selection table
Map(function(x, y){tab_ll(x) %>% dplyr::mutate(port = y, composition = "Lingcod Effort")}, 
    x = ll_E,
    y = factor(c("Homer", "Kodiak", "Seward", "Valdez", "Whittier"), ordered = TRUE)) %>%
  do.call(rbind, .) %>% 
  dplyr::arrange(desc(composition), port) %>%
  dplyr::select(port, composition, model, ll, diff, p_diff)

#high sd predictions are BAD!
as.data.frame(Epost_alpha_pred_Homer[[1]]$sims.list$q_pred) %>%
  tidyr::pivot_longer(cols = where(is.numeric)) %>%
  ggplot(aes(x = value)) + geom_histogram() + facet_grid(.~name)
#In those situations the median wi better than the mean
apply(Epost_alpha_pred_Homer[[1]]$sims.list$q_pred, 2, median)
apply(Epost_alpha_pred_Homer[[1]]$sims.list$q_pred, 2, mean)

#Better if estiamte is near edge of support.
as.data.frame(Epost_beta_pred_Kodiak[[1]]$sims.list$q_pred) %>%
  tidyr::pivot_longer(cols = where(is.numeric)) %>%
  ggplot(aes(x = value)) + geom_histogram() + facet_grid(.~name)
#In those situations the median wi better than the mean
apply(Epost_beta_pred_Kodiak[[1]]$sims.list$q_pred, c(2, 3), median)
apply(Epost_beta_pred_Kodiak[[1]]$sims.list$q_pred, c(2, 3), mean)

#Good w low sd.
as.data.frame(Epost_epsilon_pred_Valdez[[10]]$sims.list$q_pred) %>%
  tidyr::pivot_longer(cols = where(is.numeric)) %>%
  ggplot(aes(x = value)) + geom_histogram() + facet_grid(.~name)
#In those situations the median wi better than the mean
apply(Epost_epsilon_pred_Valdez[[10]]$sims.list$q_pred, c(2, 3), median)
apply(Epost_epsilon_pred_Valdez[[10]]$sims.list$q_pred, c(2, 3), mean)

#Medium-high sd predictions highly influenced by support.
as.data.frame(Epost_epsilon_pred_Seward[[10]]$sims.list$q_pred) %>%
  tidyr::pivot_longer(cols = where(is.numeric)) %>%
  ggplot(aes(x = value)) + geom_histogram() + facet_grid(.~name)
#In those situations the median wi better than the mean
apply(Epost_epsilon_pred_Seward[[10]]$sims.list$q_pred, c(2, 3), median)
apply(Epost_epsilon_pred_Seward[[10]]$sims.list$q_pred, c(2, 3), mean)