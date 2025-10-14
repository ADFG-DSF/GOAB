#Looking for a method to fit groundfish harvest and effort compostion data for reporting
#would like to detect a trend to composition data
library(magrittr)
library(ggplot2)
int_boat <- readRDS("./Lingcod_Report_0323/Data/int_boat.rds") %>% 
  dplyr::filter(port %in% c('Homer', 'Seward', 'Kodiak', 'Valdez', 'Whittier'))
source("./Lingcod_Report_0323/functions.R")
source("./Lingcod_Report_0323/models.R")

ports <- unique(int_boat$port)
# Harvest Composition -----------------------------------------------------
int_ling <- 
  int_boat %>%
  dplyr::group_by(port, year, fleet, area) %>% 
  dplyr::summarise(H = sum(lH), E = sum(E)) %>%
  dplyr::ungroup()
ports <- unique(int_ling$port)
aggregate(E ~ year + port, int_ling, sum)
jags_datH <- lapply(ports, make_jagsdat, dat = int_ling, stat = "H")

#jags w overdisersion on each observation
ni <- 1E4; nb <- ni/3; nc <- 3; nt <- 20
params <- list(parameters_alpha <- c("alpha", "sd", "re"),
               parameters_beta <- c("alpha", "beta", "re", "sd"),
               parameters_epsilon <- c("alpha", "beta", "epsilon", "re", "sd"),
               parameters_gamma <- c("alpha", "beta", "epsilon", "gamma", "sd", "re"))
models <- list(alpha <- modfile_alpha,
               beta <- modfile_beta,
               epsilon <- modfile_epsilon,
               gamma <- modfile_gamma)


# * Homer Harvest -----------------------------------------------------------
#
#
postH_Homer <- mapply(jagsUI::jags,
                     parameters.to.save = params, model.file = models,
                     MoreArgs = list(data = jags_datH[[1]], n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb),
                     SIMPLIFY = FALSE)
lapply(postH_Homer, function(x) quantile(unlist(x$Rhat), probs = c(0.5, 0.9, 0.95, 0.99, 1), na.rm = TRUE))
saveRDS(postH_Homer, "./Lingcod_Report_0323/Interview post/postH_Homer.rds")
postH_Homer[[1]] 
postH_Homer[[2]] 
postH_Homer[[3]] 
postH_Homer[[4]] 

data.frame(Deviance = sapply(postH_Homer, function(x) x$mean$deviance),
           pD = sapply(postH_Homer, function(x) x$pD), 
           DIC = sapply(postH_Homer, function(x) x$DIC),
           sd = sapply(postH_Homer, function(x) x$mean$sd))
lapply(postH_Homer, function(x) x$sims.list$sd) %>% 
  do.call(cbind, .) %>% 
  as.data.frame() %>% 
  setNames(c("alpha", "beta", "epsilon", "gamma")) %>%
  tidyr::pivot_longer(tidyr::everything()) %>% 
  ggplot(aes(x = value)) + 
  geom_histogram() + 
  facet_grid(name ~ .)

plot_post(postH_Homer[[1]], int_ling, "H", ports[1])
plot_post(postH_Homer[[2]], int_ling, "H", ports[1])
plot_post(postH_Homer[[3]], int_ling, "H", ports[1])
plot_post(postH_Homer[[4]], int_ling, "H", ports[1])

(plot_HomerH <- plot_post(postH_Homer[[1]], int_ling, "H", ports[1], title = "Homer: Harvest")) # Alpha model performs best with sd and likelihood




# * Kodiak Harvest --------------------------------------------------------
#
#
postH_Kodiak <- mapply(jagsUI::jags,
                       parameters.to.save = params, model.file = models,
                       MoreArgs = list(data = jags_datH[[2]], n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb),
                       SIMPLIFY = FALSE)
lapply(postH_Kodiak, function(x) quantile(unlist(x$Rhat), probs = c(0.5, 0.9, 0.95, 0.99, 1), na.rm = TRUE))
saveRDS(postH_Kodiak, "./Lingcod_Report_0323/Interview post/postH_Kodiak.rds")
postH_Kodiak[[1]] 
postH_Kodiak[[2]] 
postH_Kodiak[[3]] 
postH_Kodiak[[4]] 

data.frame(Deviance = sapply(postH_Kodiak, function(x) x$mean$deviance),
           pD = sapply(postH_Kodiak, function(x) x$pD), 
           DIC = sapply(postH_Kodiak, function(x) x$DIC),
           sd = sapply(postH_Kodiak, function(x) x$mean$sd))
lapply(postH_Kodiak, function(x) x$sims.list$sd) %>% 
  do.call(cbind, .) %>% 
  as.data.frame() %>% 
  setNames(c("alpha", "beta", "epsilon", "gamma")) %>%
  tidyr::pivot_longer(tidyr::everything()) %>% 
  ggplot(aes(x = value)) + 
  geom_histogram() + 
  facet_grid(name ~ .)

plot_post(postH_Kodiak[[1]], int_ling, "H", ports[2])
plot_post(postH_Kodiak[[2]], int_ling, "H", ports[2])
plot_post(postH_Kodiak[[3]], int_ling, "H", ports[2])
plot_post(postH_Kodiak[[4]], int_ling, "H", ports[2])
gridExtra::marrangeGrob(lapply(postH_Kodiak, plot_post, int_ling, "H", ports[[2]]), nrow = 2, ncol = 2)

(plot_KodiakH <- plot_post(postH_Kodiak[[2]], int_ling, "H", ports[2], title = "Kodiak: Harvest")) # Beta model performs best with sd and likelihood



# * Seward Harvest --------------------------------------------------------
#
#
postH_Seward <- mapply(jagsUI::jags,
                       parameters.to.save = params, model.file = models,
                       MoreArgs = list(data = jags_datH[[3]], n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb),
                       SIMPLIFY = FALSE)
lapply(postH_Seward, function(x) quantile(unlist(x$Rhat), probs = c(0.5, 0.9, 0.95, 0.99, 1), na.rm = TRUE))
saveRDS(postH_Seward, "./Lingcod_Report_0323/Interview post/postH_Seward.rds")
postH_Seward[[1]] 
postH_Seward[[2]] 
postH_Seward[[3]] 
postH_Seward[[4]] 

data.frame(Deviance = sapply(postH_Seward, function(x) x$mean$deviance),
           pD = sapply(postH_Seward, function(x) x$pD), 
           DIC = sapply(postH_Seward, function(x) x$DIC),
           sd = sapply(postH_Seward, function(x) x$mean$sd))
lapply(postH_Seward, function(x) x$sims.list$sd) %>% 
  do.call(cbind, .) %>% 
  as.data.frame() %>% 
  setNames(c("alpha", "beta", "epsilon", "gamma")) %>%
  tidyr::pivot_longer(tidyr::everything()) %>% 
  ggplot(aes(x = value)) + 
  geom_histogram() + 
  facet_grid(name ~ .)

plot_post(postH_Seward[[1]], int_ling, "H", ports[3])
plot_post(postH_Seward[[2]], int_ling, "H", ports[3])
plot_post(postH_Seward[[3]], int_ling, "H", ports[3])
plot_post(postH_Seward[[4]], int_ling, "H", ports[3])
gridExtra::marrangeGrob(lapply(postH_Seward, plot_post, int_ling, "H", ports[[3]]), nrow = 2, ncol = 2)

plot_post(postH_Seward[[1]], int_ling, "H", ports[3], bystock = FALSE)


(plot_SewardH <- plot_post(postH_Seward[[4]], int_ling, "H", ports[3], title = "Seward: Harvest")) # Gamma model performs best with sd and likelihood




# * Valdez Harvest --------------------------------------------------------
#
#
postH_Valdez <- mapply(jagsUI::jags,
                       parameters.to.save = params, model.file = models,
                       MoreArgs = list(data = jags_datH[[4]], n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb),
                       SIMPLIFY = FALSE)
lapply(postH_Valdez, function(x) quantile(unlist(x$Rhat), probs = c(0.5, 0.9, 0.95, 0.99, 1), na.rm = TRUE))
saveRDS(postH_Valdez, "./Lingcod_Report_0323/Interview post/postH_Valdez.rds")
postH_Valdez[[1]] 
postH_Valdez[[2]] 
postH_Valdez[[3]] 
postH_Valdez[[4]] 

data.frame(Deviance = sapply(postH_Valdez, function(x) x$mean$deviance),
           pD = sapply(postH_Valdez, function(x) x$pD), 
           DIC = sapply(postH_Valdez, function(x) x$DIC),
           sd = sapply(postH_Valdez, function(x) x$mean$sd))
lapply(postH_Valdez, function(x) x$sims.list$sd) %>% 
  do.call(cbind, .) %>% 
  as.data.frame() %>% 
  setNames(c("alpha", "beta", "epsilon", "gamma")) %>%
  tidyr::pivot_longer(tidyr::everything()) %>% 
  ggplot(aes(x = value)) + 
  geom_histogram() + 
  facet_grid(name ~ .)

plot_post(postH_Valdez[[1]], int_ling, "H", ports[4])
plot_post(postH_Valdez[[2]], int_ling, "H", ports[4])
plot_post(postH_Valdez[[3]], int_ling, "H", ports[4])
plot_post(postH_Valdez[[4]], int_ling, "H", ports[4])

plot_post(postH_Valdez[[1]], int_ling, "H", ports[4], bystock = FALSE)

(plot_ValdezH <- plot_post(postH_Valdez[[3]], int_ling, "H", ports[4], title = "Valdez: Harvest")) # Epsilon model performs best with sd and likelihood




# * Whittier Harvest ------------------------------------------------------
#
#
postH_Whittier <- mapply(jagsUI::jags,
                         parameters.to.save = params, model.file = models,
                         MoreArgs = list(data = jags_datH[[5]], n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb),
                         SIMPLIFY = FALSE)
lapply(postH_Whittier, function(x) quantile(unlist(x$Rhat), probs = c(0.5, 0.9, 0.95, 0.99, 1), na.rm = TRUE))
saveRDS(postH_Whittier, "./Lingcod_Report_0323/Interview post/postH_Whittier.rds")
postH_Whittier[[1]] 
postH_Whittier[[2]] 
postH_Whittier[[3]] 
postH_Whittier[[4]]

data.frame(Deviance = sapply(postH_Whittier, function(x) x$mean$deviance),
           pD = sapply(postH_Whittier, function(x) x$pD), 
           DIC = sapply(postH_Whittier, function(x) x$DIC),
           sd = sapply(postH_Whittier, function(x) x$mean$sd))
lapply(postH_Whittier, function(x) x$sims.list$sd) %>% 
  do.call(cbind, .) %>% 
  as.data.frame() %>% 
  setNames(c("alpha", "beta", "epsilon", "gamma")) %>%
  tidyr::pivot_longer(tidyr::everything()) %>% 
  ggplot(aes(x = value)) + 
  geom_histogram() + 
  facet_grid(name ~ .)

plot_post(postH_Whittier[[1]], int_ling, "H", ports[5])
plot_post(postH_Whittier[[2]], int_ling, "H", ports[5])
plot_post(postH_Whittier[[3]], int_ling, "H", ports[5])
plot_post(postH_Whittier[[4]], int_ling, "H", ports[5])
jags_datH[[5]]$M

(plot_WhittierH <- plot_post(postH_Whittier[[3]], int_ling, "H", ports[5], title = "Whittier: Harvest")) # Epsilon model performs best with sd and likelihood


# Lingcod Effort --------------------------------------------------


################################################################################
# The following is copied from Rockfish Report analysis and adapted for lingcod
################################################################################


# Effort Composition -----------------------------------------------------
#Elected to eliminate Halibut focused trips from the analysis. This is consistent with past practices and the other options we identified eliminated so may interviews.
#Also limiting data to only the open lingcod season (After July 1)
int_E <-
  int_boat[int_boat$target %in% c("Rockfish", "Bottomfish", "Bottomfish & Salmon", "Lingcod"), ] %>%
  dplyr::filter(month >= 7) %>% 
  dplyr::group_by(port, year, fleet, area) %>%
  dplyr::summarise(H = sum(lH, na.rm = TRUE), E = sum(E, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::filter(year >= 2003)
jags_datE <- lapply(ports, make_jagsdat, dat = int_E, stat = "E")

# # * Homer Effort --------------------------------------------------------
# ni <- 5E6; nb <- ni/3; nc <- 3; nt <- 1000
postE_Homer <- mapply(jagsUI::jags,
                      parameters.to.save = params, model.file = models,
                      MoreArgs = list(data = jags_datE[[1]], n.chains = nc, n.thin = nt, n.iter = 2 * ni, n.burnin = nb, store.data = TRUE, parallel = TRUE),
                      SIMPLIFY = FALSE)
lapply(postE_Homer, function(x) quantile(unlist(x$Rhat), probs = c(0.5, 0.9, 0.95, 0.99, 1), na.rm = TRUE))
saveRDS(postE_Homer, "./Lingcod_Report_0323/Interview post/postE_Homer.rds")

postE_Homer[[1]] 
postE_Homer[[2]] 
postE_Homer[[3]] 
postE_Homer[[4]] 

data.frame(Deviance = sapply(postE_Homer, function(x) x$mean$deviance),
           pD = sapply(postE_Homer, function(x) x$pD), 
           DIC = sapply(postE_Homer, function(x) x$DIC),
           sd = sapply(postE_Homer, function(x) x$mean$sd))
lapply(postE_Homer, function(x) x$sims.list$sd) %>% 
  do.call(cbind, .) %>% 
  as.data.frame() %>% 
  setNames(c("alpha", "beta", "epsilon", "gamma")) %>%
  tidyr::pivot_longer(tidyr::everything()) %>% 
  ggplot(aes(x = value)) + 
  geom_histogram() + 
  facet_grid(name ~ .)

plot_post(postE_Homer[[1]], int_E, "E", ports[1])
plot_post(postE_Homer[[2]], int_E, "E", ports[1])
plot_post(postE_Homer[[3]], int_E, "E", ports[1])
plot_post(postE_Homer[[4]], int_E, "E", ports[1])

(plot_HomerE <- plot_post(postE_Homer[[3]], int_E, "E", ports[1], title = "Homer: Effort")) # Epsilon model performs best with sd and likelihood

jags_datE[[1]]$M

# ni <- 1E5; nb <- ni/3; nc <- 3; nt <- 200
# # * Kodiak Effort --------------------------------------------------------
postE_Kodiak <- mapply(jagsUI::jags,
                       parameters.to.save = params, model.file = models,
                       MoreArgs = list(data = jags_datE[[2]], n.chains = nc, n.thin = nt, n.iter = 1.5 * ni, n.burnin = nb, store.data = TRUE),
                       SIMPLIFY = FALSE)
lapply(postE_Kodiak, function(x) quantile(unlist(x$Rhat), probs = c(0.5, 0.9, 0.95, 0.99, 1), na.rm = TRUE))
saveRDS(postE_Kodiak, "./Lingcod_Report_0323/Interview post/postE_Kodiak.rds")

postE_Kodiak[[1]] 
postE_Kodiak[[2]] 
postE_Kodiak[[3]] 
postE_Kodiak[[4]] 

data.frame(Deviance = sapply(postE_Kodiak, function(x) x$mean$deviance),
           pD = sapply(postE_Kodiak, function(x) x$pD), 
           DIC = sapply(postE_Kodiak, function(x) x$DIC),
           sd = sapply(postE_Kodiak, function(x) x$mean$sd))
lapply(postE_Kodiak, function(x) x$sims.list$sd) %>% 
  do.call(cbind, .) %>% 
  as.data.frame() %>% 
  setNames(c("alpha", "beta", "epsilon", "gamma")) %>%
  tidyr::pivot_longer(tidyr::everything()) %>% 
  ggplot(aes(x = value)) + 
  geom_histogram() + 
  facet_grid(name ~ .)

plot_post(postE_Kodiak[[1]], int_E, "E", ports[2])
plot_post(postE_Kodiak[[2]], int_E, "E", ports[2])
plot_post(postE_Kodiak[[3]], int_E, "E", ports[2])
plot_post(postE_Kodiak[[4]], int_E, "E", ports[2])

(plot_KodiakE <- plot_post(postE_Kodiak[[2]], int_E, "E", ports[2], title = "Kodiak: Effort")) # Beta model performs best with sd and likelihood



# * Seward Effort --------------------------------------------------------
postE_Seward <- mapply(jagsUI::jags,
                         parameters.to.save = params, model.file = models,
                         MoreArgs = list(data = jags_datE[[3]], n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, store.data = TRUE),
                         SIMPLIFY = FALSE)
lapply(postE_Seward, function(x) quantile(unlist(x$Rhat), probs = c(0.5, 0.9, 0.95, 0.99, 1), na.rm = TRUE))
saveRDS(postE_Seward, "./Lingcod_Report_0323/Interview post/postE_Seward.rds")

postE_Seward[[1]] 
postE_Seward[[2]] 
postE_Seward[[3]] 
postE_Seward[[4]] 

data.frame(Deviance = sapply(postE_Seward, function(x) x$mean$deviance),
           pD = sapply(postE_Seward, function(x) x$pD), 
           DIC = sapply(postE_Seward, function(x) x$DIC),
           sd = sapply(postE_Seward, function(x) x$mean$sd))
lapply(postE_Seward, function(x) x$sims.list$sd) %>% 
  do.call(cbind, .) %>% 
  as.data.frame() %>% 
  setNames(c("alpha", "beta", "epsilon", "gamma")) %>%
  tidyr::pivot_longer(tidyr::everything()) %>% 
  ggplot(aes(x = value)) + 
  geom_histogram() + 
  facet_grid(name ~ .)

plot_post(postE_Seward[[1]], int_E, "E", ports[3])
plot_post(postE_Seward[[2]], int_E, "E", ports[3])
plot_post(postE_Seward[[3]], int_E, "E", ports[3])
plot_post(postE_Seward[[4]], int_E, "E", ports[3])

(plot_SewardE <- plot_post(postE_Seward[[2]], int_E, "E", ports[3], title = "Seward: Effort")) # Beta and Gamma models perform similar with sd, Betal performs better with likelihood






# * Valdez Effort --------------------------------------------------------
postE_Valdez <- mapply(jagsUI::jags,
                         parameters.to.save = params, model.file = models,
                         MoreArgs = list(data = jags_datE[[4]], n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, store.data = TRUE),
                         SIMPLIFY = FALSE)
lapply(postE_Valdez, function(x) quantile(unlist(x$Rhat), probs = c(0.5, 0.9, 0.95, 0.99, 1), na.rm = TRUE))
saveRDS(postE_Valdez, "./Lingcod_Report_0323/Interview post/postE_Valdez.rds")

postE_Valdez[[1]] 
postE_Valdez[[2]] 
postE_Valdez[[3]] 
postE_Valdez[[4]] 

data.frame(Deviance = sapply(postE_Valdez, function(x) x$mean$deviance),
           pD = sapply(postE_Valdez, function(x) x$pD), 
           DIC = sapply(postE_Valdez, function(x) x$DIC),
           sd = sapply(postE_Valdez, function(x) x$mean$sd))
lapply(postE_Homer, function(x) x$sims.list$sd) %>% 
  do.call(cbind, .) %>% 
  as.data.frame() %>% 
  setNames(c("alpha", "beta", "epsilon", "gamma")) %>%
  tidyr::pivot_longer(tidyr::everything()) %>% 
  ggplot(aes(x = value)) + 
  geom_histogram() + 
  facet_grid(name ~ .)

plot_post(postE_Valdez[[1]], int_E, "E", ports[4])
plot_post(postE_Valdez[[2]], int_E, "E", ports[4])
plot_post(postE_Valdez[[3]], int_E, "E", ports[4])
plot_post(postE_Valdez[[4]], int_E, "E", ports[4])

(plot_ValdezE <- plot_post(postE_Valdez[[3]], int_E, "E", ports[4], title = "Valdez: Effort")) # Epsilon model performs best with sd and likelihood
jags_datE[[4]]$M


# * Whittier Effort ------------------------------------------------------
postE_Whittier <- mapply(jagsUI::jags,
                           parameters.to.save = params, model.file = models,
                           MoreArgs = list(data = jags_datE[[5]], n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, store.data = TRUE),
                           SIMPLIFY = FALSE)
lapply(postE_Whittier, function(x) quantile(unlist(x$Rhat), probs = c(0.5, 0.9, 0.95, 0.99, 1), na.rm = TRUE))
saveRDS(postE_Whittier, "./Lingcod_Report_0323/Interview post/postE_Whittier.rds")

postE_Whittier[[1]] 
postE_Whittier[[2]] 
postE_Whittier[[3]] 
postE_Whittier[[4]] 

data.frame(Deviance = sapply(postE_Whittier, function(x) x$mean$deviance),
           pD = sapply(postE_Whittier, function(x) x$pD), 
           DIC = sapply(postE_Whittier, function(x) x$DIC),
           sd = sapply(postE_Whittier, function(x) x$mean$sd))
lapply(postE_Whittier, function(x) x$sims.list$sd) %>% 
  do.call(cbind, .) %>% 
  as.data.frame() %>% 
  setNames(c("alpha", "beta", "epsilon", "gamma")) %>%
  tidyr::pivot_longer(tidyr::everything()) %>% 
  ggplot(aes(x = value)) + 
  geom_histogram() + 
  facet_grid(name ~ .)

plot_post(postE_Whittier[[1]], int_E, "E", ports[5])
plot_post(postE_Whittier[[2]], int_E, "E", ports[5])
plot_post(postE_Whittier[[3]], int_E, "E", ports[5])
plot_post(postE_Whittier[[4]], int_E, "E", ports[5])

(plot_WhittierE <- plot_post(postE_Whittier[[3]], int_E, "E", ports[5], title = "Whittier: Effort")) # Epsilon and gama model perform similar with sd, Epsilon performs better with likelihood


# Save results ------------------------------------------------------------
#code to load all the posteriors
postE_Homer <- readRDS("./Lingcod_Report_0323/Interview post/postE_Homer.rds")
postE_Kodiak <- readRDS("./Lingcod_Report_0323/Interview post/postE_Kodiak.rds")
postE_Seward <- readRDS("./Lingcod_Report_0323/Interview post/postE_Seward.rds")
postE_Valdez <- readRDS("./Lingcod_Report_0323/Interview post/postE_Valdez.rds")
postE_Whittier <- readRDS("./Lingcod_Report_0323/Interview post/postE_Whittier.rds")

postH_Homer <- readRDS("./Lingcod_Report_0323/Interview post/postH_Homer.rds")
postH_Kodiak <- readRDS("./Lingcod_Report_0323/Interview post/postH_Kodiak.rds")
postH_Seward <- readRDS("./Lingcod_Report_0323/Interview post/postH_Seward.rds")
postH_Valdez <- readRDS("./Lingcod_Report_0323/Interview post/postH_Valdez.rds")
postH_Whittier <- readRDS("./Lingcod_Report_0323/Interview post/postH_Whittier.rds")



#save comp plots
comp_plots <- 
  list(
    plot_HomerE = plot_HomerE, 
       plot_HomerH = plot_HomerH, 
       plot_KodiakE = plot_KodiakE, 
       plot_KodiakH = plot_KodiakH, 
       plot_SewardE = plot_SewardE, 
       plot_SewardH = plot_SewardH, 
       plot_ValdezE = plot_ValdezE, 
       plot_ValdezH = plot_ValdezH, 
       plot_WhittierE = plot_WhittierE, 
       plot_WhittierH = plot_WhittierH
       )
mapply(ggsave, 
       filename = paste0(names(comp_plots), ".png"), 
       plot = comp_plots, 
       MoreArgs = list(device = "png", 
                       path = "./Lingcod_Report_0323/Figures/"))

#save model posterior estimates

p_post_clean(
  post  = postE_Seward[[4]],
  areas = ports[[3]],
  fleets = c("charter", "private")
)

mod_p <- 
  list(
    HomerE = p_post(postE_Homer[[3]], yrs = NULL, areas = ports[[2]]),
    HomerH = p_post2(postH_Homer[[1]], yrs = NULL, areas = ports[[2]]),
    KodiakE = p_post(postE_Kodiak[[2]], yrs = 0, areas = ports[[1]]),
    KodiakH = p_post(postH_Kodiak[[2]], yrs = 0, areas = ports[[1]]),
    SewardE = p_post(postE_Seward[[2]], yrs = 0, areas = ports[[3]]),
    SewardH = p_post(postH_Seward[[4]], yrs = 0, areas = ports[[3]]),
    ValdezE = p_post(postE_Valdez[[3]], yrs = 0, areas = ports[[4]]),
    ValdezH = p_post(postH_Valdez[[3]], yrs = 0, areas = ports[[4]]),
    WhittierE = p_post(postE_Whittier[[3]], yrs = 0, areas = ports[[5]]),
    WhittierH = p_post(postH_Whittier[[3]], yrs = 0, areas = ports[[5]])
  )
saveRDS(mod_p, "./Lingcod_Report_0323/Data/mod_p.rds")

post_list <- list(postH_Homer=postH_Homer, postE_Homer=postE_Homer, 
                  postH_Kodiak=postH_Kodiak,postE_Kodiak=postE_Kodiak, 
                  postH_Seward=postH_Seward, postE_Seward=postE_Seward, 
                  postH_Valdez=postH_Valdez, postE_Valdez=postE_Valdez, 
                  postH_Whittier=postH_Whittier, postE_Whittier=postE_Whittier)
tab_sd <- 
  lapply(seq_along(post_list), get_sd, posts = post_list, obj_names = names(post_list)) %>% 
  do.call(rbind, .)
saveRDS(tab_sd, "./Lingcod_Report_0323/Data/tab_sd.rds")




