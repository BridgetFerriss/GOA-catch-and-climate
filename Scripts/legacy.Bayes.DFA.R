library(loo)
library(bayesdfa)
library(ggplot2)
library(dplyr)
library(viridis)
library(ggrepel)
library(tidyverse)

options(mc.cores = parallel::detectCores())

d <- read.csv("./data/legacy.dfa.dat.csv", stringsAsFactors = FALSE)

# check out data - should we change 0s to NA??
ggplot(d, aes(year, value)) +
  geom_line() +
  facet_wrap(~name, scales = "free_y") +
  theme_bw()

# lots of zero catches

# replace with NA
change <- d$value == 0
d$value[change] <- NA

# plot again
ggplot(d, aes(year, value)) +
  geom_line() +
  facet_wrap(~name, scales = "free_y") +
  theme_bw()

# that looks easier to analyze to me!
d <- d %>% 
  pivot_wider(names_from = name, values_from = value) %>%
  select(-year)

mcmc_iter <- 4000
mcmc_chains <- 3
thin <- 10
m <- list()
loos <- list()

m[[1]] <- fit_dfa(y = t(d), iter = mcmc_iter, chains = mcmc_chains, num_trends = 1,par_list="all")
loos[[1]] = loo::loo(m[[1]]$model,moment_match=TRUE)
saveRDS(loos, "Output/legacy_catch_loos.rds")

# fit models with varying numbers of knots in a b-spline
m[[2]] <- fit_dfa(y = t(d), iter = mcmc_iter, chains = mcmc_chains, thin = thin, num_trends = 1, trend_model = "bs", n_knots = 6,par_list="all")
loos[[2]] = loo::loo(m[[2]]$model,moment_match=TRUE)
saveRDS(loos, "Output/legacy_catch_loos.rds")

m[[3]] <- fit_dfa(y = t(d), iter = mcmc_iter, chains = mcmc_chains, thin = thin, num_trends = 1, trend_model = "bs", n_knots = 12,par_list="all")
loos[[3]] = loo::loo(m[[3]]$model,moment_match=TRUE)
saveRDS(loos, "Output/legacy_catch_loos.rds")

m[[4]] <- fit_dfa(y = t(d), iter = mcmc_iter, chains = mcmc_chains, thin = thin, num_trends = 1, trend_model = "bs", n_knots = 18,par_list="all")
loos[[4]] = loo::loo(m[[4]]$model,moment_match=TRUE)
saveRDS(loos, "Output/legacy_catch_loos.rds")

m[[5]] <- fit_dfa(y = t(d), iter = mcmc_iter, chains = mcmc_chains, thin = thin, num_trends = 1, trend_model = "bs", n_knots = 24,par_list="all")
loos[[5]] = loo::loo(m[[5]]$model,moment_match=TRUE)
saveRDS(loos, "Output/legacy_catch_loos.rds")

m[[6]] <- fit_dfa(y = t(d), iter = mcmc_iter, chains = mcmc_chains, thin = thin, num_trends = 1, trend_model = "bs", n_knots = 30,par_list="all")
loos[[6]] = loo::loo(m[[6]]$model,moment_match=TRUE)
saveRDS(loos, "Output/legacy_catch_loos.rds")

# fit models with varying numbers of knots in a b-spline
m[[7]] <- fit_dfa(y = t(d), iter = mcmc_iter, chains = mcmc_chains, thin = thin, num_trends = 1, trend_model = "gp", n_knots = 6,par_list="all")
loos[[7]] = loo::loo(m[[7]]$model,moment_match=TRUE)
saveRDS(m,"Output/legacy_catch_models.rds")
saveRDS(loos, "Output/legacy_catch_loos.rds")

m[[8]] <- fit_dfa(y = t(d), iter = mcmc_iter, chains = mcmc_chains, thin = thin, num_trends = 1, trend_model = "gp", n_knots = 12,par_list="all")
loos[[8]] = loo::loo(m[[8]]$model,moment_match=TRUE)
saveRDS(m,"Output/legacy_catch_models.rds")
saveRDS(loos, "Output/legacy_catch_loos.rds")

m[[9]] <- fit_dfa(y = t(d), iter = mcmc_iter, chains = mcmc_chains, thin = thin, num_trends = 1, trend_model = "gp", n_knots = 18,par_list="all")
loos[[9]] = loo::loo(m[[9]]$model,moment_match=TRUE)
saveRDS(m,"Output/legacy_catch_models.rds")
saveRDS(loos, "Output/legacy_catch_loos.rds")

m[[10]] <- fit_dfa(y = t(d), iter = mcmc_iter, chains = mcmc_chains, thin = thin, num_trends = 1, trend_model = "gp", n_knots = 24,par_list="all")
loos[[10]] = loo::loo(m[[10]]$model,moment_match=TRUE)
saveRDS(loos, "Output/legacy_catch_loos.rds")
saveRDS(m,"Output/legacy_catch_models.rds")

m[[11]] <- fit_dfa(y = t(d), iter = mcmc_iter, chains = mcmc_chains, thin = thin, num_trends = 1, trend_model = "gp", n_knots = 30,par_list="all")
loos[[11]] = loo::loo(m[[11]]$model,moment_match=TRUE)
saveRDS(loos, "Output/legacy_catch_loos.rds")
saveRDS(m,"Output/legacy_catch_models.rds")

m[[12]] <- fit_dfa(y = t(d), iter = mcmc_iter, chains = mcmc_chains, thin = thin, num_trends = 1, trend_model = "gp", n_knots = 39,par_list="all")
loos[[12]] = loo::loo(m[[12]]$model,moment_match=TRUE)

# fit models with varying numbers of knots in a b-spline
m[[13]] <- fit_dfa(y = t(d), iter = mcmc_iter, chains = mcmc_chains, thin = thin, num_trends = 1, trend_model = "ps", n_knots = 6,par_list="all")
loos[[13]] = loo::loo(m[[13]]$model,moment_match=TRUE)
saveRDS(loos, "Output/legacy_catch_loos.rds")

m[[14]] <- fit_dfa(y = t(d), iter = mcmc_iter, chains = mcmc_chains, thin = thin, num_trends = 1, trend_model = "ps", n_knots = 12,par_list="all")
loos[[14]] = loo::loo(m[[14]]$model,moment_match=TRUE)
saveRDS(loos, "Output/legacy_catch_loos.rds")

m[[15]] <- fit_dfa(y = t(d), iter = mcmc_iter, chains = mcmc_chains, thin = thin, num_trends = 1, trend_model = "ps", n_knots = 18,par_list="all")
loos[[15]] = loo::loo(m[[15]]$model,moment_match=TRUE)
saveRDS(loos, "Output/legacy_catch_loos.rds")

m[[16]] <- fit_dfa(y = t(d), iter = mcmc_iter, chains = mcmc_chains, thin = thin, num_trends = 1, trend_model = "ps", n_knots = 24,par_list="all")
loos[[16]] = loo::loo(m[[16]]$model,moment_match=TRUE)
saveRDS(loos, "Output/legacy_catch_loos.rds")

m[[17]] <- fit_dfa(y = t(d), iter = mcmc_iter, chains = mcmc_chains, thin = thin, num_trends = 1, trend_model = "ps", n_knots = 30,par_list="all")
loos[[17]] = loo::loo(m[[17]]$model,moment_match=TRUE)
saveRDS(loos, "Output/legacy_catch_loos.rds")

saveRDS(loos, "Output/legacy_catch_loos.rds")
saveRDS(m,"Output/legacy_catch_models.rds")
