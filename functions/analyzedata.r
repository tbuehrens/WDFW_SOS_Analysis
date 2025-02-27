analyzedata <- function(data_date, ESUsubset, ESU_DPS_list, cores = 4, chains = 4, iter = 2000, warmup = 1000, thin = 1, control = list(adapt_delta = 0.9995)) {
  if (length(ESUsubset) > 0) {
    ESU_DPSs <- data.frame(read.csv(paste("data/", ESU_DPS_list, sep = "")))
    SubDir <- paste("results ", data_date, sep = "")
    if (!file.exists(SubDir)) {
      dir.create(file.path(SubDir))
    }
    analysislist <- paste(ESUsubset, "_", data_date, ".csv", sep = "")
    for (i in 1:length(analysislist)) {
      # fit model to real data
      dat <- data.frame(read.csv(paste(SubDir, "/", analysislist[i], sep = "")))
      # dat<-dat[dat$COMMON_POPULATION_NAME=="Coweeman River - late Coho salmon",]
      dat <- merge(dat, ESU_DPSs[, colnames(ESU_DPSs) %in% c("ESU_DPS_COMMONNAME", "ESA.listing.year")], by.x = "ESU", by.y = "ESU_DPS_COMMONNAME")
      pops <- length(unique(dat$COMMON_POPULATION_NAME))
      if (pops > 1) {
        stan.dat <- list(
          run_estimation = 1,
          T = max(dat$BROOD_YEAR) - min(dat$BROOD_YEAR) + 1,
          T_forward = as.numeric(format(Sys.Date(), "%Y")) - max(dat$BROOD_YEAR),
          T_backward = min(dat$BROOD_YEAR) - min(dat$ESA.listing.year),
          P = length(unique(dat$COMMON_POPULATION_NAME)),
          n = nrow(dat),
          N_obs = pmax(dat$NUMBER_OF_SPAWNERS, 1),
          pop_obs = as.numeric(as.factor(dat$COMMON_POPULATION_NAME)),
          year_obs = dat$BROOD_YEAR - min(dat$BROOD_YEAR) + 1,
          N_0_med_prior = pmax(as.numeric(c(unlist(data.frame(dat %>% group_by(COMMON_POPULATION_NAME) %>% filter(BROOD_YEAR == min(BROOD_YEAR)) %>% summarise(first(NUMBER_OF_SPAWNERS)))[, 2]))), 1)
        )
        model <- stan_model("models/model_mv_v3.stan")
      }
      if (pops == 1) {
        stan.dat <- list(
          T = max(dat$BROOD_YEAR) - min(dat$BROOD_YEAR) + 1,
          T_forward = as.numeric(format(Sys.Date(), "%Y")) - max(dat$BROOD_YEAR),
          T_backward = min(dat$BROOD_YEAR) - min(dat$ESA.listing.year),
          n = nrow(dat),
          N_obs = pmax(dat$NUMBER_OF_SPAWNERS, 1),
          year_obs = dat$BROOD_YEAR - min(dat$BROOD_YEAR) + 1,
          N_0_med_prior = pmax(unlist(data.frame(dat %>% filter(BROOD_YEAR == min(BROOD_YEAR)) %>% summarise(first(NUMBER_OF_SPAWNERS)))), 1)
        )
        model <- stan_model("models/model_uv.stan")
      }
      stanfit <- sampling(model,
        data = stan.dat,
        cores = cores,
        chains = chains,
        iter = iter, # 21000,#2000,#3000
        warmup = warmup, # 20000,#1000,#1000
        thin = thin,
        control = control # , max_treedepth=12) # for model 2
        # ,control=list(adapt_delta=0.99999)#, max_treedepth=12) # for model 2
        , verbose = F,
        open_progress = F,
        show_messages = F,
        algorithm = "NUTS"
      )
      # library(shinystan)
      # launch_shinystan(stanfit)
      # parset<-c("sigma_rn","sigma_wn","slope")
      # pairs(stanfit,pars=parset)
      summary <- summary(stanfit)$summary
      write.csv(summary, paste(SubDir, "/", ESUsubset[i], "_STAN_summary", data_date, ".csv", sep = ""))
      res <- rstan::extract(stanfit)
      if (pops > 1) {
        N_quants <- apply(res$N_all, 2:3, function(x) quantile(x, c(0.025, 0.25, 0.5, 0.75, 0.975)))
        dimnames(N_quants)[[3]] <- unique(dat$COMMON_POPULATION_NAME)
        dimnames(N_quants)[[2]] <- min(dat$ESA.listing.year):as.numeric(format(Sys.Date(), "%Y"))
        dimnames(N_quants)[[1]] <- rownames(N_quants)
        N_quants <- melt(N_quants)
        colnames(N_quants) <- c("Quantile", "BROOD_YEAR", "COMMON_POPULATION_NAME", "NUMBER_OF_SPAWNERS")
        N_quants <- data.frame(N_quants %>% group_by(COMMON_POPULATION_NAME, BROOD_YEAR) %>% spread(key = Quantile, value = NUMBER_OF_SPAWNERS))
        colnames(N_quants) <- gsub("X", "Q", colnames(N_quants))
        N_quants$ESU <- ESUsubset[i]
        dat2 <- merge(N_quants, dat[, colnames(dat) %in% c("COMMON_POPULATION_NAME", "BROOD_YEAR", "NUMBER_OF_SPAWNERS")], by = c("COMMON_POPULATION_NAME", "BROOD_YEAR"), all.x = T)
        dat3 <- data.frame(t(apply(res$slope, 2, function(x) quantile(x, c(0.025, 0.25, 0.5, 0.75, 0.975)))))
        colnames(dat3) <- gsub("X", "Q", colnames(dat3))
        dat3$COMMON_POPULATION_NAME <- unique(dat$COMMON_POPULATION_NAME)
        dat3$ESU <- ESUsubset[i]
      }
      if (pops == 1) {
        N_quants <- apply(res$N_all, 2, function(x) quantile(x, c(0.025, 0.25, 0.5, 0.75, 0.975)))
        dimnames(N_quants)[[2]] <- min(dat$ESA.listing.year):as.numeric(format(Sys.Date(), "%Y"))
        dimnames(N_quants)[[1]] <- rownames(N_quants)
        N_quants <- melt(N_quants)
        colnames(N_quants) <- c("Quantile", "BROOD_YEAR", "NUMBER_OF_SPAWNERS")
        N_quants$COMMON_POPULATION_NAME <- dat$COMMON_POPULATION_NAME[1]
        N_quants <- data.frame(N_quants %>% group_by(BROOD_YEAR) %>% spread(key = Quantile, value = NUMBER_OF_SPAWNERS))
        colnames(N_quants) <- gsub("X", "Q", colnames(N_quants))
        N_quants$ESU <- ESUsubset[i]
        dat2 <- merge(N_quants, dat[, colnames(dat) %in% c("COMMON_POPULATION_NAME", "BROOD_YEAR", "NUMBER_OF_SPAWNERS")], by = c("COMMON_POPULATION_NAME", "BROOD_YEAR"), all.x = T)
        dat3 <- data.frame(t(quantile(res$slope, c(0.025, 0.25, 0.5, 0.75, 0.975))))
        colnames(dat3) <- gsub("X", "Q", colnames(dat3))
        dat3$COMMON_POPULATION_NAME <- unique(dat$COMMON_POPULATION_NAME)
        dat3$ESU <- ESUsubset[i]
      }
      write.csv(dat2, paste(SubDir, "/", ESUsubset[i], "_", data_date, "_SmoothResults.csv", sep = ""), row.names = F)
      write.csv(dat3, paste(SubDir, "/", ESUsubset[i], "_", data_date, "_Slope.csv", sep = ""), row.names = F)
    }
  }
}
