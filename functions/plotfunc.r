# plot raw data function
plotfunc <- function(data_date, data, ESU_DPS_list, Recovery_Goals, Withgoalsonly) {
  SubDir <- paste("results ", data_date, sep = "")
  if (!file.exists(SubDir)) {
    dir.create(file.path(SubDir))
  }
  # add listing dates
  ESU_DPSs <- data.frame(read.csv(paste("data/", ESU_DPS_list, sep = "")))
  data <- merge(data, ESU_DPSs[, colnames(ESU_DPSs) %in% c("ESU_DPS", "ESA.listing.year")], by = "ESU_DPS")
  # add recovery goals
  Recovery_Goals <- data.frame(read.csv(paste("data/", Recovery_Goals, sep = "")))
  data <- merge(data, Recovery_Goals[, colnames(Recovery_Goals) %in% c("ESU_DPS", "COMMON_POPULATION_NAME", "Recovery.goal", "High.productivity.recovery.goal")], by.x = c("ESU_DPS", "COMMONPOPNAME2"), by.y = c("ESU_DPS", "COMMON_POPULATION_NAME"), all.x = T, all.y = T)
  print(data.frame(data %>% group_by(ESU_DPS, COMMONPOPNAME2) %>% summarise(goal = first(Recovery.goal)) %>% filter(is.na(goal))))
  if (Withgoalsonly == "yes") {
    data <- data.frame(data %>% filter(!is.na(Recovery.goal)))
  }
  # new names for ESUs
  Location <- data.frame(unlist(strsplit(data$ESU_DPS, "\\(|\\)"))) %>% filter(row_number() %% 2 == 0)
  Location[, 1] <- capitalize_str(gsub("-run", "", Location[, 1]))
  Location[, 1] <- gsub("/s", "/S", Location[, 1])
  Species <- data.frame(unlist(strsplit(data$ESU_DPS, "\\(|\\)"))) %>% filter(row_number() %% 2 != 0)
  Species <- data.frame(strsplit(as.character(Species[, 1]), ","))[2, ]
  Species <- as.character(gsub(" ", "", as.character(unlist(Species))))
  Species <- paste(toupper(substring(Species, 1, 1)), substring(Species, 2), sep = "")
  ESU_DPS2 <- paste(as.character(Location[, 1]), as.character(Species), sep = "")
  ESU_DPS2 <- gsub("DPS", "", ESU_DPS2)
  ESU_DPS2 <- gsub("ESU", "", ESU_DPS2)
  # ESU_DPS2<-gsub("Basin","",ESU_DPS2)
  ESU_DPS2 <- gsub("  ", " ", ESU_DPS2)
  ESU_DPS2 <- gsub("  ", " ", ESU_DPS2)
  ESU_DPS2[grep(c("Steelhead"), ESU_DPS2)] <- paste(ESU_DPS2[grep(c("Steelhead"), ESU_DPS2)], "DPS")
  ESU_DPS2[grep(c("Chinook"), ESU_DPS2)] <- paste(ESU_DPS2[grep(c("Chinook"), ESU_DPS2)], "ESU")
  ESU_DPS2[grep(c("Coho"), ESU_DPS2)] <- paste(ESU_DPS2[grep(c("Coho"), ESU_DPS2)], "ESU")
  ESU_DPS2[grep(c("Sockeye"), ESU_DPS2)] <- paste(ESU_DPS2[grep(c("Sockeye"), ESU_DPS2)], "ESU")
  ESU_DPS2[grep(c("Chum"), ESU_DPS2)] <- paste(ESU_DPS2[grep(c("Chum"), ESU_DPS2)], "ESU")
  data$ESU_DPS2 <- ESU_DPS2
  # visualize data
  p <- list(NULL)
  pdf(paste(SubDir, "/ESU_and_DIP_Plots.pdf", sep = ""), onefile = TRUE)
  ESU_list <- unique(data$ESU_DPS2)
  for (i in 1:length(ESU_list)) {
    # visualize data at ESU level
    p[[i]] <- ggplot(subset(data, ESU_DPS2 == ESU_list[i]), aes(y = final_abundance, x = SPAWNINGYEAR, color = COMMONPOPNAME2)) +
      geom_line() +
      theme_bw() +
      theme(legend.position = "right", legend.text = element_text(size = 5), legend.title = element_text(size = 6)) +
      labs(color = ESU_list[i]) +
      ylab(label = "Natural Origin Spawner Abundance") +
      xlim(min(subset(data$ESA.listing.year, data$ESU_DPS2 == ESU_list[i])), as.numeric(format(Sys.Date(), "%Y"))) +
      geom_vline(aes(xintercept = min(subset(data$ESA.listing.year, data$ESU_DPS2 == ESU_list[i]))), linetype = 5) +
      expand_limits(y = 0)
    # scale_y_log10()
    print(p[[i]])
  }
  # visualize data at ESU level
  for (j in 1:length(ESU_list)) {
    for (k in 1:ceiling(length(unique(data$COMMONPOPNAME2[data$ESU_DPS2 == ESU_list[j]])) / 5)) {
      p[[length(ESU_list) + j]] <- ggplot(subset(data, ESU_DPS2 == ESU_list[j]), aes(y = final_abundance, x = SPAWNINGYEAR, color = COMMONPOPNAME2)) +
        geom_line() +
        facet_wrap_paginate(~COMMONPOPNAME2, nrow = 5, ncol = 1, scales = "free", strip.position = "top", page = k) +
        theme_bw() +
        theme(legend.position = "none", strip.text = element_text(size = 5)) +
        ylab(label = "Natural Origin Spawner Abundance") +
        xlim(min(subset(data$ESA.listing.year, data$ESU_DPS2 == ESU_list[j])), as.numeric(format(Sys.Date(), "%Y"))) +
        labs(title = ESU_list[j]) +
        geom_vline(aes(xintercept = min(subset(data$ESA.listing.year, data$ESU_DPS2 == ESU_list[j]))), linetype = 5, show.legend = T) +
        geom_hline(data = subset(data, ESU_DPS2 == ESU_list[j]), aes(yintercept = Recovery.goal), linetype = 3, show.legend = T) +
        expand_limits(y = 0)
      # scale_y_log10()
      print(p[[length(ESU_list) + j]])
    }
  }
  dev.off()
}
