# plot ESU DIP TIMESERIES RESULTS function
plotfunc2 <- function(resultsdata, Recovery_Goals, Withgoalsonly, data_date) {
  SubDir <- paste("results ", data_date, sep = "")
  if (!file.exists(SubDir)) {
    dir.create(file.path(SubDir))
  }
  Recovery_Goals <- data.frame(read.csv(paste("data/", Recovery_Goals, sep = "")))
  data <- resultsdata
  if (Withgoalsonly == "yes") {
    nogoal <- unique(data$COMMONPOPNAME2[is.na(dat$Recovery.goal)])
    data <- data.frame(data %>% filter(!COMMONPOPNAME2 %in% nogoal))
  }
  # make pretty ESU names
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
  pdf(paste(SubDir, "/ESU_and_DIP_Plots_with_Results_", data_date, ".pdf", sep = ""), onefile = TRUE)
  ESU_list <- unique(data$ESU_DPS2)
  counter <- 0
  popcount <- c(NULL)
  for (i in 1:length(ESU_list)) {
    counter <- counter + 1
    # visualize data at ESU level
    p[[counter]] <- ggplot(subset(data, ESU_DPS2 == ESU_list[i]), aes(y = final_abundance, x = SPAWNINGYEAR, color = COMMONPOPNAME2)) +
      geom_line() +
      theme_bw() +
      theme(legend.position = "right", legend.text = element_text(size = 5), legend.title = NULL, legend.key.height = unit(0.2, "in")) +
      labs(color = "Observed Abundance") +
      guides(colour = guide_legend(ncol = 1)) +
      ggtitle(ESU_list[i]) +
      ylab(label = "Natural Origin Spawner Abundance") +
      xlim(min(subset(data$ESA.listing.year, data$ESU_DPS2 == ESU_list[i]), na.rm = T), as.numeric(format(Sys.Date(), "%Y"))) +
      geom_vline(aes(xintercept = min(subset(data$ESA.listing.year, data$ESU_DPS2 == ESU_list[i]), na.rm = T)), linetype = 5) +
      expand_limits(y = 0) +
      annotate("text", x = Inf, y = Inf, label = paste("Data Updated ", data_date, sep = ""), vjust = 1.5, hjust = 1, col = "grey60", cex = 2, alpha = 0.8, srt = 00)
    # labs(caption="Caption: Observed abundance since year \nof ESA-listing (vertical dashed line).")+
    # theme(legend.position =  "none",axis.text=element_text(size=6),
    #       plot.caption = element_text(vjust = -1, hjust=0, colour = "blue",size=10))

    # scale_y_log10()
    print(p[[counter]])
    # visualize data at ESU level
    popcount[i] <- ceiling(length(unique(data$COMMONPOPNAME2[data$ESU_DPS2 == ESU_list[i]])) / 5)
    for (k in 1:ceiling(length(unique(data$COMMONPOPNAME2[data$ESU_DPS2 == ESU_list[i]])) / 5)) {
      counter <- counter + 1
      p[[counter]] <- ggplot(subset(data, ESU_DPS2 == ESU_list[i]), aes(y = final_abundance, x = SPAWNINGYEAR, color = COMMONPOPNAME2)) +
        geom_point() +
        facet_wrap_paginate(~COMMONPOPNAME2, nrow = 5, ncol = 1, scales = "free", strip.position = "top", page = k) +
        theme_bw() +
        theme(legend.position = "none", strip.text = element_text(size = 5)) +
        ylab(label = "Natural Origin Spawner Abundance") +
        xlim(min(subset(data$ESA.listing.year, data$ESU_DPS2 == ESU_list[i]), na.rm = T), as.numeric(format(Sys.Date(), "%Y"))) +
        labs(title = ESU_list[i]) +
        geom_vline(aes(xintercept = min(subset(data$ESA.listing.year, data$ESU_DPS2 == ESU_list[i]), na.rm = T)), linetype = 5, show.legend = T) +
        geom_hline(data = subset(data, ESU_DPS2 == ESU_list[i]), aes(yintercept = Recovery.goal), linetype = 3, show.legend = T) +
        # geom_hline(data=subset(Recovery_Goals,ESU_DPS==ESU_list[j] & COMMON_POPULATION_NAME == unique(data$COMMONPOPNAME2[data$ESU_DPS==ESU_list[j]])[k]),aes(yintercept=Recovery.goal),linetype = 3,show.legend = T)+
        geom_line(aes(Smoothed.Abundance, x = SPAWNINGYEAR, color = COMMONPOPNAME2)) +
        expand_limits(y = 0) +
        labs(caption = "Caption: Observed (points) and smoothed (line) abundance since year of ESA-listing (vertical dashed \nline). Horizontal dotted line is recovery goal.") +
        theme(
          legend.position = "none", axis.text = element_text(size = 6),
          plot.caption = element_text(vjust = -1, hjust = 0, colour = "blue", size = 10)
        ) +
        annotate("text", x = Inf, y = Inf, label = paste("Data Updated ", data_date, sep = ""), vjust = 1.5, hjust = 1, col = "grey60", cex = 2, alpha = 0.8, srt = 00)
      # scale_y_log10()
      print(p[[counter]])
    }
  }
  dev.off()
  results <- list(p, popcount)
  names(results) <- c("plots", "popcount")
  return(results)
}
