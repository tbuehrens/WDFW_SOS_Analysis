combineresults <- function(data_date, Recovery_Goals) { # ESU_DIP_list_all
  SubDir <- paste("results ", data_date, sep = "")
  if (!file.exists(SubDir)) {
    dir.create(file.path(SubDir))
  }
  # ESU_DIP_list_all<-data.frame(read.csv(ESU_DIP_list_all))
  Recovery_Goals <- data.frame(read.csv(paste("data/", Recovery_Goals, sep = "")))
  files <- list.files(path = SubDir)
  slope_files <- files[grep("Slope", files)]
  Percentchange <- do.call("rbind", lapply(slope_files, FUN = function(files) {
    read.csv(paste(SubDir, "/", files, sep = ""))
  }))
  # Percentchange_formatted<- merge(Percentchange_formatted,ESU_DIP_list_all[,colnames(ESU_DIP_list_all)%in%c("ESU_DPS","COMMON_POPULATION_NAME")],by="COMMON_POPULATION_NAME",all.x = T)
  Percentchange <- merge(Percentchange, Recovery_Goals[, colnames(Recovery_Goals) %in% c("ESU_DPS", "COMMON_POPULATION_NAME")], by = "COMMON_POPULATION_NAME", all.x = T)
  Percentchange <- Percentchange[order(Percentchange$ESU_DPS, Percentchange$COMMON_POPULATION_NAME), ]
  write.csv(Percentchange, paste(SubDir, "/Percent_change_", data_date, ".csv", sep = ""), row.names = F)

  smooth_files <- files[grep("SmoothResults", files)]
  Smoothed_Abundance <- do.call("rbind", lapply(smooth_files, FUN = function(files) {
    read.csv(paste(SubDir, "/", files, sep = ""))
  }))
  # Smoothed_Abundance<- merge(Smoothed_Abundance,ESU_DIP_list_all[,colnames(ESU_DIP_list_all)%in%c("ESU_DPS","COMMON_POPULATION_NAME")],by="COMMON_POPULATION_NAME",all.x = T)
  Smoothed_Abundance <- merge(Smoothed_Abundance, Recovery_Goals[, colnames(Recovery_Goals) %in% c("ESU_DPS", "COMMON_POPULATION_NAME")], by = "COMMON_POPULATION_NAME", all.x = T)
  Smoothed_Abundance <- Smoothed_Abundance[order(Smoothed_Abundance$ESU_DPS, Smoothed_Abundance$COMMON_POPULATION_NAME), ]
  Smoothed_Abundance <- data.frame(Smoothed_Abundance %>% dplyr::select(ESU = ESU, ESU_DPS = ESU_DPS, COMMON_POPULATION_NAME, BROOD_YEAR, Smoothed.Abundance = Q50.))
  write.csv(Smoothed_Abundance, paste(SubDir, "/Smoothed_Abundance_", data_date, ".csv", sep = ""), row.names = F)
}
