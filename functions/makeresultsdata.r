makeresultsdata <- function(data_date, data, ESU_DPS_list, Recovery_Goals, Smoothed_Abundance) {
  SubDir <- paste("results ", data_date, sep = "")
  if (!file.exists(SubDir)) {
    dir.create(file.path(SubDir))
  }
  # add smoothed abundance
  Smoothed_Abundance <- data.frame(read.csv(paste(SubDir, "/Smoothed_Abundance_", data_date, ".csv", sep = "")))
  tdat <- data.frame(merge(data[, colnames(data) %in% c("ESU_DPS", "COMMONPOPNAME2", "SPAWNINGYEAR", "final_abundance")], Smoothed_Abundance[, colnames(Smoothed_Abundance) %in% c("ESU_DPS", "COMMON_POPULATION_NAME", "BROOD_YEAR", "Smoothed.Abundance")], by.x = c("ESU_DPS", "COMMONPOPNAME2", "SPAWNINGYEAR"), by.y = c("ESU_DPS", "COMMON_POPULATION_NAME", "BROOD_YEAR"), all = T))
  tdat[, colnames(tdat) %in% c("final_abundance", "Smoothed.Abundance")] <- apply(tdat[, colnames(tdat) %in% c("final_abundance", "Smoothed.Abundance")], 2, as.numeric)
  print("datasets with no smoothed abundance estimates: ")
  print(data.frame(tdat %>% group_by(ESU_DPS, COMMONPOPNAME2) %>% summarise(SA = max(Smoothed.Abundance, na.rm = T)) %>% filter(SA < 1)))
  tdat2 <- data.frame(data %>% group_by(ESU_DPS, COMMONPOPNAME2) %>% summarise(
    COMMONNAME = first(COMMONNAME),
    RUN = first(RUN),
    MAJORPOPGROUP = first(MAJORPOPGROUP),
    WATERBODY = first(WATERBODY),
    datasource = first(datasource)
  ))
  data <- merge(tdat2, tdat, by = c("ESU_DPS", "COMMONPOPNAME2"), all.x = T)
  # add listing dates
  ESU_DPSs <- data.frame(read.csv(paste("data/", ESU_DPS_list, sep = "")))
  data <- merge(data, ESU_DPSs[, colnames(ESU_DPSs) %in% c("ESU_DPS", "ESA.listing.year")], by = "ESU_DPS", all = T)
  # add recovery goals
  Recovery_Goals <- data.frame(read.csv(paste("data/", Recovery_Goals, sep = "")))
  # data<-merge(data,Recovery_Goals[,colnames(Recovery_Goals)%in%c("ESU_DPS", "COMMON_POPULATION_NAME","Recovery.goal","High.productivity.recovery.goal")],by.x=c("ESU_DPS","COMMONPOPNAME2"),by.y=c("ESU_DPS","COMMON_POPULATION_NAME"),all.x=T)
  data <- merge(data, Recovery_Goals[, colnames(Recovery_Goals) %in% c("ESU_DPS", "COMMON_POPULATION_NAME", "Recovery.goal", "High.productivity.recovery.goal")], by.x = c("ESU_DPS", "COMMONPOPNAME2"), by.y = c("ESU_DPS", "COMMON_POPULATION_NAME"), all.x = T, all.y = T)
  # set results directory
  return(data)
}
