make_ESU_DIP_results <- function(resultsdata, lastyear, data_date, geomeanyears, filtergeomeansforpopswithnonewdata) {
  SubDir <- paste("results ", data_date, sep = "")
  if (!file.exists(SubDir)) {
    dir.create(file.path(SubDir))
  }
  data <- resultsdata
  if (filtergeomeansforpopswithnonewdata == "Yes") {
    # filter so we DO NOT report geomeans and trends for pops with NO new data during years of interest (for calc of geomean)
    nonewdata <- data.frame(data %>% group_by(ESU_DPS, COMMONPOPNAME2) %>% filter(SPAWNINGYEAR <= lastyear & SPAWNINGYEAR >= lastyear - (geomeanyears - 1) & is.na(final_abundance)) %>% summarise(count = n()) %>% filter(count > 4))
    data$Smoothed.Abundance[data$COMMONPOPNAME2 %in% nonewdata$COMMONPOPNAME2] <- NA
  }
  # filter to calculated geomean based on specified yrs of smoothed abundance
  data <- data.frame(data %>% group_by(ESU_DPS, COMMONPOPNAME2) %>% filter(SPAWNINGYEAR <= lastyear & SPAWNINGYEAR >= lastyear - (geomeanyears - 1) | is.na(SPAWNINGYEAR)) %>% summarise(minYR = min(SPAWNINGYEAR), maxYR = max(SPAWNINGYEAR), Geomean_5yr = exp(mean(log(Smoothed.Abundance))), Recovery.goal = first(Recovery.goal)))
  data$Geomean_5yr_prop_goal <- data$Geomean_5yr / data$Recovery.goal
  data$abovegoal <- ifelse(data$Geomean_5yr_prop_goal >= 1, 1, ifelse(!data$Geomean_5yr_prop_goal %in% c(NA, Inf), 0, NA))
  Percent_change <- data.frame(read.csv(paste(SubDir, "/Percent_change_", data_date, ".csv", sep = "")))
  Percent_change$Percent.change.per.year <- (exp(Percent_change$Q50.) - 1) * 100
  data <- merge(data, Percent_change[, colnames(Percent_change) %in% c("ESU_DPS", "COMMON_POPULATION_NAME", "Percent.change.per.year")], by.x = c("ESU_DPS", "COMMONPOPNAME2"), by.y = c("ESU_DPS", "COMMON_POPULATION_NAME"), all.x = T)
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
  # writeout results
  writedata <- data[, !colnames(data) %in% c("ESU_DPS")]
  # colnames(writedata)[colnames(writedata)%in%c("ESU_DPS2","COMMONPOPNAME2")]<-c("ESU_DPS","COMMON_POPULATION_NAME")
  write.csv(writedata, paste(SubDir, "/DIP_Results ", data_date, ".csv", sep = ""), row.names = F)
  return(data)
}
