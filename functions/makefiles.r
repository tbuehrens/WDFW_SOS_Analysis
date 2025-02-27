# make files for analysis function
makefiles <- function(data, data_date, databeforelisting = "No") {
  SubDir <- paste("results ", data_date, sep = "")
  if (!file.exists(SubDir)) {
    dir.create(file.path(SubDir))
  }
  if (databeforelisting == "No") {
    for (i in 1:length(unique(data$ESU_DPS))) {
      tdat <- data.frame(data %>%
        filter(ESU_DPS == unique(data$ESU_DPS)[i]) %>%
        dplyr::select(
          ESU = ESU_DPS_COMMONNAME,
          MAJOR_POPULATION_GROUP = MAJORPOPGROUP,
          COMMON_POPULATION_NAME = COMMONPOPNAME2,
          SPECIES = COMMONNAME,
          RUN_TIMING = RUN,
          STREAM_NAME = WATERBODY,
          BROOD_YEAR = SPAWNINGYEAR,
          NUMBER_OF_SPAWNERS = final_abundance
        ))
      tdat$FRACWILD <- 1
      tdat$CATCH <- -99
      tdat$AGE_1_RETURNS <- -99
      tdat$AGE_2_RETURNS <- -99
      tdat$AGE_3_RETURNS <- -99
      tdat$AGE_4_RETURNS <- -99
      tdat$AGE_5_RETURNS <- -99
      tdat$AGE_6_RETURNS <- -99
      tdat$AGE_7_RETURNS <- -99
      tdat <- tdat[order(tdat$COMMON_POPULATION_NAME, tdat$BROOD_YEAR), ]
      tdat <- tdat[!is.na(tdat$NUMBER_OF_SPAWNERS), ]
      write.csv(tdat, paste(SubDir, "/", unique(data$ESU_DPS_COMMONNAME)[i], "_", data_date, ".csv", sep = ""), row.names = F)
    }
    spi_data <- data.frame(read.csv(paste(SubDir, "/All_SPi_Data_", data_date, ".csv", sep = "")))
    names <- colnames(data)
    All_Data <- merge(data, spi_data[, !colnames(spi_data) %in% names | colnames(spi_data) %in% c("ESU_DPS", "COMMONPOPNAME2", "SPAWNINGYEAR")], by = c("ESU_DPS", "COMMONPOPNAME2", "SPAWNINGYEAR"), all = T)
    write.csv(All_Data, paste(SubDir, "/All_Data_", data_date, ".csv", sep = ""), row.names = F)
  }
  if (databeforelisting == "Yes") {
    spi_data <- data.frame(read.csv(paste(SubDir, "/All_SPi_Data_incl_before_listing_", data_date, ".csv", sep = "")))
    names <- colnames(data)
    All_Data <- merge(data, spi_data[, !colnames(spi_data) %in% names | colnames(spi_data) %in% c("ESU_DPS", "COMMONPOPNAME2", "SPAWNINGYEAR")], by = c("ESU_DPS", "COMMONPOPNAME2", "SPAWNINGYEAR"), all = T)
    write.csv(All_Data, paste(SubDir, "/All_Data_incl_before_listing_", data_date, ".csv", sep = ""), row.names = F)
  }
}
