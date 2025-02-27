make_escapement_data_wa_gov <- function(data_date, Recovery_Goals_LUT_edited, databeforelisting = "No") {
  SubDir <- paste("results ", data_date, sep = "")
  if (!file.exists(SubDir)) {
    dir.create(file.path(SubDir))
  }
  if (databeforelisting == "Yes") {
    # ===========================================================
    # re-run data filter code to include data from before listing
    # ===========================================================
    # prep and filter ca data
    dat <- prepCAdata(
      CAfilename = CAfilename,
      data_date = data_date,
      ESU_DPS_list = ESU_DPS_list,
      Recovery_Goals = Recovery_Goals,
      POPFIT_exceptions = POPFIT_exceptions,
      specialcaselist = specialcaselist,
      databeforelisting = "Yes"
    )
    # prep NON CA data (data must be pre-filtered/final )
    dat2 <- prepNONCAdata(
      data_date = data_date,
      NONCAfilename = NONCAfilename,
      ESU_DPS_list = ESU_DPS_list,
      databeforelisting = "Yes"
    )
    # make analysis files for status and trend analysis
    # (we use bind_rows to combine CA and Non-CA data)
    makefiles(
      data = bind_rows(dat[, colnames(dat) %in% colnames(dat2)], dat2),
      data_date = data_date,
      databeforelisting = "Yes"
    )
  }
  Recovery_Goals_LUT <- data.frame(read.csv(paste("data/", Recovery_Goals_LUT_edited, sep = "")))
  # Smoothed_Abundance<-data.frame(read.csv(paste(mainDir,"ESU_DPS_FILES"," ",data_date,"\\Smoothed_Abundance_",data_date,".csv",sep="")))
  if (databeforelisting == "Yes") {
    All_Data <- data.frame(read.csv(paste(SubDir, "/All_Data_incl_before_listing_", data_date, ".csv", sep = "")))
  } else {
    (All_Data <- data.frame(read.csv(paste(SubDir, "/All_Data_", data_date, ".csv", sep = ""))))
  }
  dat <- Recovery_Goals_LUT %>%
    left_join(All_Data, by = c("COMMON_POPULATION_NAME" = "COMMONPOPNAME2")) %>%
    mutate(`Stock Number` = Stock.Number, `Population Name` = COMMON_POPULATION_NAME, Year = SPAWNINGYEAR, `Abundance Quantity` = final_abundance, `Production Type` = "Natural", `Calculation Type` = "Expanded", `Report Types` = "SCoRE,SOS") %>%
    dplyr::select(`Stock Number`, `Population Name`, Year, `Abundance Quantity`, `Production Type`, `Calculation Type`, `Report Types`) %>%
    mutate(`Calculation Type` = ifelse(is.na(Year), NA, `Calculation Type`))
  write.csv(dat, paste(SubDir, "/Escapement_Data_For_data_wa_gov.csv", sep = ""), row.names = F)
  # version 2 with all of the preexisting data
  dat <- dat %>% mutate(`Escapement Methodology` = "SOS 2020 Abundance Analysis")
  SASI <- data.frame(read_csv("https://data.wa.gov/api/views/fgyz-n3uk/rows.csv?accessType=DOWNLOAD&bom=true&format=true"))
  newnames <- gsub("\\.", " ", colnames(SASI))
  SASI <- SASI %>%
    rename_at(vars(colnames(SASI)), ~ gsub("\\.", " ", colnames(SASI))) %>%
    filter(`Escapement Methodology` != "SOS 2020 Abundance Analysis")
  dat <- bind_rows(dat, SASI)
  write.csv(dat, paste(SubDir, "/Escapement_Data_For_data_wa_gov_with_existing.csv", sep = ""), row.names = F)
}
