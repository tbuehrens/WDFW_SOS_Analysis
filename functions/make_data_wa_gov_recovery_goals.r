make_data_wa_gov_recovery_goals <- function(data_date, Recovery_Goals, Recovery_Goals_LUT_edited, ESU_DPS_list) {
  SubDir <- paste("results ", data_date, sep = "")
  if (!file.exists(SubDir)) {
    dir.create(file.path(SubDir))
  }
  Recovery_Goals <- data.frame(read.csv(paste("data/", Recovery_Goals, sep = "")))
  ESU_DPS_list <- data.frame(read.csv(paste("data/", ESU_DPS_list, sep = "")))
  try(Recovery_Goals_LUT_edited <- data.frame(read.csv(paste("data/", Recovery_Goals_LUT_edited, sep = ""))))
  if (!exists("Recovery_Goals_LUT_edited")) {
    stop("You have not specified Recovery_Goals_LUT_edited; please review Recovery_Goals_LUT_raw and after making changes, save in same location as 'Recovery_Goals_LUT_edited.csv'")
  }
  Recovery_Goals_new <- Recovery_Goals %>%
    left_join(ESU_DPS_list, by = c("ESU_DPS")) %>%
    left_join(Recovery_Goals_LUT_edited) %>%
    mutate(`Stock Number` = Stock.Number, `Population Name` = COMMON_POPULATION_NAME, Year = ESA.listing.year, `Recovery Goal` = Recovery.goal, `Goal Type` = "Minimum Viability or Low Productivity") %>%
    dplyr::select(`Stock Number`, `Population Name`, Year, `Recovery Goal`, `Goal Type`) %>%
    filter(!is.na(`Recovery Goal`))
  write.csv(Recovery_Goals_new, paste(SubDir, "/Recovery_Goals_For_data_wa_gov.csv", sep = ""), row.names = F, na = "")
}
