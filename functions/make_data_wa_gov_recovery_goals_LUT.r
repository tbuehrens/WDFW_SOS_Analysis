make_data_wa_gov_recovery_goals_LUT <- function(data_date, Recovery_Goals, WDFW_Salmonid_Stock_Inventory_Population_Recovery_Goals) {
  SubDir <- paste("results ", data_date, sep = "")
  if (!file.exists(SubDir)) {
    dir.create(file.path(SubDir))
  }
  Recovery_Goals_LUT <- data.frame(read.csv(paste("data/", Recovery_Goals, sep = "")))
  WDFW_Salmonid_Stock_Inventory_Population_Recovery_Goals <- data.frame(read.csv(paste("data/", WDFW_Salmonid_Stock_Inventory_Population_Recovery_Goals, sep = "")))
  Recovery_Goals_LUT$Population.Name <- ClosestMatch2(Recovery_Goals_LUT$COMMON_POPULATION_NAME, WDFW_Salmonid_Stock_Inventory_Population_Recovery_Goals$Population.Name)
  WDFWnames <- WDFW_Salmonid_Stock_Inventory_Population_Recovery_Goals %>%
    dplyr::select(Population.Name, Stock.Number) %>%
    group_by(Population.Name) %>%
    summarise(Stock.Number = first(Stock.Number))
  Recovery_Goals_LUT <- data.frame(Recovery_Goals_LUT %>% left_join(WDFWnames, by = c("Population.Name")) %>% dplyr::select(COMMON_POPULATION_NAME, WDFW.Name = Population.Name, Stock.Number))
  write.csv(Recovery_Goals_LUT, paste(SubDir, "/recoverygoals_LUT_raw.csv", sep = ""), row.names = F)
}
