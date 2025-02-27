make_population_data_wa_gov <- function(data_date, Recovery_Goals, Recovery_Goals_LUT_edited, WDFW_Salmonid_Stock_Inventory_Populations) {
  SubDir <- paste("results ", data_date, sep = "")
  if (!file.exists(SubDir)) {
    dir.create(file.path(SubDir))
  }
  Recovery_Goals_LUT <- data.frame(read.csv(paste("data/", Recovery_Goals_LUT_edited, sep = "")))
  Recovery_Goals <- data.frame(read.csv(paste("data/", Recovery_Goals, sep = "")))
  WDFW_Salmonid_Stock_Inventory_Populations <- data.frame(read.csv(paste("data/", WDFW_Salmonid_Stock_Inventory_Populations, sep = "")))
  dat <- Recovery_Goals %>%
    left_join(Recovery_Goals_LUT, by = c("COMMON_POPULATION_NAME")) %>%
    rename(Population.Name = WDFW.Name) %>%
    left_join(WDFW_Salmonid_Stock_Inventory_Populations, by = c("Population.Name", "Stock.Number")) %>%
    mutate(
      `Stock Number` = Stock.Number,
      `Population Name` = COMMON_POPULATION_NAME,
      `ESU/DPS Name` = ESU.DPS.Name,
      `Federal Status` = Federal.Status,
      `Listing Date` = Listing.Date,
      `Salmon Recovery Region` = Salmon.Recovery.Region,
      `Recovery Plan` = Recovery.Plan,
      `Recovery Plan Year` = Recovery.Plan.Year,
      `Hatchery Standards Met` = Hatchery.Standards.Met,
      `Major Population Grouping` = Major.Population.Grouping,
      `Local Biologist Name` = Local.Biologist.Name,
      `Local Biologist Email` = Local.Biologist.Email,
      `Last Updated` = data_date
    ) %>%
    dplyr::select(
      `Stock Number`,
      `Population Name`,
      `ESU/DPS Name`,
      `Federal Status`,
      `Listing Date`,
      `Salmon Recovery Region`,
      `Recovery Plan`,
      `Recovery Plan Year`,
      `Hatchery Standards Met`,
      `Major Population Grouping`,
      `Local Biologist Name`,
      `Local Biologist Email`,
      `Last Updated`
    )
  specieslist <- c("Coho salmon", "Chinook salmon", "Steelhead", "Chum salmon", "sockeye salmon", "chum salmon")
  results <- sapply(X = specieslist, FUN = grepl, x = dat$`Population Name`)
  results <- unlist(apply(results, 1, function(x) which(x == TRUE)))
  results <- specieslist[results]
  results <- gsub("salmon", "", x = results)
  results <- gsub(" ", "", x = results)
  dat$Species <- capitalize_str(results)
  dat <- dat %>% dplyr::select(
    `Stock Number`,
    `Population Name`,
    `Species`,
    `ESU/DPS Name`,
    `Federal Status`,
    `Listing Date`,
    `Salmon Recovery Region`,
    `Recovery Plan`,
    `Recovery Plan Year`,
    `Hatchery Standards Met`,
    `Major Population Grouping`,
    `Local Biologist Name`,
    `Local Biologist Email`,
    `Last Updated`
  )
  write.csv(dat, paste(SubDir, "/Population_Data_For_data_wa_gov_raw.csv", sep = ""), row.names = F, na = "") # need to fix data for pops that are not in the WDFW table!
}
