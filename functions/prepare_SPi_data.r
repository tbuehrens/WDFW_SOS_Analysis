# prepare SPi data for analysis function
prepare_SPi_data <- function(mainDir, data_date, ESU_DPS_list, Recovery_Goals_LUT_edited, Recovery_Goals, POPFIT_exceptions, specialcaselistif, databeforelisting = "No") {
  SubDir <- paste("results ", data_date, sep = "")
  if (!file.exists(SubDir)) {
    dir.create(file.path(SubDir))
  }

  database_args <- list(
    Driver = "PostgreSQL Unicode",
    Server = Sys.getenv("POSTGRES_SPI_IP"),
    Database = "FISH",
    Port = 5433,
    UID = Sys.getenv("POSTGRES_SPI_UN"),
    PWD = Sys.getenv("POSTGRES_SPI_PW"),
    Trusted_Connection = "True"
  )

  con <- DBI::dbConnect(
    odbc::odbc(),
    Driver = database_args$Driver,
    Server = database_args$Server,
    Database = database_args$Database,
    Port = database_args$Port,
    UID = database_args$UID,
    PWD = database_args$PWD,
    Trusted_Connection = database_args$Trusted_Connection
  )

  # dat<- data.frame(dbGetQuery(con, "SELECT * FROM spi.vw_ca_nosa_sos;"))
  dat <- data.frame(dbGetQuery(con, "SELECT * FROM spi.vw_ca_nosa_sos2;"))
  # dat<-data.frame(read_csv("vw_ca_nosa_sos.csv"))

  stock <- data.frame(dbGetQuery(con, "SELECT * FROM spi.stock;"))
  dbDisconnect(con)

  Recovery_Goals_LUT_edited <- read_csv(file.path("data", Recovery_Goals_LUT_edited)) %>%
    dplyr::rename(sasi_stock_num = "Stock.Number", COMMONPOPNAME2 = "COMMON_POPULATION_NAME") %>%
    mutate(sasi_stock_num = as.character(sasi_stock_num)) %>%
    dplyr::select(sasi_stock_num, COMMONPOPNAME2)

  ESU_DPS_list <- data.frame(read_csv(paste("data/", ESU_DPS_list, sep = "")))
  Recovery_Goals <- data.frame(read_csv(paste("data/", Recovery_Goals, sep = "")))


  dat <- dat %>%
    dplyr::rename(sasi_stock_num = SASISTOCKNUM) %>%
    left_join(stock %>%
      dplyr::select(stock_id, sasi_stock_num)) %>%
    mutate(sasi_stock_num = gsub(pattern = " ", x = sasi_stock_num, replacement = "")) %>%
    left_join(Recovery_Goals_LUT_edited) %>% # get correct common pop names
    rename_all(., .funs = toupper) %>%
    dplyr::select(!ESU_DPS) %>% # replace ESU_DPS with correct values from ESU_DPS list
    left_join(Recovery_Goals %>%
      dplyr::select(ESU_DPS, COMMON_POPULATION_NAME) %>%
      dplyr::rename(COMMONPOPNAME2 = COMMON_POPULATION_NAME))


  dat[dat == "NA"] <- NA

  # filter ESUs you want to use
  dat <- dat[dat$ESU_DPS %in% ESU_DPS_list$USECA, ]

  # filter pops in WA list
  dat <- dat[dat$COMMONPOPNAME2 %in% Recovery_Goals$COMMON_POPULATION_NAME, ]

  write.csv(dat, paste0(SubDir, "/raw_SPi_nosa_data.csv_", data_date, ".csv"), row.names = F)

  # pop fit is same/multiple (data at Recovery Pop Scale only, not subpops)
  dat <- dat[dat$POPFIT %in% c("Same", "Multiple") | dat$COMMONPOPNAME2 %in% POPFIT_exceptions, ]

  # best value true...best estimate for year and pop
  dat <- dat[dat$BESTVALUE == "Yes", ]

  # manually eliminate/fix sketchy data, duplicates and data errors
  for (i in 1:length(specialcaselist)) {
    dat <- subset(dat, eval(specialcaselist[[i]]))
  }

  # manually set TSAIJ and TSAEJ to NA for pops where it is NOT APPROPRIATE to use these as a subsitute for NOSA (e.g., total spawners includes substantial hatchery fish) )
  # dat[dat$COMMONPOPNAME2%in%HatchPops,colnames(dat)%in%c("TASIJ","TSAEJ")]<-NA
  dat <- dat[!dat$COMMONPOPNAME2 %in% HatchPops, ]

  # merge with ESU_DPS_list including listing years
  dat <- dat %>%
    inner_join(ESU_DPS_list, by = "ESU_DPS")

  # filter data for after listing?
  if (databeforelisting == "No") {
    dat <- dat %>%
      filter(SPAWNINGYEAR >= ESA.listing.year)
  }

  # identify final abundance data: priority 1:4 = NOSAIJ, NOSAEJ,TSAIJ,TSAEJ.
  NOSAIJpops <- dat %>%
    group_by(COMMONPOPNAME2) %>%
    filter(max(NOSAIJ, na.rm = T) > 0) %>%
    summarise(minyr = min(SPAWNINGYEAR), maxyr = max(SPAWNINGYEAR))
  NOSAEJpops <- dat %>%
    group_by(COMMONPOPNAME2) %>%
    filter(max(NOSAEJ, na.rm = T) > 0 & !COMMONPOPNAME2 %in% NOSAIJpops$COMMONPOPNAME2) %>%
    summarise(minyr = min(SPAWNINGYEAR), maxyr = max(SPAWNINGYEAR))
  TSAIJpops <- dat %>%
    group_by(COMMONPOPNAME2) %>%
    filter(max(TSAIJ, na.rm = T) > 0 & !COMMONPOPNAME2 %in% NOSAIJpops$COMMONPOPNAME2 & !COMMONPOPNAME2 %in% NOSAEJpops$COMMONPOPNAME2) %>%
    summarise(minyr = min(SPAWNINGYEAR), maxyr = max(SPAWNINGYEAR))
  TSAEJpops <- dat %>%
    group_by(COMMONPOPNAME2) %>%
    filter(max(TSAEJ, na.rm = T) > 0 & !COMMONPOPNAME2 %in% NOSAIJpops$COMMONPOPNAME2 & !COMMONPOPNAME2 %in% NOSAEJpops$COMMONPOPNAME2 & !COMMONPOPNAME2 %in% TSAIJpops$COMMONPOPNAME2) %>%
    summarise(minyr = min(SPAWNINGYEAR), maxyr = max(SPAWNINGYEAR))

  # see how many pops
  print(paste("number of pops included: ", length(c(NOSAIJpops$COMMONPOPNAME2, NOSAEJpops$COMMONPOPNAME2, TSAIJpops$COMMONPOPNAME2, TSAEJpops$COMMONPOPNAME2)), sep = ""))

  # see  pops missing data
  popsnodata <- unique(dat$COMMONPOPNAME2[!dat$COMMONPOPNAME2 %in% c(NOSAIJpops$COMMONPOPNAME2, NOSAEJpops$COMMONPOPNAME2, TSAIJpops$COMMONPOPNAME2, TSAEJpops$COMMONPOPNAME2)])
  print(paste("number of populations not filtered that have no suitable data: ", length(popsnodata), sep = ""))
  if (length(popsnodata) > 0) {
    print(popsnodata)
  }

  # create final abundance data field
  dat$final_abundance_data_type <- NA
  dat$final_abundance_data_type[dat$COMMONPOPNAME2 %in% NOSAIJpops$COMMONPOPNAME2] <- "NOSAIJ"
  dat$final_abundance_data_type[dat$COMMONPOPNAME2 %in% NOSAEJpops$COMMONPOPNAME2] <- "NOSAEJ"
  dat$final_abundance_data_type[dat$COMMONPOPNAME2 %in% TSAIJpops$COMMONPOPNAME2] <- "TSAIJ"
  dat$final_abundance_data_type[dat$COMMONPOPNAME2 %in% TSAEJpops$COMMONPOPNAME2] <- "TSAEJ"
  dat$final_abundance_data_type <- as.factor(dat$final_abundance_data_type)
  dat$final_abundance <- NA
  for (i in 1:nrow(dat)) {
    dat[i, colnames(dat) == "final_abundance"] <- ifelse(!is.na(dat$final_abundance_data_type[i]), dat[i, colnames(dat) == dat$final_abundance_data_type[i]], NA)
  }
  dat <- dat[!is.na(dat$final_abundance), ]

  # check for duplicate abundance data (more than 1 series per year)
  dups <- data.frame(dat %>% group_by(ESU_DPS, COMMONPOPNAME2, SPAWNINGYEAR) %>% summarise(count = n())) %>% filter(count > 1)
  print(paste("Number of duplicate entries (>1 value per year and pop): ", nrow(dups), sep = ""))

  if (nrow(dups) > 1) {
    print(dups)
    stop("There is more than one data point per pop per year; additional filters needed!")
  }


  # look at table of final ESUs, Pops, data type, yrs of data (to see what"s there)
  print("Summary of ESUs, Populations, and Years of Data (a more complete dataset will be saved to your results folder)")
  dat %>%
    group_by(ESU_DPS, COMMONPOPNAME2) %>%
    summarise(final_abundance_data_type = first(final_abundance_data_type), minyr = min(SPAWNINGYEAR), maxyr = max(SPAWNINGYEAR)) %>%
    write.csv(file = "year_pops.csv", row.names = F)
  #
  # DataSource
  dat$datasource <- "Coordinated_Assessments"
  dat <- dat[, !colnames(dat) == "ESA.listing.year"]
  # write out all data
  alldat <- data.frame(dat %>%
    mutate(dsid = ifelse(final_abundance_data_type == "NOSAIJ", NOSAIJ_SDSID, ifelse(final_abundance_data_type == "NOSAEJ", NOSAEJ_SDSID, ifelse(final_abundance_data_type == "TSAIJ", TSAIJ_SDSID, TSAEJ_SDSID)))) %>%
    dplyr::select(
      ESU_DPS = ESU_DPS,
      ESU_DPS_COMMONNAME = ESU_DPS_COMMONNAME,
      MAJORPOPGROUP = MAJORPOPGROUP,
      COMMONPOPNAME2 = COMMONPOPNAME2,
      COMMONNAME = COMMONNAME,
      RUN = RUN,
      WATERBODY = WATERBODY,
      SPAWNINGYEAR = SPAWNINGYEAR,
      final_abundance = final_abundance,
      final_abundance_data_type = final_abundance_data_type,
      METHODNUMBER = METHODNUMBER,
      dsid = dsid
      # CONTACTAGENCY = CONTACTAGENCY,
      # ESUBMITAGENCY = SUBMITAGENCY,
      # LASTUPDATED = LASTUPDATED,
      # ID = ID,
      # PROTMETHNAME = PROTMETHNAME
    ))
  alldat <- alldat[order(alldat$ESU_DPS, alldat$COMMONPOPNAME2, alldat$SPAWNINGYEAR), ]
  alldat %>%
    group_by(dsid) %>%
    summarise(n = n()) %>%
    write.csv(paste0("dsid_list_", data_date, ".csv"), row.names = F)
  if (databeforelisting == "No") {
    write.csv(alldat, paste(SubDir, "/All_SPi_Data_", data_date, ".csv", sep = ""), row.names = F)
  } else {
    (write.csv(alldat, paste(SubDir, "/All_SPi_Data_incl_before_listing_", data_date, ".csv", sep = ""), row.names = F))
  }
  dat <- dat %>% mutate_if(is.factor, as.character)
  return(dat)
}
