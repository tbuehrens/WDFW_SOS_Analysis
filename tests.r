pacman::p_load(httr,jsonlite,tidyverse,RSocrata)
df <- read.socrata(
  "https://data.wa.gov/resource/x25s-cxg8.json",
  app_token = Sys.getenv("SPI_SOCRATA_APP_TOKEN"),
  email     = Sys.getenv("SPI_SOCRATA_APP_email"),
  password  = Sys.getenv("SPI_SOCRATA_APP_pw")
)

n2<-sort(names(df))
n1<-sort(names(dat))%>%tolower()

n1
n2

n2[which(!n2%in%n1)]

n1[which(!n1%in%n2)]



