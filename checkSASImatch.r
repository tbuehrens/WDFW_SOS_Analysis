library(tidyverse)
#load SASI data and filter for Escapement Methodology = SOS 2020 records, create stock number/year key
SASI<-data.frame(read_csv("https://data.wa.gov/api/views/fgyz-n3uk/rows.csv?accessType=DOWNLOAD&bom=true&format=true"))
newnames<-gsub("\\."," ",colnames(SASI))
SASI<-SASI%>%rename_at(vars(colnames(SASI)), ~ gsub("\\."," ",colnames(SASI)))%>%
  filter(`Escapement Methodology`=="SOS 2020 Abundance Analysis")%>%
  mutate(key=paste0(`Stock Number`,Year))

#load SASI data and filter for Escapement Methodology = SOS 2020 records, create stock number/year key
dat<-data.frame(read_csv("https://raw.githubusercontent.com/tbuehrens/WDFW_SOS_Analysis/main/results%202020-11-10/Escapement_Data_For_data_wa_gov_with_existing.csv"))
newnames<-gsub("\\."," ",colnames(dat))
dat<-dat%>%rename_at(vars(colnames(dat)), ~ gsub("\\."," ",colnames(dat)))%>%
  filter(`Escapement Methodology`=="SOS 2020 Abundance Analysis")%>%
  mutate(key=paste0(`Stock Number`,Year))

dat%>%filter(is.na(`Population Name`))
dat%>%filter(`Population Name`=="Cispus River - winter Steelhead")
#look at dinensions
dim(SASI)
dim(dat)

#full join the datasets and match by key
dat<-dat%>%full_join(SASI,by="key",suffix = c(".Thomas", ".SASI"))%>%
  mutate(match=ifelse(`Abundance Quantity.Thomas`==`Abundance Quantity.SASI` | is.na(`Abundance Quantity.Thomas`) & is.na(`Abundance Quantity.SASI`) ,"Match","NoMatch"))

#count of matching and non matching records
dat%>%group_by(match)%>%
  summarise(count=n())

#look at non-matching records
dat%>%filter(is.na(match))



