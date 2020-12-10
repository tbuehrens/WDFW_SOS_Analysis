make_escapement_data_wa_gov<-function(data_date,Recovery_Goals_LUT_edited){
  SubDir <- paste("results ", data_date,sep="")
  if (!file.exists(SubDir)){
    dir.create(file.path(SubDir))
  }
  Recovery_Goals_LUT<-data.frame(read.csv(paste("data/",Recovery_Goals_LUT_edited,sep="")))
  #Smoothed_Abundance<-data.frame(read.csv(paste(mainDir,"ESU_DPS_FILES"," ",data_date,"\\Smoothed_Abundance_",data_date,".csv",sep="")))
  All_Data<-data.frame(read.csv(paste(SubDir,"/All_Data_",data_date,".csv",sep="")))
  dat<-Recovery_Goals_LUT%>%left_join(All_Data,by=c("COMMON_POPULATION_NAME"="COMMONPOPNAME2"))%>%
    mutate(`Stock Number`=Stock.Number,`Population Name`=COMMON_POPULATION_NAME,Year=SPAWNINGYEAR,`Abundance Quantity`=final_abundance, `Production Type`="Natural",`Calculation Type`="Expanded",`Report Types`="SCoRE,SOS")%>%
    dplyr::select(`Stock Number`,`Population Name`,Year,`Abundance Quantity`, `Production Type`,`Calculation Type`,`Report Types`)%>%
    mutate(`Calculation Type`=ifelse(is.na(Year),NA,`Calculation Type`))
  write.csv(dat,paste(SubDir,"/Escapement_Data_For_data_wa_gov.csv",sep=""),row.names = F)# na=""
}