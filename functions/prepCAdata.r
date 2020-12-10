#prep CA data for analysis function
prepCAdata<-function(mainDir,CAfilename, data_date, ESU_DPS_list, Recovery_Goals, POPFIT_exceptions,specialcaselist){
  SubDir <- paste("results ", data_date,sep="")
  if (!file.exists(SubDir)){
    dir.create(file.path(SubDir))
  }
  dat<-data.frame(read_xlsx(paste("data/",CAfilename,sep=""),sheet="NOSA",col_types = NULL,guess_max = 5))
  ESU_DPS_list<-data.frame(read.csv(paste("data/",ESU_DPS_list,sep="")))
  Recovery_Goals <- data.frame(read.csv(paste("data/",Recovery_Goals,sep="")))
  dat[dat=="NA"] <- NA
  
  #make second common name
  #temp<-data.frame(unlist(strsplit(dat$ESAPOPNAME,") ")))%>%filter(row_number() %% 2 == 0)
  temp<-as.data.frame(strsplit(dat$ESAPOPNAME,") "))[2,]
  #dat$COMMONPOPNAME2<-temp$unlist.strsplit.dat.ESAPOPNAME........
  dat$COMMONPOPNAME2<-t(temp[1:dim(temp)[2]])
  dat$COMMONPOPNAME2<-paste(dat$COMMONPOPNAME2,dat$COMMONNAME,sep=" ")
  
  #filter ESUs you want to use
  dat<-dat[dat$ESU_DPS%in%ESU_DPS_list$USECA,]
  
  #filter pops in WA list
  # unique(dat$COMMONPOPNAME2[dat$SUBMITAGENCY=="WDFW"])
  # unique(Recovery_Goals$COMMON_POPULATION_NAME)
  # unique(dat$COMMONPOPNAME2[dat$SUBMITAGENCY=="WDFW"])[!unique(dat$COMMONPOPNAME2[dat$SUBMITAGENCY=="WDFW"]) %in%  unique(Recovery_Goals$COMMON_POPULATION_NAME)]
  dat<-dat[dat$COMMONPOPNAME2%in%Recovery_Goals$COMMON_POPULATION_NAME,]
  
  #pop fit is same/multiple (data at Recovery Pop Scale only, not subpops)
  dat<-dat[dat$POPFIT%in%c("Same","Multiple") | dat$COMMONPOPNAME2%in%POPFIT_exceptions,]
  
  #best value true...best estimate for year and pop
  dat<-dat[dat$BESTVALUE =="Yes",]
  
  #manually eliminate/fix sketchy data, duplicates and data errors
  for(i in 1:length(specialcaselist)){dat<-subset(dat, eval(specialcaselist[[i]]))}
  
  #manually set TSAIJ and TSAEJ to NA for pops where it is NOT APPROPRIATE to use these as a subsitute for NOSA (e.g., total spawners includes substantial hatchery fish) )
  #dat[dat$ESAPOPNAME%in%HatchPops,colnames(dat)%in%c("TASIJ","TSAEJ")]<-NA
  dat<-dat[!dat$ESAPOPNAME%in%HatchPops,]
  
  #merge with ESU_DPS_list including listing years
  dat<-merge(ESU_DPS_list,dat,by="ESU_DPS",all.x = F,all.y=F)
  dat<-dat[dat$SPAWNINGYEAR>=dat$ESA.listing.year,]
  
  #identify final abundance data: priority 1:4 = NOSAIJ, NOSAEJ,TSAIJ,TSAEJ.
  NOSAIJpops<-dat%>%group_by(ESAPOPNAME)%>%filter(max(NOSAIJ,na.rm = T)>0)%>%summarise(minyr=min(SPAWNINGYEAR),maxyr=max(SPAWNINGYEAR))
  NOSAEJpops<-dat%>%group_by(ESAPOPNAME)%>%filter(max(NOSAEJ,na.rm = T)>0 & !ESAPOPNAME%in%NOSAIJpops$ESAPOPNAME)%>%summarise(minyr=min(SPAWNINGYEAR),maxyr=max(SPAWNINGYEAR))
  TSAIJpops<-dat%>%group_by(ESAPOPNAME)%>%filter(max(TSAIJ,na.rm = T)>0 & !ESAPOPNAME%in%NOSAIJpops$ESAPOPNAME & !ESAPOPNAME%in%NOSAEJpops$ESAPOPNAME)%>%summarise(minyr=min(SPAWNINGYEAR),maxyr=max(SPAWNINGYEAR))
  TSAEJpops<-dat%>%group_by(ESAPOPNAME)%>%filter(max(TSAEJ,na.rm = T)>0 & !ESAPOPNAME%in%NOSAIJpops$ESAPOPNAME & !ESAPOPNAME%in%NOSAEJpops$ESAPOPNAME & !ESAPOPNAME%in%TSAIJpops$ESAPOPNAME)%>%summarise(minyr=min(SPAWNINGYEAR),maxyr=max(SPAWNINGYEAR))
  
  #see how many pops
  print(paste("number of pops included: ",length(c(NOSAIJpops$ESAPOPNAME,NOSAEJpops$ESAPOPNAME,TSAIJpops$ESAPOPNAME,TSAEJpops$ESAPOPNAME)),sep=""))
  
  #see  pops missing data
  popsnodata<-unique(dat$ESAPOPNAME[!dat$ESAPOPNAME%in%c(NOSAIJpops$ESAPOPNAME,NOSAEJpops$ESAPOPNAME,TSAIJpops$ESAPOPNAME,TSAEJpops$ESAPOPNAME)])
  print(paste("number of populations not filtered that have no suitable data: ", length(popsnodata),sep=""))
  if(length(popsnodata)>0){print(popsnodata)}
  
  #create final abundance data field
  dat$final_abundance_data_type<-NA
  dat$final_abundance_data_type[dat$ESAPOPNAME%in%NOSAIJpops$ESAPOPNAME]<-"NOSAIJ"
  dat$final_abundance_data_type[dat$ESAPOPNAME%in%NOSAEJpops$ESAPOPNAME]<-"NOSAEJ"
  dat$final_abundance_data_type[dat$ESAPOPNAME%in%TSAIJpops$ESAPOPNAME]<-"TSAIJ"
  dat$final_abundance_data_type[dat$ESAPOPNAME%in%TSAEJpops$ESAPOPNAME]<-"TSAEJ"
  dat$final_abundance_data_type<-as.factor(dat$final_abundance_data_type)
  dat$final_abundance<-NA
  for(i in 1:nrow(dat)){dat[i,colnames(dat)=="final_abundance"]=ifelse(!is.na(dat$final_abundance_data_type[i]), dat[i,colnames(dat)==dat$final_abundance_data_type[i]],NA)}
  dat<-dat[!is.na(dat$final_abundance),]
  
  #check for duplicate abundance data (more than 1 series per year)
  dups<-data.frame(dat%>%group_by(ESU_DPS,ESAPOPNAME,SPAWNINGYEAR)%>%summarise(count=n()))%>%filter(count>1)
  print(paste("Number of duplicate entries (>1 value per year and pop): ",nrow(dups),sep=""))
  if(nrow(dups)>1){print(dups)}
  
  #look at table of final ESUs, Pops, data type, yrs of data (to see what's there)
  #print("Summary of ESUs, Populations, and Years of Data (a more complete dataset will be saved to your results folder)")
  #print(data.frame(dat%>%group_by(ESU_DPS,ESAPOPNAME)%>%summarise(final_abundance_data_type=first(final_abundance_data_type),minyr=min(SPAWNINGYEAR),maxyr=max(SPAWNINGYEAR))))
  
  #DataSource
  dat$datasource <-"Coordinated_Assessments"
  dat<-dat[,!colnames(dat)=="ESA.listing.year"]
  #write out all data
  alldat<-data.frame(dat%>%
                       dplyr::select(ESU_DPS = ESU_DPS,
                                     ESU_DPS_COMMONNAME = ESU_DPS_COMMONNAME,
                                     MAJORPOPGROUP=MAJORPOPGROUP,
                                     COMMONPOPNAME2=COMMONPOPNAME2,
                                     COMMONNAME = COMMONNAME,
                                     RUN = RUN,
                                     WATERBODY = WATERBODY,
                                     SPAWNINGYEAR = SPAWNINGYEAR,
                                     final_abundance = final_abundance,
                                     final_abundance_data_type = final_abundance_data_type,
                                     METHODNUMBER = METHODNUMBER,
                                     CONTACTAGENCY = CONTACTAGENCY,
                                     SUBMITAGENCY = SUBMITAGENCY, 
                                     LASTUPDATED = LASTUPDATED,
                                     ID = ID,
                                     PROTMETHNAME = PROTMETHNAME
                       )
  )
  alldat<-alldat[order(alldat$ESU_DPS,alldat$COMMONPOPNAME2,alldat$SPAWNINGYEAR),]
  write.csv(alldat,paste(SubDir,"/All_CA_Data_",data_date,".csv",sep=""),row.names = F)
  dat<-dat %>% mutate_if(is.factor, as.character)
  return(dat)
}
