# results plotting function
esu_results_plot <- function(ESU_DIP_results, data_date, futureyears = futureyears, resultsplots = resultsplots, bypopsplots = "No", geomeanyears = geomeanyears, lastyear = lastyear) {
  logit <- function(x) {
    log(x / (1 - x))
  }
  ilogit <- function(x) {
    exp(x) / (1 + exp(x))
  }
  SubDir <- paste("results ", data_date, sep = "")
  if (!file.exists(SubDir)) {
    dir.create(file.path(SubDir))
  }
  DIP_Results <- ESU_DIP_results
  print(unique(DIP_Results$ESU_DPS))
  DIP_Results <- data.frame(DIP_Results %>% filter(!is.na(Recovery.goal)))
  missingtrend <- data.frame(DIP_Results %>% filter(is.na(Percent.change.per.year)) %>% group_by(ESU_DPS, COMMONPOPNAME2) %>% dplyr::select(ESU_DPS, COMMONPOPNAME2))
  missingstatus <- data.frame(DIP_Results %>% filter(is.na(Geomean_5yr)) %>% group_by(ESU_DPS, COMMONPOPNAME2) %>% dplyr::select(ESU_DPS, COMMONPOPNAME2))
  print("datasets unable to assess for trend")
  print(missingtrend)
  print("datasets unable to assess for status")
  print(missingstatus)
  totalmissing <- totalmissing <- merge(missingstatus, missingtrend, by = c("ESU_DPS", "COMMONPOPNAME2"), all = T)
  totalmissing <- data.frame(totalmissing %>% group_by(ESU_DPS) %>% summarise(total_missing = n()))
  dat_stat <- NULL
  dat_stat <- data.frame(DIP_Results %>% filter(!is.na(Percent.change.per.year)) %>% group_by(ESU_DPS, COMMONPOPNAME2))
  dat_stat <- data.frame(dat_stat %>% filter(!is.na(Geomean_5yr)) %>% group_by(ESU_DPS, COMMONPOPNAME2))
  dat_stat <- data.frame(dat_stat %>% group_by(ESU_DPS) %>% summarize(
    Q1 = quantile(Percent.change.per.year * 0.01, c(0.01)),
    Q2 = quantile(Percent.change.per.year * 0.01, c(0.25)),
    Q3 = quantile(Percent.change.per.year * 0.01, c(0.5)),
    Q4 = quantile(Percent.change.per.year * 0.01, c(0.75)),
    Q5 = quantile(Percent.change.per.year * 0.01, c(0.99)),
    rec_prop = sum(abovegoal) / n(),
    Q1_RR = quantile(Geomean_5yr_prop_goal, c(0.01)),
    Q2_RR = quantile(Geomean_5yr_prop_goal, c(0.25)),
    Q3_RR = quantile(Geomean_5yr_prop_goal, c(0.5)),
    Q4_RR = quantile(Geomean_5yr_prop_goal, c(0.75)),
    Q5_RR = quantile(Geomean_5yr_prop_goal, c(0.99)),
    Geomean_Geomean_Recovery_ratio = exp(mean(log(Geomean_5yr_prop_goal))),
    count_of_pops = n(),
    FutureStatus = Q3_RR * 100 * (1 + Q3)^futureyears,
    OverallCategory = as.numeric(cut(FutureStatus, breaks = c(0, 25, 50, 100, 10000)))
  ))
  dat_stat <- merge(dat_stat, totalmissing, by = "ESU_DPS", all.x = T)
  dat_stat$total_missing[is.na(dat_stat$total_missing)] <- 0
  dat_stat$p_missing <- dat_stat$total_missing / (dat_stat$count_of_pops + dat_stat$total_missing)
  Location <- data.frame(unlist(strsplit(dat_stat$ESU_DPS, "\\(|\\)"))) %>% filter(row_number() %% 2 == 0)
  Location[, 1] <- capitalize_str(gsub("-run", "", Location[, 1]))
  Location[, 1] <- gsub("/s", "/S", Location[, 1])
  Species <- data.frame(unlist(strsplit(dat_stat$ESU_DPS, "\\(|\\)"))) %>% filter(row_number() %% 2 != 0)
  Species <- data.frame(strsplit(as.character(Species[, 1]), ","))[2, ]
  Species <- as.character(gsub(" ", "", as.character(unlist(Species))))
  Species <- paste(toupper(substring(Species, 1, 1)), substring(Species, 2), sep = "")
  ESU_DPS2 <- paste(as.character(Location[, 1]), as.character(Species), sep = "")
  ESU_DPS2 <- gsub("DPS", "", ESU_DPS2)
  ESU_DPS2 <- gsub("ESU", "", ESU_DPS2)
  # ESU_DPS2<-gsub("Basin","",ESU_DPS2)
  ESU_DPS2 <- gsub("  ", " ", ESU_DPS2)
  ESU_DPS2 <- gsub("  ", " ", ESU_DPS2)
  ESU_DPS2[grep(c("Steelhead"), ESU_DPS2)] <- paste(ESU_DPS2[grep(c("Steelhead"), ESU_DPS2)], "DPS")
  ESU_DPS2[grep(c("Chinook"), ESU_DPS2)] <- paste(ESU_DPS2[grep(c("Chinook"), ESU_DPS2)], "ESU")
  ESU_DPS2[grep(c("Coho"), ESU_DPS2)] <- paste(ESU_DPS2[grep(c("Coho"), ESU_DPS2)], "ESU")
  ESU_DPS2[grep(c("Sockeye"), ESU_DPS2)] <- paste(ESU_DPS2[grep(c("Sockeye"), ESU_DPS2)], "ESU")
  ESU_DPS2[grep(c("Chum"), ESU_DPS2)] <- paste(ESU_DPS2[grep(c("Chum"), ESU_DPS2)], "ESU")
  dat_stat$ESU_DPS2 <- ESU_DPS2

  dat_stat_csv <- dat_stat
  dat_stat_csv[, colnames(dat_stat_csv) %in% c("Q1", "Q2", "Q3", "Q4", "Q5")] <- dat_stat_csv[, colnames(dat_stat_csv) %in% c("Q1", "Q2", "Q3", "Q4", "Q5")] * 100
  dat_stat_csv$ESU_DPS <- dat_stat_csv$ESU_DPS2
  dat_stat_csv <- dat_stat_csv[, !colnames(dat_stat_csv) %in% c("ESU_DPS2", "Geomean_Geomean_Recovery_ratio")]
  colnames(dat_stat_csv) <- c(
    "ESU_DPS",
    "Lowest Growth Rate (% change per year)",
    "25th Percentile Growth Rate (% change per year)",
    "Median Growth Rate (% change per year)",
    "75th Percentile Growth Rate (% change per year)",
    "Highest Growth Rate (% change per year)",
    "Proportion of Populations with 5-yr Geomean above Goal",
    "Lowest 5-yr Geomean Abundance as Proportion of Goal",
    "25th Percentile 5-yr Geomean Abundance as Proportion of Goal",
    "Median 5-yr Geomean Abundance as Proportion of Goal",
    "75th Percentile 5-yr Geomean Abundance as Proportion of Goal",
    "Highest 5-yr Geomean Abundance as Proportion of Goal",
    "pops_with_usable_data",
    "FutureStatus (see methods)",
    "OverallRecoveryCategory",
    "total_missing_pops",
    "proportion_missing_pops"
  )
  write.csv(dat_stat_csv, paste(SubDir, "/ESU_DPS_Results ", data_date, ".csv", sep = ""), row.names = F)
  rownames(dat_stat) <- paste(dat_stat[, colnames(dat_stat) == "ESU_DPS2"], " (n = ", dat_stat$count_of_pops, "/", dat_stat$count_of_pops + dat_stat$total_missing, ")", sep = "")
  dat_stat <- dat_stat[, !colnames(dat_stat) == "ESU_DPS2"]
  dat_stat3 <- dat_stat[order(dat_stat$Q3_RR), colnames(dat_stat) %in% c("Q1_RR", "Q2_RR", "Q3_RR", "Q4_RR", "Q5_RR")]
  dat_stat <- dat_stat[, colnames(dat_stat) %in% c("Q1", "Q2", "Q3", "Q4", "Q5", "rec_prop")]
  dat_stat2 <- NULL
  dat_stat2 <- dat_stat
  dat_stat4 <- dat_stat2

  # edit data for plot v1
  dat_stat <- dat_stat[order(dat_stat$Q3), ]
  dat_stat <- dat_stat[, colnames(dat_stat) %in% c("Q1", "Q2", "Q3", "Q4", "Q5")]
  col_list <- brewer.pal(9, "RdYlGn")[c(1, 4, 8)]
  # plotting v1
  # pdf("ESU Plot_Trend.pdf",width=11,height=8.5)
  # mat<-matrix(c(rep(1,30),rep(2,10)),nrow=5)
  # layout(mat)
  # par(mar=c(5,15,5,0),oma=c(3,3,3,3),xpd=F)
  # dat_stat<-t(dat_stat)
  # plot<-boxplot.matrix(dat_stat,plot=F)
  # plot$stats<-dat_stat
  # breaks<-c(-10,-0.05,0.05,10)
  # cols<-col_list[as.numeric(cut(dat_stat[rownames(dat_stat)=="Q3",],breaks = breaks))]
  # bxp(plot,outline = F,horizontal = T,las=2,ylim=c(min(dat_stat)*1.1,max(dat_stat*1.1)),border=F,horiz=T,xaxt="n",yaxt="n")
  # x<-breaks[2:3]
  # y<-c(-20,20)
  # polygon(x=c(-5,-5,x[1],x[1]),y=c(y,rev(y)),border=NULL,col=adjustcolor(col = col_list[1],alpha.f = 0.25))
  # polygon(x=c(x[1],x[1],x[2],x[2]),y=c(y,rev(y)),border=NULL,col=adjustcolor(col = col_list[2],alpha.f = 0.25))
  # polygon(x=c(x[2],x[2],50,50),y=c(y,rev(y)),border=NULL,col=adjustcolor(col = col_list[3],alpha.f = 0.25))
  # abline(v=0,lty=3,lwd=2,col="white")
  # par(new=T)
  # bxp(plot,outline = F,horizontal = T,las=2,boxfill=cols,ylim=c(min(dat_stat)*1.1,max(dat_stat*1.1)),xaxt="n",yaxt="n")
  # axis(side=2,at=1:dim(dat_stat)[2],las=2,labels=colnames(dat_stat),cex.axis=0.75)
  # axis(side=1,at=pretty(c(min(dat_stat)*1.1,max(dat_stat*1.1)),6),las=1,labels=paste(pretty(c(min(dat_stat)*1.1,max(dat_stat*1.1)),6)*100,"%",sep=""))
  # mtext("Trend Since ESA Listing (% change per year)",1,line =3)
  # box()
  # par(mar=c(10,5,25,10))
  # boxplot(c(0:5),outline=F,bty="n",yaxt="n",frame=F,col="grey")
  # mtext(side=4,at=c(0,1,2.5,4,5),text=rev(c("99th Percentile", "75th Percentile","50th Percentile","25th Percentile","1st Percentile" )),las=2,cex=0.6)
  # mtext(side=4,at=6,line=-6, text="Population trends \n      within ESU",las=2)
  # par(xpd=T)
  # legend(0,9,legend=rev(c("Increasing","Little Change","Decreasing")),fill=col_list,bty="n",title = "Overall ESU Trend",cex=1.5)
  # dev.off()

  # edit data for v2
  # dat_stat2$rec_cat<-as.numeric(cut(dat_stat2[,colnames(dat_stat2)=="rec_prop"],breaks = c(3)))
  # dat_stat2<-dat_stat2[order(dat_stat2$rec_cat,dat_stat2$Q3),]
  # dat_stat2<-dat_stat2[colnames(dat_stat2)%in%c("Q1","Q2","Q3","Q4","Q5","rec_prop","rec_cat")]
  # rec_cat<-dat_stat2$rec_cat
  # cols2<-col_list[rec_cat]
  # dat_stat2<-apply(t(dat_stat2),1:2,as.numeric)
  # dat_stat2<-dat_stat2[rownames(dat_stat2)%in%paste("Q",c(1,2,3,4,5),sep=""),]
  # #plotting v 2
  # pdf("ESU Plot_Status_Trend.pdf",width=11,height=8.5)
  # mat<-matrix(c(rep(1,30),rep(2,10)),nrow=5)
  # layout(mat)
  # par(mar=c(15,5,5,0),oma=c(3,3,3,3),xpd=F)
  # plot<-boxplot.matrix(dat_stat2,plot=F)
  # x<-which(diff(rec_cat)!=0)+0.5
  # y<-c(-20,20)
  # plot$stats<-dat_stat2
  # bxp(plot,outline = F,horizontal = F,las=2,ylim=c(min(dat_stat2)*1.1,max(dat_stat2)*1.1),border=F,xaxt="n",yaxt="n")
  # polygon(x=c(-5,-5,x[1],x[1]),y=c(y,rev(y)),border=NULL,col=adjustcolor(col = col_list[1],alpha.f = 0.25))
  # polygon(x=c(x[1],x[1],x[2],x[2]),y=c(y,rev(y)),border=NULL,col=adjustcolor(col = col_list[2],alpha.f = 0.25))
  # polygon(x=c(x[2],x[2],50,50),y=c(y,rev(y)),border=NULL,col=adjustcolor(col = col_list[3],alpha.f = 0.25))
  # abline(h=0,lty=3,lwd=2,col="black")
  #
  # par(new=T)
  # bxp(plot,outline = F,horizontal = F,las=2,boxfill=cols2,ylim=c(min(dat_stat2)*1.1,max(dat_stat2)*1.1),xaxt="n",yaxt="n")
  # axis(side=1,at=1:dim(dat_stat2)[2],las=2,labels=colnames(dat_stat2),cex.axis=0.75)
  # axis(side=2,at=pretty(c(min(dat_stat2)*1.1,max(dat_stat2)*1.1),6),las=1,labels=paste(pretty(c(min(dat_stat2)*1.1,max(dat_stat2)*1.1),6)*100,"%",sep=""))
  # mtext("Trend Since ESA Listing (% change per year )",2,line =4)
  # box()
  # abline(v=which(diff(rec_cat)!=0)+0.5,lty=1,lwd=1)
  # #legend("topleft",legend=c("Increasing","Little Change","Decreasing"),fill=col_list,bty="n",title = "Overall ESU Trend")
  # par(mar=c(15,0,20,10))
  # boxplot(c(0:5),outline=F,bty="n",yaxt="n",frame=F,col="grey")
  # mtext(side=4,at=c(0,1,2.5,4,5),text=rev(c("99th Percentile", "75th Percentile","50th Percentile","25th Percentile","1st Percentile" )),las=2,cex=0.6)
  # mtext(side=4,at=5.5,line=-6, text="ESU population trends",las=2)
  # par(xpd=T)
  # legend(0.5,8,legend=rev(c(" > 2/3 Pops Above Goal","1/3 - 2/3 Pops Above Goal","< 1/3 Pops Above Goal")),fill=col_list,bty="n",
  #        title = "5-yr Geomean\n Smoothed Abundance\n Relative to Recovery Goals",cex=1.5)
  # dev.off()

  # edit data for v4
  dat_stat4$rec_cat <- as.numeric(cut(dat_stat4[, colnames(dat_stat4) == "rec_prop"], breaks = c(4)))
  dat_stat4 <- dat_stat4[order(dat_stat4$rec_cat, dat_stat4$Q3), ]
  dat_stat4 <- dat_stat4[colnames(dat_stat4) %in% c("Q1", "Q2", "Q3", "Q4", "Q5", "rec_prop", "rec_cat")]
  rec_cat <- dat_stat4$rec_cat
  # col_list4<-brewer.pal(9,"RdYlGn")[c(1,3,6,9)]
  # colorRampPalette(c("lightseagreen","powderblue"))(3)
  col_list4 <- c("red", "#E79A38D9", "#45A3AD80", "#45A3ADFF")
  cols2 <- col_list4[rec_cat]
  dat_stat4 <- apply(t(dat_stat4), 1:2, as.numeric)
  dat_stat4 <- dat_stat4[rownames(dat_stat4) %in% paste("Q", c(1, 2, 3, 4, 5), sep = ""), ]
  # plotting v 4
  jpeg(paste(SubDir, "/ESU Plot_Status_Trend_4_category_", data_date, ".jpeg", sep = ""), width = 11, height = 8.5, units = "in", res = 700)
  mat <- matrix(c(rep(1, 30), rep(2, 10)), nrow = 5)
  layout(mat)
  par(mar = c(15, 5, 5, 0), oma = c(3, 3, 3, 3), xpd = F)
  plot <- boxplot.matrix(dat_stat4, plot = F)
  x <- which(diff(rec_cat) != 0) + 0.5
  y <- c(-20, 20)
  plot$stats <- dat_stat4
  bxp(plot, outline = F, horizontal = F, las = 2, ylim = c(min(dat_stat4) * 1.1, max(dat_stat4) * 1.1), border = F, xaxt = "n", yaxt = "n")
  polygon(x = c(-5, -5, x[1], x[1]), y = c(y, rev(y)), border = NULL, col = adjustcolor(col = col_list4[1], alpha.f = 0.25))
  polygon(x = c(x[1], x[1], x[2], x[2]), y = c(y, rev(y)), border = NULL, col = adjustcolor(col = col_list4[2], alpha.f = 0.25))
  polygon(x = c(x[2], x[2], x[3], x[3]), y = c(y, rev(y)), border = NULL, col = adjustcolor(col = col_list4[3], alpha.f = 0.25))
  polygon(x = c(x[3], x[3], 50, 50), y = c(y, rev(y)), border = NULL, col = adjustcolor(col = col_list4[4], alpha.f = 0.25))
  legend("topright", legend = paste("Data Updated ", data_date, sep = ""), col = "grey70", cex = 0.6, bty = "n")
  abline(h = 0, lty = 2, lwd = 2, col = "black")

  par(new = T)
  bxp(plot, outline = F, horizontal = F, las = 2, boxfill = cols2, ylim = c(min(dat_stat4) * 1.1, max(dat_stat4) * 1.1), xaxt = "n", yaxt = "n")
  axis(side = 1, at = 1:dim(dat_stat4)[2], las = 2, labels = colnames(dat_stat4), cex.axis = 0.75)
  axis(side = 2, at = pretty(c(min(dat_stat4) * 1.1, max(dat_stat4) * 1.1), 6), las = 1, labels = paste(pretty(c(min(dat_stat4) * 1.1, max(dat_stat4) * 1.1), 6) * 100, "%", sep = ""))
  mtext("Trend Since ESA Listing  (% change per year )", 2, line = 4)
  box()
  abline(v = which(diff(rec_cat) != 0) + 0.5, lty = 1, lwd = 1)
  # legend("topleft",legend=c("Increasing","Little Change","Decreasing"),fill=col_list,bty="n",title = "Overall ESU Trend")
  par(mar = c(15, 0, 20, 10))
  boxplot(c(0:5), outline = F, bty = "n", yaxt = "n", frame = F, col = "grey")
  mtext(side = 4, at = c(0, 1, 2.5, 4, 5), text = rev(c("99th Percentile", "75th Percentile", "50th Percentile", "25th Percentile", "1st Percentile")), las = 2, cex = 0.6)
  mtext(side = 4, at = 5.5, line = -6, text = "ESU population trends", las = 2)
  par(xpd = T)
  legend(0.5, 8,
    legend = rev(c("Pops Above Goal  > 75%", "50% < Pops Above Goal <= 75%", "25% < Pops Above Goal <= 50%", "Pops Above Goal <= 25% ")), fill = col_list4, bty = "n",
    title = "5-yr Geomean\n Smoothed Abundance\n Relative to Recovery Goals", cex = 1.25
  )
  dev.off()



  # plotting v1 STatus
  # pdf("ESU Plot_Status.pdf",width=11,height=8.5)
  # mat<-matrix(c(rep(1,30),rep(2,10)),nrow=5)
  # layout(mat)
  # par(mar=c(5,15,5,0),oma=c(3,3,3,3),xpd=F)
  # dat_stat3<-t(dat_stat3)
  # dat_stat3<-log10(dat_stat3)
  # plot<-boxplot.matrix(dat_stat3,plot=F)
  # plot$stats<-dat_stat3
  # breaks<-log10(c(0,0.5,1,100))
  # cols<-col_list[as.numeric(cut(dat_stat3[rownames(dat_stat3)=="Q3_RR",],breaks = breaks))]
  # bxp(plot,outline = F,horizontal = T,las=2,ylim=c(min(dat_stat3)*1.1,max(dat_stat3*1.1)),border=F,horiz=T,xaxt="n",yaxt="n")
  # x<-breaks[2:3]
  # y<-c(-20,20)
  # polygon(x=c(-5,-5,x[1],x[1]),y=c(y,rev(y)),border=NULL,col=adjustcolor(col = col_list[1],alpha.f = 0.25))
  # polygon(x=c(x[1],x[1],x[2],x[2]),y=c(y,rev(y)),border=NULL,col=adjustcolor(col = col_list[2],alpha.f = 0.25))
  # polygon(x=c(x[2],x[2],50,50),y=c(y,rev(y)),border=NULL,col=adjustcolor(col = col_list[3],alpha.f = 0.25))
  # abline(v=0,lty=3,lwd=2,col="white")
  # par(new=T)
  # bxp(plot,outline = F,horizontal = T,las=2,boxfill=cols,ylim=c(min(dat_stat3)*1.1,max(dat_stat3*1.1)),xaxt="n",yaxt="n")
  # axis(side=2,at=1:dim(dat_stat3)[2],las=2,labels=colnames(dat_stat3),cex.axis=0.75)
  # #xvals<-pretty(c(min(dat_stat3),max(dat_stat3*1.1)),6)[1:8]
  # xvals<-c(-3:3)
  # axis(side=1,at=xvals,las=1,labels=paste((10^xvals)*100,"%",sep=""))
  # mtext("5-yr Geomean % of Recovery Goal",1,line =3)
  # box()
  # par(mar=c(10,5,25,10))
  # boxplot(c(0:5),outline=F,bty="n",yaxt="n",frame=F,col="grey")
  # mtext(side=4,at=c(0,1,2.5,4,5),text=rev(c("99th Percentile", "75th Percentile","50th Percentile","25th Percentile","1st Percentile" )),las=2,cex=0.6)
  # mtext(side=4,at=6,line=-6, text="Population status \n      within ESU",las=2)
  # par(xpd=T)
  # legend(0,9,legend=c("< 50% of Goal","50 - 100% of Goal","> Recovery Goal"),fill=col_list,bty="n",title = "Overall ESU Status",cex=1.5)
  # dev.off()
  #

  # DIP Results Plots
  Location <- data.frame(unlist(strsplit(DIP_Results$ESU_DPS, "\\(|\\)"))) %>% filter(row_number() %% 2 == 0)
  Location[, 1] <- capitalize_str(gsub("-run", "", Location[, 1]))
  Location[, 1] <- gsub("/s", "/S", Location[, 1])
  Species <- data.frame(unlist(strsplit(DIP_Results$ESU_DPS, "\\(|\\)"))) %>% filter(row_number() %% 2 != 0)
  Species <- data.frame(strsplit(as.character(Species[, 1]), ","))[2, ]
  Species <- as.character(gsub(" ", "", as.character(unlist(Species))))
  Species <- paste(toupper(substring(Species, 1, 1)), substring(Species, 2), sep = "")
  ESU_DPS2 <- paste(as.character(Location[, 1]), as.character(Species), sep = "")
  ESU_DPS2 <- gsub("DPS", "", ESU_DPS2)
  ESU_DPS2 <- gsub("ESU", "", ESU_DPS2)
  # ESU_DPS2<-gsub("Basin","",ESU_DPS2)
  ESU_DPS2 <- gsub("  ", " ", ESU_DPS2)
  ESU_DPS2 <- gsub("  ", " ", ESU_DPS2)
  ESU_DPS2[grep(c("Steelhead"), ESU_DPS2)] <- paste(ESU_DPS2[grep(c("Steelhead"), ESU_DPS2)], "DPS")
  ESU_DPS2[grep(c("Chinook"), ESU_DPS2)] <- paste(ESU_DPS2[grep(c("Chinook"), ESU_DPS2)], "ESU")
  ESU_DPS2[grep(c("Coho"), ESU_DPS2)] <- paste(ESU_DPS2[grep(c("Coho"), ESU_DPS2)], "ESU")
  ESU_DPS2[grep(c("Sockeye"), ESU_DPS2)] <- paste(ESU_DPS2[grep(c("Sockeye"), ESU_DPS2)], "ESU")
  ESU_DPS2[grep(c("Chum"), ESU_DPS2)] <- paste(ESU_DPS2[grep(c("Chum"), ESU_DPS2)], "ESU")
  DIP_Results$ESU_DPS2 <- ESU_DPS2

  pdf(paste(SubDir, "/DIP_RESULTS_PLOTS_", data_date, ".pdf", sep = ""))
  counter <- 0
  for (i in 1:length(unique(DIP_Results$ESU_DPS))) {
    p <- ggplot(data = DIP_Results %>% filter(ESU_DPS == unique(DIP_Results$ESU_DPS)[i]), aes(x = Geomean_5yr_prop_goal * 100, y = reorder(COMMONPOPNAME2, Geomean_5yr_prop_goal), fill = "blue")) +
      geom_bar(stat = "identity") +
      geom_vline(xintercept = 100, linetype = "dashed") +
      ggtitle(unique(DIP_Results$ESU_DPS2)[i]) +
      xlab(label = "5 Year Geomean % of Recovery Goal") +
      ylab(label = "") +
      theme_bw() +
      labs(caption = paste0("Caption: Five year (", lastyear - (geomeanyears - 1), "-", lastyear, ") geomean smoothed\nabundance as a percentage of recovery goals.\nPopulations with no bars had no usable data.")) +
      theme(
        legend.position = "none", axis.text = element_text(size = 6),
        plot.caption = element_text(vjust = -1, hjust = 0, colour = "blue", size = 10)
      ) +
      annotate("text", x = Inf, y = Inf, label = paste("Data Updated ", data_date, sep = ""), vjust = 1.5, hjust = 1, col = "grey60", cex = 2, alpha = 0.8, srt = 00)
    print(p)
    p <- ggplot(data = DIP_Results %>% filter(ESU_DPS == unique(DIP_Results$ESU_DPS)[i]), aes(x = Percent.change.per.year, y = reorder(COMMONPOPNAME2, Percent.change.per.year), fill = "blue")) +
      geom_bar(stat = "identity") +
      geom_vline(xintercept = 0, linetype = "dashed") +
      ggtitle(unique(DIP_Results$ESU_DPS2)[i]) +
      xlab(label = "Trend Since ESA Listing (% change per year)") +
      ylab(label = "") +
      theme_bw() +
      labs(caption = "Caption: Trend since ESA-listing (percent change in\nabundance per year). Populations with no bars \nhad no usable data.") +
      xlim(min(DIP_Results$Percent.change.per.year) * 1.05, max(DIP_Results$Percent.change.per.year) * 1.05) +
      theme(
        legend.position = "none", axis.text = element_text(size = 6),
        plot.caption = element_text(vjust = -1, hjust = 0, colour = "blue", size = 10)
      ) +
      annotate("text", x = Inf, y = Inf, label = paste("Data Updated ", data_date, sep = ""), vjust = 1.5, hjust = 1, col = "grey60", cex = 2, alpha = 0.8, srt = 00)
    print(p)
    counter <- counter + 1
    print(resultsplots$plots[[counter]])
    for (k in 1:resultsplots$popcount[[i]]) {
      counter <- counter + 1
      print(resultsplots$plots[[counter]])
    }
  }
  dev.off()

  # plot 4 status and trend continuous
  DIP_Results <- data.frame(DIP_Results %>% filter(!is.na(Percent.change.per.year)) %>% group_by(ESU_DPS, COMMONPOPNAME2))
  DIP_Results <- data.frame(DIP_Results %>% filter(!is.na(Geomean_5yr)) %>% group_by(ESU_DPS, COMMONPOPNAME2))
  DIP_Results$slope <- log(DIP_Results$Percent.change.per.year / 100 + 1)
  DIP_Results$log_geomean_prop_goal <- log(DIP_Results$Geomean_5yr_prop_goal)
  plotdat <- data.frame(DIP_Results %>% group_by(ESU_DPS) %>% summarise(
    percent_above_goal = sum(abovegoal, na.rm = T) / n(),
    geomean_percent_change = (exp(mean(slope, na.rm = T)) - 1) * 100,
    median_percent_change = (exp(median(slope, na.rm = T)) - 1) * 100,
    min_percent_change = (exp(min(slope, na.rm = T)) - 1) * 100,
    max_percent_change = (exp(max(slope, na.rm = T)) - 1) * 100,
    Q25_percent_change = (exp(quantile(slope, 0.25, na.rm = T)) - 1) * 100,
    Q75_percent_change = (exp(quantile(slope, 0.75, na.rm = T)) - 1) * 100,
    geomean_ratio_goal = exp(mean(log_geomean_prop_goal, na.rm = T)) * 100,
    median_ratio_goal = exp(median(log_geomean_prop_goal, na.rm = T)) * 100,
    min_ratio_goal = exp(min(log_geomean_prop_goal, na.rm = T)) * 100,
    max_ratio_goal = exp(max(log_geomean_prop_goal, na.rm = T)) * 100,
    Q25_ratio_goal = exp(quantile(log_geomean_prop_goal, 0.25, na.rm = T)) * 100,
    Q75_ratio_goal = exp(quantile(log_geomean_prop_goal, 0.75, na.rm = T)) * 100,
    count_of_pops = n(),
    FutureStatus = median_ratio_goal * (1 + median_percent_change / 100)^futureyears
  ))
  plotdat <- merge(plotdat, totalmissing, by = "ESU_DPS", all = T)
  plotdat$total_missing[is.na(plotdat$total_missing)] <- 0
  plotdat$count_of_pops[is.na(plotdat$count_of_pops)] <- 0
  plotdat$p_missing <- plotdat$total_missing / (plotdat$count_of_pops + plotdat$total_missing)
  plotdat$p_not_missing <- plotdat$count_of_pops / (plotdat$count_of_pops + plotdat$total_missing)
  plotdat <- data.frame(plotdat %>% mutate(ESU_rank = rank(1 / FutureStatus)))

  Location <- data.frame(unlist(strsplit(plotdat$ESU_DPS, "\\(|\\)"))) %>% filter(row_number() %% 2 == 0)
  Location[, 1] <- capitalize_str(gsub("-run", "", Location[, 1]))
  Location[, 1] <- gsub("/s", "/S", Location[, 1])
  Species <- data.frame(unlist(strsplit(plotdat$ESU_DPS, "\\(|\\)"))) %>% filter(row_number() %% 2 != 0)
  Species <- data.frame(strsplit(as.character(Species[, 1]), ","))[2, ]
  Species <- as.character(gsub(" ", "", as.character(unlist(Species))))
  Species <- paste(toupper(substring(Species, 1, 1)), substring(Species, 2), sep = "")
  ESU_DPS2 <- paste(as.character(Location[, 1]), as.character(Species), sep = "")
  ESU_DPS2 <- gsub("DPS", "", ESU_DPS2)
  ESU_DPS2 <- gsub("ESU", "", ESU_DPS2)
  # ESU_DPS2<-gsub("Basin","",ESU_DPS2)
  ESU_DPS2 <- gsub("  ", " ", ESU_DPS2)
  ESU_DPS2 <- gsub("  ", " ", ESU_DPS2)
  ESU_DPS2[grep(c("Steelhead"), ESU_DPS2)] <- paste(ESU_DPS2[grep(c("Steelhead"), ESU_DPS2)], "DPS")
  ESU_DPS2[grep(c("Chinook"), ESU_DPS2)] <- paste(ESU_DPS2[grep(c("Chinook"), ESU_DPS2)], "ESU")
  ESU_DPS2[grep(c("Coho"), ESU_DPS2)] <- paste(ESU_DPS2[grep(c("Coho"), ESU_DPS2)], "ESU")
  ESU_DPS2[grep(c("Sockeye"), ESU_DPS2)] <- paste(ESU_DPS2[grep(c("Sockeye"), ESU_DPS2)], "ESU")
  ESU_DPS2[grep(c("Chum"), ESU_DPS2)] <- paste(ESU_DPS2[grep(c("Chum"), ESU_DPS2)], "ESU")
  plotdat$ESU_DPS2 <- ESU_DPS2
  plotdat$ESU_DPS_names <- paste(plotdat$ESU_DPS2, " (n = ", plotdat$count_of_pops, "/", plotdat$count_of_pops + plotdat$total_missing, ")", sep = "")

  cols3 <- brewer.pal(n = 11, "RdBu")
  # cols2<-brewer.pal(9,"RdYlGn")
  # specialcol<-add.alpha("#45A3AD01",alpha=0.5)
  # col2rgb("#E3AE26")
  # specialcol<-rgb(red=69,green=163,blue=173,alpha=1,maxColorValue = 255)
  # plot(x=1,y=1,pch=20,cex=80,col="#E0E0D0" )
  # colorRampPalette(c("#E79A38D9","#45A3AD80"))(3)[2]
  # r
  cols2 <- c("red", "#E79A38D9", "#45A3AD80", "#45A3ADFF")
  # plot(x=c(1:4),y=c(1:4),col=cols2,cex=80,pch=20)
  # pdf("ESU Plot_Status_Trend_Continuous_geomean.pdf",width=11,height=8.5)
  #   p<-ggplot(plotdat, aes(y=geomean_ratio_goal,x = geomean_percent_change)) +
  #   geom_rect(data=NULL,aes(xmin=-Inf,xmax=0,ymin=0,ymax=100),fill=cols2[3],color="black")+
  #   geom_rect(data=NULL,aes(xmin=0,xmax=Inf,ymin=0,ymax=100),fill="white",color="black")+
  #   geom_rect(data=NULL,aes(xmin=-Inf,xmax=0,ymin=100,ymax=Inf),fill="white",color="black")+
  #   geom_rect(data=NULL,aes(xmin=0,xmax=Inf,ymin=100,ymax=Inf),fill=cols2[8],color="black")+
  #   geom_segment(data=plotdat%>%filter(plotdat$p_missing<0.4), mapping=aes(y=geomean_ratio_goal,x = min_percent_change,yend=geomean_ratio_goal,xend= max_percent_change),color=cols2[11],size=0.25,alpha=0.2,linetype="dashed") +
  #   geom_segment(data=plotdat%>%filter(plotdat$p_missing<0.4), mapping=aes(y=min_ratio_goal,x = geomean_percent_change,yend=max_ratio_goal,xend= geomean_percent_change),color=cols2[11],size=0.25,alpha=0.2,linetype="dashed") +
  #   #geom_segment(data=plotdat, mapping=aes(y=geomean_ratio_goal,x = Q25_percent_change,yend=geomean_ratio_goal,xend= Q75_percent_change),color=cols2[11],size=2,alpha=0.4) +
  #   #geom_segment(data=plotdat, mapping=aes(y=Q25_ratio_goal,x = geomean_percent_change,yend=Q75_ratio_goal,xend= geomean_percent_change),color=cols2[11],size=2,alpha=0.4) +
  #   geom_point(data=plotdat, mapping=aes(y=geomean_ratio_goal,x = geomean_percent_change),size=4,color=cols2[11],shape=1) +
  #   geom_point(data=plotdat%>%filter(plotdat$p_missing<0.4), mapping=aes(y=geomean_ratio_goal,x = geomean_percent_change),size=4,color=cols2[11]) +
  #   theme_classic() +
  #   theme(legend.position = "none", panel.border = element_rect(colour = "black", fill=NA, size=2))+
  #   geom_text_repel(data=plotdat, mapping=aes(y=geomean_ratio_goal,x = geomean_percent_change,label=ESU_DPS_names),color="black",size=3)+
  #   ylab(label="5 Yr Geomean % of Recovery Goal")+
  #   xlab(label="Trend Since ESA Listing (% change per year)")+
  #   scale_y_continuous(expand = c(0, 0), limits = c(0,max(plotdat$max_ratio_goal[plotdat$p_missing<0.4])*1.05))+
  #   scale_y_log10()+
  #   annotation_logticks(side="lr")
  #   print(p)
  # dev.off()

  jpeg(paste(SubDir, "/ESU Plot_Status_Trend_Continuous_median_log", data_date, ".jpeg", sep = ""), width = 11, height = 8.5, units = "in", res = 700)
  p <- ggplot(plotdat, aes(y = median_ratio_goal, x = median_percent_change)) +
    # annotate("rect",xmin=-Inf,xmax=0,ymin=0,ymax=100,fill=cols2[1],alpha=0.5,color="black")+
    # annotate("rect",xmin=0,xmax=Inf,ymin=0,ymax=100,fill=cols2[4],alpha=0.5,color="black")+
    # annotate("rect",xmin=-Inf,xmax=0,ymin=100,ymax=Inf,fill=cols2[4],alpha=0.5,color="black")+
    # annotate("rect",xmin=0,xmax=Inf,ymin=100,ymax=Inf,fill=cols2[9],alpha=0.5,color="black")+
    annotate("rect", xmin = -Inf, xmax = 0, ymin = 0, ymax = 100, fill = cols2[1], color = "black") +
    # annotate("rect",xmin=0,xmax=Inf,ymin=0,ymax=100,fill=colorRampPalette(c(cols2[2],cols2[3]))(3)[2],color="black")+
    # annotate("rect",xmin=-Inf,xmax=0,ymin=100,ymax=Inf,fill=colorRampPalette(c(cols2[2],cols2[3]))(3)[2],color="black")+
    annotate("rect", xmin = 0, xmax = Inf, ymin = 100, ymax = Inf, fill = cols2[4], color = "black") +
    geom_segment(data = plotdat %>% filter(plotdat$p_missing < 0.4), mapping = aes(y = median_ratio_goal, x = min_percent_change, yend = median_ratio_goal, xend = max_percent_change), color = cols3[11], size = 0.25, linetype = "dashed", alpha = 0.2) +
    geom_segment(data = plotdat %>% filter(plotdat$p_missing < 0.4), mapping = aes(y = min_ratio_goal, x = median_percent_change, yend = max_ratio_goal, xend = median_percent_change), color = cols3[11], size = 0.25, linetype = "dashed", alpha = 0.2) +
    geom_segment(data = plotdat %>% filter(plotdat$p_missing < 0.4), mapping = aes(y = median_ratio_goal, x = Q25_percent_change, yend = median_ratio_goal, xend = Q75_percent_change), color = cols3[11], size = 1, alpha = 0.4) +
    geom_segment(data = plotdat %>% filter(plotdat$p_missing < 0.4), mapping = aes(y = Q25_ratio_goal, x = median_percent_change, yend = Q75_ratio_goal, xend = median_percent_change), color = cols3[11], size = 1, alpha = 0.4) +
    geom_point(data = plotdat, mapping = aes(y = median_ratio_goal, x = median_percent_change), size = 4, color = cols3[11], shape = 1) +
    geom_point(data = plotdat %>% filter(plotdat$p_missing < 0.4), mapping = aes(y = median_ratio_goal, x = median_percent_change), size = 4, color = cols3[11]) +
    theme(legend.position = "none", panel.border = element_rect(colour = "black", fill = NA, size = 2)) +
    geom_text_repel(data = plotdat %>% filter(plotdat$p_missing < 1), mapping = aes(y = median_ratio_goal, x = median_percent_change, label = ESU_DPS_names), color = "black", size = 3) +
    ylab(label = "Current Status (5 year geomean % of recovery goal)") +
    xlab(label = "Trend Since ESA Listing (% change per year)") +
    # scale_y_continuous(expand = c(0, 0), limits = c(0,max(plotdat$max_ratio_goal[plotdat$p_missing<0.4])*1.05))+
    scale_y_log10() +
    annotation_logticks(side = "l", outside = F) +
    # coord_cartesian(clip = "off")+
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    annotate("text", x = Inf, y = Inf, label = paste("Data Updated ", data_date, sep = ""), vjust = 1.5, hjust = 1, col = "white", cex = 2, alpha = 0.8, srt = 00)
  print(p)
  dev.off()

  jpeg(paste(SubDir, "/ESU Plot_Status_Trend_Continuous_median", data_date, ".jpeg", sep = ""), width = 11, height = 8.5, units = "in", res = 700)
  p <- ggplot(plotdat, aes(y = median_ratio_goal, x = median_percent_change)) +
    # annotate("rect", xmin=-Inf,xmax=0,ymin=0,ymax=100,fill=cols2[1],alpha=0.5,color="black")+
    # annotate("rect", xmin=0,xmax=Inf,ymin=0,ymax=100,fill=cols2[4],alpha=0.5,color="black")+
    # annotate("rect", xmin=-Inf,xmax=0,ymin=100,ymax=Inf,fill=cols2[4],alpha=0.5,color="black")+
    # annotate("rect", xmin=0,xmax=Inf,ymin=100,ymax=Inf,fill=cols2[9],alpha=0.5,color="black")+
    annotate("rect", xmin = -Inf, xmax = 0, ymin = 0, ymax = 100, fill = cols2[1], color = "black") +
    # annotate("rect",xmin=0,xmax=Inf,ymin=0,ymax=100,fill=colorRampPalette(c(cols2[2],cols2[3]))(3)[2],color="black")+
    # annotate("rect",xmin=-Inf,xmax=0,ymin=100,ymax=Inf,fill=colorRampPalette(c(cols2[2],cols2[3]))(3)[2],color="black")+
    annotate("rect", xmin = 0, xmax = Inf, ymin = 100, ymax = Inf, fill = cols2[4], color = "black") +
    geom_segment(data = plotdat %>% filter(plotdat$p_missing < 0.4), mapping = aes(y = median_ratio_goal, x = min_percent_change, yend = median_ratio_goal, xend = max_percent_change), color = cols3[11], size = 0.25, linetype = "dashed", alpha = 0.2) +
    geom_segment(data = plotdat %>% filter(plotdat$p_missing < 0.4), mapping = aes(y = min_ratio_goal, x = median_percent_change, yend = max_ratio_goal, xend = median_percent_change), color = cols3[11], size = 0.25, linetype = "dashed", alpha = 0.2) +
    geom_segment(data = plotdat %>% filter(plotdat$p_missing < 0.4), mapping = aes(y = median_ratio_goal, x = Q25_percent_change, yend = median_ratio_goal, xend = Q75_percent_change), color = cols3[11], size = 1, alpha = 0.4) +
    geom_segment(data = plotdat %>% filter(plotdat$p_missing < 0.4), mapping = aes(y = Q25_ratio_goal, x = median_percent_change, yend = Q75_ratio_goal, xend = median_percent_change), color = cols3[11], size = 1, alpha = 0.4) +
    geom_point(data = plotdat, mapping = aes(y = median_ratio_goal, x = median_percent_change), size = 4, color = cols3[11], shape = 1) +
    geom_point(data = plotdat %>% filter(plotdat$p_missing < 0.4), mapping = aes(y = median_ratio_goal, x = median_percent_change), size = 4, color = cols3[11]) +
    theme(legend.position = "none", panel.border = element_rect(colour = "black", fill = NA, size = 2)) +
    geom_text_repel(data = plotdat, mapping = aes(y = median_ratio_goal, x = median_percent_change, label = ESU_DPS_names), color = "black", size = 3) +
    ylab(label = "Current Status (5 year geomean % of recovery goal)") +
    xlab(label = "Trend Since ESA Listing (% change per year)") +
    scale_y_continuous(expand = c(-0.01, -0.01), limits = c(0, 300)) +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    annotate("text", x = Inf, y = Inf, label = paste("Data Updated ", data_date, sep = ""), vjust = 1.5, hjust = 1, col = "white", cex = 2, alpha = 0.8, srt = 00)
  # scale_y_log10()+
  # annotation_logticks(side="lr")
  print(p)
  dev.off()


  jpeg(paste(SubDir, "/ESU Plot_Status_Trend_Continuous_median_combinedscore_log", data_date, ".jpeg", sep = ""), width = 11, height = 8.5, units = "in", res = 700)

  # log version
  dat <- expand.grid(exp(seq(log(min(plotdat$min_ratio_goal[plotdat$p_missing < 0.4]) * 0.95), log(max(plotdat$max_ratio_goal, na.rm = T) * 1.05), 0.1)), seq(min(plotdat$min_percent_change, na.rm = T) * 1.05, max(plotdat$max_percent_change, na.rm = T) * 1.05, 0.1))
  #----------
  colnames(dat) <- c("Status", "Trend")
  dat$FutureStatus <- dat$Status * (1 + dat$Trend / 100)^futureyears
  dat <- dat[order(dat$FutureStatus), ]
  breaks <- c(0.0, 0.25, 0.5, 1) * 100
  # colorpal<-"Spectral"
  colorpal <- "RdYlGn"
  # limitcol<-brewer.pal(n=9,colorpal)[9]
  limitcol <- cols2[4]
  p <- ggplot(dat, aes(y = Status, x = Trend)) +
    geom_raster(aes(fill = FutureStatus), interpolate = TRUE) +
    # geom_tile(aes(fill = FutureStatus))+
    geom_contour(aes(z = FutureStatus), breaks = breaks, color = "white", show.legend = T) +
    # scale_fill_distiller(palette = colorpal, direction = 1,limits=c(0,100),na.value = limitcol)+
    scale_fill_gradientn(colours = cols2, values = c(0, 0.25, 0.5, 1), limits = c(0, 100), na.value = limitcol) +
    geom_text_contour(aes(z = FutureStatus), breaks = breaks, stroke = 0.2) +
    ylab("Current Status (5 year geomean % of recovery goal)") +
    xlab("Trend Since ESA Listing (% change per year)") +
    labs(fill = paste("Future Status \n(% of recovery goal in ", futureyears, " years)", sep = "")) +
    scale_x_continuous(expand = c(0, 0)) +
    geom_hline(yintercept = 100, linetype = "dashed", color = "black", size = 0.5) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "black", size = 0.5) +
    geom_segment(data = plotdat %>% filter(plotdat$p_missing < 0.4), mapping = aes(y = median_ratio_goal, x = min_percent_change, yend = median_ratio_goal, xend = max_percent_change), color = cols3[11], size = 0.25, linetype = "dashed", alpha = 0.2) +
    geom_segment(data = plotdat %>% filter(plotdat$p_missing < 0.4), mapping = aes(y = min_ratio_goal, x = median_percent_change, yend = max_ratio_goal, xend = median_percent_change), color = cols3[11], size = 0.25, linetype = "dashed", alpha = 0.2) +
    geom_segment(data = plotdat %>% filter(plotdat$p_missing < 0.4), mapping = aes(y = median_ratio_goal, x = Q25_percent_change, yend = median_ratio_goal, xend = Q75_percent_change), color = cols3[11], size = 1, alpha = 0.4) +
    geom_segment(data = plotdat %>% filter(plotdat$p_missing < 0.4), mapping = aes(y = Q25_ratio_goal, x = median_percent_change, yend = Q75_ratio_goal, xend = median_percent_change), color = cols3[11], size = 1, alpha = 0.4) +
    geom_point(data = plotdat, mapping = aes(y = median_ratio_goal, x = median_percent_change), size = 4, color = cols3[11], shape = 1) +
    geom_point(data = plotdat %>% filter(plotdat$p_missing < 0.4), mapping = aes(y = median_ratio_goal, x = median_percent_change), size = 4, color = cols3[11]) +
    geom_text_repel(data = plotdat, mapping = aes(y = median_ratio_goal, x = median_percent_change, label = ESU_DPS_names), color = "black", size = 3) +
    scale_y_log10(expand = c(-0.01, -0.01), limits = c(min(plotdat$min_ratio_goal[plotdat$p_missing < 0.4]), max(plotdat$max_ratio_goal[plotdat$p_missing < 0.4]))) +
    annotation_logticks(side = "l", outside = F) +
    # coord_cartesian(clip = "off")+
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    theme(panel.border = element_rect(fill = NA, color = "black")) +
    annotate("text", x = Inf, y = Inf, label = paste("Data Updated ", data_date, sep = ""), vjust = 1.5, hjust = 1, col = "white", cex = 2, alpha = 0.8, srt = 00)
  print(p)
  dev.off()

  jpeg(paste(SubDir, "/ESU Plot_Status_Trend_Continuous_median_combinedscore", data_date, ".jpeg", sep = ""), width = 11, height = 8.5, units = "in", res = 700)
  # no log version
  dat <- expand.grid(seq(min(plotdat$min_ratio_goal[plotdat$p_missing < 0.4]), max(plotdat$max_ratio_goal, na.rm = T) * 1.05, 0.1), seq(min(plotdat$min_percent_change, na.rm = T) * 1.05, max(plotdat$max_percent_change, na.rm = T) * 1.05, 0.1))
  #----------
  colnames(dat) <- c("Status", "Trend")
  dat$FutureStatus <- dat$Status * (1 + dat$Trend / 100)^futureyears
  dat <- dat[order(dat$FutureStatus), ]
  breaks <- c(0.0, 0.25, 0.5, 1) * 100

  p <- ggplot(dat, aes(y = Status, x = Trend)) +
    geom_raster(aes(fill = FutureStatus), interpolate = TRUE) +
    # geom_tile(aes(fill = FutureStatus))+
    geom_contour(aes(z = FutureStatus), breaks = breaks, color = "white", show.legend = T) +
    # scale_fill_distiller(palette = colorpal, direction = 1,limits=c(0,100),na.value = limitcol)+
    scale_fill_gradientn(colours = cols2, values = c(0, 0.25, 0.5, 1), limits = c(0, 100), na.value = limitcol) +
    geom_text_contour(aes(z = FutureStatus), breaks = breaks, stroke = 0.2) +
    ylab("Current Status (5 year geomean % of recovery goal)") +
    xlab("Trend Since ESA Listing (% change per year)") +
    labs(fill = paste("Future Status \n(% of recovery goal in ", futureyears, " years)", sep = "")) +
    scale_x_continuous(expand = c(0, 0)) +
    geom_hline(yintercept = 100, linetype = "dashed", color = "black", size = 0.5) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "black", size = 0.5) +
    geom_segment(data = plotdat %>% filter(plotdat$p_missing < 0.4), mapping = aes(y = median_ratio_goal, x = min_percent_change, yend = median_ratio_goal, xend = max_percent_change), color = cols3[11], size = 0.25, linetype = "dashed", alpha = 0.2) +
    geom_segment(data = plotdat %>% filter(plotdat$p_missing < 0.4), mapping = aes(y = min_ratio_goal, x = median_percent_change, yend = max_ratio_goal, xend = median_percent_change), color = cols3[11], size = 0.25, linetype = "dashed", alpha = 0.2) +
    geom_segment(data = plotdat %>% filter(plotdat$p_missing < 0.4), mapping = aes(y = median_ratio_goal, x = Q25_percent_change, yend = median_ratio_goal, xend = Q75_percent_change), color = cols3[11], size = 1, alpha = 0.4) +
    geom_segment(data = plotdat %>% filter(plotdat$p_missing < 0.4), mapping = aes(y = Q25_ratio_goal, x = median_percent_change, yend = Q75_ratio_goal, xend = median_percent_change), color = cols3[11], size = 1, alpha = 0.4) +
    geom_point(data = plotdat, mapping = aes(y = median_ratio_goal, x = median_percent_change), size = 4, color = cols3[11], shape = 1) +
    geom_point(data = plotdat %>% filter(plotdat$p_missing < 0.4), mapping = aes(y = median_ratio_goal, x = median_percent_change), size = 4, color = cols3[11]) +
    geom_text_repel(data = plotdat, mapping = aes(y = median_ratio_goal, x = median_percent_change, label = ESU_DPS_names), color = "black", size = 3) +
    scale_y_continuous(expand = c(0, -0.01), limits = c(0, 300)) +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    annotate("text", x = Inf, y = Inf, label = paste("Data Updated ", data_date, sep = ""), vjust = 1.5, hjust = 1, col = "white", cex = 2, alpha = 0.8, srt = 00)
  print(p)
  dev.off()


  jpeg(paste(SubDir, "/ESU Plot_Status_Trend_Continuous_median_combinedscore_log_nolines", data_date, ".jpeg", sep = ""), width = 11, height = 8.5, units = "in", res = 700)
  # versions with no lines
  dat <- expand.grid(exp(seq(log(min(plotdat$min_ratio_goal[plotdat$p_missing < 0.4]) * 0.95), log(max(plotdat$max_ratio_goal, na.rm = T) * 1.05), 0.1)), seq(min(plotdat$min_percent_change, na.rm = T) * 1.05, max(plotdat$max_percent_change, na.rm = T) * 1.05, 0.1))
  #----------
  colnames(dat) <- c("Status", "Trend")
  dat$FutureStatus <- dat$Status * (1 + dat$Trend / 100)^futureyears
  dat <- dat[order(dat$FutureStatus), ]
  breaks <- c(0.0, 0.25, 0.5, 1) * 100
  p <- ggplot(dat, aes(y = Status, x = Trend)) +
    geom_raster(aes(fill = FutureStatus), interpolate = TRUE) +
    # geom_tile(aes(fill = FutureStatus))+
    geom_contour(aes(z = FutureStatus), breaks = breaks, color = "white", show.legend = T) +
    # cale_fill_distiller(palette = colorpal, direction = 1,limits=c(0,100),na.value = limitcol)+
    scale_fill_gradientn(colours = cols2, values = c(0, 0.25, 0.5, 1), limits = c(0, 100), na.value = limitcol) +
    geom_text_contour(aes(z = FutureStatus), breaks = breaks, stroke = 0.2) +
    ylab("Current Status (5 year geomean % of recovery goal)") +
    xlab("Trend Since ESA Listing (% change per year)") +
    labs(fill = paste("Future Status \n(% of recovery goal in ", futureyears, " years)", sep = "")) +
    scale_x_continuous(expand = c(0, 0)) +
    # geom_hline(yintercept=100, linetype="dashed", color="black", size=0.5)+
    # geom_vline(xintercept=0, linetype="dashed", color="black", size=0.5)+
    # geom_segment(data=plotdat%>%filter(plotdat$p_missing<0.4), mapping=aes(y=median_ratio_goal,x = min_percent_change,yend=median_ratio_goal,xend= max_percent_change),color=cols3[11],size=0.25,linetype="dashed",alpha=0.2) +
    # geom_segment(data=plotdat%>%filter(plotdat$p_missing<0.4), mapping=aes(y=min_ratio_goal,x = median_percent_change,yend=max_ratio_goal,xend= median_percent_change),color=cols3[11],size=0.25,linetype="dashed",alpha=0.2) +
    # geom_segment(data=plotdat%>%filter(plotdat$p_missing<0.4), mapping=aes(y=median_ratio_goal,x = Q25_percent_change,yend=median_ratio_goal,xend= Q75_percent_change),color=cols3[11],size=1,alpha=0.4) +
    # geom_segment(data=plotdat%>%filter(plotdat$p_missing<0.4), mapping=aes(y=Q25_ratio_goal,x = median_percent_change,yend=Q75_ratio_goal,xend= median_percent_change),color=cols3[11],size=1,alpha=0.4) +
    geom_point(data = plotdat, mapping = aes(y = median_ratio_goal, x = median_percent_change), size = 4, color = cols3[11], shape = 1) +
    geom_point(data = plotdat %>% filter(plotdat$p_missing < 0.4), mapping = aes(y = median_ratio_goal, x = median_percent_change), size = 4, color = cols3[11]) +
    geom_text_repel(data = plotdat, mapping = aes(y = median_ratio_goal, x = median_percent_change, label = ESU_DPS_names), color = "black", size = 3) +
    scale_y_log10(expand = c(-0.01, -0.01), limits = c(min(plotdat$min_ratio_goal[plotdat$p_missing < 0.4]), max(plotdat$max_ratio_goal[plotdat$p_missing < 0.4]))) +
    annotation_logticks(side = "l", outside = F) +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    annotate("text", x = Inf, y = Inf, label = paste("Data Updated ", data_date, sep = ""), vjust = 1.5, hjust = 1, col = "white", cex = 2, alpha = 0.8, srt = 00)
  print(p)
  dev.off()

  jpeg(paste(SubDir, "/ESU Plot_Status_Trend_Continuous_median_combinedscore_nolines", data_date, ".jpeg", sep = ""), width = 11, height = 8.5, units = "in", res = 700)
  # no log version
  dat <- expand.grid(seq(min(plotdat$min_ratio_goal[plotdat$p_missing < 0.4]), max(plotdat$max_ratio_goal, na.rm = T) * 1.05, 0.1), seq(min(plotdat$min_percent_change, na.rm = T) * 1.05, max(plotdat$max_percent_change, na.rm = T) * 1.05, 0.1))
  #----------
  colnames(dat) <- c("Status", "Trend")
  dat$FutureStatus <- dat$Status * (1 + dat$Trend / 100)^futureyears
  dat <- dat[order(dat$FutureStatus), ]
  breaks <- c(0.0, 0.25, 0.5, 1) * 100
  p <- ggplot(dat, aes(y = Status, x = Trend)) +
    geom_raster(aes(fill = FutureStatus), interpolate = TRUE) +
    # geom_tile(aes(fill = FutureStatus))+
    geom_contour(aes(z = FutureStatus), breaks = breaks, color = "white", show.legend = T) +
    # scale_fill_distiller(palette = colorpal, direction = 1,limits=c(0,100),na.value = limitcol)+
    scale_fill_gradientn(colours = cols2, values = c(0, 0.25, 0.5, 1), limits = c(0, 100), na.value = limitcol) +
    geom_text_contour(aes(z = FutureStatus), breaks = breaks, stroke = 0.2) +
    ylab("Current Status (5 year geomean % of recovery goal)") +
    xlab("Trend Since ESA Listing (% change per year)") +
    labs(fill = paste("Future Status \n(% of recovery goal in ", futureyears, " years)", sep = "")) +
    scale_x_continuous(expand = c(0, 0)) +
    # geom_hline(yintercept=100, linetype="dashed", color="black", size=0.5)+
    # geom_vline(xintercept=0, linetype="dashed", color="black", size=0.5)+
    # geom_segment(data=plotdat%>%filter(plotdat$p_missing<0.4), mapping=aes(y=median_ratio_goal,x = min_percent_change,yend=median_ratio_goal,xend= max_percent_change),color=cols3[11],size=0.25,linetype="dashed",alpha=0.2) +
    # geom_segment(data=plotdat%>%filter(plotdat$p_missing<0.4), mapping=aes(y=min_ratio_goal,x = median_percent_change,yend=max_ratio_goal,xend= median_percent_change),color=cols3[11],size=0.25,linetype="dashed",alpha=0.2) +
    # geom_segment(data=plotdat%>%filter(plotdat$p_missing<0.4), mapping=aes(y=median_ratio_goal,x = Q25_percent_change,yend=median_ratio_goal,xend= Q75_percent_change),color=cols3[11],size=1,alpha=0.4) +
    # geom_segment(data=plotdat%>%filter(plotdat$p_missing<0.4), mapping=aes(y=Q25_ratio_goal,x = median_percent_change,yend=Q75_ratio_goal,xend= median_percent_change),color=cols3[11],size=1,alpha=0.4) +
    geom_point(data = plotdat, mapping = aes(y = median_ratio_goal, x = median_percent_change), size = 4, color = cols3[11], shape = 1) +
    geom_point(data = plotdat %>% filter(plotdat$p_missing < 0.4), mapping = aes(y = median_ratio_goal, x = median_percent_change), size = 4, color = cols3[11]) +
    geom_text_repel(data = plotdat, mapping = aes(y = median_ratio_goal, x = median_percent_change, label = ESU_DPS_names), color = "black", size = 3) +
    scale_y_continuous(expand = c(0, -0.01), limits = c(0, 300)) +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    annotate("text", x = Inf, y = Inf, label = paste("Data Updated ", data_date, sep = ""), vjust = 1.5, hjust = 1, col = "white", cex = 2, alpha = 0.8, srt = 00)
  print(p)
  dev.off()

  if (bypopsplots == "Yes") {
    pdf(paste(SubDir, "/ESU Plot_Status_Trend_Continuous_median_combinedscore_byPop.pdf", sep = ""), width = 11, height = 8.5)
    for (i in 1:length(plotdat$ESU_DPS_names)) {
      # versions with no lines
      dat <- expand.grid(exp(seq(log(min(plotdat$min_ratio_goal[plotdat$p_missing < 0.4]) * 0.95), log(max(plotdat$max_ratio_goal, na.rm = T) * 1.05), 0.1)), seq(min(plotdat$min_percent_change, na.rm = T) * 1.05, max(plotdat$max_percent_change, na.rm = T) * 1.05, 0.1))
      #----------
      colnames(dat) <- c("Status", "Trend")
      dat$FutureStatus <- dat$Status * (1 + dat$Trend / 100)^futureyears
      dat <- dat[order(dat$FutureStatus), ]
      breaks <- c(0.0, 0.25, 0.5, 1) * 100
      # colorpal<-"Spectral"
      p <- ggplot(dat, aes(y = Status, x = Trend)) +
        geom_raster(aes(fill = FutureStatus), interpolate = TRUE) +
        # geom_tile(aes(fill = FutureStatus))+
        geom_contour(aes(z = FutureStatus), breaks = breaks, color = "white", show.legend = T) +
        # scale_fill_distiller(palette = colorpal, direction = 1,limits=c(0,100),na.value = limitcol)+
        scale_fill_gradientn(colours = cols2, values = c(0, 0.25, 0.5, 1), limits = c(0, 100), na.value = limitcol) +
        geom_text_contour(aes(z = FutureStatus), breaks = breaks, stroke = 0.2) +
        ylab("Current Status (5 year geomean % of recovery goal)") +
        xlab("Trend Since ESA Listing (% change per year)") +
        labs(fill = paste("Future Status \n(% of recovery goal in ", futureyears, " years)", sep = "")) +
        scale_x_continuous(expand = c(0, 0)) +
        geom_point(data = plotdat %>% filter(ESU_DPS_names != ESU_DPS_names[i]), mapping = aes(y = median_ratio_goal, x = median_percent_change), size = 4, color = "white", shape = 1) +
        geom_point(data = plotdat %>% filter(ESU_DPS_names != ESU_DPS_names[i], plotdat$p_missing < 0.4), mapping = aes(y = median_ratio_goal, x = median_percent_change), size = 4, color = "white") +
        geom_point(data = plotdat %>% filter(ESU_DPS_names == ESU_DPS_names[i]), mapping = aes(y = median_ratio_goal, x = median_percent_change), size = 4, color = cols3[11], shape = 1) +
        geom_point(data = plotdat %>% filter(ESU_DPS_names == ESU_DPS_names[i], plotdat$p_missing < 0.4), mapping = aes(y = median_ratio_goal, x = median_percent_change), size = 4, color = cols3[11]) +
        geom_text_repel(data = plotdat, mapping = aes(y = median_ratio_goal, x = median_percent_change, label = ESU_DPS2), color = c(rep("white", i - 1), "black", rep("white", length(plotdat$ESU_DPS_names) - i)), size = 3) +
        scale_y_log10(expand = c(-0.01, -0.01), limits = c(min(plotdat$min_ratio_goal[plotdat$p_missing < 0.4]), max(plotdat$max_ratio_goal[plotdat$p_missing < 0.4]))) +
        annotation_logticks(side = "l", outside = F) +
        # coord_cartesian(clip = "off")+
        theme_bw() +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
        annotate("text", x = Inf, y = Inf, label = paste("Data Updated ", data_date, sep = ""), vjust = 1.5, hjust = 1, cex = 2, col = "white", alpha = 0.8, srt = 00)
      print(p)


      # no log version
      dat <- expand.grid(seq(min(plotdat$min_ratio_goal[plotdat$p_missing < 0.4]), max(plotdat$max_ratio_goal, na.rm = T) * 1.05, 0.1), seq(min(plotdat$min_percent_change, na.rm = T) * 1.05, max(plotdat$max_percent_change, na.rm = T) * 1.05, 0.1))
      #----------
      colnames(dat) <- c("Status", "Trend")
      dat$FutureStatus <- dat$Status * (1 + dat$Trend / 100)^futureyears
      dat <- dat[order(dat$FutureStatus), ]
      breaks <- c(0.0, 0.25, 0.5, 1) * 100
      p <- ggplot(dat, aes(y = Status, x = Trend)) +
        geom_raster(aes(fill = FutureStatus), interpolate = TRUE) +
        # geom_tile(aes(fill = FutureStatus))+
        geom_contour(aes(z = FutureStatus), breaks = breaks, color = "white", show.legend = T) +
        # scale_fill_distiller(palette = colorpal, direction = 1,limits=c(0,100),na.value = limitcol)+
        scale_fill_gradientn(colours = cols2, values = c(0, 0.25, 0.5, 1), limits = c(0, 100), na.value = limitcol) +
        geom_text_contour(aes(z = FutureStatus), breaks = breaks, stroke = 0.2) +
        ylab("Current Status (5 year geomean % of recovery goal)") +
        xlab("Trend Since ESA Listing (% change per year)") +
        labs(fill = paste("Future Status \n(% of recovery goal in ", futureyears, " years)", sep = "")) +
        scale_x_continuous(expand = c(0, 0)) +
        geom_point(data = plotdat %>% filter(ESU_DPS_names != ESU_DPS_names[i]), mapping = aes(y = median_ratio_goal, x = median_percent_change), size = 4, color = "white", shape = 1) +
        geom_point(data = plotdat %>% filter(ESU_DPS_names != ESU_DPS_names[i], plotdat$p_missing < 0.4), mapping = aes(y = median_ratio_goal, x = median_percent_change), size = 4, color = "white") +
        geom_point(data = plotdat %>% filter(ESU_DPS_names == ESU_DPS_names[i]), mapping = aes(y = median_ratio_goal, x = median_percent_change), size = 4, color = cols3[11], shape = 1) +
        geom_point(data = plotdat %>% filter(ESU_DPS_names == ESU_DPS_names[i], plotdat$p_missing < 0.4), mapping = aes(y = median_ratio_goal, x = median_percent_change), size = 4, color = cols3[11]) +
        geom_text_repel(data = plotdat, mapping = aes(y = median_ratio_goal, x = median_percent_change, label = ESU_DPS2), color = c(rep("white", i - 1), "black", rep("white", length(plotdat$ESU_DPS_names) - i)), size = 3) +
        scale_y_continuous(expand = c(0, -0.01), limits = c(0, max(plotdat$max_ratio_goal[plotdat$p_missing < 0.4]))) +
        theme_bw() +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
        annotate("text", x = Inf, y = Inf, label = paste("Data Updated ", data_date, sep = ""), vjust = 1.5, hjust = 1, col = "white", cex = 2, alpha = 0.8, srt = 00)
      print(p)
    }
    dev.off()


    # by pop but simplied
    pdf(paste(SubDir, "/ESU Plot_Status_Trend_Continuous_median_combinedscore_byPop_simple.pdf", sep = ""), width = 11, height = 8.5)
    for (i in 1:length(plotdat$ESU_DPS_names)) {
      # versions with no lines
      dat <- expand.grid(exp(seq(log(min(plotdat$min_ratio_goal[plotdat$p_missing < 0.4]) * 0.95), log(max(plotdat$max_ratio_goal, na.rm = T) * 1.05), 0.1)), seq(min(plotdat$min_percent_change, na.rm = T) * 1.05, max(plotdat$max_percent_change, na.rm = T) * 1.05, 0.1))
      #----------
      colnames(dat) <- c("Status", "Trend")
      dat$FutureStatus <- dat$Status * (1 + dat$Trend / 100)^futureyears
      dat <- dat[order(dat$FutureStatus), ]
      breaks <- c(0.0, 0.25, 0.5, 1) * 100
      # colorpal<-"Spectral"
      p <- ggplot(dat, aes(y = Status, x = Trend)) +
        geom_raster(aes(fill = FutureStatus), interpolate = TRUE) +
        # geom_tile(aes(fill = FutureStatus))+
        geom_contour(aes(z = FutureStatus), breaks = breaks, color = "white", show.legend = T) +
        # scale_fill_distiller(palette = colorpal, direction = 1,limits=c(0,100),na.value = limitcol)+
        scale_fill_gradientn(colours = cols2, values = c(0, 0.25, 0.5, 1), limits = c(0, 100), na.value = limitcol) +
        # geom_text_contour(aes(z = FutureStatus), breaks=breaks,stroke = 0.2,label.size=0)+
        ylab("Current Status (5 year geomean % of recovery goal)") +
        xlab("Trend Since ESA Listing (% change per year)") +
        labs(fill = paste("Future Status \n(% of recovery goal in ", futureyears, " years)", sep = "")) +
        scale_x_continuous(expand = c(0, 0)) +
        geom_point(data = plotdat %>% filter(ESU_DPS_names != ESU_DPS_names[i]), mapping = aes(y = median_ratio_goal, x = median_percent_change), size = 4, color = "white", shape = 1) +
        geom_point(data = plotdat %>% filter(ESU_DPS_names != ESU_DPS_names[i], plotdat$p_missing < 0.4), mapping = aes(y = median_ratio_goal, x = median_percent_change), size = 4, color = "white") +
        geom_point(data = plotdat %>% filter(ESU_DPS_names == ESU_DPS_names[i]), mapping = aes(y = median_ratio_goal, x = median_percent_change), size = 4, color = cols3[11], shape = 1) +
        geom_point(data = plotdat %>% filter(ESU_DPS_names == ESU_DPS_names[i], plotdat$p_missing < 0.4), mapping = aes(y = median_ratio_goal, x = median_percent_change), size = 4, color = cols3[11]) +
        # geom_text_repel(data=plotdat, mapping=aes(y=median_ratio_goal,x = median_percent_change,label=ESU_DPS2),color=c(rep("white",i-1),"black",rep("white",length(plotdat$ESU_DPS_names)-i)),size=3)+
        scale_y_log10(expand = c(-0.01, -0.01), limits = c(min(plotdat$min_ratio_goal[plotdat$p_missing < 0.4]), max(plotdat$max_ratio_goal[plotdat$p_missing < 0.4]))) +
        annotation_logticks(side = "l", outside = F) +
        # coord_cartesian(clip = "off")+
        theme_bw() +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
        annotate("text", x = Inf, y = Inf, label = paste("Data Updated ", data_date, sep = ""), vjust = 1.5, hjust = 1, col = "white", cex = 2, alpha = 0.8, srt = 00)
      print(p)


      # no log version
      dat <- expand.grid(seq(min(plotdat$min_ratio_goal[plotdat$p_missing < 0.4]), max(plotdat$max_ratio_goal, na.rm = T) * 1.05, 0.1), seq(min(plotdat$min_percent_change, na.rm = T) * 1.05, max(plotdat$max_percent_change, na.rm = T) * 1.05, 0.1))
      #----------
      colnames(dat) <- c("Status", "Trend")
      dat$FutureStatus <- dat$Status * (1 + dat$Trend / 100)^futureyears
      dat <- dat[order(dat$FutureStatus), ]
      breaks <- c(0.0, 0.25, 0.5, 1) * 100
      p <- ggplot(dat, aes(y = Status, x = Trend)) +
        geom_raster(aes(fill = FutureStatus), interpolate = TRUE) +
        # geom_tile(aes(fill = FutureStatus))+
        geom_contour(aes(z = FutureStatus), breaks = breaks, color = "white", show.legend = T) +
        # scale_fill_distiller(palette = colorpal, direction = 1,limits=c(0,100),na.value = limitcol)+
        scale_fill_gradientn(colours = cols2, values = c(0, 0.25, 0.5, 1), limits = c(0, 100), na.value = limitcol) +
        # geom_text_contour(aes(z = FutureStatus), breaks=breaks,stroke = 0.2,label.size=0)+
        ylab("Current Status (5 year geomean % of recovery goal)") +
        xlab("Trend Since ESA Listing (% change per year)") +
        labs(fill = paste("Future Status \n(% of recovery goal in ", futureyears, " years)", sep = "")) +
        scale_x_continuous(expand = c(0, 0)) +
        geom_point(data = plotdat %>% filter(ESU_DPS_names != ESU_DPS_names[i]), mapping = aes(y = median_ratio_goal, x = median_percent_change), size = 4, color = "white", shape = 1) +
        geom_point(data = plotdat %>% filter(ESU_DPS_names != ESU_DPS_names[i], plotdat$p_missing < 0.4), mapping = aes(y = median_ratio_goal, x = median_percent_change), size = 4, color = "white") +
        geom_point(data = plotdat %>% filter(ESU_DPS_names == ESU_DPS_names[i]), mapping = aes(y = median_ratio_goal, x = median_percent_change), size = 4, color = cols3[11], shape = 1) +
        geom_point(data = plotdat %>% filter(ESU_DPS_names == ESU_DPS_names[i], plotdat$p_missing < 0.4), mapping = aes(y = median_ratio_goal, x = median_percent_change), size = 4, color = cols3[11]) +
        # geom_text_repel(data=plotdat, mapping=aes(y=median_ratio_goal,x = median_percent_change,label=ESU_DPS2),color=c(rep("white",i-1),"black",rep("white",length(plotdat$ESU_DPS_names)-i)),size=3)+
        scale_y_continuous(expand = c(0, -0.01), limits = c(0, max(plotdat$max_ratio_goal[plotdat$p_missing < 0.4]))) +
        theme_bw() +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
        annotate("text", x = Inf, y = Inf, label = paste("Data Updated ", data_date, sep = ""), vjust = 1.5, hjust = 1, col = "white", cex = 2, alpha = 0.8, srt = 00)
      print(p)
    }
    dev.off()
  }

  # #log version
  # dat<-expand.grid(exp(seq(log(min(plotdat$min_ratio_goal[plotdat$p_missing<0.4])*0.95),log(max(plotdat$max_ratio_goal,na.rm = T)*1.05),0.1)),seq(min(plotdat$min_percent_change,na.rm=T)*1.05,max(plotdat$max_percent_change,na.rm=T)*1.05,0.1))
  # #----------
  # colnames(dat)<-c("Status","Trend")
  # dat$FutureStatus<-dat$Status*(1+dat$Trend/100)^futureyears
  # dat<-dat[order(dat$FutureStatus),]
  # breaks<-c(0.0,0.25,0.5,0.75,1)*100
  # #colorpal<-"Spectral"
  # colorpal<-"RdYlGn"
  # limitcol<-brewer.pal(n=9,colorpal)[9]
  # pdf("ESU Plot_Status_Trend_Continuous_geomean_combinedscore.pdf",width=11,height=8.5)
  # p <- ggplot(dat, aes(y=Status, x=Trend))+
  #   geom_raster(aes(fill = FutureStatus), interpolate = TRUE)+
  #   #geom_tile(aes(fill = FutureStatus))+
  #   geom_contour(aes(z=FutureStatus),breaks=breaks,color="white",show.legend = T)+
  #   scale_fill_distiller(palette = colorpal, direction = 1,limits=c(0,100),na.value = limitcol)+
  #   geom_text_contour(aes(z = FutureStatus), breaks=breaks,stroke = 0.2)+
  #   ylab("Current Status (5 year geomean % of recovery goal)")+
  #   xlab("Trend Since ESA Listing (% change per year)")+
  #   labs(fill = paste("Future Status \n(% of recovery goal in ",futureyears," years)",sep=""))+
  #   scale_x_continuous(expand = c(0, 0))+
  #   geom_hline(yintercept=100, linetype="dashed", color="black", size=0.5)+
  #   geom_vline(xintercept=0, linetype="dashed", color="black", size=0.5)+
  #   geom_segment(data=plotdat%>%filter(plotdat$p_missing<0.4), mapping=aes(y=geomean_ratio_goal,x = min_percent_change,yend=geomean_ratio_goal,xend= max_percent_change),color=cols3[11],size=0.25,linetype="dashed",alpha=0.2) +
  #   geom_segment(data=plotdat%>%filter(plotdat$p_missing<0.4), mapping=aes(y=min_ratio_goal,x = geomean_percent_change,yend=max_ratio_goal,xend= geomean_percent_change),color=cols3[11],size=0.25,linetype="dashed",alpha=0.2) +
  #   geom_segment(data=plotdat%>%filter(plotdat$p_missing<0.4), mapping=aes(y=geomean_ratio_goal,x = Q25_percent_change,yend=geomean_ratio_goal,xend= Q75_percent_change),color=cols3[11],size=1,alpha=0.4) +
  #   geom_segment(data=plotdat%>%filter(plotdat$p_missing<0.4), mapping=aes(y=Q25_ratio_goal,x = geomean_percent_change,yend=Q75_ratio_goal,xend= geomean_percent_change),color=cols3[11],size=1,alpha=0.4) +
  #   geom_point(data=plotdat, mapping=aes(y=geomean_ratio_goal,x = geomean_percent_change),size=4,color=cols3[11],shape=1) +
  #   geom_point(data=plotdat%>%filter(plotdat$p_missing<0.4), mapping=aes(y=geomean_ratio_goal,x = geomean_percent_change),size=4,color=cols3[11]) +
  #   geom_text_repel(data=plotdat, mapping=aes(y=geomean_ratio_goal,x = geomean_percent_change,label=ESU_DPS_names),color="black",size=3)+
  #   scale_y_log10(expand=c(-0.01,-0.01),limits = c(min(plotdat$min_ratio_goal[plotdat$p_missing<0.4]),max(plotdat$max_ratio_goal[plotdat$p_missing<0.4])))+
  #   annotation_logticks(side="lr")+
  #   theme_bw()
  # print(p)
  # dev.off()
  #
  # #no log version
  # dat<-expand.grid(seq(min(plotdat$min_ratio_goal[plotdat$p_missing<0.4]),max(plotdat$max_ratio_goal,na.rm = T)*1.05,0.1),seq(min(plotdat$min_percent_change,na.rm=T)*1.05,max(plotdat$max_percent_change,na.rm=T)*1.05,0.1))
  # #----------
  # colnames(dat)<-c("Status","Trend")
  # dat$FutureStatus<-dat$Status*(1+dat$Trend/100)^futureyears
  # dat<-dat[order(dat$FutureStatus),]
  # breaks<-c(0.0,0.25,0.5,0.75,1)*100
  #
  # pdf("ESU Plot_Status_Trend_Continuous_geomean_nolog_combinedscore.pdf",width=11,height=8.5)
  # p <- ggplot(dat, aes(y=Status, x=Trend))+
  #   geom_raster(aes(fill = FutureStatus), interpolate = TRUE)+
  #   #geom_tile(aes(fill = FutureStatus))+
  #   geom_contour(aes(z=FutureStatus),breaks=breaks,color="white",show.legend = T)+
  #   scale_fill_distiller(palette = colorpal, direction = 1,limits=c(0,100),na.value = limitcol)+
  #   geom_text_contour(aes(z = FutureStatus), breaks=breaks,stroke = 0.2)+
  #   ylab("Current Status (5 year geomean % of recovery goal)")+
  #   xlab("Trend Since ESA Listing (% change per year)")+
  #   labs(fill = paste("Future Status \n(% of recovery goal in ",futureyears," years)",sep=""))+
  #   scale_x_continuous(expand = c(0, 0))+
  #   geom_hline(yintercept=100, linetype="dashed", color="black", size=0.5)+
  #   geom_vline(xintercept=0, linetype="dashed", color="black", size=0.5)+
  #   geom_segment(data=plotdat%>%filter(plotdat$p_missing<0.4), aes(y=geomean_ratio_goal,x = min_percent_change,yend=geomean_ratio_goal,xend= max_percent_change),color=cols3[11],size=0.25,linetype="dashed",alpha=0.2) +
  #   geom_segment(data=plotdat%>%filter(plotdat$p_missing<0.4), aes(y=min_ratio_goal,x = geomean_percent_change,yend=max_ratio_goal,xend= geomean_percent_change),color=cols3[11],size=0.25,linetype="dashed",alpha=0.2) +
  #   geom_segment(data=plotdat%>%filter(plotdat$p_missing<0.4), aes(y=geomean_ratio_goal,x = Q25_percent_change,yend=geomean_ratio_goal,xend= Q75_percent_change),color=cols3[11],size=1,alpha=0.4) +
  #   geom_segment(data=plotdat%>%filter(plotdat$p_missing<0.4), aes(y=Q25_ratio_goal,x = geomean_percent_change,yend=Q75_ratio_goal,xend= geomean_percent_change),color=cols3[11],size=1,alpha=0.4) +
  #   geom_point(data=plotdat, aes(y=geomean_ratio_goal,x = geomean_percent_change),size=4,color=cols3[11],shape=1) +
  #   geom_point(data=plotdat%>%filter(plotdat$p_missing<0.4), mapping=aes(y=geomean_ratio_goal,x = geomean_percent_change),size=4,color=cols3[11]) +
  #   geom_text_repel(data=plotdat, mapping=aes(y=geomean_ratio_goal,x = geomean_percent_change,label=ESU_DPS_names),color="black",size=3)+
  #   scale_y_continuous(expand=c(0,-0.01),limits = c(0,max(plotdat$max_ratio_goal[plotdat$p_missing<0.4])))+
  #   # scale_color_manual(name = "",values = blue"",labels="Geomean of Recovery Ratios")+
  #   # scale_linetype_manual("Interquartile Range",values=c("segment legend"=2))+
  #   theme_bw()
  # print(p)
  # dev.off()

  # pdf("ESU Plot_Status_Trend_Continuous_median_transpose_nolog.pdf",width=11,height=8.5)
  # p<-ggplot(plotdat, aes(x=median_ratio_goal,y = median_percent_change)) +
  #   geom_rect(data=NULL,aes(ymin=-Inf,ymax=0,xmin=0,xmax=100),fill=cols2[3],color="black")+
  #   geom_rect(data=NULL,aes(ymin=0,ymax=Inf,xmin=0,xmax=100),fill="white",color="black")+
  #   geom_rect(data=NULL,aes(ymin=-Inf,ymax=0,xmin=100,xmax=Inf),fill="white",color="black")+
  #   geom_rect(data=NULL,aes(ymin=0,ymax=Inf,xmin=100,xmax=Inf),fill=cols2[8],color="black")+
  #   geom_segment(data=plotdat%>%filter(plotdat$p_missing<0.4), mapping=aes(x=median_ratio_goal,y = min_percent_change,xend=median_ratio_goal,yend= max_percent_change),color=cols2[11],size=0.25,linetype="dashed",alpha=0.2) +
  #   geom_segment(data=plotdat%>%filter(plotdat$p_missing<0.4), mapping=aes(x=min_ratio_goal,y = median_percent_change,xend=max_ratio_goal,yend= median_percent_change),color=cols2[11],size=0.25,linetype="dashed",alpha=0.2) +
  #   geom_segment(data=plotdat%>%filter(plotdat$p_missing<0.4), mapping=aes(x=median_ratio_goal,y = Q25_percent_change,xend=median_ratio_goal,yend= Q75_percent_change),color=cols2[11],size=1,alpha=0.4) +
  #   geom_segment(data=plotdat%>%filter(plotdat$p_missing<0.4), mapping=aes(x=Q25_ratio_goal,y = median_percent_change,xend=Q75_ratio_goal,yend= median_percent_change),color=cols2[11],size=1,alpha=0.4) +
  #   geom_point(data=plotdat, mapping=aes(x=median_ratio_goal,y = median_percent_change),size=4,color=cols2[11],shape=1) +
  #   geom_point(data=plotdat%>%filter(plotdat$p_missing<0.4), mapping=aes(x=median_ratio_goal,y = median_percent_change),size=4,color=cols2[11]) +
  #   theme_classic() +
  #   theme(legend.position = "none", panel.border = element_rect(colour = "black", fill=NA, size=2))+
  #   geom_text_repel(data=plotdat, mapping=aes(x=median_ratio_goal,y = median_percent_change,label=ESU_DPS_names),color="black",size=3)+
  #   xlab(label="5 Yr Geomean % of Recovery Goal")+
  #   ylab(label="Trend Since ESA Listing (% change per year)")+
  #   scale_x_continuous(expand = c(0, 0), limits = c(0,max(plotdat$max_ratio_goal[plotdat$p_missing<0.4])*1.05))
  #   #scale_x_log10()+
  #   #annotation_logticks(side="tb")
  # print(p)
  # dev.off()


  # missing data plot
  # pdf(paste("ESU Plot_Available_Data_",data_date,".pdf",sep=""),width=11,h
  # plotdat<-plotdat[order(-plotdat$p_not_missing),]
  # #p<-ggplot(plotdat, aes(x=p_not_missing*100,y=reorder(ESU_DPS2,p_missing))) +
  # p<-ggplot(plotdat, aes(x=p_not_missing*100,y=reorder(ESU_DPS_names,p_missing))) +
  # geom_bar(stat="identity",fill="#45A3ADFF")+
  # #geom_text(data=plotdat, mapping=aes(y=reorder(ESU_DPS2,p_not_missing),x = p_not_missing*100-5,label=paste("n = ",count_of_pops," / ",count_of_pops+total_missing, sep="")),color="black",size=3)+
  # #geom_text(data=plotdat, mapping=aes(y=reorder(ESU_DPS2,p_not_missing),x = 50,label=paste("n = ",count_of_pops," / ",count_of_pops+total_missing, sep="")),color="black",size=3)+
  # theme_classic() +
  # theme(axis.text.y = element_text(angle=00,hjust=1,vjust=0.5), plot.margin = unit(c(0.5,0.5,0.5,0.5),"cm"))+
  # scale_x_continuous( expand = c(0,0),limits = c(0,100))+
  # ylab(label = NULL)+
  # xlab(label = "Percent of Populations with Usable Data")
  # print(p)
  # dev.off()


  jpeg(paste(SubDir, "/ESU Plot_Available_Data_", data_date, ".jpeg", sep = ""), width = 11, height = 8.5, units = "in", res = 700)
  plotdat <- data.frame(plotdat %>% dplyr::select(ESU_DPS_names, ESU_DPS2, p_missing, p_not_missing) %>%
    pivot_longer(!ESU_DPS_names & !ESU_DPS2, names_to = "Type", values_to = "Proportion"))
  plotdat <- plotdat[rev(order(plotdat$Type, plotdat$Proportion)), ]
  plotdat$ESU_DPS_names <- factor(plotdat$ESU_DPS_names, levels = rev(plotdat$ESU_DPS_names[plotdat$Type == "p_missing"]))
  p <- ggplot(plotdat %>% mutate(order = paste(Proportion, Type)), aes(x = Proportion * 100, y = ESU_DPS_names, fill = Type)) +
    geom_bar(stat = "identity", color = "black") +
    # geom_point()+
    scale_fill_manual(values = c("white", "#45A3ADFF"), labels = c("No Usable Data", "Usable Data"), name = NULL) +
    # geom_text(data=plotdat, mapping=aes(y=reorder(ESU_DPS2,p_not_missing),x = p_not_missing*100-5,label=paste("n = ",count_of_pops," / ",count_of_pops+total_missing, sep="")),color="black",size=3)+
    # geom_text(data=plotdat, mapping=aes(y=reorder(ESU_DPS2,p_not_missing),x = 50,label=paste("n = ",count_of_pops," / ",count_of_pops+total_missing, sep="")),color="black",size=3)+
    theme_classic() +
    theme(axis.text.y = element_text(angle = 00, hjust = 1, vjust = 0.5), plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")) +
    scale_x_continuous(expand = c(0, 0), limits = c(0, 100)) +
    ylab(label = NULL) +
    xlab(label = "Percent of Populations with Usable Data") +
    annotate("text", x = Inf, y = Inf, label = paste("Data Updated ", data_date, sep = ""), vjust = 1, hjust = 1, col = "grey60", cex = 2, alpha = 0.8, srt = 00)
  print(p)
  dev.off()
}
