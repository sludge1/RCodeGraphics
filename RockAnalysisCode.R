rockdata<-read.csv(file ='RockCreekDailyAtzFlow.csv', header = TRUE)
class(rockdata$StatisticsDate)
rockdata$StatisticsDate<-as.Date(rockdata$StatisticsDate, "%d-%b-%Y")
plot(rockdata$StatisticsDate, rockdata$FlowCFSDayAvg,"l", col = "burlywood4")
plot(rockdata$StatisticsDate, rockdata$AVGAtzPPB,"p", col = "brown")

plot(rockdata$StatisticsDate, rockdata$AVGAtzPPB,"p", col = "brown", log = 'y')

rockdata$dayofyear<-as.numeric(strftime(rockdata$StatisticsDate, format = '%j'))
rockdata$yearfactor<- as.numeric(strftime(rockdata$StatisticsDate, format = '%Y'))
 library(ggplot2)
library(grid)
pdf("rockgraph.pdf", width = 8, height = 32)
ggplot(aes(dayofyear,AVGAtzPPB), data=rockdata, color = 'black') + geom_line() + facet_grid(yearfactor~.) +
  scale_x_continuous( limits = c(100,220))
dev.off()

pdf("rockgraph1.pdf", width = 8, height = 32)
ggplot(aes(dayofyear,AVGAtzPPB), data=rockdata, color = 'black') + geom_line() + facet_grid(yearfactor~.) +
  scale_x_continuous( limits = c(100,220)) + scale_y_log10()
dev.off()

pdf("rockgraph2.pdf", width = 8, height = 32)
ggplot(aes(dayofyear,AVGAtzPPB), data=rockdata, color = 'black') + geom_point() + facet_grid(yearfactor~.) +
  scale_x_continuous( limits = c(100,220))
dev.off()

pdf("rockgraph4.pdf", width = 8, height = 32)
ggplot(aes(dayofyear,AVGAtzPPB, color = FlowCFSDayAvg ), data=rockdata) + geom_point() + facet_grid(yearfactor~.) +
  scale_x_continuous( limits = c(100,220)) + scale_color_gradient(low = 'blue', high = 'red') +
  xlab('Day of Year') + ylab('Average Daily Atrazine Concentration ug/L') + theme_bw()
dev.off()

pdf("rockgraph3.pdf", width = 8, height = 32)
ggplot(aes(dayofyear,AVGAtzPPB), data=rockdata, color = 'black') + geom_point() + facet_grid(yearfactor~.) +
  scale_x_continuous( limits = c(100,220)) + scale_y_log10()
dev.off()

# data already ordered - so do not need to reorder rockdata<-order(rockdata$StatisticsDate)

rockdataGT3<-subset(rockdata, AVGAtzPPB>=3.0)

pdf("rockgraph5.pdf", width = 8, height = 32)
ggplot(aes(dayofyear,AVGAtzPPB, color = FlowCFSDayAvg ), data=rockdataGT3) + geom_point() + facet_grid(yearfactor~.) +
  scale_x_continuous( limits = c(100,220)) + scale_color_gradient(low = 'blue', high = 'red') +
  xlab('Day of Year') + ylab('Average Daily Atrazine Concentration ug/L') + theme_bw()
dev.off()

write.table(summary(rockdataGT3), file = "RockDataSummaryGT3.txt")

rockGT3bar<-data.frame(summaryBy(AVGAtzPPB~dayofyear, data=rockdataGT3, FUN=function(x) c(count=length(x),mean(x))))

barplot(rockGT3bar$AVGAtzPPB.FUN1, names.arg = rockGT3bar$dayofyear, ylim = c(0,max(rockGT3bar$AVGAtzPPB.FUN1)+5))

pdf("rockcountsbyDOY.pdf")
ggplot(aes(dayofyear, AVGAtzPPB.FUN1), data = rockGT3bar, color = 'burlywood4' ) + geom_bar(stat='identity')+theme_bw()+xlab('Day of Year') + ylab('Counts of Days Across Years GTE 3 ug/L Avg Daily Conc')
dev.off()

write.csv(rockGT3bar, file = 'rockcountsbyDOY.csv', col.names = TRUE)

dayofyear<-data.frame(seq(1:366))
names(dayofyear)<-"dayofyear"
mrgdayofyear<-merge(dayofyear,rockGT3bar, by='dayofyear', all.x = TRUE)
mrgdayofyear[is.na(mrgdayofyear)]<-0
# mrgdayofyear<-na.omit(mrgdayofyear)

write.csv(mrgdayofyear, file = 'mrgdayofyear.csv')

# need to plot new pocis overlaps

POCISdep<- read.csv(file = 'POCISDeployments.csv', header = TRUE)

POCISdep$dayofyear<-as.numeric(strftime(as.Date(POCISdep$Deployment, "%m/%d/%Y"), format = '%j'))
delPOCIS<-27
# ggplot(aes(dayofyear, POCIS), data = POCISdep, color = 'burlywood4' ) + geom_segment(aes(x=dayofyear, xend = dayofyear+delPOCIS, y=POCIS, yend=POCIS),size = 1.0, arrow = arrow(ends="both", type = "open"))+theme_bw()
ggplot(aes(dayofyear, POCIS), data = POCISdep, color = 'burlywood4' ) + geom_segment(aes(x=dayofyear, xend = dayofyear+delPOCIS, y=POCIS, yend=POCIS), size = 1.0)+theme_bw() + scale_y_continuous(limits = c(1,25), breaks = seq(1,25, by =1))


grid.newpage()
pushViewport(viewport(layout = grid.layout(2,1)))
vplayout<-function(x,y){viewport(layout.pos.row = x, layout.pos.col = y)}
  
a<-ggplot(aes(dayofyear, POCIS), data = POCISdep, color = 'burlywood4' ) + geom_segment(aes(x=dayofyear, xend = dayofyear+delPOCIS, y=POCIS, yend=POCIS), size = 1.0)+theme_bw() + scale_y_continuous(limits = c(1,25), breaks = seq(1,25, by =1)) +scale_x_continuous(limits = c(75,300))
b <- ggplot(aes(dayofyear, AVGAtzPPB.FUN1), data = rockGT3bar, color = 'burlywood4' ) + geom_bar(stat='identity')+theme_bw()+xlab('Day of Year') + ylab('Counts of Days Across Years GTE 3 ug/L Avg Daily Conc')+scale_x_continuous(limits = c(75,300))
print(a, vp=vplayout(1,1))
print(b, vp=vplayout(2,1))
#dev.off

# 28 day Plus add 14 and 56 day deployments

POCISdep<- read.csv(file = 'POCISDeployments.csv', header = TRUE)
POCISdep$dayofyear<-as.numeric(strftime(as.Date(POCISdep$Deployment, "%m/%d/%Y"), format = '%j'))
delPOCIS<-27


POCISdep14<- read.csv(file = 'POCISDeployments14.csv', header = TRUE)
POCISdep14$dayofyear<-as.numeric(strftime(as.Date(POCISdep14$Deployment, "%m/%d/%Y"), format = '%j'))
delPOCIS14<-13


POCISdep56<- read.csv(file = 'POCISDeployments56.csv', header = TRUE)
POCISdep56$dayofyear<-as.numeric(strftime(as.Date(POCISdep56$Deployment, "%m/%d/%Y"), format = '%j'))
delPOCIS56<-55


pdf("PrimeSecondaryTertiaryPOCIS.pdf", width = 8, height = 18)
grid.newpage()
pushViewport(viewport(layout = grid.layout(4,1)))
vplayout<-function(x,y){viewport(layout.pos.row = x, layout.pos.col = y)}

a<-ggplot(aes(dayofyear, POCIS), data = POCISdep, color = 'burlywood4' ) + geom_segment(aes(x=dayofyear, xend = dayofyear+delPOCIS, y=POCIS, yend=POCIS), size = 1.0)+theme_bw() + scale_y_continuous(limits = c(1,21), breaks = seq(1,25, by =1)) +scale_x_continuous(limits = c(75,300))+xlab('Day of Year')
b <- ggplot(aes(dayofyear, AVGAtzPPB.FUN1), data = rockGT3bar, color = 'burlywood4' ) + geom_bar(stat='identity')+theme_bw()+xlab('Day of Year') + ylab('Cnt Days GTE 3 ppb Avg Dly')+scale_x_continuous(limits = c(75,300))
c<-ggplot(aes(dayofyear, POCIS), data = POCISdep14, color = 'burlywood4' ) + geom_segment(aes(x=dayofyear, xend = dayofyear+delPOCIS14, y=POCIS, yend=POCIS), size = 1.0)+theme_bw() + scale_y_continuous(limits = c(1,15), breaks = seq(1,15, by =1))+scale_x_continuous(limits = c(75,300))+xlab('Day of Year')
d<-ggplot(aes(dayofyear, POCIS), data = POCISdep56, color = 'burlywood4' ) + geom_segment(aes(x=dayofyear, xend = dayofyear+delPOCIS56, y=POCIS, yend=POCIS), size = 1.0)+theme_bw() + scale_y_continuous(limits = c(1,11), breaks = seq(1,11, by =1))+scale_x_continuous(limits = c(75,300))+xlab('Day of Year')

print(a, vp=vplayout(1,1))
print(c, vp=vplayout(2,1))
print(d, vp=vplayout(3,1))
print(b, vp=vplayout(4,1))
dev.off()