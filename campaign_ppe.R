library(adjust)
Sys.setlocale("LC_ALL","English")
adjust.setup(user.token = 'f21v9869EoTq_u6Q5DLe')
gametokenlist<-read.csv(
  #'C:/Users/hefengqi/Desktop/adjustgametoken1.csv',
  '/Users/yeye/adjustgametoken.csv',
  header=TRUE)

report<-function(start_date=Sys.Date()-2,end_date=Sys.Date()-1){
  start_date<-as.Date(start_date)
  end_date<-as.Date(end_date)
  j<-1
  for(i in gametokenlist$token){
    daily<-adjust.deliverables(app.token=c(i),
                               start_date=start_date,
                               end_date=end_date,
                               kpis=c('clicks','installs','Revenue'),
                               os_names=as.character(gametokenlist[j,3]),
                               grouping=c('day','networks','campaign'))
    daily$date<-as.Date(daily$date,origin='1970-01-01')
    daily$tracker_token<-as.character(daily$tracker_token)
    daily<-daily[network%in%c('ironsource','appier','fyber','tapjoy')]
    daily<-daily[!campaign%in%c('unknown')]
    
    
    lastrate<-adjust.cohorts(app.token=c(i),
                             start_date=start_date,
                             end_date=end_date,
                             kpis=c('retained_users'),
                             os_names=as.character(gametokenlist[j,3]),
                             grouping=c('day','networks','campaign'))
    lastrate<-lastrate[period==1]
    lastrate<-lastrate[,period:=NULL]
    lastrate$date<-as.Date(lastrate$date, origin='1970-01-01')
    lastrate$tracker_token<-as.character(lastrate$tracker_token)
    lastrate<-lastrate[network%in%c('ironsource','appier','fyber','tapjoy')]
    lastrate<-lastrate[!campaign%in%c('unknown')]
    
    report<-merge(daily,lastrate,by=c('date','tracker_token','network','campaign'),all=TRUE,allow.cartesian=TRUE)
    report<-report[,tracker_token:=NULL]
    #filename<-paste('C:/Users/hefengqi/Desktop/adjust123/',gametokenlist[j,1],'.csv')
    
    filename<-paste('/Users/yeye/file_work/adjust123/campaign ppe',gametokenlist[j,1],'.csv')
    report1<-copy(report)
    #bb<-c('nz')  
    
    #report1[,country:=replace(country,which(country%in%(bb)),'au')]
    
    report1[is.na(report1)]<-0
    report1<-report1[,rate:=retained_users/installs]
    report1$rate<-format(report1$rate,digits = 4)
    write.csv(report1,file=filename,row.names = FALSE)
    j<-j+1
  }
}



















# daily<-adjust.deliverables(app.token=c('43qqulakhps0'),
#                                start_date='2016-10-16',
#                                end_date='2016-10-17',
#                                kpis=c('clicks','installs','Revenue'),
#                                grouping=c('day','trackers','countries'))
# 
# 
# lastrate<-adjust.cohorts(app.token=c('43qqulakhps0'),
#                          start_date='2016-10-16',
#                          end_date='2016-10-17',
#                          kpis=c('retained_users'),
#                          grouping=c('day','trackers','countries'))
# 
# #lastrate<-lastrate[,tracker_token:=NULL]
# lastrate<-lastrate[period==1]
# lastrate<-lastrate[,period:=NULL]
# 
# 
# report<-merge(daily,lastrate,by=c('date','tracker_token','country'),all=TRUE,allow.cartesian=TRUE)
# report<-report[,tracker_name.y:=NULL]
# filename<-paste('C:/Users/yejiawei/Desktop/adjust123/',gametokenlist[j,1],'.csv')
# report1<-copy(report)
# bb<-c('nz')
# facebook<-c('Facebook Installs','Off-Facebook Installs','Instagram Installs')
# report1[,country:=replace(country,which(country%in%(bb)),'au')]
# report1[,tracker_name.x:=replace(tracker_name.x,which(tracker_name.x%in%(facebook)),'Facebook Installs')]
# report1<-report1[,lapply(.SD,sum,na.rm=TRUE),by=list(date,country,tracker_name.x),.SDcols=c('clicks','installs','revenue','retained_users')]
# 
# 
# report1<-report1[,rate:=retained_users/installs]
# report1$rate<-format(report1$rate,digits = 4)
# #write.csv(report,file='C:/Users/yejiawei/Desktop/report.csv',row.names = FALSE)
# write.csv(report1,file='C:/Users/yejiawei/Desktop/report1.csv',row.names = FALSE)




























