library(adjust)
adjust.setup(user.token = 'f21v9869EoTq_u6Q5DLe')
gametokenlist<-read.csv('/Users/yeye/adjustgametoken.csv',
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
                               utc_offset='+03:00',
                               grouping=c('day','trackers'))
    
    lastrate<-adjust.cohorts(app.token=c(i),
                             start_date=start_date,
                             end_date=end_date,
                             os_names=as.character(gametokenlist[j,3]),
                             utc_offset='+03:00',
                             grouping=c('day','trackers'))
    daily<-as.data.table(daily)
    lastrate<-as.data.table(lastrate)
    daily<-daily[,tracker_token:=NULL]
    daily$date<-as.Date(daily$date)
    lastrate<-lastrate[,tracker_token:=NULL]
    lastrate<-lastrate[period==1]
    lastrate<-lastrate[,period:=NULL]
    lastrate<-lastrate[,date:=as.Date(date,origin="1970-01-01")+1]
    filename<-paste('/Users/yeye/file_work/adjust123/',gametokenlist[j,1],'.csv')
    lastrate$tracker_name=as.character(lastrate$tracker_name)
    dailyreport<-merge(daily,lastrate,by=c('date','tracker_name'),all=TRUE)
  
    
    network_i_do_not_need<-c('Facebook Installs','Off-Facebook Installs','Instagram Installs','Adwords UAC Installs','Adwords (unknown)',
                      'Facebook Messenger Installs')
    
    dailyreport<-dailyreport[!tracker_name%in%network_i_do_not_need]
    dailyreport[is.na(dailyreport)]<-0
    write.csv(dailyreport,file=filename,row.names = FALSE)
    j<-j+1
  }
}




















