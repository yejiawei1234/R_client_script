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
                               grouping=c('day','networks','campaigns'))
    
    lastrate<-adjust.cohorts(app.token=c(i),
                             start_date=start_date-1,
                             end_date=end_date-1,
                             os_names=as.character(gametokenlist[j,3]),
                             grouping=c('day','networks','campaigns'))
    daily<-as.data.table(daily)
    lastrate<-as.data.table(lastrate)
    daily<-daily[,tracker_token:=NULL]
    daily$date<-as.Date(daily$date)
    lastrate<-lastrate[,tracker_token:=NULL]
    lastrate<-lastrate[period==1]
    lastrate<-lastrate[,period:=NULL]
    lastrate<-lastrate[,date:=as.Date(date)+1]
    filename<-paste('/Users/yeye/file_work/adjust123/campaign',gametokenlist[j,1],'.csv')
    dailyreport<-merge(daily,lastrate,by=c('date','network','campaign'),all=TRUE)
    
    frate<-adjust.cohorts(app.token=c(i),
                          start_date=start_date,
                          end_date=end_date,
                          grouping=c('day','network','campaigns'),
                          os_names=as.character(gametokenlist[j,3]),
                          kpis=c('retained_users'))
    
    frate<-frate[,tracker_token:=NULL]
    frate<-frate[period==1]
    frate<-frate[,period:=NULL]
    frate<-frate[,date:=as.Date(date)]
    frate<-frate[network%in%c('Facebook Installs','Off-Facebook Installs','Instagram Installs')]
    frate<-frate[,new:=sum(retained_users),by=c('date')]
    grate<-frate[network%in%c('adwords-display','adwords-search','Google Universal App Campaigns','Google (unknown)')]
    grate<-grate[,new:=sum(retained_users),by=c('date')]
    
    fdaily<-daily[network%in%c('Facebook Installs','Off-Facebook Installs','Instagram Installs')]
    fdaily<-fdaily[,ren:=sum(installs),by=c('date')]
    
    gdaily<-daily[network%in%c('adwords-display','adwords-search','Google Universal App Campaigns','Google (unknown)')]
    gdaily<-gdaily[,ren:=sum(installs),by=c('date')]
    
    freport<-merge(fdaily,frate,by=c('date','network','campaign'),all=TRUE)
    freport<-freport[,frate:=new/ren,by=c('date')]
    freport[is.na(freport)]<-0
    freport<-freport[,ren:=NULL]
    freport<-freport[,new:=NULL]
    freport<-freport[,c('date','network','campaign','frate'),with=FALSE]
    freport<-freport[,date:=as.Date(date)+1]
    
    greport<-merge(gdaily,grate,by=c('date','network','campaign'),all=TRUE)
    greport<-greport[,grate:=new/ren,by=c('date')]
    greport<-greport[,ren:=NULL]
    greport<-greport[,new:=NULL]
    greport<-greport[,c('date','network','campaign','grate'),with=FALSE]
    greport<-greport[,date:=as.Date(date)+1]
    
    
    
    
    dailyreport1<-merge(dailyreport,freport,by=c('date','network','campaign'),all=TRUE)
    dailyreport2<-merge(dailyreport1,greport,by=c('date','network','campaign'),all=TRUE)
    dailyreport2[is.na(dailyreport2)]<-0
    dailyreport2$frate<-format(dailyreport2$frate,digits = 4)
    dailyreport2$grate<-format(dailyreport2$grate,digits = 4)
    
    dailyreport2[is.na(dailyreport2)]<-0
    if ((end_date-start_date)==1){
      dailyreport1<-dailyreport2[dailyreport2$date==end_date]
    }
    write.csv(dailyreport2,file=filename,row.names = FALSE)
    j<-j+1
  }
}