library(adjust)
library(data.table)
adjust.setup(user.token = 'f21v9869EoTq_u6Q5DLe')
gametokenlist<-read.csv('C:/Users/yejiawei/Desktop/adjustgametoken2.csv',
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
                               grouping=c('day','networks','campaigns'))
    lastrate<-adjust.cohorts(app.token=c(i),
                             start_date=start_date-1,
                             end_date=end_date-1,
                             grouping=c('day','networks','campaigns'))
    daily<-as.data.table(daily)
    lastrate<-as.data.table(lastrate)
    daily<-daily[,tracker_token:=NULL]
    daily$date<-as.Date(daily$date)
    lastrate<-lastrate[,tracker_token:=NULL]
    lastrate<-lastrate[period==1]
    lastrate<-lastrate[,period:=NULL]
    lastrate<-lastrate[,date:=as.Date(date)+1]
    filename<-paste('C:/Users/yejiawei/Desktop/adjust123/',gametokenlist[j,1],'.csv')
    dailyreport<-merge(daily,lastrate,by=c('date','network','campaign'),all=TRUE)
    
    frate<-adjust.cohorts(app.token=c(i),
                          start_date=start_date,
                          end_date=end_date,
                          grouping=c('day','networks','campaigns'),
                          kpis=c('retained_users'))
    
    frate<-frate[,tracker_token:=NULL]
    frate<-frate[period==1]
    frate<-frate[,period:=NULL]
    frate<-frate[,date:=as.Date(date)]
    frate<-frate[network%in%c('Facebook Installs','Off-Facebook Installs','Instagram Installs')]
    frate<-frate[,new:=sum(retained_users),by=c('date')]
    
    grate<-adjust.cohorts(app.token=c(i),
                          start_date=start_date,
                          end_date=end_date,
                          grouping=c('day','networks','campaigns'),
                          kpis=c('retained_users'))
    
    grate<-grate[,tracker_token:=NULL]
    grate<-grate[period==1]
    grate<-grate[,period:=NULL]
    grate<-grate[,date:=as.Date(date)]
    
    grate<-grate[network%in%c('adwords-display','adwords-search','Google Universal App Campaigns','Google (unknown)','Youtube installs')]
    grate<-grate[,new:=sum(retained_users),by=c('date')]
    
    fdaily<-daily[network%in%c('Facebook Installs','Off-Facebook Installs','Instagram Installs')]
    fdaily<-fdaily[,ren:=sum(installs),by=c('date')]
    
    gdaily<-daily[network%in%c('adwords-display','adwords-search','Google Universal App Campaigns','Google (unknown)','Youtube installs')]
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
    dailyreport2<-dailyreport2[!network%in%c('Facebook Installs','Off-Facebook Installs','Instagram Installs',
                                             'adwords-display','adwords-search','Google Universal App Campaigns',
                                             'Google (unknown)','Youtube installs')]
    
    if ((end_date-start_date)==1){
      dailyreport2<-dailyreport2[dailyreport2$date==end_date]
    }
    write.csv(dailyreport2,file=filename,row.names = FALSE)
    j<-j+1
  }
}
























































