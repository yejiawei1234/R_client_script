library(adjust)
adjust.setup(user.token = 'f21v9869EoTq_u6Q5DLe')
# daily<-adjust.deliverables(app.token=c('mtx0qehuterk'),
#                            start_date='2018-05-01',
#                            end_date='2018-05-21',
#                            kpis=c('installs'),
#                            os_names='ios',
# 
#                            grouping=c('day','networks','countries'))
# daily<-daily[network%in%c('Organic')]
# daily<-daily[country%in%c('us')]

# cost<-adjust.deliverables(app.token=c('bzbiackq7d34'),
#                           start_date='2017-07-28',
#                           end_date='2017-07-30',
#                           event_kpis='z97z3n_events',
#                           kpis=c('clicks','installs','Revenue'),
#                           grouping=c('trackers'))

# ll<-adjust.cohorts(app.token=c('mtx0qehuterk'),
#                    start_date='2018-05-01',
#                    end_date='2018-05-21',
#                    period='day',
#                    kpis=c('retained_users','cohort_size','revenue','revenue_total'),
#                    grouping=c('day','networks'))
# ll<-ll[network%in%c('Unity')]
# ll2<-copy(ll)
# ll2<-ll2[period==7]
# 
# cost<-adjust.deliverables(app.token=c('mtx0qehuterk'),
#                           start_date='2018-05-01',
#                           end_date='2018-05-21',
#                           kpis=c('installs','cost','return_on_investment'),
#                           grouping=c('day','networks'))
# cost<-cost[network%in%c('Unity')]
# 
# report<-merge(ll2,cost,by=c('date','tracker_token','network'),all=TRUE)
# report<-report[,sevendaysROI:=(revenue_total-cost)/cost]
# 
# report2<-report[,c('date','tracker_token','network','installs','cost','sevendaysROI'),with=FALSE]
# event<-adjust.events(app.token=c('fd661x98u1og'),
#                      start_date='2017-07-18',
#                      end_date='2017-09-28',
#                      kpis=c('clicks','installs','Revenue'),
#                      event_kpis='all_revenue|events|revenue_per_event',
#                      grouping=c('day','networks'))

last<-adjust.cohorts(app.token=c('8759i8o42mio'),
                     start_date='2018-05-01',
                     end_date='2018-05-31',
                     kpis=c('cohort_size','revenue','revenue_total'),
                     period='month',
                     os_names='ios',
                     grouping=c('network','countries'))


last1<-copy(last)
#last1<-last1[network%in%c('Off-Facebook Installs','Instagram Installs','Facebook Installs','Facebook Messenger Installs')]
#last1<-last1[network%in%c('Off-Facebook Installs','Instagram Installs','Facebook Installs','Facebook Messenger Installs',
#                          'Adwords UAC Installs','Adwords (unknown)','Google Organic Search')]

last1<-last1[country%in%c('au','ph')]
last1<-last1[,tracker_token:=NULL]
last1<-last1[period%in%c(0)]

last1[network=='Google Organic Search',network:='Adwords']
last1[network=='Adwords (unknown)',network:='Adwords']
last1[network=='Adwords UAC Installs',network:='Adwords']
last1[network=='Facebook Installs',network:='facebook']
last1[network=='Instagram Installs',network:='facebook']
last1[network=='Off-Facebook Installs',network:='facebook']
last1[network=='Facebook Messenger Installs',network:='facebook']
last2<-last1[,lapply(.SD, sum, na.rm=TRUE),by=c('network','country','period'),.SDcols=c("cohort_size", "revenue", "revenue_total")]

last2<-last2[network%in%c('facebook','Adwords','Snapchat Installs','Chartboost','unity')]

write.csv(last2,file='/Users/yeye/Desktop/ff_ios.csv',row.names = FALSE)


