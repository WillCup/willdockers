#用户行为异常度p_value
user_action_surprise<-function(uservector,latestdate=NULL,check_date){
  # check_date<- "2016-12-10"
  library(data.table)
  source('mysql_settings.R')
  library(RMySQL)
  drv <- MySQL()
  conn<- dbConnect(drv, user=mysql_setting[1], password=mysql_setting[2],dbname=mysql_setting[3],
                   host=mysql_setting[4], port=as.integer(mysql_setting[5]))
  #source('~/RStudio/(fun)mysqlconn.R', echo=TRUE)
  source('(fun)user_premium_predict.R')

  # user_data<-user_premium_predict(uservector = uservector,forecast_step = 1,latestdate = latestdate)
  user_data<-user_premium_predict(doplot=T,path="F:", uservector = uservector,forecast_step = 1,latestdate = latestdate)
  pred<-user_data$result
  user_pred<-as.data.table(pred[,c(1:10,13)])
  setkey(user_pred,userid)
  
  z_invest<-paste0("select userid,
                           sum(amount) as invest_amount 
                   from invest_record where date='",as.Date(check_date),"' 
                   group by userid")
  actual_invest<-as.data.table(dbGetQuery(conn,z_invest))
  setkey(actual_invest,userid)
  
  z_redeem<-paste0("select userid,
                           sum(amount) as redeem_amount 
                   from redeem_record where date='",as.Date(check_date),"' 
                   group by userid")
  actual_redeem<-as.data.table(dbGetQuery(conn,z_redeem))
  setkey(actual_redeem,userid)
  
  actual_action<-actual_invest[user_pred,nomatch=NA,mult="all"]
  actual_action<-actual_redeem[actual_action,nomatch=NA,mult="all"] 
  actual_action[is.na(invest_amount),invest_amount:=0]
  actual_action[is.na(redeem_amount),redeem_amount:=0]  
  
  actual_action[,h1_premium:=ifelse((actual_base+invest_amount-redeem_amount)<=0,0.01,(actual_base+invest_amount-redeem_amount))]
  actual_action[,Inv_h1_premium:=ifelse(lambda==0,log(h1_premium),(h1_premium^lambda-1)/lambda)]
  actual_action[,p_value:=2*(1-pnorm(abs(Inv_h1_premium-Inv_mean)/ifelse(Inv_se==0,0.000001,Inv_se)))]
  dbDisconnect(conn)
  return(actual_action)
}
