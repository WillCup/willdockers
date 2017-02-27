user_action<-function(uservector,reg_latestdate=NULL,latestdate=NULL){
  #uservector <- c("000016c3098f47068c9c75438a714997", "00005479a9fb45968da87ff80f62db0a")
  #reg_latestdate<-NULL
  #latestdate<-NULL
  library(data.table)
  library(plyr)
  library(varhandle)
  library(DBI)
#  source('~/RStudio/(fun)mysqlconn.R')
  source('(fun)stringbind.R')
#  conn<-mysqlconn("mysql_settings.csv")
  #library(RODBC)
  #conn<-odbcConnect("MySQL",uid="root",pwd="***",DBMSencoding = "UTF-8")
  source('mysql_settings.R')
  library(RMySQL)
  drv <- MySQL()
  conn<- dbConnect(drv, user=mysql_setting[1], password=mysql_setting[2],dbname=mysql_setting[3],
                   host=mysql_setting[4], port=as.integer(mysql_setting[5]))
  # dbSendQuery(conn,'SET NAMES utf8')
  # dbSendQuery(conn,'SET NAMES gbk')
  # yink = dbGetQuery(conn, "SELECT * FROM yinker.user_info limit 100")
  # View(yink)
  # dbDisconnect(conn)
  
  userstring_0<-stringbind(uservector)
  
  #收集用户基础信息
  aa<-ifelse(is.null(reg_latestdate),"",paste0(" and reg_time<='",as.Date(reg_latestdate),"' "))
  
  z0<-paste0("select	userid,
		                  date(reg_time) as reg_day,
                      date(invest1st_time) as invest1st_day,
                      gender,
                      birthday,
                      province,
                      channel
              from user_info where userid in (",userstring_0,")" ,aa," order by userid")
  user_info<-as.data.table(dbGetQuery(conn,z0))
  user_info$reg_day <- as.Date(user_info$reg_day)
  
  print("user_info query ok!")
  
  type<-ifelse(uservector %in% user_info$userid,1,0)        # 1->正常投资用户，有数据 ； 0->无数据用户
  user_type<-data.frame(userid=uservector,type,stringsAsFactors = F)
  # normal_users<-unfactor(user_info$userid)
  normal_users <- user_info$userid
  userstring <- stringbind(user_info$userid)
  # userstring<-stringbind(normal_users)                      #选取正常投资用户
  
  
  #收集用户投资、赎回、定期信息
  bb<-ifelse(is.null(latestdate),"",paste0(" and date<='",as.Date(latestdate),"' "))

  # 投资信息
  z_invest<-paste0("select userid,date,sum(amount) as invest_amount from invest_record 
                   where userid in (",userstring,")",bb," group by userid,date order by userid,date")
  o_invest<-as.data.table(dbGetQuery(conn,z_invest))
  o_invest$date <- as.Date(o_invest$date)
  print("user invest_record query ok!")

  # 赎回信息
  z_redeem<-paste0("select userid,date,sum(amount) as redeem_amount from redeem_record 
                   where userid in (",userstring,")",bb," group by userid,date order by userid,date")
  o_redeem<-as.data.table(dbGetQuery(conn,z_redeem))
  o_redeem$date <- as.Date(o_redeem$date)
  print("user redeem_record query ok!")
  
  # 定期信息
  z_regular<-paste0("select userid,date,num,sum(amount) as regular_amount from regular_info 
                    where userid in (",userstring,")",bb," group by userid,date,num 
                    order by userid,date,num")
  o_regular<-as.data.table(dbGetQuery(conn,z_regular))
  o_regular$date <- as.Date(o_regular$date)
  print("user regular_info query ok!")
  
  latestdate<-as.Date(max(o_invest$date,o_redeem$date))

  #汇总用户各行为信息，并计算用户存量信息
  num<-length(normal_users)
  user_info<-data.table(user_info,id=seq(from=3,length.out = num))         #增加用户列表的所在序号
  # user_info<-data.table(user_info,id=seq(from=3))
  user_record<-list(user_type,user_info)
  user_regular_record<-list(user_type,user_info)
  for (i in 1:num) {
    # i <- 1
    timeline<-seq.Date(from=as.Date(user_info[userid==normal_users[i],]$reg_day, "%Y-%m-%d"),to=latestdate,by="day")
    timeline<-as.data.table(timeline)
    colnames(timeline)<-c("date")
    # 复制timeline相同行数的userid
    user<-rep(normal_users[i],dim(timeline)[1]) 
    user_invest_data<-o_invest[userid==normal_users[i],]
    user_redeem_data<-o_redeem[userid==normal_users[i],]
    user_regular_data<-o_regular[userid==normal_users[i],]
    setkey(user_invest_data,date)
    setkey(user_redeem_data,date)
    # data.table join
    user_invest_data_all<-user_invest_data[timeline,nomatch=NA,mult="all"]
    user_redeem_data_all<-user_redeem_data[timeline,nomatch=NA,mult="all"]
    # remove userid column
    user_invest_data_all[,userid:=NULL]
    user_invest_data_all[is.na(invest_amount),invest_amount:=0]
    user_redeem_data_all[is.na(redeem_amount),redeem_amount:=0]
    premium_add<-user_invest_data_all$invest_amount-user_redeem_data_all$redeem_amount
    premium<-cumsum(premium_add)
    
    #产生存量、投资、赎回行为数据
    temp<-data.table(user,user_invest_data_all,redeem_amount=user_redeem_data_all$redeem_amount,premium)
    #累计用户数据
    user_record<-c(user_record,list(temp))
    user_regular_record<-c(user_regular_record,list(user_regular_data))
    print(paste(i,"/",num,"action data done"))
  }

  
  action_data<-list(user_record,user_regular_record) 
  dbDisconnect(conn)
  return(action_data)
  
  
}
