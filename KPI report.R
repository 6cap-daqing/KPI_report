
df <- read.csv('C:\\Daqing\\RB\\KPI_report_test\\testdata.csv')

# column names
# prameter setting
session_sart_time <- c('13:30','18:00') # format 24-hour hour:minute
window_duration <- 30 # number of minutes
n_windows <- 8 # number of windows of each session

# session_start_time <- strftime(strptime(sart_time,'%H:%M'),'%H:%M')
# session_start_time <- strptime(sart_time,'%H:%M')
# session_end_time <- session_start_time+n_windows*window_length*60
# window_duration <-  strftime(strptime(window_length,'%M'),'%M')

login_id <- df[,c('LOGIN')]
trader_id <- unique(login_id)
n_traders <- length(unique(trader_id))

window_session <- function(sdf,session_start,l_windows,n_windows)
{
  w_record <- mat.or.vec(1,n_windows)
  window_sart_time <- strptime(paste(t_date,session_sart_time),'%Y-%m-%d %H:%M')
}

date_session <- function(date_df,t_date,session_sart_time,l_windows,n_windows)
{
  date_session_sart_time <- strptime(paste(t_date,session_sart_time),'%Y-%m-%d %H:%M')
  date_session_end_time <- date_session_sart_time+n_windows*l_windows*60
  ld <- length(date_session_sart_time)
  date_session_traded <- mat.or.vec(1,ld)
  date_session_profit <- mat.or.vec(1,ld)
  session_idx <- data.frame(aname=NA, bname=NA)[numeric(0), ]
  for (j in 1:nrow(date_df))
  {
    t_time <- strptime(date_df[,c('OPEN_TIME')][j],'%Y-%m-%d %H:%M:%S')
    t <- 1
    a <- 0
    while (t & a < ld)
    {
      a <- a+1
      if (as.double(difftime(date_session_sart_time[a], t_time)) <=0  & as.double(difftime(date_session_end_time[a], t_time)) >=0 )
      {
        t <- 0
        session_idx <- rbind(session_idx,a)
      }
    }
    if (t == 1)
    {
      session_idx <- rbind(session_idx,0)
    }
    date_session_traded[a] <- date_session_traded[a]+1
    date_session_profit[a] <- date_session_profit[a]+date_df[,c('PROFIT')][j]
    
    colnames(session_idx) <- 'session_idx'
    date_df <- cbind(date_df,session_idx)
    session_record <- mat.or.vec(ld,4) # column names: #trade, #win, PL,follow_plan_idx 
    for (s in 1:ld)
    {
      sdf <- subset(date_df,date_df[,c('session_idx')]==s)
      session_record[s,] <- window_session(sdf,session_sart_time[s],l_windows,n_windows)
    }
    date_session_record <- rbind(date_session_traded,date_session_profit)
  }
  return(date_session_record)
}

total_session <- function(tdf,session_sart_time,l_windows,n_windows)
{
  trading_date <- as.Date(strptime(tdf[,c('OPEN_TIME')], '%Y-%m-%d'))
  
}
for (k in 1:n_traders)
{
  tdf <- subset(df,df[,c('LOGIN')]==trader_id[k])
  tdf <- tdf[order(tdf$OPEN_TIME),]
  trading_date <- unique(as.Date(strptime(tdf[,c('OPEN_TIME')], '%Y-%m-%d')))
  session_traded <- mat.or.vec(length(trading_date),length(session_sart_time))
  session_profit <- mat.or.vec(length(trading_date),length(session_sart_time))
  for (j in 1:length(trading_date))
  {
    date_df <- subset(tdf,as.Date(strptime(tdf[,c('OPEN_TIME')], '%Y-%m-%d'))==trading_date[j])
    session_record <- date_session(date_df,trading_date[j],session_sart_time,window_duration,n_windows)
    session_traded[j,] <- session_record[1,]
    session_profit[j,] <- session_record[2,]
  }
  
#   trader_KPI1 <- do.call(data.frame, 
#                         list(
#                           total_trading = length(tdf[,c('LOGIN')])
#                           # total_trading = apply(tdf['LOGIN'], 2, length)
#                         ))
}
trading_date <- as.Date(strptime(df[,c('OPEN_TIME')], '%Y-%m-%d'))

open_time <- strptime(df[,c('OPEN_TIME')], '%Y-%m-%d %H:%M:%S')
close_time <- strptime(df[,c('CLOSE_TIME')], '%Y-%m-%d %H:%M:%S')

holding_time <- as.numeric(close_time-open_time)

d <- strptime(df[,c('OPEN_TIME')], '%Y-%m-%d %H:%M:%S')
sort_tdf <- tdf[order(tdf$OPEN_TIME),]