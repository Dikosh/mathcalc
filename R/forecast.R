#' @title Predicts Stock Price Movement for Given Stock Symbol
#'
#' @description This package predicts whether the stock price at tommorow's market close would be higher or lower compared to today's closing place.
#'
#' @param symbol
#'
#' @return NULL
#'
#' @examples  stock_predict('AAPL')
#'
#' @export forecast_math

#' @import data.table,dplyr,data.table,randomForest,rpart,plotly,readr,readxl,xgboost,Matrix,caret,splitstackshape,stringr,lubridate


forecast_math <- function(df){
  
  df <- df[!is.na(df$phone),]
  aia <- df %>% group_by(phone) %>% summarise(nn = n())
  aia$nchar <- nchar(as.character(aia$phone))
  aia_tiny <- aia[aia$nchar==16 | aia$nchar==17,]
  aia_tiny$phone <-   str_replace_all(aia_tiny$phone, "[[:punct:]]", " ")
  aia_tiny$code <-  substring(aia_tiny$phone, 3, 3)
  aia_tiny <- aia_tiny[aia_tiny$code==7,]
  aia_tiny <- aia_tiny[,1]
  aia_tiny <- na.omit(aia_tiny)
  aia_tiny$phone <- as.factor(aia_tiny$phone)
  df <- df[!is.na(df$phone),]
  df <- inner_join(df,aia_tiny)
  phones <- df %>% group_by(phone) %>% summarise(nn=n())
  phones$class <- ifelse(phones$nn>=10,'Перекуп','Не перекуп')
  write.csv(phones,"dictionaries/phones.csv",row.names = F)
  phones <- read.csv("dictionaries/phones.csv")
  df <- inner_join(df,phones)
  df <- df[df$class!="Перекуп",]
  df$volume <-  as.numeric(sapply(strsplit(df$volume_dvigatel, " "), "[", 1))
  df$dvigatel <- sapply(strsplit(df$volume_dvigatel, " "), "[", 2)
  df$nn <- NULL
  df$data_dobavlenia_v_parser <-  ymd_hms(df$data_dobavlenia_v_parser)
  df$data_posl_obyavlenia <- ymd_hms(df$data_posl_obyavlenia)
  df$data_dobavlenia_v_parser <- as.Date(df$data_dobavlenia_v_parser)
  df$data_posl_obyavlenia <- as.Date(df$data_posl_obyavlenia)
  df <- df[df$data_posl_obyavlenia!="2020-01-01",]
  df$date_difference <- (as.integer(df$data_posl_obyavlenia -  df$data_dobavlenia_v_parser))
  ai <- df %>% group_by(City,Marka,Model,Year,phone) %>% summarise(schet = n())
  df <- inner_join(df,ai)
  gc()
  gc(reset = TRUE)
  df$opcii <- NULL
  df$text <- NULL
  sapply(df, function(x){sum(is.na(x))})
  unique_cars = df[df$schet==1,]
  not_unique_cars = df[df$schet!=1,]
  
  not_unique_cars <- not_unique_cars %>% mutate_if(is.character,as.factor)
  not_unique_cars <- not_unique_cars[!is.na(not_unique_cars$Model),]
  numbs <- not_unique_cars %>% group_by(City,Marka,Model,Year,phone) %>% summarise(SUM_DIF = sum(date_difference))# ZVEZDOCHKA
  maxs <- not_unique_cars %>% group_by(City,Marka,Model,Year,phone) %>% summarise(data_posl_obyavlenia = max(data_posl_obyavlenia),
                                                                                  dama = min(data_dobavlenia_v_parser))
  data <- inner_join(not_unique_cars,maxs)
  data <- inner_join(data,numbs)
  data$data_dobavlenia_v_parser <- data$dama
  data$dama <- NULL
  data$date_difference <- data$SUM_DIF
  data$SUM_DIF <- NULL
  daud <- rbind(unique_cars,data)
  daud <- daud[daud$date_difference<=quantile(data$date_difference,0.99),]
  daud$Max_date <- max(daud$data_posl_obyavlenia)
  daud$data_date <- as.integer(daud$Max_date-daud$data_posl_obyavlenia)
  #daud <- daud[daud$data_posl_obyavlenia<=daud$Max_date-30,]
  daud <- daud[!(daud$date_difference==7 & daud$kod_statusa==2),]
  daud$sale <- ifelse((daud$kod_statusa==4 & daud$data_date>30 ) | (daud$kod_statusa==2 & daud$data_date>30),'yes','no')
  days <- as.integer(max(daud$data_dobavlenia_v_parser)-min(daud$data_dobavlenia_v_parser))
  daud <- daud[daud$date_difference<=days+40,]
  
  yes <- daud#[daud$sale=='yes',]
  q <- yes %>% group_by(date_difference,sale) %>% summarise(n=n())
  #logic3 <- ggplotly(qplot(q$date_difference,q$n,color=q$sale,geom = 'line',xlab = "Без  7 дней + архив"))
  #Wsummary(q$date_difference)
  #logic3
  daud$data_date <- NULL
  daud$Max_date <- NULL
  daud$schet <- NULL
  
  daud <- as.data.frame(daud)
  
  return(daud)
}

