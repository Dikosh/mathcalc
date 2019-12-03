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
#' @export ff_fast

#' @import data.table

ff_fast <- function(df){
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
  daud <- daud[!(daud$date_difference==7 & daud$kod_statusa==2),]
  daud$sale <- ifelse((daud$kod_statusa==4 & daud$data_date>30 ) | (daud$kod_statusa==2 & daud$data_date>30),'yes','no')
  days <- as.integer(max(daud$data_dobavlenia_v_parser)-min(daud$data_dobavlenia_v_parser))
  daud <- daud[daud$date_difference<=days+40,]
  
  daud$data_date <- NULL
  daud$Max_date <- NULL
  daud$schet <- NULL
  df <- daud
  df$cena
  str(df)
  df <- df[df$Year>=1990,]
  cena_min <- as.numeric(cofs[47,]$value)
  cena_max <- as.numeric(cofs[48,]$value)
  df <- df[df$cena<30000000,]
  df <- df[df$cena>cena_min,]
  group_1 <- df %>% group_by(Marka) %>% summarise(Marka_n=n()) %>% arrange(-Marka_n)
  group_1 <- group_1[1:round(nrow(group_1)*0.5),]
  write.csv(group_1,"dictionaries/group1.csv",row.names = F)
  group_1 <- read.csv("dictionaries/group1.csv")
  df <- inner_join(df,group_1)
  group_2 <- df %>% group_by(Marka,Model) %>% summarise(Marka_model_n=n()) %>% arrange(-Marka_model_n)
  group_2 <- group_2[group_2$Marka_model_n>=20,]
  write.csv(group_2,"dictionaries/group2.csv",row.names = F)
  df <- inner_join(df,group_2)
  df$phone <- NULL
  df <- unique(df)
  group_3 <- df %>% group_by(Marka,Model,Year) %>% summarise(Count = n(),
                                                             Mean = mean(cena),
                                                             Median = median(cena),
                                                             Third_quartile = quantile(cena,0.95),
                                                             First_quantile = quantile(cena,0.05),
                                                             Second_quantile = quantile(cena,0.2),
                                                             Third_quantile = quantile(cena,0.8))
  group_3$difficulty_classificator <- ifelse(group_3$Count<=8,"Impossible_fit","Fittable")
  write.csv(group_3,"dictionaries/group3.csv",row.names = F)
  df <- inner_join(df,group_3)
  df$anomaly_by_price <- ifelse(df$cena>=df$Third_quartile,"too_expensive",
                                ifelse(df$cena>=df$Third_quantile,"expensive",
                                       ifelse(df$cena<=df$First_quantile,"too_cheap",
                                              ifelse(df$cena<=df$Second_quantile,"cheap","normal"))))
  prop.table(table(df$anomaly_by_price))
  group_4 <- df %>% group_by(Marka,Model) %>% summarise(year_mean = mean(Year),
                                                        First_q_year = quantile(Year,0.05),
                                                        Second_q_year = quantile(Year,0.2),
                                                        Third_q_year = quantile(Year,0.8),
                                                        Fourth_q_year = quantile(Year,0.95))
  write.csv(group_4,"dictionaries/group_4.csv",row.names = F)
  df <- inner_join(df,group_4)
  df$anomaly_by_year <- ifelse(df$Year>=df$Fourth_q_year,"too_new",
                               ifelse(df$Year>=df$Third_q_year,"new",
                                      ifelse(df$Year<=df$Second_q_year,"old",
                                             ifelse(df$Year<=df$First_q_year,"too_old","new"))))
  
  prop.table(table(df$anomaly_by_year))
  hist(df$Year)
  df$Sell_classs <- ifelse(df$sale=='yes',"Продан","Неизвестно")
  df$sale <- NULL
  sales_table <- df %>% group_by(Marka,Model,Year) %>% filter(Sell_classs =="Продан") %>% summarise(Prodan = n(),
                                                                                                    Prodan_days = mean(date_difference),
                                                                                                    Prodan_3 = quantile(date_difference,0.9),
                                                                                                    Prodan_1 = quantile(date_difference,0.1),
                                                                                                    Prodan_cena = mean(cena))
  sales_table_3 <- df %>% group_by(Marka,Model,Year) %>%  summarise(Count = n(),
                                                                    days = mean(date_difference),
                                                                    q3 = quantile(date_difference,0.9),
                                                                    q1 = quantile(date_difference,0.1),
                                                                    cena = mean(cena))
  sales <- left_join(sales_table_3,sales_table)
  sales <- sales[sales$Count>13,]
  sales$predictable <- ifelse(sales$Count<=10,"cant predict","can predict")
  sales$Prodan_days <- as.integer(sales$Prodan_days)
  sales$Sales_days_group <- ifelse(sales$Prodan_days<10,"less than 10 days",
                                   ifelse(sales$Prodan_days<20,"less than 20 days",
                                          ifelse(sales$Prodan_days<30,"less than 30 days","more than 30 days")))
  sales_to_train <- select(sales,-c(Count,days,q3,q1,cena,Prodan))
  write.csv(sales_to_train,"BIG/SALES_TO_TRAIN.csv",row.names = F)
  write.csv(df,"BIG/df.csv",row.names = F)
  df <- fread("BIG/df.csv")
  colnames(df)
  table(df$City)
  df = df[df$City=="Алматы" | df$City=='Нур-Султан (Астана)' | df$City=="Шымкент",]
  train <- select(df,c(Marka,Model,Year,Tip_kuzova,KPP,privod,raspolozhenie_rulya,cvet,cena,volume,dvigatel,City,
                       Mean,Third_quartile,First_quantile,Second_quantile,Third_quantile,difficulty_classificator,anomaly_by_price,anomaly_by_year,
                       year_mean,First_q_year,Second_q_year,Third_q_year,Fourth_q_year,Marka_model_n,Marka_n,id_car_in_kolesa,probeg_gr))
  train <- train %>% mutate_if(is.character,as.factor)
  str(train)
  #sum(is.na(train$privod))
  
  
  train <- na.omit(train)
  testr <- train %>% group_by(Marka,Model,Year) %>% summarise(count = n())
  testr <- testr[testr$count>5,]
  testr$count <- NULL
  testr <- unique(testr)
  write.csv(testr,"dictionaries/group5.csv",row.names = F)
  train <- inner_join(train,testr)
  train <- unique(train)
  train <- train %>% mutate_if(is.character,as.factor)
  str(train)
  write.csv(select(train,id_car_in_kolesa),"mini_id_checker.csv",row.names = F)
  sales_to_train <- read.csv("BIG/SALES_TO_TRAIN.csv")
  train <- inner_join(train,sales_to_train)
  train <- na.omit(train)
  checkers <- select(df,c(id_car_in_kolesa,date_difference,views,kod_statusa))
  #train$City <- NULL
  #train <- unique(train)
  fwrite(checkers,"BIG/checkers.csv",row.names = F)
  write.csv(train,"BIG/TRAIN.CSV",row.names = F)
  additional_data <- df %>% 
    group_by(Marka,Model,Year,KPP,Tip_kuzova,raspolozhenie_rulya,volume,dvigatel) %>% 
    summarise(Count = n())
  additional_data1 <- df %>% filter(Sell_classs=="Продан") %>% 
    group_by(Marka,Model,Year,KPP,Tip_kuzova,raspolozhenie_rulya,volume,dvigatel) %>% 
    summarise(Sales = n()) 
  train <- fread("BIG/TRAIN.CSV",stringsAsFactors = T)
  IDs <- select(train,id_car_in_kolesa)
  train$id_car_in_kolesa <- NULL
  index <- createDataPartition(train$cena,p=0.7,list = F)
  tr <- train[index,]
  ts <- train[-index,]
  
  dtrain <- data.matrix(select(tr,-c(cena)))
  ctest <- data.matrix(select(ts,-c(cena)))
  
  dtrain <- xgb.DMatrix(data=dtrain,label=tr$cena)
  ctest <- xgb.DMatrix(data=ctest,label=ts$cena)
  dfin <- data.matrix(select(train,-c(cena)))
  dfin <- xgb.DMatrix(data=dfin,label=train$cena)
  colnames(tr)
  bst <- xgboost(data = dfin,
                 objective='reg:linear',
                 nrounds = 25,
                 #  nfold = 5,
                 metrics = "rmse",
                 maximize = FALSE,
                 verbose = 3,
                 #early_stopping_rounds = 30,
                 max_depth=7)
  pred <- predict(bst,ctest)
  mean(ts$cena-pred)
  
  
  ts$prognoz <- pred
  
  summary(ts$cena-ts$prognoz)
  
  ts$error <- (ts$cena-ts$prognoz)/(ts$cena+ts$prognoz)
  ts$diff <- ts$cena - ts$prognoz
  summary(ts$error)
  ts$prognoz <- abs(ts$prognoz)
  #importance <- xgb.importance(feature_names = colnames(select(tr,-c(cena))), model = bst)
  #print(xgb.plot.importance(importance_matrix = importance, top_n = 15))
  xgb.save(bst, 'xgb.model')
  bst <- xgb.load('xgb.model')
  
  Ids_test <- IDs[-index,]
  ts$id_car_in_kolesa <- Ids_test
  checkers <- fread("BIG/checkers.csv")
  tsss <- inner_join(ts,checkers)
  tsss$flag1 <-  ifelse(tsss$prognoz<tsss$cena,"predicted less than fact","predicted is greater")
  mean_views <- tsss %>% group_by(Marka,Model,Year) %>% summarise(mean_date_difference = mean(date_difference),
                                                                  count = n())
  mean_views
  
  tsss <- inner_join(tsss,mean_views)
  
  tsss$flag2 <- ifelse(tsss$mean_date_difference<=tsss$date_difference,"passive","active")
  #tsss$flag3 <- ifelse(tsss$mean_views<tsss$views,"interesting","less interesting")
  
  tsss$Recommended_buying_price <- ifelse(tsss$prognoz<=1000000,tsss$prognoz - 0.18*tsss$prognoz,
                                          ifelse(tsss$prognoz<=2500000,tsss$prognoz - 0.15*tsss$prognoz,
                                                 ifelse(tsss$prognoz<=4500000,tsss$prognoz - 0.12*tsss$prognoz,
                                                        ifelse(tsss$prognoz<=6000000,tsss$prognoz - 0.12*tsss$prognoz,
                                                               ifelse(tsss$prognoz<=8000000,tsss$prognoz - 0.1*tsss$prognoz,
                                                                      ifelse(tsss$prognoz<=10000000,tsss$prognoz - 0.1*tsss$prognoz,
                                                                             ifelse(tsss$prognoz<=15000000,tsss$prognoz - 0.1*tsss$prognoz,
                                                                                    ifelse(tsss$prognoz>15000000,tsss$prognoz - 0.8*tsss$prognoz,"NAN"))))))))
  
  tsss$Max_buying_price <- ifelse(tsss$prognoz<=1000000,tsss$prognoz - 120000,
                                  ifelse(tsss$prognoz<=2500000,tsss$prognoz - 200000,
                                         ifelse(tsss$prognoz<=4500000,tsss$prognoz - 350000,
                                                ifelse(tsss$prognoz<=6000000,tsss$prognoz - 450000,
                                                       ifelse(tsss$prognoz<=8000000,tsss$prognoz - 550000,
                                                              ifelse(tsss$prognoz<=10000000,tsss$prognoz - 650000,
                                                                     ifelse(tsss$prognoz<=15000000,tsss$prognoz - 900000,
                                                                            ifelse(tsss$prognoz>15000000,tsss$prognoz - 1200000,"NAN"))))))))
  
  
  
  Final <- tsss
  #Final$CLASSS <- ifelse(Final$flag1=="predicted is greater" & tsss$flag2=="active","Predicted greater and active","NAN")
  index <- createDataPartition(train$cena,p=0.7,list = F)
  tr <- train[index,-c(30:34)]
  ts <- train[-index,-c(30:34)]
  colnames(tr)
  str(tr)
  dtrain <- data.matrix(select(tr,-c(Prodan_days)))
  ctest <- data.matrix(select(ts,-c(Prodan_days)))
  train_labs <- tr$Prodan_days
  val_labs <- ts$Prodan_days
  dtrain <- xgb.DMatrix(data=dtrain,label=train_labs)
  ctest <- xgb.DMatrix(data=ctest,label=val_labs)
  
  xgb <- xgboost(data = dtrain,
                 objective='reg:linear',
                 nrounds = 25,
                 #  nfold = 5,
                 metrics = "rmse",
                 maximize = FALSE,
                 verbose = 1,
                 #early_stopping_rounds = 30,
                 max_depth=7)
  
  xgb.save(xgb, 'xgb_liklik.model')
  damn <- Final[,colnames(tr)]
  ctest_damn <- data.matrix(select(damn,-c(Prodan_days)))
  val_labs_damn <- (damn$Prodan_days) 
  ctest <- xgb.DMatrix(data=ctest_damn,label=val_labs_damn)
  xgb_model <- xgb
  xgb_val_preds <- predict(xgb_model, newdata = ctest)
  Final$Sales_days_group_pred <- predict(xgb,ctest)
  boxplot(Final$Sales_days_group_pred)
  #qplot(Final$Sales_days_group_pred,Final$Prodan_days)
  dfd <- fread("BIG/TRAIN.CSV",stringsAsFactors = T)
  train <- dfd
  IDs <- select(train,id_car_in_kolesa)
  train$id_car_in_kolesa <- NULL
  dtrain <- data.matrix(select(train,-c(cena)))
  dtrain <- xgb.DMatrix(data=dtrain,label=train$cena)
  bst <- xgb.load("xgb.model")
  train$prognoz <- predict(bst,dtrain)
  train$error <- (train$cena-train$prognoz)/(train$cena+train$prognoz)
  train$diff <- train$cena - train$prognoz
  train$prognoz <- abs(train$prognoz)
  summary(train$error)
  train <- dfd
  IDs <- select(train,id_car_in_kolesa)
  rm(dfd)
  man <- as.data.frame(train)[,colnames(tr)]
  man <- data.matrix(man[,-29])
  man <- xgb.DMatrix(data=man,label=train$Prodan_days)
  train$Sales_days_group_pred <- predict(xgb,man)
  train$prognoz <- predict(bst,dtrain)
  train$error <- (train$cena-train$prognoz)/(train$cena+train$prognoz)
  train$diff <- train$cena - train$prognoz
  train$prognoz <- abs(train$prognoz)
  colnames(train)
  #qplot(train$Prodan_days,train$Sales_days_group_pred)
  checkers <- fread("BIG/checkers.csv")
  train$id_car_in_kolesa <- IDs$id_car_in_kolesa
  train <- inner_join(train,checkers)
  train$flag1 <-  ifelse(train$prognoz<train$cena,"predicted less than fact","predicted is greater")
  
  mean_views <- train %>% group_by(Marka,Model,Year) %>% summarise(mean_date_difference = mean(date_difference),
                                                                   count = n())
  train <- inner_join(train,mean_views)
  train$flag2 <- ifelse(train$mean_date_difference<=train$date_difference,"passive","active")
  train$Recommended_buying_price <- ifelse(train$prognoz<=1000000,train$prognoz - 0.18*train$prognoz,
                                           ifelse(train$prognoz<=2500000,train$prognoz - 0.15*train$prognoz,
                                                  ifelse(train$prognoz<=4500000,train$prognoz - 0.12*train$prognoz,
                                                         ifelse(train$prognoz<=6000000,train$prognoz - 0.12*train$prognoz,
                                                                ifelse(train$prognoz<=8000000,train$prognoz - 0.1*train$prognoz,
                                                                       ifelse(train$prognoz<=10000000,train$prognoz - 0.1*train$prognoz,
                                                                              ifelse(train$prognoz<=15000000,train$prognoz - 0.1*train$prognoz,
                                                                                     ifelse(train$prognoz>15000000,train$prognoz - 0.08*train$prognoz,"NAN"))))))))
  
  train$Max_buying_price <- ifelse(train$prognoz<=1000000,train$prognoz - 120000,
                                   ifelse(train$prognoz<=2500000,train$prognoz - 200000,
                                          ifelse(train$prognoz<=4500000,train$prognoz - 350000,
                                                 ifelse(train$prognoz<=6000000,train$prognoz - 450000,
                                                        ifelse(train$prognoz<=8000000,train$prognoz - 550000,
                                                               ifelse(train$prognoz<=10000000,train$prognoz - 650000,
                                                                      ifelse(train$prognoz<=15000000,train$prognoz - 900000,
                                                                             ifelse(train$prognoz>15000000,train$prognoz - 1200000,"NAN"))))))))
  
  fwrite(train,"BIG/PREDICTED_VALUES.csv",row.names = F)
  

}