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
  df <- df[df$cena<30000000,]
  df <- df[df$cena>900000,]
  group_1 <- df %>% group_by(Marka) %>% summarise(Marka_n=n()) %>% arrange(-Marka_n)
  group_1 <- group_1[1:round(nrow(group_1)*0.6),]
  write.csv(group_1,"dictionaries/group1.csv",row.names = F)
  group_1 <- read.csv("dictionaries/group1.csv")
  df <- inner_join(df,group_1)
  group_2 <- df %>% group_by(Marka,Model) %>% summarise(Marka_model_n=n()) %>% arrange(-Marka_model_n)
  group_2 <- group_2[group_2$Marka_model_n>=15,]
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
  group_3$difficulty_classificator <- ifelse(group_3$Count<=3,"Impossible_fit","Fittable")
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
  sales <- sales[sales$Count>10,]
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
  checkers <- fread("checkers.csv")
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
  qplot(Final$Sales_days_group_pred,Final$Prodan_days)
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
  qplot(train$Prodan_days,train$Sales_days_group_pred)
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
  
  
  
  df <- fread("BIG/df.csv")
  Final <- fread("BIG/PREDICTED_VALUES.csv",stringsAsFactors = T)
  coefss <- read_excel("COEF.xlsx")
  coefss$value <- as.numeric(coefss$value)
  
  coef1 <- 0.3 # count
  coef2 <- 0.7 # sales
  coef12 <- coefss[coefss$colname=='coef12',]$value
  coef22 <- coefss[coefss$colname=='coef22',]$value
  
  coef13 <- coefss[coefss$colname=='coef13',]$value
  coef23 <- coefss[coefss$colname=='coef23',]$value
  
  coef14 <- coefss[coefss$colname=='coef14',]$value
  coef24 <- coefss[coefss$colname=='coef24',]$value
  
  coef15 <- coefss[coefss$colname=='coef15',]$value
  coef25 <- coefss[coefss$colname=='coef25',]$value
  
  
  
  
  cvet2 <- coefss[coefss$colname=='cvet2',]$value
  cvet3 <- coefss[coefss$colname=='cvet3',]$value
  cvet4 <- coefss[coefss$colname=='cvet4',]$value
  cvet5 <- coefss[coefss$colname=='cvet5',]$value
  
  dvigatel2 <- coefss[coefss$colname=='dvigatel2',]$value
  dvigatel3 <- coefss[coefss$colname=='dvigatel3',]$value
  dvigatel4 <- coefss[coefss$colname=='dvigatel4',]$value
  dvigatel5 <- coefss[coefss$colname=='dvigatel5',]$value
  
  volume2 <- coefss[coefss$colname=='volume2',]$value
  volume3 <- coefss[coefss$colname=='volume3',]$value
  volume4 <- coefss[coefss$colname=='volume4',]$value
  volume5 <- coefss[coefss$colname=='volume5',]$value
  
  rul2 <- coefss[coefss$colname=='rul2',]$value
  rul3 <- coefss[coefss$colname=='rul3',]$value
  rul4 <- coefss[coefss$colname=='rul4',]$value
  rul5 <- coefss[coefss$colname=='rul5',]$value
  
  kpp2 <- coefss[coefss$colname=='kpp2',]$value
  kpp3 <- coefss[coefss$colname=='kpp3',]$value
  kpp4 <- coefss[coefss$colname=='kpp4',]$value
  kpp5 <- coefss[coefss$colname=='kpp5',]$value
  
  kuzov2 <- coefss[coefss$colname=='kuzov2',]$value
  kuzov3 <- coefss[coefss$colname=='kuzov3',]$value
  kuzov4 <- coefss[coefss$colname=='kuzov4',]$value
  kuzov5 <- coefss[coefss$colname=='kuzov5',]$value
  
  sales_day_gr2 <- coefss[coefss$colname=='sales_day_gr2',]$value
  sales_day_gr3 <- coefss[coefss$colname=='sales_day_gr3',]$value
  sales_day_gr4 <- coefss[coefss$colname=='sales_day_gr4',]$value
  sales_day_gr5 <- coefss[coefss$colname=='sales_day_gr5',]$value
  
  cen_coef2 <- coefss[coefss$colname=='cen_coef2',]$value
  cen_coef3 <- coefss[coefss$colname=='cen_coef3',]$value
  cen_coef4 <- coefss[coefss$colname=='cen_coef4',]$value
  cen_coef5 <- coefss[coefss$colname=='cen_coef5',]$value
  
  liq2 <- coefss[coefss$colname=='liq2',]$value
  liq3 <- coefss[coefss$colname=='liq3',]$value
  liq4 <- coefss[coefss$colname=='liq4',]$value
  liq5 <- coefss[coefss$colname=='liq5',]$value
  
  
  TOTAL_LIQUIDITY_DATAMART <- Final %>% group_by(Marka,Model,Year) %>% 
    summarise(Mean_price = mean(cena),
              Count = n(),
              M_3_Q = mean(Third_quartile),
              M_1_Q = mean(First_quantile),
              M_SALES_DAYS = mean(Prodan_days)) %>% arrange(-Count)
  TOTAL_LIQUIDITY_DATAMART_1 <- TOTAL_LIQUIDITY_DATAMART[1:(nrow(TOTAL_LIQUIDITY_DATAMART)*0.2),]
  TOTAL <- anti_join(TOTAL_LIQUIDITY_DATAMART,TOTAL_LIQUIDITY_DATAMART_1)
  TOTAL_LIQUIDITY_DATAMART_2 <- TOTAL[1:(nrow(TOTAL)*0.3),]
  TOTAL_LIQUIDITY_DATAMART_3 <- anti_join(TOTAL,TOTAL_LIQUIDITY_DATAMART_2)
  A <- sum(TOTAL_LIQUIDITY_DATAMART_1$Count)/sum(TOTAL_LIQUIDITY_DATAMART$Count)
  B <- sum(TOTAL_LIQUIDITY_DATAMART_2$Count)/sum(TOTAL_LIQUIDITY_DATAMART$Count)
  C <- sum(TOTAL_LIQUIDITY_DATAMART_3$Count)/sum(TOTAL_LIQUIDITY_DATAMART$Count)
  rm(TOTAL)
  range01 <- function(x, ...){(x - min(x, ...)) / (max(x, ...) - min(x, ...))}
  TOTAL_LIQUIDITY_DATAMART_1$COEF_1 <-  (range01(TOTAL_LIQUIDITY_DATAMART_1$Count))
  TOTAL_LIQUIDITY_DATAMART_1$COEF_2 <-  abs((range01(TOTAL_LIQUIDITY_DATAMART_1$M_SALES_DAYS))-1)
  
  additional_data <- df %>% 
    group_by(Marka,Model,Year) %>% 
    summarise(Count = n())
  additional_data1 <- df %>% filter(Sell_classs=="Продан") %>% 
    group_by(Marka,Model,Year) %>% 
    summarise(Sales = n()) 
  add <- left_join(additional_data,additional_data1)
  add$sales_Percent <- add$Sales/add$Count
  add$Count <- NULL
  add$Sales <- NULL
  add <- as.data.frame(add)
  add <- add %>% mutate_if(is.character,as.factor)
  add <- na.omit(add)
  rm(additional_data,additional_data1)
  TOTAL_LIQUIDITY_DATAMART_1 <- as.data.frame(TOTAL_LIQUIDITY_DATAMART_1)
  TOTAL_LIQUIDITY_DATAMART_1 <- inner_join(TOTAL_LIQUIDITY_DATAMART_1,add)
  TOTAL_LIQUIDITY_DATAMART_1$sales_Percent <- NULL
  TOTAL_LIQUIDITY_DATAMART_1$LIQ_MINI_2 <- ((TOTAL_LIQUIDITY_DATAMART_1$COEF_1*coef1+TOTAL_LIQUIDITY_DATAMART_1$COEF_2*coef2)*10)+as.integer(A*100)
  TOTAL_LIQUIDITY_DATAMART_2$COEF_1 <-  (range01(TOTAL_LIQUIDITY_DATAMART_2$Count))
  TOTAL_LIQUIDITY_DATAMART_2$COEF_2 <-  abs((range01(TOTAL_LIQUIDITY_DATAMART_2$M_SALES_DAYS))-1)
  TOTAL_LIQUIDITY_DATAMART_2 <- as.data.frame(TOTAL_LIQUIDITY_DATAMART_2)
  TOTAL_LIQUIDITY_DATAMART_2 <- inner_join(TOTAL_LIQUIDITY_DATAMART_2,add)
  TOTAL_LIQUIDITY_DATAMART_2$sales_Percent <- NULL
  TOTAL_LIQUIDITY_DATAMART_2$LIQ_MINI_2 <- (TOTAL_LIQUIDITY_DATAMART_2$COEF_1*coef1+TOTAL_LIQUIDITY_DATAMART_2$COEF_2*coef2)*10+as.integer(0.5*100)
  TOTAL_LIQUIDITY_DATAMART_3$COEF_1 <-  (range01(TOTAL_LIQUIDITY_DATAMART_3$Count))
  TOTAL_LIQUIDITY_DATAMART_3$COEF_2 <-  abs((range01(TOTAL_LIQUIDITY_DATAMART_3$M_SALES_DAYS))-1)
  TOTAL_LIQUIDITY_DATAMART_3 <- as.data.frame(TOTAL_LIQUIDITY_DATAMART_3)
  TOTAL_LIQUIDITY_DATAMART_3$LIQ_MINI_2 <- (TOTAL_LIQUIDITY_DATAMART_3$COEF_1*coef1+TOTAL_LIQUIDITY_DATAMART_3$COEF_2*coef2)*10+as.integer(0.3*100)
  TOTAL_LIQUIDITY_DATAMART_1$Count_type <- "good"
  TOTAL_LIQUIDITY_DATAMART_2$Count_type <- "medium"
  TOTAL_LIQUIDITY_DATAMART_3$Count_type <- "bad"
  TOTAL <- rbind(TOTAL_LIQUIDITY_DATAMART_1,TOTAL_LIQUIDITY_DATAMART_2,TOTAL_LIQUIDITY_DATAMART_3)
  TOTAL$CLASS <- ifelse(TOTAL$Mean_price<2200000,"less than 2m",
                        ifelse(TOTAL$Mean_price<5500000,"less than 5m",
                               ifelse(TOTAL$Mean_price<10500000,"less than 10m","more than 10m")))
  
  
  Diinislam1 <- TOTAL
  
  TOTAL_LIQUIDITY_DATAMART <- Final %>% group_by(Marka,Model,Year) %>% 
    summarise(Mean_price = mean(cena),
              Count = n(),
              M_3_Q = mean(Third_quartile),
              M_1_Q = mean(First_quantile),
              M_SALES_DAYS = mean(Prodan_days)) %>% arrange(-Count)
  TOTAL_LIQUIDITY_DATAMART_1 <- TOTAL_LIQUIDITY_DATAMART[1:(nrow(TOTAL_LIQUIDITY_DATAMART)*0.2),]
  TOTAL <- anti_join(TOTAL_LIQUIDITY_DATAMART,TOTAL_LIQUIDITY_DATAMART_1)
  TOTAL_LIQUIDITY_DATAMART_2 <- TOTAL[1:(nrow(TOTAL)*0.3),]
  TOTAL_LIQUIDITY_DATAMART_3 <- anti_join(TOTAL,TOTAL_LIQUIDITY_DATAMART_2)
  A <- sum(TOTAL_LIQUIDITY_DATAMART_1$Count)/sum(TOTAL_LIQUIDITY_DATAMART$Count)
  B <- sum(TOTAL_LIQUIDITY_DATAMART_2$Count)/sum(TOTAL_LIQUIDITY_DATAMART$Count)
  C <- sum(TOTAL_LIQUIDITY_DATAMART_3$Count)/sum(TOTAL_LIQUIDITY_DATAMART$Count)
  rm(TOTAL)
  range01 <- function(x, ...){(x - min(x, ...)) / (max(x, ...) - min(x, ...))}
  TOTAL_LIQUIDITY_DATAMART_1$COEF_1 <-  (range01(TOTAL_LIQUIDITY_DATAMART_1$Count))
  TOTAL_LIQUIDITY_DATAMART_1$COEF_2 <-  abs((range01(TOTAL_LIQUIDITY_DATAMART_1$M_SALES_DAYS))-1)
  additional_data <- df %>% 
    group_by(Marka,Model,Year) %>% 
    summarise(Count = n())
  additional_data1 <- df %>% filter(Sell_classs=="Продан") %>% 
    group_by(Marka,Model,Year) %>% 
    summarise(Sales = n()) 
  add <- left_join(additional_data,additional_data1)
  add$sales_Percent <- add$Sales/add$Count
  add$Count <- NULL
  add$Sales <- NULL
  add <- as.data.frame(add)
  add <- add %>% mutate_if(is.character,as.factor)
  add <- na.omit(add)
  rm(additional_data,additional_data1)
  TOTAL_LIQUIDITY_DATAMART_1 <- as.data.frame(TOTAL_LIQUIDITY_DATAMART_1)
  TOTAL_LIQUIDITY_DATAMART_1 <- inner_join(TOTAL_LIQUIDITY_DATAMART_1,add)
  TOTAL_LIQUIDITY_DATAMART_1$sales_Percent <- NULL
  TOTAL_LIQUIDITY_DATAMART_1$LIQ_MINI_2 <- ((TOTAL_LIQUIDITY_DATAMART_1$COEF_1*coef12+TOTAL_LIQUIDITY_DATAMART_1$COEF_2*coef22)*10)+as.integer(A*100)
  TOTAL_LIQUIDITY_DATAMART_2$COEF_1 <-  (range01(TOTAL_LIQUIDITY_DATAMART_2$Count))
  TOTAL_LIQUIDITY_DATAMART_2$COEF_2 <-  abs((range01(TOTAL_LIQUIDITY_DATAMART_2$M_SALES_DAYS))-1)
  TOTAL_LIQUIDITY_DATAMART_2 <- as.data.frame(TOTAL_LIQUIDITY_DATAMART_2)
  TOTAL_LIQUIDITY_DATAMART_2 <- inner_join(TOTAL_LIQUIDITY_DATAMART_2,add)
  TOTAL_LIQUIDITY_DATAMART_2$sales_Percent <- NULL
  TOTAL_LIQUIDITY_DATAMART_2$LIQ_MINI_2 <- (TOTAL_LIQUIDITY_DATAMART_2$COEF_1*coef12+TOTAL_LIQUIDITY_DATAMART_2$COEF_2*coef22)*10+as.integer(0.5*100)
  TOTAL_LIQUIDITY_DATAMART_3$COEF_1 <-  (range01(TOTAL_LIQUIDITY_DATAMART_3$Count))
  TOTAL_LIQUIDITY_DATAMART_3$COEF_2 <-  abs((range01(TOTAL_LIQUIDITY_DATAMART_3$M_SALES_DAYS))-1)
  TOTAL_LIQUIDITY_DATAMART_3 <- as.data.frame(TOTAL_LIQUIDITY_DATAMART_3)
  TOTAL_LIQUIDITY_DATAMART_3$LIQ_MINI_2 <- (TOTAL_LIQUIDITY_DATAMART_3$COEF_1*coef12+TOTAL_LIQUIDITY_DATAMART_3$COEF_2*coef22)*10+as.integer(0.3*100)
  TOTAL_LIQUIDITY_DATAMART_1$Count_type <- "good"
  TOTAL_LIQUIDITY_DATAMART_2$Count_type <- "medium"
  TOTAL_LIQUIDITY_DATAMART_3$Count_type <- "bad"
  TOTAL <- rbind(TOTAL_LIQUIDITY_DATAMART_1,TOTAL_LIQUIDITY_DATAMART_2,TOTAL_LIQUIDITY_DATAMART_3)
  TOTAL$CLASS <- ifelse(TOTAL$Mean_price<2200000,"less than 2m",
                        ifelse(TOTAL$Mean_price<5500000,"less than 5m",
                               ifelse(TOTAL$Mean_price<10500000,"less than 10m","more than 10m")))
  
  
  Diinislam1_2 <- TOTAL
  
  TOTAL_LIQUIDITY_DATAMART <- Final %>% group_by(Marka,Model,Year) %>% 
    summarise(Mean_price = mean(cena),
              Count = n(),
              M_3_Q = mean(Third_quartile),
              M_1_Q = mean(First_quantile),
              M_SALES_DAYS = mean(Prodan_days)) %>% arrange(-Count)
  TOTAL_LIQUIDITY_DATAMART_1 <- TOTAL_LIQUIDITY_DATAMART[1:(nrow(TOTAL_LIQUIDITY_DATAMART)*0.2),]
  TOTAL <- anti_join(TOTAL_LIQUIDITY_DATAMART,TOTAL_LIQUIDITY_DATAMART_1)
  TOTAL_LIQUIDITY_DATAMART_2 <- TOTAL[1:(nrow(TOTAL)*0.3),]
  TOTAL_LIQUIDITY_DATAMART_3 <- anti_join(TOTAL,TOTAL_LIQUIDITY_DATAMART_2)
  A <- sum(TOTAL_LIQUIDITY_DATAMART_1$Count)/sum(TOTAL_LIQUIDITY_DATAMART$Count)
  B <- sum(TOTAL_LIQUIDITY_DATAMART_2$Count)/sum(TOTAL_LIQUIDITY_DATAMART$Count)
  C <- sum(TOTAL_LIQUIDITY_DATAMART_3$Count)/sum(TOTAL_LIQUIDITY_DATAMART$Count)
  rm(TOTAL)
  range01 <- function(x, ...){(x - min(x, ...)) / (max(x, ...) - min(x, ...))}
  TOTAL_LIQUIDITY_DATAMART_1$COEF_1 <-  (range01(TOTAL_LIQUIDITY_DATAMART_1$Count))
  TOTAL_LIQUIDITY_DATAMART_1$COEF_2 <-  abs((range01(TOTAL_LIQUIDITY_DATAMART_1$M_SALES_DAYS))-1)
  additional_data <- df %>% 
    group_by(Marka,Model,Year) %>% 
    summarise(Count = n())
  additional_data1 <- df %>% filter(Sell_classs=="Продан") %>% 
    group_by(Marka,Model,Year) %>% 
    summarise(Sales = n()) 
  add <- left_join(additional_data,additional_data1)
  add$sales_Percent <- add$Sales/add$Count
  add$Count <- NULL
  add$Sales <- NULL
  add <- as.data.frame(add)
  add <- add %>% mutate_if(is.character,as.factor)
  add <- na.omit(add)
  rm(additional_data,additional_data1)
  TOTAL_LIQUIDITY_DATAMART_1 <- as.data.frame(TOTAL_LIQUIDITY_DATAMART_1)
  TOTAL_LIQUIDITY_DATAMART_1 <- inner_join(TOTAL_LIQUIDITY_DATAMART_1,add)
  TOTAL_LIQUIDITY_DATAMART_1$sales_Percent <- NULL
  TOTAL_LIQUIDITY_DATAMART_1$LIQ_MINI_2 <- ((TOTAL_LIQUIDITY_DATAMART_1$COEF_1*coef13+TOTAL_LIQUIDITY_DATAMART_1$COEF_2*coef23)*10)+as.integer(A*100)
  TOTAL_LIQUIDITY_DATAMART_2$COEF_1 <-  (range01(TOTAL_LIQUIDITY_DATAMART_2$Count))
  TOTAL_LIQUIDITY_DATAMART_2$COEF_2 <-  abs((range01(TOTAL_LIQUIDITY_DATAMART_2$M_SALES_DAYS))-1)
  TOTAL_LIQUIDITY_DATAMART_2 <- as.data.frame(TOTAL_LIQUIDITY_DATAMART_2)
  TOTAL_LIQUIDITY_DATAMART_2 <- inner_join(TOTAL_LIQUIDITY_DATAMART_2,add)
  TOTAL_LIQUIDITY_DATAMART_2$sales_Percent <- NULL
  TOTAL_LIQUIDITY_DATAMART_2$LIQ_MINI_2 <- (TOTAL_LIQUIDITY_DATAMART_2$COEF_1*coef13+TOTAL_LIQUIDITY_DATAMART_2$COEF_2*coef23)*10+as.integer(0.5*100)
  TOTAL_LIQUIDITY_DATAMART_3$COEF_1 <-  (range01(TOTAL_LIQUIDITY_DATAMART_3$Count))
  TOTAL_LIQUIDITY_DATAMART_3$COEF_2 <-  abs((range01(TOTAL_LIQUIDITY_DATAMART_3$M_SALES_DAYS))-1)
  TOTAL_LIQUIDITY_DATAMART_3 <- as.data.frame(TOTAL_LIQUIDITY_DATAMART_3)
  TOTAL_LIQUIDITY_DATAMART_3$LIQ_MINI_2 <- (TOTAL_LIQUIDITY_DATAMART_3$COEF_1*coef13+TOTAL_LIQUIDITY_DATAMART_3$COEF_2*coef23)*10+as.integer(0.3*100)
  TOTAL_LIQUIDITY_DATAMART_1$Count_type <- "good"
  TOTAL_LIQUIDITY_DATAMART_2$Count_type <- "medium"
  TOTAL_LIQUIDITY_DATAMART_3$Count_type <- "bad"
  TOTAL <- rbind(TOTAL_LIQUIDITY_DATAMART_1,TOTAL_LIQUIDITY_DATAMART_2,TOTAL_LIQUIDITY_DATAMART_3)
  TOTAL$CLASS <- ifelse(TOTAL$Mean_price<2200000,"less than 2m",
                        ifelse(TOTAL$Mean_price<5500000,"less than 5m",
                               ifelse(TOTAL$Mean_price<10500000,"less than 10m","more than 10m")))
  
  
  Diinislam1_3 <- TOTAL
  
  TOTAL_LIQUIDITY_DATAMART <- Final %>% group_by(Marka,Model,Year) %>% 
    summarise(Mean_price = mean(cena),
              Count = n(),
              M_3_Q = mean(Third_quartile),
              M_1_Q = mean(First_quantile),
              M_SALES_DAYS = mean(Prodan_days)) %>% arrange(-Count)
  TOTAL_LIQUIDITY_DATAMART_1 <- TOTAL_LIQUIDITY_DATAMART[1:(nrow(TOTAL_LIQUIDITY_DATAMART)*0.2),]
  TOTAL <- anti_join(TOTAL_LIQUIDITY_DATAMART,TOTAL_LIQUIDITY_DATAMART_1)
  TOTAL_LIQUIDITY_DATAMART_2 <- TOTAL[1:(nrow(TOTAL)*0.3),]
  TOTAL_LIQUIDITY_DATAMART_3 <- anti_join(TOTAL,TOTAL_LIQUIDITY_DATAMART_2)
  A <- sum(TOTAL_LIQUIDITY_DATAMART_1$Count)/sum(TOTAL_LIQUIDITY_DATAMART$Count)
  B <- sum(TOTAL_LIQUIDITY_DATAMART_2$Count)/sum(TOTAL_LIQUIDITY_DATAMART$Count)
  C <- sum(TOTAL_LIQUIDITY_DATAMART_3$Count)/sum(TOTAL_LIQUIDITY_DATAMART$Count)
  rm(TOTAL)
  range01 <- function(x, ...){(x - min(x, ...)) / (max(x, ...) - min(x, ...))}
  TOTAL_LIQUIDITY_DATAMART_1$COEF_1 <-  (range01(TOTAL_LIQUIDITY_DATAMART_1$Count))
  TOTAL_LIQUIDITY_DATAMART_1$COEF_2 <-  abs((range01(TOTAL_LIQUIDITY_DATAMART_1$M_SALES_DAYS))-1)
  additional_data <- df %>% 
    group_by(Marka,Model,Year) %>% 
    summarise(Count = n())
  additional_data1 <- df %>% filter(Sell_classs=="Продан") %>% 
    group_by(Marka,Model,Year) %>% 
    summarise(Sales = n()) 
  add <- left_join(additional_data,additional_data1)
  add$sales_Percent <- add$Sales/add$Count
  add$Count <- NULL
  add$Sales <- NULL
  add <- as.data.frame(add)
  add <- add %>% mutate_if(is.character,as.factor)
  add <- na.omit(add)
  rm(additional_data,additional_data1)
  TOTAL_LIQUIDITY_DATAMART_1 <- as.data.frame(TOTAL_LIQUIDITY_DATAMART_1)
  TOTAL_LIQUIDITY_DATAMART_1 <- inner_join(TOTAL_LIQUIDITY_DATAMART_1,add)
  TOTAL_LIQUIDITY_DATAMART_1$sales_Percent <- NULL
  TOTAL_LIQUIDITY_DATAMART_1$LIQ_MINI_2 <- ((TOTAL_LIQUIDITY_DATAMART_1$COEF_1*coef14+TOTAL_LIQUIDITY_DATAMART_1$COEF_2*coef24)*10)+as.integer(A*100)
  TOTAL_LIQUIDITY_DATAMART_2$COEF_1 <-  (range01(TOTAL_LIQUIDITY_DATAMART_2$Count))
  TOTAL_LIQUIDITY_DATAMART_2$COEF_2 <-  abs((range01(TOTAL_LIQUIDITY_DATAMART_2$M_SALES_DAYS))-1)
  TOTAL_LIQUIDITY_DATAMART_2 <- as.data.frame(TOTAL_LIQUIDITY_DATAMART_2)
  TOTAL_LIQUIDITY_DATAMART_2 <- inner_join(TOTAL_LIQUIDITY_DATAMART_2,add)
  TOTAL_LIQUIDITY_DATAMART_2$sales_Percent <- NULL
  TOTAL_LIQUIDITY_DATAMART_2$LIQ_MINI_2 <- (TOTAL_LIQUIDITY_DATAMART_2$COEF_1*coef14+TOTAL_LIQUIDITY_DATAMART_2$COEF_2*coef24)*10+as.integer(0.5*100)
  TOTAL_LIQUIDITY_DATAMART_3$COEF_1 <-  (range01(TOTAL_LIQUIDITY_DATAMART_3$Count))
  TOTAL_LIQUIDITY_DATAMART_3$COEF_2 <-  abs((range01(TOTAL_LIQUIDITY_DATAMART_3$M_SALES_DAYS))-1)
  TOTAL_LIQUIDITY_DATAMART_3 <- as.data.frame(TOTAL_LIQUIDITY_DATAMART_3)
  TOTAL_LIQUIDITY_DATAMART_3$LIQ_MINI_2 <- (TOTAL_LIQUIDITY_DATAMART_3$COEF_1*coef14+TOTAL_LIQUIDITY_DATAMART_3$COEF_2*coef24)*10+as.integer(0.3*100)
  TOTAL_LIQUIDITY_DATAMART_1$Count_type <- "good"
  TOTAL_LIQUIDITY_DATAMART_2$Count_type <- "medium"
  TOTAL_LIQUIDITY_DATAMART_3$Count_type <- "bad"
  TOTAL <- rbind(TOTAL_LIQUIDITY_DATAMART_1,TOTAL_LIQUIDITY_DATAMART_2,TOTAL_LIQUIDITY_DATAMART_3)
  TOTAL$CLASS <- ifelse(TOTAL$Mean_price<2200000,"less than 2m",
                        ifelse(TOTAL$Mean_price<5500000,"less than 5m",
                               ifelse(TOTAL$Mean_price<10500000,"less than 10m","more than 10m")))
  
  
  Diinislam1_4 <- TOTAL
  
  TOTAL_LIQUIDITY_DATAMART <- Final %>% group_by(Marka,Model,Year) %>% 
    summarise(Mean_price = mean(cena),
              Count = n(),
              M_3_Q = mean(Third_quartile),
              M_1_Q = mean(First_quantile),
              M_SALES_DAYS = mean(Prodan_days)) %>% arrange(-Count)
  TOTAL_LIQUIDITY_DATAMART_1 <- TOTAL_LIQUIDITY_DATAMART[1:(nrow(TOTAL_LIQUIDITY_DATAMART)*0.2),]
  TOTAL <- anti_join(TOTAL_LIQUIDITY_DATAMART,TOTAL_LIQUIDITY_DATAMART_1)
  TOTAL_LIQUIDITY_DATAMART_2 <- TOTAL[1:(nrow(TOTAL)*0.3),]
  TOTAL_LIQUIDITY_DATAMART_3 <- anti_join(TOTAL,TOTAL_LIQUIDITY_DATAMART_2)
  A <- sum(TOTAL_LIQUIDITY_DATAMART_1$Count)/sum(TOTAL_LIQUIDITY_DATAMART$Count)
  B <- sum(TOTAL_LIQUIDITY_DATAMART_2$Count)/sum(TOTAL_LIQUIDITY_DATAMART$Count)
  C <- sum(TOTAL_LIQUIDITY_DATAMART_3$Count)/sum(TOTAL_LIQUIDITY_DATAMART$Count)
  rm(TOTAL)
  range01 <- function(x, ...){(x - min(x, ...)) / (max(x, ...) - min(x, ...))}
  TOTAL_LIQUIDITY_DATAMART_1$COEF_1 <-  (range01(TOTAL_LIQUIDITY_DATAMART_1$Count))
  TOTAL_LIQUIDITY_DATAMART_1$COEF_2 <-  abs((range01(TOTAL_LIQUIDITY_DATAMART_1$M_SALES_DAYS))-1)
  additional_data <- df %>% 
    group_by(Marka,Model,Year) %>% 
    summarise(Count = n())
  additional_data1 <- df %>% filter(Sell_classs=="Продан") %>% 
    group_by(Marka,Model,Year) %>% 
    summarise(Sales = n()) 
  add <- left_join(additional_data,additional_data1)
  add$sales_Percent <- add$Sales/add$Count
  add$Count <- NULL
  add$Sales <- NULL
  add <- as.data.frame(add)
  add <- add %>% mutate_if(is.character,as.factor)
  add <- na.omit(add)
  rm(additional_data,additional_data1)
  TOTAL_LIQUIDITY_DATAMART_1 <- as.data.frame(TOTAL_LIQUIDITY_DATAMART_1)
  TOTAL_LIQUIDITY_DATAMART_1 <- inner_join(TOTAL_LIQUIDITY_DATAMART_1,add)
  TOTAL_LIQUIDITY_DATAMART_1$sales_Percent <- NULL
  TOTAL_LIQUIDITY_DATAMART_1$LIQ_MINI_2 <- ((TOTAL_LIQUIDITY_DATAMART_1$COEF_1*coef15+TOTAL_LIQUIDITY_DATAMART_1$COEF_2*coef25)*10)+as.integer(A*100)
  TOTAL_LIQUIDITY_DATAMART_2$COEF_1 <-  (range01(TOTAL_LIQUIDITY_DATAMART_2$Count))
  TOTAL_LIQUIDITY_DATAMART_2$COEF_2 <-  abs((range01(TOTAL_LIQUIDITY_DATAMART_2$M_SALES_DAYS))-1)
  TOTAL_LIQUIDITY_DATAMART_2 <- as.data.frame(TOTAL_LIQUIDITY_DATAMART_2)
  TOTAL_LIQUIDITY_DATAMART_2 <- inner_join(TOTAL_LIQUIDITY_DATAMART_2,add)
  TOTAL_LIQUIDITY_DATAMART_2$sales_Percent <- NULL
  TOTAL_LIQUIDITY_DATAMART_2$LIQ_MINI_2 <- (TOTAL_LIQUIDITY_DATAMART_2$COEF_1*coef15+TOTAL_LIQUIDITY_DATAMART_2$COEF_2*coef25)*10+as.integer(0.5*100)
  TOTAL_LIQUIDITY_DATAMART_3$COEF_1 <-  (range01(TOTAL_LIQUIDITY_DATAMART_3$Count))
  TOTAL_LIQUIDITY_DATAMART_3$COEF_2 <-  abs((range01(TOTAL_LIQUIDITY_DATAMART_3$M_SALES_DAYS))-1)
  TOTAL_LIQUIDITY_DATAMART_3 <- as.data.frame(TOTAL_LIQUIDITY_DATAMART_3)
  TOTAL_LIQUIDITY_DATAMART_3$LIQ_MINI_2 <- (TOTAL_LIQUIDITY_DATAMART_3$COEF_1*coef15+TOTAL_LIQUIDITY_DATAMART_3$COEF_2*coef25)*10+as.integer(0.3*100)
  TOTAL_LIQUIDITY_DATAMART_1$Count_type <- "good"
  TOTAL_LIQUIDITY_DATAMART_2$Count_type <- "medium"
  TOTAL_LIQUIDITY_DATAMART_3$Count_type <- "bad"
  TOTAL <- rbind(TOTAL_LIQUIDITY_DATAMART_1,TOTAL_LIQUIDITY_DATAMART_2,TOTAL_LIQUIDITY_DATAMART_3)
  TOTAL$CLASS <- ifelse(TOTAL$Mean_price<2200000,"less than 2m",
                        ifelse(TOTAL$Mean_price<5500000,"less than 5m",
                               ifelse(TOTAL$Mean_price<10500000,"less than 10m","more than 10m")))
  
  
  Diinislam1_5 <- TOTAL
  
  Diinislam1$LIQ_MINI_2_2 <- Diinislam1_2$LIQ_MINI_2
  Diinislam1$LIQ_MINI_2_3 <- Diinislam1_3$LIQ_MINI_2
  Diinislam1$LIQ_MINI_2_4 <- Diinislam1_4$LIQ_MINI_2
  Diinislam1$LIQ_MINI_2_5 <- Diinislam1_5$LIQ_MINI_2
  Diinislam1
  
  
  
  ###########
  TOTAL_LIQUIDITY_DATAMART <- Final %>% group_by(Marka,Model,Year,Tip_kuzova) %>% 
    summarise(Mean_price = mean(cena),
              Count = n(),
              M_3_Q = mean(Third_quartile),
              M_1_Q = mean(First_quantile),
              M_SALES_DAYS = mean(Prodan_days)) %>% arrange(-Count)
  TOTAL_LIQUIDITY_DATAMART$COEF_1 <-  (range01(TOTAL_LIQUIDITY_DATAMART$Count))
  TOTAL_LIQUIDITY_DATAMART$COEF_2 <-  abs((range01(TOTAL_LIQUIDITY_DATAMART$M_SALES_DAYS))-1)
  TOTAL_LIQUIDITY_DATAMART <- as.data.frame(TOTAL_LIQUIDITY_DATAMART)
  TOTAL_LIQUIDITY_DATAMART$LIQ_MINI_2 <- ((TOTAL_LIQUIDITY_DATAMART$COEF_1*coef1+TOTAL_LIQUIDITY_DATAMART$COEF_2*coef2)*10)-5
  TOTAL <- TOTAL_LIQUIDITY_DATAMART
  Diinislam2 <- TOTAL # Tip Kuzova
  TOTAL_LIQUIDITY_DATAMART <- Final %>% group_by(Marka,Model,Year,KPP) %>% 
    summarise(Mean_price = mean(cena),
              Count = n(),
              M_3_Q = mean(Third_quartile),
              M_1_Q = mean(First_quantile),
              M_SALES_DAYS = mean(Prodan_days)) %>% arrange(-Count)
  TOTAL_LIQUIDITY_DATAMART$COEF_1 <-  (range01(TOTAL_LIQUIDITY_DATAMART$Count))
  TOTAL_LIQUIDITY_DATAMART$COEF_2 <-  abs((range01(TOTAL_LIQUIDITY_DATAMART$M_SALES_DAYS))-1)
  TOTAL_LIQUIDITY_DATAMART <- as.data.frame(TOTAL_LIQUIDITY_DATAMART)
  TOTAL_LIQUIDITY_DATAMART$LIQ_MINI_2 <- ((TOTAL_LIQUIDITY_DATAMART$COEF_1*coef1+TOTAL_LIQUIDITY_DATAMART$COEF_2*coef2)*10)-5
  TOTAL <- TOTAL_LIQUIDITY_DATAMART
  Diinislam3 <- TOTAL # KPP
  TOTAL_LIQUIDITY_DATAMART <- Final %>% group_by(Marka,Model,Year,raspolozhenie_rulya) %>% 
    summarise(Mean_price = mean(cena),
              Count = n(),
              M_3_Q = mean(Third_quartile),
              M_1_Q = mean(First_quantile),
              M_SALES_DAYS = mean(Prodan_days)) %>% arrange(-Count)
  TOTAL_LIQUIDITY_DATAMART$COEF_1 <-  (range01(TOTAL_LIQUIDITY_DATAMART$Count))
  TOTAL_LIQUIDITY_DATAMART$COEF_2 <-  abs((range01(TOTAL_LIQUIDITY_DATAMART$M_SALES_DAYS))-1)
  TOTAL_LIQUIDITY_DATAMART <- as.data.frame(TOTAL_LIQUIDITY_DATAMART)
  TOTAL_LIQUIDITY_DATAMART$LIQ_MINI_2 <- ((TOTAL_LIQUIDITY_DATAMART$COEF_1*coef1+TOTAL_LIQUIDITY_DATAMART$COEF_2*coef2)*10)-5
  Diinislam4 <- TOTAL_LIQUIDITY_DATAMART # KPP
  TOTAL_LIQUIDITY_DATAMART <- Final %>% group_by(Marka,Model,Year,volume) %>% 
    summarise(Mean_price = mean(cena),
              Count = n(),
              M_3_Q = mean(Third_quartile),
              M_1_Q = mean(First_quantile),
              M_SALES_DAYS = mean(Prodan_days)) %>% arrange(-Count)
  TOTAL_LIQUIDITY_DATAMART$COEF_1 <-  (range01(TOTAL_LIQUIDITY_DATAMART$Count))
  TOTAL_LIQUIDITY_DATAMART$COEF_2 <-  abs((range01(TOTAL_LIQUIDITY_DATAMART$M_SALES_DAYS))-1)
  TOTAL_LIQUIDITY_DATAMART <- as.data.frame(TOTAL_LIQUIDITY_DATAMART)
  TOTAL_LIQUIDITY_DATAMART$LIQ_MINI_2 <- ((TOTAL_LIQUIDITY_DATAMART$COEF_1*coef1+TOTAL_LIQUIDITY_DATAMART$COEF_2*coef2)*10)-5
  Diinislam5 <- TOTAL_LIQUIDITY_DATAMART # KPP
  TOTAL_LIQUIDITY_DATAMART <- Final %>% group_by(Marka,Model,Year,dvigatel) %>% 
    summarise(Mean_price = mean(cena),
              Count = n(),
              M_3_Q = mean(Third_quartile),
              M_1_Q = mean(First_quantile),
              M_SALES_DAYS = mean(Prodan_days)) %>% arrange(-Count)
  TOTAL_LIQUIDITY_DATAMART$COEF_1 <-  (range01(TOTAL_LIQUIDITY_DATAMART$Count))
  TOTAL_LIQUIDITY_DATAMART$COEF_2 <-  abs((range01(TOTAL_LIQUIDITY_DATAMART$M_SALES_DAYS))-1)
  TOTAL_LIQUIDITY_DATAMART <- as.data.frame(TOTAL_LIQUIDITY_DATAMART)
  TOTAL_LIQUIDITY_DATAMART$LIQ_MINI_2 <- ((TOTAL_LIQUIDITY_DATAMART$COEF_1*coef1+TOTAL_LIQUIDITY_DATAMART$COEF_2*coef2)*10)-5
  Diinislam6 <- TOTAL_LIQUIDITY_DATAMART # KPP
  TOTAL_LIQUIDITY_DATAMART <- Final %>% group_by(Marka,Model,Year,cvet) %>% 
    summarise(Mean_price = mean(cena),
              Count = n(),
              M_3_Q = mean(Third_quartile),
              M_1_Q = mean(First_quantile),
              M_SALES_DAYS = mean(Prodan_days)) %>% arrange(-Count)
  TOTAL_LIQUIDITY_DATAMART$COEF_1 <-  (range01(TOTAL_LIQUIDITY_DATAMART$Count))
  TOTAL_LIQUIDITY_DATAMART$COEF_2 <-  abs((range01(TOTAL_LIQUIDITY_DATAMART$M_SALES_DAYS))-1)
  TOTAL_LIQUIDITY_DATAMART <- as.data.frame(TOTAL_LIQUIDITY_DATAMART)
  TOTAL_LIQUIDITY_DATAMART$LIQ_MINI_2 <- ((TOTAL_LIQUIDITY_DATAMART$COEF_1*coef1+TOTAL_LIQUIDITY_DATAMART$COEF_2*coef2)*10)-5
  Diinislam7 <- TOTAL_LIQUIDITY_DATAMART # KPP
  Diinislam7$CVET_coef <- Diinislam7$LIQ_MINI_2
  Diinislam6$DVIGATEL_coef <- Diinislam6$LIQ_MINI_2
  Diinislam5$VOLUME_coef <- Diinislam5$LIQ_MINI_2
  Diinislam4$RUL_coef <- Diinislam4$LIQ_MINI_2
  Diinislam3$KPP_coef <- Diinislam3$LIQ_MINI_2
  Diinislam2$KUZOV_coef <- Diinislam2$LIQ_MINI_2
  Diinislam2$LIQ_MINI_2 <- NULL
  Diinislam3$LIQ_MINI_2 <- NULL
  Diinislam4$LIQ_MINI_2 <- NULL
  Diinislam5$LIQ_MINI_2 <- NULL
  Diinislam6$LIQ_MINI_2 <- NULL
  Diinislam7$LIQ_MINI_2 <- NULL
  Diinislam1 <- select(Diinislam1,-c(COEF_1,COEF_2))
  Diinislam2 <- select(Diinislam2,-c(COEF_1,COEF_2))
  Diinislam3 <- select(Diinislam3,-c(COEF_1,COEF_2))
  Diinislam4 <- select(Diinislam4,-c(COEF_1,COEF_2))
  Diinislam5 <- select(Diinislam5,-c(COEF_1,COEF_2))
  Diinislam6 <- select(Diinislam6,-c(COEF_1,COEF_2))
  Diinislam7 <- select(Diinislam7,-c(COEF_1,COEF_2))
  Diinislam1 <- select(Diinislam1,-c(Count,Mean_price,M_3_Q,M_1_Q,M_SALES_DAYS))
  Diinislam2 <- select(Diinislam2,-c(Count,Mean_price,M_3_Q,M_1_Q,M_SALES_DAYS))
  Diinislam3 <- select(Diinislam3,-c(Count,Mean_price,M_3_Q,M_1_Q,M_SALES_DAYS))
  Diinislam4 <- select(Diinislam4,-c(Count,Mean_price,M_3_Q,M_1_Q,M_SALES_DAYS))
  Diinislam5 <- select(Diinislam5,-c(Count,Mean_price,M_3_Q,M_1_Q,M_SALES_DAYS))
  Diinislam6 <- select(Diinislam6,-c(Count,Mean_price,M_3_Q,M_1_Q,M_SALES_DAYS))
  Diinislam7 <- select(Diinislam7,-c(Count,Mean_price,M_3_Q,M_1_Q,M_SALES_DAYS))
  Diinislam7 <- Diinislam7 %>% group_by(Marka,Model,Year,cvet) %>% arrange(-CVET_coef)
  datamart <- inner_join(Diinislam7,Diinislam6)
  datamart <- inner_join(datamart,Diinislam5)
  datamart <- inner_join(datamart,Diinislam4)
  datamart <- inner_join(datamart,Diinislam3)
  datamart <- inner_join(datamart,Diinislam2)
  datamart <- inner_join(datamart,Diinislam1)
  colnames(datamart)
  data <- datamart[,c(1,2,3,4,6,8,10,12,14,5,7,9,11,13,15,16,17,18:22)]
  
  
  
  data$Liquidity <- data$LIQ_MINI_2+data$CVET_coef+data$DVIGATEL_coef+data$VOLUME_coef+data$RUL_coef+data$KPP_coef+data$KUZOV_coef
  data$Liquidity2 <- data$LIQ_MINI_2_2+data$CVET_coef*cvet2+data$DVIGATEL_coef*dvigatel2+data$VOLUME_coef*volume2+data$RUL_coef*rul2+data$KPP_coef*kpp2+data$KUZOV_coef*kuzov2
  data$Liquidity3 <- data$LIQ_MINI_2_3+data$CVET_coef*cvet3+data$DVIGATEL_coef*dvigatel3+data$VOLUME_coef*volume3+data$RUL_coef*rul3+data$KPP_coef*kpp3+data$KUZOV_coef*kuzov3
  data$Liquidity4 <- data$LIQ_MINI_2_4+data$CVET_coef*cvet4+data$DVIGATEL_coef*dvigatel4+data$VOLUME_coef*volume4+data$RUL_coef*rul4+data$KPP_coef*kpp4+data$KUZOV_coef*kuzov4
  data$Liquidity5 <- data$LIQ_MINI_2_5+data$CVET_coef*cvet5+data$DVIGATEL_coef*dvigatel5+data$VOLUME_coef*volume5+data$RUL_coef*rul5+data$KPP_coef*kpp5+data$KUZOV_coef*kuzov5
  
  
  df_dictionary <- df %>% group_by(Marka,Model,Year,Tip_kuzova,dvigatel,volume,KPP) %>% summarise(N=n())
  df_dictionary <- df_dictionary[df_dictionary$N>2,]
  data <- as.data.frame(data)
  tochka <- inner_join(data,df_dictionary)
  tocka <- tochka[tochka$Tip_kuzova!=c("лимузин"),]
  tocka <- tocka[tochka$Tip_kuzova!=c("микроавтобус"),]
  tocka <- tocka[tochka$Tip_kuzova!=c("фургон"),]
  summary(tocka)
  write.csv(tocka,"BIG/BI_complete.csv",row.names = F)
}