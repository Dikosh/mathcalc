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
#' @export tumi

#' @import data.table


tumi <- function(x){
  
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
  
  daud$data_date <- NULL
  daud$Max_date <- NULL
  daud$schet <- NULL
  df <- daud
  df <- df[df$Year>=1990,]
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
  train <- select(df,c(Marka,Model,Year,Tip_kuzova,KPP,privod,raspolozhenie_rulya,cvet,cena,volume,dvigatel,City,
                       Mean,Third_quartile,First_quantile,difficulty_classificator,anomaly_by_price,anomaly_by_year,
                       year_mean,First_q_year,Second_q_year,Marka_model_n,Marka_n,id_car_in_kolesa))
  train <- train %>% mutate_if(is.character,as.factor)
  train$privod <- ifelse(is.na(train$privod),"NAN",train$privod)
  sapply(train,function(x){sum(is.na(x))})
  train$cvet <- ifelse(is.na(as.character(train$cvet)),"NAN_color",as.character(train$cvet))
  train <- na.omit(train)
  train <- train %>% mutate_if(is.character,as.factor)
  options(scipen = 999)
  testr <- train %>% group_by(Marka,Model,Year) %>% summarise(count = n())
  testr <- testr[testr$count>5,]
  testr$count <- NULL
  testr <- unique(testr)
  write.csv(testr,"dictionaries/group5.csv",row.names = F)
  train <- inner_join(train,testr)
  train <- train[train$cena<30000000,]
  train <- train[train$cena>900000,]
  train <- unique(train)
  train <- train %>% mutate_if(is.character,as.factor)
  write.csv(select(train,id_car_in_kolesa),"mini_id_checker.csv",row.names = F)
  sales_to_train <- read.csv("BIG/SALES_TO_TRAIN.csv")
  train <- inner_join(train,sales_to_train)
  train <- na.omit(train)
  checkers <- select(df,c(id_car_in_kolesa,date_difference,views,kod_statusa))
  train$City <- NULL
  
  fwrite(checkers,"BIG/checkers.csv",row.names = F)
  write.csv(train,"BIG/TRAIN.CSV",row.names = F)
  
  
  additional_data <- df %>% 
    group_by(Marka,Model,Year,KPP,Tip_kuzova,raspolozhenie_rulya,volume,dvigatel) %>% 
    summarise(Count = n())
  
  
  additional_data1 <- df %>% filter(Sell_classs=="Продан") %>% 
    group_by(Marka,Model,Year,KPP,Tip_kuzova,raspolozhenie_rulya,volume,dvigatel) %>% 
    summarise(Sales = n()) 
  str(df)
  
  
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
  bst <- xgboost(data = dtrain,
                 objective='reg:linear',
                 nrounds = 25,
                 #  nfold = 5,
                 metrics = "rmse",
                 maximize = FALSE,
                 verbose = 0,
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
  
  
  
  prop.table(table(tsss$flag1,tsss$flag2))
  Final <- tsss
  Final$CLASSS <- ifelse(Final$flag1=="predicted is greater" & tsss$flag2=="active","Predicted greater and active","NAN")
  index <- createDataPartition(train$cena,p=0.7,list = F)
  tr <- train[index,-c(24:27)]
  ts <- train[-index,-c(24:27)]
  colnames(tr)
  str(tr)
  dtrain <- data.matrix(select(tr,-c(Sales_days_group)))
  ctest <- data.matrix(select(ts,-c(Sales_days_group)))
  train_labs <- as.numeric(tr$Sales_days_group) - 1
  val_labs <- as.numeric(ts$Sales_days_group) - 1
  dtrain <- xgb.DMatrix(data=dtrain,label=train_labs)
  ctest <- xgb.DMatrix(data=ctest,label=val_labs)
  # Set parameters(default)
  params <- list(booster = "gbtree", objective = "multi:softprob", num_class = 4, eval_metric = "mlogloss")
  
  # Calculate # of folds for cross-validation
  xgbcv <- xgb.cv(params = params,
                  data = dtrain,
                  nrounds = 100,
                  nfold = 5,
                  showsd = TRUE,
                  stratified = TRUE,
                  print.every.n = 10,
                  early_stop_round = 20,
                  maximize = FALSE,
                  prediction = TRUE)
  
  
  classification_error <- function(conf_mat) {
    # Сделал модель, которая прогнозирует за сколько дней продастся автомобиль  
    
    
    conf_mat = as.matrix(conf_mat)
    
    error = 1 - sum(diag(conf_mat)) / sum(conf_mat)
    
    return (error)
  }
  
  # Mutate xgb output to deliver hard predictions
  xgb_train_preds <- data.frame(xgbcv$pred) %>% mutate(max = max.col(., ties.method = "last"), label = train_labs + 1)
  
  # Examine output
  head(xgb_train_preds)
  
  xgb_conf_mat <- table(true = train_labs + 1, pred = xgb_train_preds$max)
  
  # Error 
  cat("XGB Training Classification Error Rate:", classification_error(xgb_conf_mat), "\n")
  
  
  xgb_conf_mat_2 <- confusionMatrix(factor(xgb_train_preds$label),
                                    factor(xgb_train_preds$max),
                                    mode = "everything")
  
  print(xgb_conf_mat_2)
  
  
  
  xgb_model <- xgb.train(params = params, data = dtrain, nrounds = 100)
  
  # Predict for validation set
  xgb_val_preds <- predict(xgb_model, newdata = ctest)
  
  xgb_val_out <- matrix(xgb_val_preds, nrow = 4, ncol = length(xgb_val_preds) / 4) %>% 
    t() %>%
    data.frame() %>%
    mutate(max = max.col(., ties.method = "last"), label = val_labs + 1) 
  
  # Confustion Matrix
  xgb_val_conf <- table(true = val_labs + 1, pred = xgb_val_out$max)
  
  cat("XGB Validation Classification Error Rate:", classification_error(xgb_val_conf), "\n")
  
  
  xgb_val_conf2 <- confusionMatrix(factor(xgb_val_out$label),
                                   factor(xgb_val_out$max),
                                   mode = "everything")
  
  print(xgb_val_conf2)
  
  
  
  xgb.save(xgb_model, 'xgb_liklik.model')
  damn <- Final[,colnames(tr)]
  ctest_damn <- data.matrix(select(damn,-c(Sales_days_group)))
  val_labs_damn <- as.numeric(damn$Sales_days_group) - 1
  ctest <- xgb.DMatrix(data=ctest_damn,label=val_labs_damn)
  xgb_val_preds <- predict(xgb_model, newdata = ctest)
  xgb_val_out <- matrix(xgb_val_preds, nrow = 4, ncol = length(xgb_val_preds) / 4) %>% 
    t() %>%
    data.frame() %>%
    mutate(max = max.col(., ties.method = "last"), label = val_labs_damn + 1) 
  xgb_val_conf <- table(true = val_labs_damn + 1, pred = xgb_val_out$max)
  cat("XGB Validation Classification Error Rate:", classification_error(xgb_val_conf), "\n")
  xgb_val_conf2 <- confusionMatrix(factor(xgb_val_out$label),
                                   factor(xgb_val_out$max),
                                   mode = "everything")
  print(xgb_val_conf2)
  
  Final$Sales_days_group
  
  table(val_labs_damn+1 == xgb_val_out$label)
  
  Final$Sales_days_group_pred <- xgb_val_out$max
  Final$Sales_days_group_int <- xgb_val_out$label
  
  
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
  train$id_car_in_kolesa <- NULL
  index <- createDataPartition(train$cena,p=0.7,list = F)
  tr <- train[index,-c(24:27)]
  ts <- train[-index,-c(24:27)]
  params <- list(booster = "gbtree", objective = "multi:softprob", num_class = 4, eval_metric = "mlogloss")
  classification_error <- function(conf_mat) {
    # Сделал модель, которая прогнозирует за сколько дней продастся автомобиль  
    
    
    conf_mat = as.matrix(conf_mat)
    
    error = 1 - sum(diag(conf_mat)) / sum(conf_mat)
    
    return (error)
  }
  damn <- as.data.frame(train)[,colnames(tr)]
  ctest_damn <- data.matrix(select(damn,-c(Sales_days_group)))
  val_labs_damn <- as.numeric(damn$Sales_days_group) - 1
  ctest <- xgb.DMatrix(data=ctest_damn,label=val_labs_damn)
  xgb_model <- xgb.load("xgb_liklik.model")
  xgb_val_preds <- predict(xgb_model, newdata = ctest)
  xgb_val_out <- matrix(xgb_val_preds, nrow = 4, ncol = length(xgb_val_preds) / 4) %>% 
    t() %>%
    data.frame() %>%
    mutate(max = max.col(., ties.method = "last"), label = val_labs_damn + 1) 
  xgb_val_conf <- table(true = val_labs_damn + 1, pred = xgb_val_out$max)
  xgb_val_conf2 <- confusionMatrix(factor(xgb_val_out$label),
                                   factor(xgb_val_out$max),
                                   mode = "everything")
  
  train$Sales_days_group_pred <- xgb_val_out$max
  train$Sales_days_group_int <- xgb_val_out$label
  train$prognoz <- predict(bst,dtrain)
  train$error <- (train$cena-train$prognoz)/(train$cena+train$prognoz)
  train$diff <- train$cena - train$prognoz
  train$prognoz <- abs(train$prognoz)
  
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
  TOTAL_LIQUIDITY_DATAMART_1$LIQ_MINI_2 <- ((TOTAL_LIQUIDITY_DATAMART_1$COEF_1*0.3+TOTAL_LIQUIDITY_DATAMART_1$COEF_2*0.7)*10)+as.integer(A*100)
  TOTAL_LIQUIDITY_DATAMART_2$COEF_1 <-  (range01(TOTAL_LIQUIDITY_DATAMART_2$Count))
  TOTAL_LIQUIDITY_DATAMART_2$COEF_2 <-  abs((range01(TOTAL_LIQUIDITY_DATAMART_2$M_SALES_DAYS))-1)
  TOTAL_LIQUIDITY_DATAMART_2 <- as.data.frame(TOTAL_LIQUIDITY_DATAMART_2)
  TOTAL_LIQUIDITY_DATAMART_2 <- inner_join(TOTAL_LIQUIDITY_DATAMART_2,add)
  TOTAL_LIQUIDITY_DATAMART_2$sales_Percent <- NULL
  TOTAL_LIQUIDITY_DATAMART_2$LIQ_MINI_2 <- (TOTAL_LIQUIDITY_DATAMART_2$COEF_1*0.3+TOTAL_LIQUIDITY_DATAMART_2$COEF_2*0.7)*10+as.integer(0.5*100)
  TOTAL_LIQUIDITY_DATAMART_3$COEF_1 <-  (range01(TOTAL_LIQUIDITY_DATAMART_3$Count))
  TOTAL_LIQUIDITY_DATAMART_3$COEF_2 <-  abs((range01(TOTAL_LIQUIDITY_DATAMART_3$M_SALES_DAYS))-1)
  TOTAL_LIQUIDITY_DATAMART_3 <- as.data.frame(TOTAL_LIQUIDITY_DATAMART_3)
  TOTAL_LIQUIDITY_DATAMART_3$LIQ_MINI_2 <- (TOTAL_LIQUIDITY_DATAMART_3$COEF_1*0.3+TOTAL_LIQUIDITY_DATAMART_3$COEF_2*0.7)*10+as.integer(0.3*100)
  TOTAL_LIQUIDITY_DATAMART_1$Count_type <- "good"
  TOTAL_LIQUIDITY_DATAMART_2$Count_type <- "medium"
  TOTAL_LIQUIDITY_DATAMART_3$Count_type <- "bad"
  qplot(TOTAL_LIQUIDITY_DATAMART_1$M_SALES_DAYS,TOTAL_LIQUIDITY_DATAMART_1$LIQ_MINI_2)
  TOTAL <- rbind(TOTAL_LIQUIDITY_DATAMART_1,TOTAL_LIQUIDITY_DATAMART_2,TOTAL_LIQUIDITY_DATAMART_3)
  TOTAL$CLASS <- ifelse(TOTAL$Mean_price<2200000,"less than 2m",
                        ifelse(TOTAL$Mean_price<5500000,"less than 5m",
                               ifelse(TOTAL$Mean_price<10500000,"less than 10m","more than 10m")))
  write.csv(TOTAL,"BI.csv",row.names = F)
  
  TOTAL %>% group_by(Count_type) %>% summarise(Correlation = cor(M_SALES_DAYS,LIQ_MINI_2))
  
  Diinislam1 <- TOTAL
  
  TOTAL_LIQUIDITY_DATAMART <- Final %>% group_by(Marka,Model,Year,Tip_kuzova) %>% 
    summarise(Mean_price = mean(cena),
              Count = n(),
              M_3_Q = mean(Third_quartile),
              M_1_Q = mean(First_quantile),
              M_SALES_DAYS = mean(Prodan_days)) %>% arrange(-Count)
  TOTAL_LIQUIDITY_DATAMART$COEF_1 <-  (range01(TOTAL_LIQUIDITY_DATAMART$Count))
  TOTAL_LIQUIDITY_DATAMART$COEF_2 <-  abs((range01(TOTAL_LIQUIDITY_DATAMART$M_SALES_DAYS))-1)
  TOTAL_LIQUIDITY_DATAMART <- as.data.frame(TOTAL_LIQUIDITY_DATAMART)
  TOTAL_LIQUIDITY_DATAMART$LIQ_MINI_2 <- ((TOTAL_LIQUIDITY_DATAMART$COEF_1*0.3+TOTAL_LIQUIDITY_DATAMART$COEF_2*0.7)*10)-5
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
  TOTAL_LIQUIDITY_DATAMART$LIQ_MINI_2 <- ((TOTAL_LIQUIDITY_DATAMART$COEF_1*0.3+TOTAL_LIQUIDITY_DATAMART$COEF_2*0.7)*10)-5
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
  TOTAL_LIQUIDITY_DATAMART$LIQ_MINI_2 <- ((TOTAL_LIQUIDITY_DATAMART$COEF_1*0.3+TOTAL_LIQUIDITY_DATAMART$COEF_2*0.7)*10)-5
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
  TOTAL_LIQUIDITY_DATAMART$LIQ_MINI_2 <- ((TOTAL_LIQUIDITY_DATAMART$COEF_1*0.3+TOTAL_LIQUIDITY_DATAMART$COEF_2*0.7)*10)-5
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
  TOTAL_LIQUIDITY_DATAMART$LIQ_MINI_2 <- ((TOTAL_LIQUIDITY_DATAMART$COEF_1*0.3+TOTAL_LIQUIDITY_DATAMART$COEF_2*0.7)*10)-5
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
  TOTAL_LIQUIDITY_DATAMART$LIQ_MINI_2 <- ((TOTAL_LIQUIDITY_DATAMART$COEF_1*0.3+TOTAL_LIQUIDITY_DATAMART$COEF_2*0.7)*10)-5
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
  data <- datamart[,c(1,2,3,4,6,8,10,12,14,5,7,9,11,13,15,16,17,18)]
  data$Liquidity <- data$LIQ_MINI_2+data$CVET_coef+data$DVIGATEL_coef+data$VOLUME_coef+data$RUL_coef+data$KPP_coef+data$KUZOV_coef
  
  df_dictionary <- df %>% group_by(Marka,Model,Year,Tip_kuzova,dvigatel,volume,KPP) %>% summarise(N=n())
  df_dictionary <- df_dictionary[df_dictionary$N>2,]
  data <- as.data.frame(data)
  tochka <- inner_join(data,df_dictionary)
  tocka <- tochka[tochka$Tip_kuzova!=c("лимузин"),]
  tocka <- tocka[tochka$Tip_kuzova!=c("микроавтобус"),]
  tocka <- tocka[tochka$Tip_kuzova!=c("фургон"),]
  write.csv(tocka,"BIG/BI_complete.csv",row.names = F)
  
  df <- fread("BIG/df.csv")
  Final <- fread("BIG/PREDICTED_VALUES.csv",stringsAsFactors = T)
  TOTAL_LIQUIDITY_DATAMART <- Final %>% group_by(Marka,Model,Year,Tip_kuzova,KPP,raspolozhenie_rulya,volume,dvigatel,cvet) %>% 
    summarise(Mean_price = mean(cena),
              Count = n(),
              M_3_Q = mean(Third_quartile),
              M_1_Q = mean(First_quantile),
              M_SALES_DAYS = mean(Prodan_days))
  TOTAL_LIQUIDITY_DATAMART_1 <- TOTAL_LIQUIDITY_DATAMART[TOTAL_LIQUIDITY_DATAMART$Count>=15,]
  TOTAL <- anti_join(TOTAL_LIQUIDITY_DATAMART,TOTAL_LIQUIDITY_DATAMART_1)
  TOTAL_LIQUIDITY_DATAMART_2 <- TOTAL[TOTAL$Count>=6,]
  TOTKA <- anti_join(TOTAL,TOTAL_LIQUIDITY_DATAMART_2)
  TOTAL_LIQUIDITY_DATAMART_3 <- TOTKA
  rm(TOTAL,TOTKA)
  rm(TOTAL_LIQUIDITY_DATAMART)
  range01 <- function(x, ...){(x - min(x, ...)) / (max(x, ...) - min(x, ...))}
  TOTAL_LIQUIDITY_DATAMART_1$COEF_1 <-  (range01(TOTAL_LIQUIDITY_DATAMART_1$Count))
  TOTAL_LIQUIDITY_DATAMART_1$COEF_2 <-  abs((range01(TOTAL_LIQUIDITY_DATAMART_1$M_SALES_DAYS))-1)
  additional_data <- df %>% 
    group_by(Marka,Model,Year,KPP,Tip_kuzova,raspolozhenie_rulya,volume,dvigatel,cvet) %>% 
    summarise(Count = n())
  additional_data1 <- df %>% filter(Sell_classs=="Продан") %>% 
    group_by(Marka,Model,Year,KPP,Tip_kuzova,raspolozhenie_rulya,volume,dvigatel,cvet) %>% 
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
  TOTAL_LIQUIDITY_DATAMART_1 <- TOTAL_LIQUIDITY_DATAMART_1 %>% mutate_if(is.character,as.factor)
  TOTAL_LIQUIDITY_DATAMART_1 <- inner_join(TOTAL_LIQUIDITY_DATAMART_1,add)
  TOTAL_LIQUIDITY_DATAMART_1$LIQ_MINI_2 <- (TOTAL_LIQUIDITY_DATAMART_1$COEF_1*0.25+TOTAL_LIQUIDITY_DATAMART_1$COEF_2*0.6+TOTAL_LIQUIDITY_DATAMART_1$sales_Percent*0.05)*100
  TOTAL_LIQUIDITY_DATAMART_2$COEF_1 <-  (range01(TOTAL_LIQUIDITY_DATAMART_2$Count))
  TOTAL_LIQUIDITY_DATAMART_2$COEF_2 <-  abs((range01(TOTAL_LIQUIDITY_DATAMART_2$M_SALES_DAYS))-1)
  TOTAL_LIQUIDITY_DATAMART_2 <- as.data.frame(TOTAL_LIQUIDITY_DATAMART_2)
  TOTAL_LIQUIDITY_DATAMART_2 <- TOTAL_LIQUIDITY_DATAMART_2 %>% mutate_if(is.character,as.factor)
  TOTAL_LIQUIDITY_DATAMART_2 <- inner_join(TOTAL_LIQUIDITY_DATAMART_2,add)
  TOTAL_LIQUIDITY_DATAMART_2$LIQ_MINI_2 <- (TOTAL_LIQUIDITY_DATAMART_2$COEF_1*0.25+TOTAL_LIQUIDITY_DATAMART_2$COEF_2*0.6+TOTAL_LIQUIDITY_DATAMART_2$sales_Percent*0.05)*100
  TOTAL_LIQUIDITY_DATAMART_3$COEF_1 <-  (range01(TOTAL_LIQUIDITY_DATAMART_3$Count))
  TOTAL_LIQUIDITY_DATAMART_3$COEF_2 <-  abs((range01(TOTAL_LIQUIDITY_DATAMART_3$M_SALES_DAYS))-1)
  TOTAL_LIQUIDITY_DATAMART_3 <- as.data.frame(TOTAL_LIQUIDITY_DATAMART_3)
  TOTAL_LIQUIDITY_DATAMART_3 <- TOTAL_LIQUIDITY_DATAMART_3 %>% mutate_if(is.character,as.factor)
  TOTAL_LIQUIDITY_DATAMART_3 <- inner_join(TOTAL_LIQUIDITY_DATAMART_3,add)
  TOTAL_LIQUIDITY_DATAMART_3$LIQ_MINI_2 <- (TOTAL_LIQUIDITY_DATAMART_3$COEF_1*0.25+TOTAL_LIQUIDITY_DATAMART_3$COEF_2*0.6+TOTAL_LIQUIDITY_DATAMART_3$sales_Percent*0.05)*100
  TOTAL_LIQUIDITY_DATAMART_1$Count_type <- "good"
  TOTAL_LIQUIDITY_DATAMART_2$Count_type <- "medium"
  TOTAL_LIQUIDITY_DATAMART_3$Count_type <- "bad"
  TOTAL_LIQUIDITY_DATAMART_1$TOTAL_LIQ <- TOTAL_LIQUIDITY_DATAMART_1$LIQ_MINI_2+30
  TOTAL_LIQUIDITY_DATAMART_2$TOTAL_LIQ <- TOTAL_LIQUIDITY_DATAMART_2$LIQ_MINI_2-10
  TOTAL_LIQUIDITY_DATAMART_3$TOTAL_LIQ <- TOTAL_LIQUIDITY_DATAMART_3$LIQ_MINI_2-30
  TOTAL <- rbind(TOTAL_LIQUIDITY_DATAMART_1,TOTAL_LIQUIDITY_DATAMART_2,TOTAL_LIQUIDITY_DATAMART_3)
  Diinislam1 <- TOTAL
  D <- Diinislam1 %>% group_by(Marka,Model) %>% top_n(10,TOTAL_LIQ)
  TOTAL_LIQUIDITY_DATAMART <- Final %>% group_by(Marka) %>% 
    summarise(Mean_price = mean(cena),
              Count = n(),
              M_3_Q = mean(Third_quartile),
              M_1_Q = mean(First_quantile),
              M_SALES_DAYS = mean(Prodan_days)) %>% arrange(-Count)
  r1 <- TOTAL_LIQUIDITY_DATAMART[round(nrow(TOTAL_LIQUIDITY_DATAMART)*0.2),]$Count
  r1 <- round(round(r1)/1000)*1000
  r2 <- TOTAL_LIQUIDITY_DATAMART[round(nrow(TOTAL_LIQUIDITY_DATAMART)*0.5),]$Count
  r2 <- round(round(r2)/10)*10
  TOTAL_LIQUIDITY_DATAMART_1 <- TOTAL_LIQUIDITY_DATAMART[TOTAL_LIQUIDITY_DATAMART$Count>=r1,]
  TOTAL <- anti_join(TOTAL_LIQUIDITY_DATAMART,TOTAL_LIQUIDITY_DATAMART_1)
  TOTAL_LIQUIDITY_DATAMART_2 <- TOTAL[TOTAL$Count>=r2,]
  TOTKA <- anti_join(TOTAL,TOTAL_LIQUIDITY_DATAMART_2)
  TOTAL_LIQUIDITY_DATAMART_3 <- TOTKA
  rm(TOTAL,TOTKA)
  rm(TOTAL_LIQUIDITY_DATAMART)
  range01 <- function(x, ...){(x - min(x, ...)) / (max(x, ...) - min(x, ...))}
  TOTAL_LIQUIDITY_DATAMART_1$COEF_1 <-  (range01(TOTAL_LIQUIDITY_DATAMART_1$Count))
  TOTAL_LIQUIDITY_DATAMART_1$COEF_2 <-  abs((range01(TOTAL_LIQUIDITY_DATAMART_1$M_SALES_DAYS))-1)
  additional_data <- df %>% 
    group_by(Marka) %>% 
    summarise(Count = n())
  additional_data1 <- df %>% filter(Sell_classs=="Продан") %>% 
    group_by(Marka) %>% 
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
  TOTAL_LIQUIDITY_DATAMART_1 <- TOTAL_LIQUIDITY_DATAMART_1 %>% mutate_if(is.character,as.factor)
  TOTAL_LIQUIDITY_DATAMART_1 <- inner_join(TOTAL_LIQUIDITY_DATAMART_1,add)
  TOTAL_LIQUIDITY_DATAMART_1$LIQ_MINI_2 <- (TOTAL_LIQUIDITY_DATAMART_1$COEF_1*0.4+TOTAL_LIQUIDITY_DATAMART_1$COEF_2*0.4+TOTAL_LIQUIDITY_DATAMART_1$sales_Percent*0.2)*100
  TOTAL_LIQUIDITY_DATAMART_2$COEF_1 <-  (range01(TOTAL_LIQUIDITY_DATAMART_2$Count))
  TOTAL_LIQUIDITY_DATAMART_2$COEF_2 <-  abs((range01(TOTAL_LIQUIDITY_DATAMART_2$M_SALES_DAYS))-1)
  TOTAL_LIQUIDITY_DATAMART_2 <- as.data.frame(TOTAL_LIQUIDITY_DATAMART_2)
  TOTAL_LIQUIDITY_DATAMART_2 <- TOTAL_LIQUIDITY_DATAMART_2 %>% mutate_if(is.character,as.factor)
  TOTAL_LIQUIDITY_DATAMART_2 <- inner_join(TOTAL_LIQUIDITY_DATAMART_2,add)
  TOTAL_LIQUIDITY_DATAMART_2$LIQ_MINI_2 <- (TOTAL_LIQUIDITY_DATAMART_2$COEF_1*0.2+TOTAL_LIQUIDITY_DATAMART_2$COEF_2*0.6+TOTAL_LIQUIDITY_DATAMART_2$sales_Percent*0.1)*100
  TOTAL_LIQUIDITY_DATAMART_3$COEF_1 <-  (range01(TOTAL_LIQUIDITY_DATAMART_3$Count))
  TOTAL_LIQUIDITY_DATAMART_3$COEF_2 <-  abs((range01(TOTAL_LIQUIDITY_DATAMART_3$M_SALES_DAYS))-1)
  TOTAL_LIQUIDITY_DATAMART_3 <- as.data.frame(TOTAL_LIQUIDITY_DATAMART_3)
  TOTAL_LIQUIDITY_DATAMART_3 <- TOTAL_LIQUIDITY_DATAMART_3 %>% mutate_if(is.character,as.factor)
  TOTAL_LIQUIDITY_DATAMART_3 <- inner_join(TOTAL_LIQUIDITY_DATAMART_3,add)
  TOTAL_LIQUIDITY_DATAMART_3$LIQ_MINI_2 <- (TOTAL_LIQUIDITY_DATAMART_3$COEF_1*0.2+TOTAL_LIQUIDITY_DATAMART_3$COEF_2*0.6+TOTAL_LIQUIDITY_DATAMART_3$sales_Percent*0.1)*100
  TOTAL_LIQUIDITY_DATAMART_1$Count_type <- "good"
  TOTAL_LIQUIDITY_DATAMART_2$Count_type <- "medium"
  TOTAL_LIQUIDITY_DATAMART_3$Count_type <- "bad"
  TOTAL_LIQUIDITY_DATAMART_1$TOTAL_LIQ <- TOTAL_LIQUIDITY_DATAMART_1$LIQ_MINI_2
  TOTAL_LIQUIDITY_DATAMART_2$TOTAL_LIQ <- TOTAL_LIQUIDITY_DATAMART_2$LIQ_MINI_2-40
  TOTAL_LIQUIDITY_DATAMART_3$TOTAL_LIQ <- TOTAL_LIQUIDITY_DATAMART_3$LIQ_MINI_2-55
  TOTAL <- rbind(TOTAL_LIQUIDITY_DATAMART_1,TOTAL_LIQUIDITY_DATAMART_2,TOTAL_LIQUIDITY_DATAMART_3)
  TOTAL$TOTAL_LIQ_Range <- range01(TOTAL$TOTAL_LIQ)*30-15
  Diinislam2 <- TOTAL
  TOTAL_LIQUIDITY_DATAMART <- Final %>% group_by(Marka,Model) %>% 
    summarise(Mean_price = mean(cena),
              Count = n(),
              M_3_Q = mean(Third_quartile),
              M_1_Q = mean(First_quantile),
              M_SALES_DAYS = mean(Prodan_days)) %>% arrange(-Count)
  r1 <- TOTAL_LIQUIDITY_DATAMART[round(nrow(TOTAL_LIQUIDITY_DATAMART)*0.2),]$Count
  r1 <- round(round(r1)/10)*10
  r2 <- TOTAL_LIQUIDITY_DATAMART[round(nrow(TOTAL_LIQUIDITY_DATAMART)*0.5),]$Count
  r2 <- round(r2)
  TOTAL_LIQUIDITY_DATAMART_1 <- TOTAL_LIQUIDITY_DATAMART[TOTAL_LIQUIDITY_DATAMART$Count>=r1,]
  TOTAL <- anti_join(TOTAL_LIQUIDITY_DATAMART,TOTAL_LIQUIDITY_DATAMART_1)
  TOTAL_LIQUIDITY_DATAMART_2 <- TOTAL[TOTAL$Count>=r2,]
  TOTKA <- anti_join(TOTAL,TOTAL_LIQUIDITY_DATAMART_2)
  TOTAL_LIQUIDITY_DATAMART_3 <- TOTKA
  rm(TOTAL,TOTKA)
  rm(TOTAL_LIQUIDITY_DATAMART)
  range01 <- function(x, ...){(x - min(x, ...)) / (max(x, ...) - min(x, ...))}
  TOTAL_LIQUIDITY_DATAMART_1$COEF_1 <-  (range01(TOTAL_LIQUIDITY_DATAMART_1$Count))
  TOTAL_LIQUIDITY_DATAMART_1$COEF_2 <-  abs((range01(TOTAL_LIQUIDITY_DATAMART_1$M_SALES_DAYS))-1)
  additional_data <- df %>% 
    group_by(Marka,Model) %>% 
    summarise(Count = n())
  additional_data1 <- df %>% filter(Sell_classs=="Продан") %>% 
    group_by(Marka,Model) %>% 
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
  TOTAL_LIQUIDITY_DATAMART_1 <- TOTAL_LIQUIDITY_DATAMART_1 %>% mutate_if(is.character,as.factor)
  TOTAL_LIQUIDITY_DATAMART_1 <- inner_join(TOTAL_LIQUIDITY_DATAMART_1,add)
  TOTAL_LIQUIDITY_DATAMART_1$LIQ_MINI_2 <- (TOTAL_LIQUIDITY_DATAMART_1$COEF_1*0.4+TOTAL_LIQUIDITY_DATAMART_1$COEF_2*0.4+TOTAL_LIQUIDITY_DATAMART_1$sales_Percent*0.2)*100
  TOTAL_LIQUIDITY_DATAMART_2$COEF_1 <-  (range01(TOTAL_LIQUIDITY_DATAMART_2$Count))
  TOTAL_LIQUIDITY_DATAMART_2$COEF_2 <-  abs((range01(TOTAL_LIQUIDITY_DATAMART_2$M_SALES_DAYS))-1)
  TOTAL_LIQUIDITY_DATAMART_2 <- as.data.frame(TOTAL_LIQUIDITY_DATAMART_2)
  TOTAL_LIQUIDITY_DATAMART_2 <- TOTAL_LIQUIDITY_DATAMART_2 %>% mutate_if(is.character,as.factor)
  TOTAL_LIQUIDITY_DATAMART_2 <- inner_join(TOTAL_LIQUIDITY_DATAMART_2,add)
  TOTAL_LIQUIDITY_DATAMART_2$LIQ_MINI_2 <- (TOTAL_LIQUIDITY_DATAMART_2$COEF_1*0.2+TOTAL_LIQUIDITY_DATAMART_2$COEF_2*0.6+TOTAL_LIQUIDITY_DATAMART_2$sales_Percent*0.1)*100
  TOTAL_LIQUIDITY_DATAMART_3$COEF_1 <-  (range01(TOTAL_LIQUIDITY_DATAMART_3$Count))
  TOTAL_LIQUIDITY_DATAMART_3$COEF_2 <-  abs((range01(TOTAL_LIQUIDITY_DATAMART_3$M_SALES_DAYS))-1)
  TOTAL_LIQUIDITY_DATAMART_3 <- as.data.frame(TOTAL_LIQUIDITY_DATAMART_3)
  TOTAL_LIQUIDITY_DATAMART_3 <- TOTAL_LIQUIDITY_DATAMART_3 %>% mutate_if(is.character,as.factor)
  TOTAL_LIQUIDITY_DATAMART_3 <- inner_join(TOTAL_LIQUIDITY_DATAMART_3,add)
  TOTAL_LIQUIDITY_DATAMART_3$LIQ_MINI_2 <- (TOTAL_LIQUIDITY_DATAMART_3$COEF_1*0.2+TOTAL_LIQUIDITY_DATAMART_3$COEF_2*0.6+TOTAL_LIQUIDITY_DATAMART_3$sales_Percent*0.1)*100
  TOTAL_LIQUIDITY_DATAMART_1$Count_type <- "good"
  TOTAL_LIQUIDITY_DATAMART_2$Count_type <- "medium"
  TOTAL_LIQUIDITY_DATAMART_3$Count_type <- "bad"
  TOTAL_LIQUIDITY_DATAMART_1$TOTAL_LIQ <- TOTAL_LIQUIDITY_DATAMART_1$LIQ_MINI_2
  TOTAL_LIQUIDITY_DATAMART_2$TOTAL_LIQ <- TOTAL_LIQUIDITY_DATAMART_2$LIQ_MINI_2-40
  TOTAL_LIQUIDITY_DATAMART_3$TOTAL_LIQ <- TOTAL_LIQUIDITY_DATAMART_3$LIQ_MINI_2-55
  TOTAL <- rbind(TOTAL_LIQUIDITY_DATAMART_1,TOTAL_LIQUIDITY_DATAMART_2,TOTAL_LIQUIDITY_DATAMART_3)
  TOTAL$TOTAL_LIQ_Range <- range01(TOTAL$TOTAL_LIQ)*30-15
  Diinislam3 <- TOTAL
  TOTAL_LIQUIDITY_DATAMART <- Final %>% group_by(Marka,Model,Year) %>% 
    summarise(Mean_price = mean(cena),
              Count = n(),
              M_3_Q = mean(Third_quartile),
              M_1_Q = mean(First_quantile),
              M_SALES_DAYS = mean(Prodan_days)) %>% arrange(-Count)
  r1 <- TOTAL_LIQUIDITY_DATAMART[round(nrow(TOTAL_LIQUIDITY_DATAMART)*0.2),]$Count
  r1 <- round(r1)
  r2 <- TOTAL_LIQUIDITY_DATAMART[round(nrow(TOTAL_LIQUIDITY_DATAMART)*0.5),]$Count
  r2 <- round(r2)
  TOTAL_LIQUIDITY_DATAMART_1 <- TOTAL_LIQUIDITY_DATAMART[TOTAL_LIQUIDITY_DATAMART$Count>=r1,]
  TOTAL <- anti_join(TOTAL_LIQUIDITY_DATAMART,TOTAL_LIQUIDITY_DATAMART_1)
  TOTAL_LIQUIDITY_DATAMART_2 <- TOTAL[TOTAL$Count>=r2,]
  TOTKA <- anti_join(TOTAL,TOTAL_LIQUIDITY_DATAMART_2)
  TOTAL_LIQUIDITY_DATAMART_3 <- TOTKA
  rm(TOTAL,TOTKA)
  rm(TOTAL_LIQUIDITY_DATAMART)
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
  TOTAL_LIQUIDITY_DATAMART_1 <- TOTAL_LIQUIDITY_DATAMART_1 %>% mutate_if(is.character,as.factor)
  TOTAL_LIQUIDITY_DATAMART_1 <- inner_join(TOTAL_LIQUIDITY_DATAMART_1,add)
  TOTAL_LIQUIDITY_DATAMART_1$LIQ_MINI_2 <- (TOTAL_LIQUIDITY_DATAMART_1$COEF_1*0.4+TOTAL_LIQUIDITY_DATAMART_1$COEF_2*0.4+TOTAL_LIQUIDITY_DATAMART_1$sales_Percent*0.2)*100
  TOTAL_LIQUIDITY_DATAMART_2$COEF_1 <-  (range01(TOTAL_LIQUIDITY_DATAMART_2$Count))
  TOTAL_LIQUIDITY_DATAMART_2$COEF_2 <-  abs((range01(TOTAL_LIQUIDITY_DATAMART_2$M_SALES_DAYS))-1)
  TOTAL_LIQUIDITY_DATAMART_2 <- as.data.frame(TOTAL_LIQUIDITY_DATAMART_2)
  TOTAL_LIQUIDITY_DATAMART_2 <- TOTAL_LIQUIDITY_DATAMART_2 %>% mutate_if(is.character,as.factor)
  TOTAL_LIQUIDITY_DATAMART_2 <- inner_join(TOTAL_LIQUIDITY_DATAMART_2,add)
  TOTAL_LIQUIDITY_DATAMART_2$LIQ_MINI_2 <- (TOTAL_LIQUIDITY_DATAMART_2$COEF_1*0.2+TOTAL_LIQUIDITY_DATAMART_2$COEF_2*0.6+TOTAL_LIQUIDITY_DATAMART_2$sales_Percent*0.1)*100
  TOTAL_LIQUIDITY_DATAMART_3$COEF_1 <-  (range01(TOTAL_LIQUIDITY_DATAMART_3$Count))
  TOTAL_LIQUIDITY_DATAMART_3$COEF_2 <-  abs((range01(TOTAL_LIQUIDITY_DATAMART_3$M_SALES_DAYS))-1)
  TOTAL_LIQUIDITY_DATAMART_3 <- as.data.frame(TOTAL_LIQUIDITY_DATAMART_3)
  TOTAL_LIQUIDITY_DATAMART_3 <- TOTAL_LIQUIDITY_DATAMART_3 %>% mutate_if(is.character,as.factor)
  TOTAL_LIQUIDITY_DATAMART_3 <- inner_join(TOTAL_LIQUIDITY_DATAMART_3,add)
  TOTAL_LIQUIDITY_DATAMART_3$LIQ_MINI_2 <- (TOTAL_LIQUIDITY_DATAMART_3$COEF_1*0.2+TOTAL_LIQUIDITY_DATAMART_3$COEF_2*0.6+TOTAL_LIQUIDITY_DATAMART_3$sales_Percent*0.1)*100
  TOTAL_LIQUIDITY_DATAMART_1$Count_type <- "good"
  TOTAL_LIQUIDITY_DATAMART_2$Count_type <- "medium"
  TOTAL_LIQUIDITY_DATAMART_3$Count_type <- "bad"
  TOTAL_LIQUIDITY_DATAMART_1$TOTAL_LIQ <- TOTAL_LIQUIDITY_DATAMART_1$LIQ_MINI_2
  TOTAL_LIQUIDITY_DATAMART_2$TOTAL_LIQ <- TOTAL_LIQUIDITY_DATAMART_2$LIQ_MINI_2-40
  TOTAL_LIQUIDITY_DATAMART_3$TOTAL_LIQ <- TOTAL_LIQUIDITY_DATAMART_3$LIQ_MINI_2-55
  TOTAL <- rbind(TOTAL_LIQUIDITY_DATAMART_1,TOTAL_LIQUIDITY_DATAMART_2,TOTAL_LIQUIDITY_DATAMART_3)
  TOTAL$TOTAL_LIQ_Range <- range01(TOTAL$TOTAL_LIQ)*30-15
  Diinislam4 <- TOTAL
  Diinislam4 <- Diinislam4[,c(1:3,15)]
  colnames(Diinislam4) <- c("Marka",
                            "Model",
                            "Year",
                            "COEF_MMY")
  Diinislam3 <- Diinislam3[,c(1:2,14)]
  colnames(Diinislam3) <- c("Marka",
                            "Model",
                            "COEF_MM")
  Diinislam2 <- Diinislam2[,c(1,13)]
  colnames(Diinislam2) <- c("Marka",
                            "COEF_M")
  Diinislam4 <- inner_join(Diinislam4,Diinislam3)
  Diinislam4 <- inner_join(Diinislam4,Diinislam2)
  Diinislam1 <- inner_join(Diinislam1,Diinislam4)
  Diinislam1$NEW_Liq <- Diinislam1$TOTAL_LIQ+Diinislam1$COEF_MMY+Diinislam1$COEF_MM+Diinislam1$COEF_M
  write.csv(Diinislam1,"BIG/liquidity.csv",row.names = F)
  write.csv(Diinislam2,"BIG/liquidity1.csv",row.names = F)
  write.csv(Diinislam3,"BIG/liquidity2.csv",row.names = F)
  write.csv(Diinislam4,"BIG/liquidity3.csv",row.names = F)
  Finka <- inner_join(Final,Diinislam1)
  colnames(Finka)
  Finka$views <- NULL
  Finka$M_1_Q <- NULL
  Finka$M_3_Q <- NULL
  Finka$Prodan_1 <- NULL
  Finka$Prodan_3 <- NULL
  Finka$Count <- NULL
  Finka$count <- NULL
  Finka$First_q_year <- NULL
  Finka$CLASSS <- NULL
  Finka$year_mean <- NULL
  Finka$Second_q_year <- NULL
  Finka$COEF_1 <- NULL
  Finka$COEF_2 <- NULL
  Finka$LIQ_MINI_2 <- NULL
  Finka$Prodan_cena <- NULL
  Finka <- Finka %>% arrange(-NEW_Liq)
  Finka <- Finka[Finka$anomaly_by_price!="too_expensive",]
  Finka <- Finka[Finka$anomaly_by_year=='normal',]
  Finka <- Finka[Finka$predictable=='can predict',]
  Finka$anomaly_by_year <- NULL
  Finka$predictable <- NULL
  Finka <- Finka %>% mutate_if(is.character,as.factor)
  Finn <- Finka %>% group_by(Marka,Model) %>% top_n(15,TOTAL_LIQ)
  
  Finn$Count_type
  qplot(Finn$Prodan_days,Finn$TOTAL_LIQ,color=Finn$Count_type)
  Finn %>% group_by(Count_type) %>% summarise(cor(Finn$Prodan_days,Finn$TOTAL_LIQ))
  
  
  
  Final <- fread("BIG/PREDICTED_VALUES.csv",stringsAsFactors = T)
  BI <- fread("BIG/BI_complete.csv",stringsAsFactors = T)
  df <- fread("BIG/df.csv",stringsAsFactors = T)
  reliz <- inner_join(Final,BI)
  gc()
  
  colnames(reliz)
  reliz$year_mean <- NULL
  reliz$First_q_year <- NULL
  reliz$Second_q_year <- NULL
  reliz$Prodan_cena <- NULL
  reliz$CLASSS <- NULL
  reliz$Third_quartile <- NULL
  reliz$First_quantile <- NULL
  reliz$Marka_model_n <- NULL
  reliz$Prodan_3 <- NULL
  reliz$Prodan_1 <- NULL
  reliz$Prodan_days <- NULL
  reliz$N <- NULL
  #reliz$kod_statusa <- NULL
  data <- reliz
  length(unique(data$id_car_in_kolesa)) # adverts
  data$error <- (data$cena-data$prognoz)/(data$cena+data$prognoz)
  summary(data$error)
  data <- data[data$error>-0.3,]
  data <- data[data$error<0.3,]
  length(unique(data$id_car_in_kolesa)) # adverts
  Lik  <- read.csv("BIG/liquidity.csv")
  Marka <- unique(Lik[,c(1,23)])
  Marka_model <- unique(Lik[,c(1,2,22)])
  Marka_model_YEAR <- unique(Lik[,c(1,2,3,21)])
  data$ADVERT_ORDER <- ifelse(data$error>0.2,-10,
                              ifelse(data$error>0.1,-6,
                                     ifelse(data$error>=0,-5,
                                            ifelse(data$error<-0.2,10,
                                                   ifelse(data$error<-0.1,5,
                                                          ifelse(data$error<=0,2,"NAN"))))))
  table(data$ADVERT_ORDER)
  table((data$Sales_days_group_pred*-6)+15)
  data$Sales_days_group_pred <- (data$Sales_days_group_pred*-6)+15
  data$PREDICTABLE <- ifelse(data$predictable=='can predict',10,-10)
  data$predictable <- NULL
  table(data$PREDICTABLE)
  data$ACTIVITY <- ifelse(data$flag2=='active',10,-10)
  data$flag2 <- NULL
  data$flag1
  data$SCORING <- data$Liquidity + data$Sales_days_group_pred + data$ADVERT_ORDER + data$PREDICTABLE + data$ACTIVITY
  t <- data %>% group_by(Marka,Model) %>% summarise(Lik = mean(Liquidity),
                                                    Scor = mean(SCORING))
  
  
  write.csv(data,"BIG/BI_SCORING.csv",row.names = F)
  
  return(data)
}