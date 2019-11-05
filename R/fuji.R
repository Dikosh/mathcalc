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
#' @export fuji

#' @import data.table,dplyr,data.table,randomForest,rpart,plotly,readr,readxl,xgboost,Matrix,caret,splitstackshape,stringr,lubridate
#' @import data.table

fuji <- function(df){
  
  df <- df[is.na(df$avtosalon),]
  df <- df[is.na(df$new_avaria_nahodu),]
  df <- df[df$rastamozhen!="Нет",]
  df <- df[is.na(df$V_nalichii),]
  df <- select(df,-c(V_nalichii,vin,avtosalon,new_avaria_nahodu,ne_ispolzuetsa))
  df$probeg <-  substr(df$probeg,1,nchar(df$probeg)-3)
  df$probeg <- as.integer(gsub(" ","",df$probeg))
  min <- 1900
  max <- 390000
  df$to_delete <- ifelse(is.na(df$probeg),55555,df$probeg)
  df <- df[df$to_delete>=min,]
  df <- df[df$to_delete<=max,]
  df$to_delete <- NULL
  df <- df[!is.na(df$Marka),]
  df <- df[!is.na(df$Model),]
  
  df$PID <- 1:nrow(df)
  df <- cSplit(as.data.table(df)[, phone := gsub("[][\"]", "", phone)],
               "phone", ",", "long")
  df$phone <-   str_replace_all(df$phone, "[[:punct:]]", " ")
  
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
  logic3 <- ggplotly(qplot(q$date_difference,q$n,color=q$sale,geom = 'line',xlab = "Без  7 дней + архив"))
  summary(q$date_difference)
  #logic3
  library(tidyr)
  q1 <- yes %>% group_by(date_difference,sale) %>% summarise(n=n()) %>% spread(sale,n)
  q1$sum <- q1$yes+q1$no
  cor.test(q1$sum,q1$yes)
  qplot(q1$sum,q1$yes)
  daud$data_date <- NULL
  daud$Max_date <- NULL
  daud$schet <- NULL
  df <- daud
  df <- df[df$Year>=1990,]
  group_1 <- df %>% group_by(Marka) %>% summarise(Marka_n=n()) %>% arrange(-Marka_n)
  group_1 <- group_1[1:round(nrow(group_1)*0.6),]
  #file.remove("dictionaries/group1.csv")
  write.csv(group_1,"dictionaries/group1.csv",row.names = F)
  group_1 <- read.csv("dictionaries/group1.csv")
  df <- inner_join(df,group_1)
  group_2 <- df %>% group_by(Marka,Model) %>% summarise(Marka_model_n=n()) %>% arrange(-Marka_model_n)
  group_2 <- group_2[group_2$Marka_model_n>=15,]
  write.csv(group_2,"dictionaries/group2.csv",row.names = F)
  df <- inner_join(df,group_2)
  df$phone <- NULL
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
  write.csv(sales_to_train,"SALES_TO_TRAIN.csv",row.names = F)
  
  df <- fread("initial_df.csv")
  
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
  train <- train[train$Year>=1990,]
  train <- train %>% mutate_if(is.character,as.factor)
  write.csv(select(train,id_car_in_kolesa),"mini_id_checker.csv",row.names = F)
  sales_to_train <- fread("SALES_TO_TRAIN.csv")
  train <- inner_join(train,sales_to_train)
  train <- na.omit(train)
  checkers <- select(df,c(id_car_in_kolesa,date_difference,views,kod_statusa))
  train$City <- NULL
  
  write.csv(checkers,"checkers.csv",row.names = F)
  write.csv(train,"TRAIN.CSV",row.names = F)
  
  
  additional_data <- df %>%
    group_by(Marka,Model,Year,KPP,Tip_kuzova,raspolozhenie_rulya,volume,dvigatel) %>%
    summarise(Count = n())
  
  
  additional_data1 <- df %>% filter(Sell_classs=="Продан") %>%
    group_by(Marka,Model,Year,KPP,Tip_kuzova,raspolozhenie_rulya,volume,dvigatel) %>%
    summarise(Sales = n())
  str(df)
  df <- df %>% mutate_if(is.character,as.factor)
  write.csv(df,"df.csv",row.names = F)
  
  
  train <- fread("TRAIN.CSV",stringsAsFactors = T)
  
  IDs <- select(train,id_car_in_kolesa)
  train$id_car_in_kolesa <- NULL
  
  
  index <- createDataPartition(train$cena,p=0.7,list = F)
  tr <- train[index,]
  ts <- train[-index,]
  
  dtrain <- data.matrix(select(tr,-c(cena)))
  ctest <- data.matrix(select(ts,-c(cena)))
  
  dtrain <- xgb.DMatrix(data=dtrain,label=tr$cena)
  ctest <- xgb.DMatrix(data=ctest,label=ts$cena)
  
  #bst <- xgb.cv(data = dtrain,
  #              objective='reg:linear',
  #              nrounds = 300,
  #              nfold = 5,
  #              metrics = "rmse",
  #              maximize = FALSE,
  #              verbose = 3,
  #             early_stopping_rounds = 30,
  #             max_depth=7,
  #  colsample_bytree = 0.7
  
  #maximize = FALSE
  #              max_depth=4
  #)
  # with new data 381675.906250+8968.554660
  # withoyt test-rmse:382291.606250+15700.118058
  bst <- xgboost(data = dtrain,
                 objective='reg:linear',
                 nrounds = 25,
                 #  nfold = 5,
                 metrics = "rmse",
                 maximize = FALSE,
                 verbose = 0,
                 #early_stopping_rounds = 30,
                 max_depth=7)
  
  xgbImp1 <- xgb.importance(model = bst)
  xgbImp1 <- xgbImp1 %>% mutate(rank = dense_rank(desc(Gain)))
  
  pred <- predict(bst,ctest)
  ggplot(data=xgbImp1[which(xgbImp1$rank <= 20),], aes(x = reorder(Feature, -Gain), y = Gain)) +
    geom_bar(stat="identity") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(title = "XG Boosted Feature Importance (Top 20)", x = "Features", y = "Information Gain")
  mean(ts$cena-pred)
  qplot(ts$cena-pred)
  ts$prognoz <- pred
  
  summary(ts$cena-ts$prognoz)
  
  ts$error <- (ts$cena-ts$prognoz)/(ts$cena+ts$prognoz)
  ts$diff <- ts$cena - ts$prognoz
  qplot(ts$error)
  summary(ts$error)
  ts$prognoz <- abs(ts$prognoz)
  qplot(ts$prognoz,ts$cena,color = ts$anomaly_by_price)
  
  
  xgb.save(bst, 'xgb.model')
  bst <- xgb.load('xgb.model')
  
  Ids_test <- IDs[-index,]
  ts$id_car_in_kolesa <- Ids_test
  
  
  checkers <- fread("checkers.csv")
  #checkers <- select(df,c(id_car_in_kolesa,date_difference,views,kod_statusa))
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
  colnames(Final)
  l = list(xgb_model = xgb_model,bst = bst,group_1 = group_1)
  return(l)
}
