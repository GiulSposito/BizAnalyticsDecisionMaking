library(xlsx)
library(tidyverse)
library(tidymodels)

# the life, the universe an everything else...
set.seed(42)

# dataset
rawdata <- xlsx::read.xlsx("./appointments/data.xlsx", sheetIndex = 1)

# clean_up
appdata <- rawdata %>% 
  as_tibble() %>% 
  janitor::clean_names() %>% 
  mutate_if(is.character, as.factor)

# training & test data partition
appsplit <- initial_split(appdata)

# basic transformatino
apprecp <- appsplit %>% 
  training() %>% 
  recipe(status ~ ., data=.) %>% 
  update_role(date_id, new_role = "id variable") %>% 
  update_role(status, new_role = "outcome") %>% 
  step_log(lag) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  prep() 

# training & test set
app_tr <- juice(apprecp)
app_ts <- bake(apprecp, testing(appsplit))

# fit a model
app_model <- rand_forest(trees = 100, mode="classification") %>% 
  set_engine("ranger") %>% 
  fit(status ~ . - date_id, data=app_tr)

# eval it
app_pred <- predict(app_model, app_ts) %>%  # class outcome
  bind_cols(predict(app_model, app_ts, type = "prob")) %>% # class probs
  bind_cols(select(app_ts,status)) %>%  # true value
  relocate(status, everything())

# performance
app_pred %>% 
  conf_mat(status, .pred_class) %T>%
  print() %>% 
  summary()

# AUC
app_pred %>% 
  roc_auc(.pred_Arrived, truth=status)

# business case
#
# cost of a reminder call: $5
# benefit of serving the patient: $60
# reverse cancelation: 30%

calcRoi <- function(.threshold=.5, .predData,  benefit=60, cost=5, rev_rate=.3){
  
  cm <- tibble(
    truth=.predData$status, 
    estimate=unique(.predData$status)[as.integer(.predData$.pred_Cancelled>=.threshold)+1]) %>% 
    conf_mat(truth, estimate)
  total_cost    <- sum(cm$table["Cancelled",])*cost
  total_benefit <- cm$table["Cancelled","Cancelled"]*rev_rate*benefit
  roi <- total_benefit - total_cost
  return(roi)
  
}

genConfMatrix <- function(.threshold=.5, .predData,  beneft=60, cost=5, rev_rate=.3){
  tibble(
      truth=.predData$status, 
      estimate=unique(.predData$status)[as.integer(.predData$.pred_Cancelled>=.threshold)+1]
    ) %>%
    conf_mat(truth, estimate) %>% 
    return()
}

getTPFP <- function(cm){
  tibble(
    TP = cm$table["Cancelled","Cancelled"],
    FP = cm$table["Cancelled","Arrived"],
    P  = sum(cm$table["Cancelled",])
  ) %>% 
    mutate(
      TP_rate = TP/P,
      FP_rate = FP/P
    ) %>% 
    return()
}

genConfMatrix(1, app_pred)

thresholds <- seq(0,.8,.01)
rois <- thresholds %>% 
  map_dbl(calcRoi, .predData=app_pred)

tibble(threshold = thresholds,
       roi = rois) %>% 
  ggplot(aes(x=threshold, y=roi)) +
    # geom_point(size=2) +
    geom_line() +
    geom_hline(yintercept = 0, linetype="dashed", color="red") +
    geom_vline(xintercept = 0.5, linetype="dashed", color="darkgrey") +
    scale_x_continuous(breaks=seq(0,.8,.1)) +
    labs(title="Return of Investment", subtitle = "Influence of the Precision [TP/(TP+FP)]")
    
cm %>%
  summary() %>% 
  select(-.estimator)



# cenarios de investimentos
tibble(threshold = seq(0,1,.01)) %>% 
  mutate( cm = map(threshold, genConfMatrix, .predData=app_pred),
          predTrue = map(cm, getTPFP),
          metrics = map(cm, function(.x){
            .x %>% 
              summary() %>% 
              select(-.estimator) %>% 
              pivot_wider(names_from = .metric, values_from = .estimate) %>% 
              return()
          })) %>% 
  unnest(metrics,predTrue) %>% View()
  select(threshold, accuracy, sens, spec, precision, recall, TP_rate, FP_rate) %>%
  pivot_longer(cols = -threshold, names_to = "metric", values_to = "value") %>% 
  ggplot(aes(x=threshold, y=value, color=metric))+
  geom_line() +
  theme_minimal()


ms$.metric

ms$.estimate %>% 
  as_tibble()

