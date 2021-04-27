library(data.table)
library(Rtsne)
library(ggplot2)
library(ClusterR)
library(xgboost)
library(hash)

datatestemb <- fread("./project/volume/data/raw/test_emb.csv")
datatest <- fread("./project/volume/data/raw/test_file.csv")
datatrain<- fread("./project/volume/data/raw/training_data.csv")
datatrainemb <- fread("./project/volume/data/raw/training_emb.csv")

train_id <- datatrain$id
datatrain$id <- NULL
trian_text <- datatrain$text
datatrain$text <- NULL

inds <- which(rowSums(datatrain)==0)
datatrain$tranformed <- names(datatrain)[max.col(datatrain)]
datatrain$tranformed[inds] <- NA

subredditsl <- datatrain$tranformed
subreddits <- unique(subredditsl)
h<-hash()
i=0
for (x in subreddits){
  h[[x]]<-i
  i = i + 1
}
label = c()
for (y in subredditsl){
  label <- c(label, h[[y]])
}

xgb.train = xgb.DMatrix(data=as.matrix(datatrainemb), label=as.matrix(label))
xgb.test = xgb.DMatrix(data=as.matrix(datatestemb))

params = list(
  booster="gbtree",
  eta=0.001,
  max_depth=5,
  gamma=3,
  subsample=0.75,
  colsample_bytree=1,
  objective="multi:softprob",
  eval_metric="mlogloss",
  num_class=10
)

# Train the XGBoost classifer
xgb.fit=xgb.train(
  params=params,
  data=xgb.train,
  nrounds=5000,
  nthreads=1,
  early_stopping_rounds=5,
  watchlist=list(val1=xgb.train),
  verbose=1
)
xgb.pred <- predict(xgb.fit,as.matrix(datatestemb),reshape=T)
preds <- data.table(xgb.pred)
colnames(preds) <- subreddits

preds$id <- datatest$id
fwrite(preds, "submit.csv")
