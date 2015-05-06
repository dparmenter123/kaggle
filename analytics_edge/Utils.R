#################################################################
# measurements
#################################################################

library(ROCR)
Utils.GetAuc <- function(pred, col) {
  roc = prediction(pred, col)
  as.numeric(performance(roc, "auc")@y.values)
}

Utils.GetAccuracy <- function(pred, col) {
  return(sum(diag(table(pred, col)))/length(col))
}

Utils.Importance <- function(model) {
  # sort variables by importance
  library(caret)
  library(mlbench)
  imp <- varImp(model, scale=TRUE)
  imp = cbind(imp, names=row.names(imp))
  result = imp[with(imp,  order(-Overall, names)),]
  return(result)  
}

