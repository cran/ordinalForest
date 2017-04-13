#' Prediction using ordinal forest objects
#'
#' Prediction of test data using ordinal forest.
#'
#' @param object object of class \code{ordfor}. See function \code{\link{ordfor}}.
#' @param newdata data.frame. Data frame containing new data.
#' @param ... further arguments passed to or from other methods.
#'
#' @return
#' \code{predict.ordfor} returns an object of class \code{ordforpred}.
#' An object of class "\code{ordforpred}" is a list containing the following components: 
#' \item{ypred}{ vector of length \code{nrow(newdata)}. Factor-valued test data predictions. }
#' \item{yforestpredmetric}{ vector of length \code{nrow(newdata)}. Numeric test data predictions: Result of applying the regression forest \code{forestfinal} returned by \code{\link{ordfor}}. }
#'
#' @examples
#' data(hearth)
#'
#' set.seed(123)
#' trainind <- sort(sample(1:nrow(hearth), size=floor(nrow(hearth)*(1/2))))
#' testind <- setdiff(1:nrow(hearth), trainind)
#'
#' datatrain <- hearth[trainind,]
#' datatest <- hearth[testind,]
#'
#' ordforres <- ordfor(depvar="Class", data=datatrain, ndiv=80, nbest=5)
#' # NOTE: ndiv=80 is not enough!! In practice, ndiv=1000 (default value) or a higher
#' # number should be used.
#'
#' preds <- predict(ordforres, newdata=datatest)
#' preds
#'  
#' table(data.frame(true_values=datatest$Class, predictions=preds$ypred))
#' 
#' par(mfrow=c(1,2)) 
#' plot(preds$yforestpredmetric, as.numeric(preds$ypred))
#' plot(pnorm(preds$yforestpredmetric), as.numeric(preds$ypred), xlim=c(0,1))
#' par(mfrow=c(1,1))
#'
#' @importFrom stats predict qnorm runif
#' 
#' @export
predict.ordfor <-
function(object, newdata, ...) {
  
  # object <- ordres; newdata <- dat[testind,]
  
  # Ensure that 'object' is of class 'ordfor':
  if (!inherits(object, "ordfor"))
    stop("object not of class ordfor")
  
  
  # Extract some information from 'object':
  
  nforests <- length(object$perfmeasurevalues)
  ntreeperdiv <- object$ntree/nforests
  
  classes <- object$classes
  bordersb <- object$bordersb
  perfmeasurevalues <- object$perfmeasurevalues
  
  J <- length(classes)
  
  
  
  yforestpredmetric <- predict(object=object$forestfinal, data=newdata)$predictions
  
  ynumpred <- sapply(yforestpredmetric, function(x) max(which(x >= qnorm(object$bordersbest))))
  names(ynumpred) <- NULL
  
  # Recode the predictions as a factor variable with levels
  # equal to that of the training data:
  
  ypred <- factor(classes[ynumpred], levels=classes)
  
  
  # Output informations:
  res <- list(ypred=ypred, yforestpredmetric=yforestpredmetric)
  
  class(res) <- "ordforpred"
  
  # Output results:
  return(res)
  
}
