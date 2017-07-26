#' Performance measures based on Youden's J statistic
#' 
#' These performance measures based on Youden's J statistic are used in \code{\link{ordfor}} to measure the performance
#' of the smaller regression forests constructed for the estimation of the optimal score set. They may, however, also be used to measure the precision of
#' predictions on new data or the precision of OOB predictions.
#' 
#' \code{perfm_equal} should be used if it is of interest to predict each class with the same precision irrespective of the numbers of observations representing the individual classes. 
#' Youden's J statistic is calculated with respect to each class ("class j yes" vs. "class j no" (j=1,...,J))
#' and the simple average of the J results taken.
#'
#' \code{perfm_irrespective} should be used if it is of interest to predict all observations with the same precision independent of their categories. 
#' Youden's J statistic is calculated with respect to each class and subsequently a weighted average of these values is taken - with weights 
#' proportional to the number of observations representing the respective classes in the training data.
#'
#' \code{perfm_onecateg} should be used if it is merely relevant that a particular class of the classes of the ordinal target variable
#' is predicted as accurately as possible.
#' This particular class must be provided via the argument \code{categ}.
#' Youden's J statistic is calculated with respect to class \code{categ} ("class \code{categ} yes" vs. "class \code{categ} no").
#'
#' \code{perfm_custom} should be used if there is a particular ranking of the classes with respect to their importance. 
#' Youden's J statistic is calculated with respect to each class. Subsequently a weighted average
#' is taken using user-specified weights (provided via the argument \code{categweights}). Classes with 
#' higher weights are to be predicted more precisely than those with smaller weights.
#'  
#' @param ytest factor. True values of the target variable.
#' @param ytestpred factor. Predicted values of the target variable.
#' @param categ character. Needed in the case of \code{perfm_onecateg}: Category to predict as precisely as possible.
#' @param categweights numeric. Needed in the case of \code{perfm_custom}: Vector of length equal to the number of classes. Class weights - classes with 
#' higher weights are to be predicted more precisely than those with smaller weights.
#' @name perfm
NULL
#> NULL

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
#' ordforres <- ordfor(depvar="Class", data=datatrain, nsets=60, nbest=5)
#' # NOTE: nsets=60 is not enough, because the prediction performance of the resulting 
#' # ordinal forest will be suboptimal!! In practice, nsets=1000 (default value) or a 
#' # number should be used.
#'
#' preds <- predict(ordforres, newdata=datatest)
#'
#'
#' perfm_equal(ytest=datatest$Class, ytestpred=preds$ypred)
#'  
#' perfm_irrespective(ytest=datatest$Class, ytestpred=preds$ypred)
#'  
#' perfm_onecateg(ytest=datatest$Class, ytestpred=preds$ypred, categ="1")
#' perfm_onecateg(ytest=datatest$Class, ytestpred=preds$ypred, categ="2")
#' perfm_onecateg(ytest=datatest$Class, ytestpred=preds$ypred, categ="3")
#' perfm_onecateg(ytest=datatest$Class, ytestpred=preds$ypred, categ="4")
#' perfm_onecateg(ytest=datatest$Class, ytestpred=preds$ypred, categ="5")
#'
#' 
#' perfm_custom(ytest=datatest$Class, ytestpred=preds$ypred, categweights=c(1,2,1,1,1))
#' 
#' 
#' # perfm_equal(), perfm_irrespective(), and perfm_onecateg() are special cases of perfm_custom():
#' 
#' perfm_custom(ytest=datatest$Class, ytestpred=preds$ypred, categweights=c(1,1,1,1,1))
#' perfm_equal(ytest=datatest$Class, ytestpred=preds$ypred)
#' 
#' perfm_custom(ytest=datatest$Class, ytestpred=preds$ypred, categweights=table(datatest$Class))
#' perfm_irrespective(ytest=datatest$Class, ytestpred=preds$ypred)
#' 
#' perfm_custom(ytest=datatest$Class, ytestpred=preds$ypred, categweights=c(0,2,0,0,0))
#' perfm_onecateg(ytest=datatest$Class, ytestpred=preds$ypred, categ="2")
#' 
#' @rdname perfm
#' @export
perfm_equal <-
function(ytest, ytestpred, categ, categweights) {
  
  categs <- sort(unique(ytest))
  mean(sapply(categs, function(x) youdenindex(ytest, ytestpred, x)))
  
}

#' @rdname perfm
#' @export
perfm_irrespective <-
function(ytest, ytestpred, categ, categweights) {
  
  categs <- sort(unique(ytest))
  sum(sapply(categs, function(x) (sum(ytest==x)/length(ytest))*youdenindex(ytest, ytestpred, x)))
  
}

#' @rdname perfm
#' @export
perfm_onecateg <-
function(ytest, ytestpred, categ, categweights) {
  
  categind <- which(levels(ytest)==categ)
  youdenindex(ytest, ytestpred, categind)
  
} 

#' @rdname perfm
#' @export
perfm_custom <-
function(ytest, ytestpred, categ, categweights) {
  
  categs <- sort(unique(ytest))
  categweights <- sapply(categs, function(x) categweights[levels(ytest)==x])
  categweights <- categweights/sum(categweights)
  sum(categweights*sapply(categs, function(x) youdenindex(ytest, ytestpred, x)))
  
} 