#' Performance functions based on Youden's J statistic
#' 
#' These performance functions based on Youden's J statistic are used in \code{\link{ordfor}} to measure the performance
#' of the smaller regression forests constructed prior to the approximation of the optimal score set. These functions may, however, also be used to measure the precision of
#' predictions on new data or the precision of OOB predictions.
#' 
#' \code{perff_equal} should be used if it is of interest to classify observations from each class with the same accuracy independent of the class sizes. 
#' Youden's J statistic is calculated with respect to each class ("observation/prediction in class j" vs. "observation/prediction NOT in class j" (j=1,...,J))
#' and the simple average of the J results taken.
#'
#' \code{perff_proportional} should be used if the main goal is to classify
#' correctly as many observations as possible. The latter is associated with a preference for larger classes at the 
#' expense of a lower classification accuracy with respect to smaller classes.
#' Youden's J statistic is calculated with respect to each class and subsequently a weighted average of these values is taken - with weights 
#' proportional to the number of observations representing the respective classes in the training data.
#'
#' \code{perff_oneclass} should be used if it is merely relevant that observations 
#' in class \code{categ} can be distinguished as reliably as possible from observations not in class \code{categ}.
#' Class \code{categ} must be passed to \code{perff_oneclass} via the argument \code{categ}.
#' Youden's J statistic is calculated with respect to class \code{categ}.
#'
#' \code{perff_custom} should be used if there is a particular ranking of the classes with respect to their importance. 
#' Youden's J statistic is calculated with respect to each class. Subsequently, a weighted average
#' with user-specified weights (provided via the argument \code{classweights}) is taken. In this way, classes with 
#' higher weights are prioritized by the OF algorithm over classes with smaller weights.
#'  
#' @param ytest factor. True values of the target variable.
#' @param ytestpred factor. Predicted values of the target variable.
#' @param categ character. Needed in the case of \code{perff_oneclass}: Class to prioiritize.
#' @param classweights numeric. Needed in the case of \code{perff_custom}: Vector of length equal to the number of classes. Class weights - classes with 
#' higher weights are to be prioiritzed over those with smaller weights.
#' @name perff
#'
#' @references
#' Hornung R. (2019) Ordinal Forests. Journal of Classification, <\doi{10.1007/s00357-018-9302-x}>.
NULL
#> NULL

#' @examples
#' data(hearth)
#'
#' set.seed(123)
#' trainind <- sort(sample(1:nrow(hearth), size=floor(nrow(hearth)*(1/2))))
#' testind <- sort(sample(setdiff(1:nrow(hearth), trainind), size=20))
#'
#' datatrain <- hearth[trainind,]
#' datatest <- hearth[testind,]
#'
#' ordforres <- ordfor(depvar="Class", data=datatrain, nsets=60, nbest=5)
#' # NOTE: nsets=60 is not enough, because the prediction performance of the resulting 
#' # ordinal forest will be suboptimal!! In practice, nsets=1000 (default value) or a larger
#' # number should be used.
#'
#' preds <- predict(ordforres, newdata=datatest)
#'
#' table('true'=datatest$Class, 'predicted'=preds$ypred)
#'
#'
#' perff_equal(ytest=datatest$Class, ytestpred=preds$ypred)
#'  
#' perff_proportional(ytest=datatest$Class, ytestpred=preds$ypred)
#'  
#' perff_oneclass(ytest=datatest$Class, ytestpred=preds$ypred, categ="1")
#' perff_oneclass(ytest=datatest$Class, ytestpred=preds$ypred, categ="2")
#' perff_oneclass(ytest=datatest$Class, ytestpred=preds$ypred, categ="3")
#' perff_oneclass(ytest=datatest$Class, ytestpred=preds$ypred, categ="4")
#' perff_oneclass(ytest=datatest$Class, ytestpred=preds$ypred, categ="5")
#'
#' 
#' perff_custom(ytest=datatest$Class, ytestpred=preds$ypred, classweights=c(1,2,1,1,1))
#' 
#' 
#' # perff_equal, perff_proportional, and perff_oneclass are special cases of perff_custom:
#' 
#' perff_custom(ytest=datatest$Class, ytestpred=preds$ypred, classweights=c(1,1,1,1,1))
#' perff_equal(ytest=datatest$Class, ytestpred=preds$ypred)
#' 
#' perff_custom(ytest=datatest$Class, ytestpred=preds$ypred, classweights=table(datatest$Class))
#' perff_proportional(ytest=datatest$Class, ytestpred=preds$ypred)
#' 
#' perff_custom(ytest=datatest$Class, ytestpred=preds$ypred, classweights=c(0,0,0,1,0))
#' perff_oneclass(ytest=datatest$Class, ytestpred=preds$ypred, categ="4")
#' 
#' @rdname perff
#' @export
perff_equal <-
function(ytest, ytestpred, categ, classweights) {
  
  categs <- sort(unique(ytest))
  mean(sapply(categs, function(x) youdenindex(ytest, ytestpred, x)))
  
}

#' @rdname perff
#' @export
perff_proportional <-
function(ytest, ytestpred, categ, classweights) {
  
  categs <- sort(unique(ytest))
  sum(sapply(categs, function(x) (sum(ytest==x)/length(ytest))*youdenindex(ytest, ytestpred, x)))
  
}

#' @rdname perff
#' @export
perff_oneclass <-
function(ytest, ytestpred, categ, classweights) {
  
  youdenindex(ytest, ytestpred, categ)
  
} 

#' @rdname perff
#' @export
perff_custom <-
function(ytest, ytestpred, categ, classweights) {
  
  categs <- sort(unique(ytest))
  classweights <- sapply(categs, function(x) classweights[levels(ytest)==x])
  classweights <- classweights/sum(classweights)
  sum(classweights*sapply(categs, function(x) youdenindex(ytest, ytestpred, x)))
  
} 