#' Ordinal Forests: Prediction and Variable Ranking with Ordinal Target Variables
#'
#' The ordinal forest (OF) method allows ordinal regression with high-dimensional
#' and low-dimensional data. After having constructed an OF prediction rule using a training dataset, 
#' it can be used to predict the values of the ordinal target variable for new observations.
#' Moreover, by means of the (permutation-based) variable importance measure of OF, it is also possible to rank the covariates 
#' with respect to their importances in the prediction of the values of the ordinal target 
#' variable. \cr
#' OF is presented in Hornung (2019).
#'
#' For a brief, practice-orientated introduction to OF see: \code{\link{ordfor}}
#'
#' The main functions are: \code{\link{ordfor}} (construction of OF prediction rules) and
#' \code{\link{predict.ordfor}} (prediction of the values of the target variable values of new observations).
#'
#' NOTE: \pkg{ordinalForest} uses R code and C++ code from the R package \pkg{ranger} for the involved regression forests.
#' \pkg{ordinalForest} does, however, not depend on \pkg{ranger} or import \pkg{ranger}, because it was necessary to
#' copy the C++ code and parts of the R code from \pkg{ranger} to \pkg{ordinalForest} instead. The reason for this
#' is that \pkg{ranger}'s C++ code had to be altered in order to calculate a suitable permutation variable importance
#' measure for ordinal forests.
#'
#' @references
#' Hornung R. (2019) Ordinal Forests. Journal of Classification, <\doi{10.1007/s00357-018-9302-x}>.
#' 
#' @examples
#' \dontrun{
#' # Illustration of the key functionalities of the package:
#' ##########################################################
#' 
#' # Load example dataset:
#' 
#' data(hearth)
#' 
#' # Inspect the data:
#' table(hearth$Class)
#' dim(hearth)
#'
#' head(hearth) 
#'
#' 
#' # Split into training dataset and test dataset:
#' 
#' set.seed(123)
#' trainind <- sort(sample(1:nrow(hearth), size=floor(nrow(hearth)*(2/3))))
#' testind <- setdiff(1:nrow(hearth), trainind)
#' 
#' datatrain <- hearth[trainind,]
#' datatest <- hearth[testind,]
#' 
#' 
#' # Construct OF prediction rule using the training dataset:
#' 
#' ordforres <- ordfor(depvar="Class", data=datatrain, nsets=1000, ntreeperdiv=100, 
#'   ntreefinal=5000, perffunction = "equal")
#' ordforres
#' 
#' # Study variable importance values:
#' sort(ordforres$varimp, decreasing=TRUE)
#'
#' # Take a closer look at the top variables:
#' boxplot(datatrain$oldpeak ~ datatrain$Class, horizontal=TRUE)
#' fisher.test(table(datatrain$exang, datatrain$Class))
#' 
#' # Predict values of the ordinal target variable in the test dataset:
#' 
#' preds <- predict(ordforres, newdata=datatest)
#' preds
#' 
#' # Compare predicted values with true values:
#' table(data.frame(true_values=datatest$Class, predictions=preds$ypred))
#' } 
#'
#' @name ordinalForest-package
#' @aliases ordinalForest
NULL