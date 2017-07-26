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
#' \item{classfreqtree}{ matrix of dimension \code{nrow(newdata)} x J. The value in the j-th column of the i-th row contains the frequency of trees that predicted class j for test observation i. }
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
#' ordforres <- ordfor(depvar="Class", data=datatrain, nsets=60, nbest=5)
#' # NOTE: nsets=60 is not enough, because the prediction performance of the resulting 
#' # ordinal forest will be suboptimal!! In practice, nsets=1000 (default value) or a 
#' # number should be used.
#'
#' preds <- predict(ordforres, newdata=datatest)
#' preds
#'  
#' table(data.frame(true_values=datatest$Class, predictions=preds$ypred))
#' 
#' head(preds$classfreqtree)
#'
#' @importFrom stats predict qnorm runif
#' 
#' @export
predict.ordfor <-
  function(object, newdata, ...) {
    
    # Ensure that 'object' is of class 'ordfor':
    if (!inherits(object, "ordfor"))
      stop("object not of class ordfor")
    
    
    # Extract some information from 'object':
    
    nforests <- length(object$perfmeasurevalues)
    
    classes <- object$classes
    bordersb <- object$bordersb
    perfmeasurevalues <- object$perfmeasurevalues
    
    J <- length(classes)
 
    
    # Calculations of class probabilities using individual tree predictions:
    
    yforestpredmetricmat <- predict(object=object$forestfinal, data=newdata, predict.all = TRUE)$predictions
    
    if(!is.na(object$perfmeasurevalues[1]))
      ynumpred <- Reduce("+", lapply(qnorm(object$bordersbest)[1:J], function(x) x <= yforestpredmetricmat))
    else
      ynumpred <- Reduce("+", lapply(c((1:J) - 0.5, J + 0.5)[1:J], function(x) x <= yforestpredmetricmat))
    
    freqs <- sapply(1:J, function(x) rowSums(ynumpred == x, na.rm = TRUE))/object$ntreefinal
    colnames(freqs) <- classes
	rownames(freqs) <- 1:nrow(freqs)

	
	# Recode the predictions as a factor variable with levels
    # equal to that of the training data:
    
	ynumpred <- apply(freqs, 1, which.max)
    ypred <- factor(classes[ynumpred], levels=classes)
	
	
    # Output informations:
    res <- list(ypred=ypred, classfreqtree=freqs)
    
    class(res) <- "ordforpred"
    
	
    # Output results:
    return(res)
	
  }
