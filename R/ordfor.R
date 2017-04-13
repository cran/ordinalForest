#' Ordinal forests
#'
#' Constructs ordinal forests (OF) as presented in Hornung et al. (in prep). \cr
#' The following tasks can be performed using OF: 1) Predicting the values of an ordinal target variable for new observations based on covariate values (see \code{\link{predict.ordfor}});
#' 2) Estimation of the relative widths of the classes of the ordinal target variable; 3) Ranking the importances of the covariates with respect to predicting the values of the ordinal target variable. \cr
#' The default values for the following hyperparameters are appropriate for most applications: \code{ndiv}, \code{ntreeperdiv}, \code{ntreefinal}, \code{npermtrial}, and \code{nbest}. Thus, these arguments do in general \bold{NOT} have to be specified by hand. \cr
#' For details on OF see the 'Details' section below.
#'
#' @param depvar character. Name of the dependent variable in \code{data}.
#' @param data data.frame. Data frame containing the covariates and a factor-valued ordinal target variable.
#' @param ndiv integer. Number of random partitions of [0,1] to consider in the optimization.
#' @param ntreeperdiv integer. Number of trees in the smaller regression forests constructed for the \code{ndiv} different partitions.
#' @param ntreefinal integer. Number of trees in the larger regression forest constructed using the optimized partition (i.e., the OF).
#' @param perfmeasure character. Performance measure. The default is \code{"equal"}. See 'Details' subsection 'Performance measures' below and \code{\link{perfm}}.
#' @param categimp character. Class to priorize if \code{perfmeasure="onecateg"}.
#' @param categweights numeric. Needed if \code{perfmeasure="custom"}: vector of length equal to the number of classes. Class weights - classes with 
#' higher weights are to be predicted more precisely then those with smaller weights. 
#' @param npermtrial integer. Number of permutations to try for each partition considered during the optimization.
#' @param nbest integer. Number of forests to consider when averaging the parititions corresponding to the \code{nbest} forests with the highest levels of prediction performance.
#' @param importance character. Variable importance mode, one of 'none', 'impurity', 'permutation'. The 'impurity' measure is the variance of the responses for regression. Passed to the function \code{ranger} from the package of the same name.
#'
#' @details
#'
#' \subsection{Introduction}{
#' Ordinal forests (OF) are a method for ordinal regression with high-dimensional and low-dimensional 
#' data that is able to predict the values of the ordinal target variable for new observations 
#' and at the same time estimate the relative widths of the classes of the ordinal target variable. Using a 
#' (permutation-based) variable importance measure it is moreover possible to rank the importances 
#' of the covariates. \cr
#' OF will be presented in an upcoming technical report by Hornung et al..
#' }
#'
#' \subsection{Methods}{
#' The concept of OF is based on the following assumption: There exists a (possibly latent) refined continuous 
#' variable y* underlying the observed ordinal target variable y (y in \{1,...,J\}, J number of classes), where y* determines the
#' values of y. The functional relationship between y* and y takes the form of a monotonically increasing
#' step function. Depending on which of J intervals ]a_1,\verb{ }a_2], \verb{ }]a_2,\verb{ }a_3], \verb{ } ..., \verb{ } ]a_J,\verb{ }a_\{J+1\}[
#' contains the value of y*, the ordinal target variable y takes a different value.
#'
#' In the context of conditional inference trees, for situations in which the intervals ]a_1,\verb{ }a_2], \verb{ } ..., \verb{ } ]a_J,\verb{ }a_\{J+1\}[ 
#' are available, Hothorn et al. (2006) suggest to use as a metric target variable the midpoints of these intervals. 
#' OF are however concerned with settings in which the intervals ]a_1,\verb{ }a_2], \verb{ } ..., \verb{ } ]a_J,\verb{ }a_\{J+1\}[ are unknown.
#' The main idea of OF is to estimate the unknown interval borders by maximizing the out-of-bag prediction performance 
#' (see section "Performance measures") of standard regression forests that use as a
#' target variable a quantity very similar to the midpoints of the intervals.
#' More precisely, we assume y* to have a standard normal distribution and consider the following score values as the values of the target
#' variable: phi^(-1)((phi(a_j) + phi(a_\{j+1\}))/2) if y = j with a_1 = -infinite and a_\{J+1\} = +infinite, where "phi" denotes
#' the cumulative distribution function of the standard normal distribution.
#' The assumption of normality is simply made, because normally distributed target variables can be expected to
#' be well suited for standard regression forests.
#' 
#' The estimation of the optimal partition is based on constructing many regression forests (b in 1,...,\code{ndiv})
#' of smaller size, where each of these uses as the values of the target variable a score set obtained using random sampling:
#' \{phi^(-1)((phi(a_\{b,j\}) + phi(a_\{b,j+1\}))/2) : j in 1,...,J\}, where phi(a_\{b,1\})=0 < phi(a_\{b,2\}) <... < phi(a_\{b,J\}) < phi(a_\{b,J+1\})=1
#' is a random partition of [0,1].
#'
#' After estimating the intervals, a larger regression forest is built using as values of the target variable 
#' the score values phi^(-1)((phi(a_\{opt,j\}) + phi(a_\{opt,j+1\}))/2), where the a_\{opt,j\} values are the optimized
#' borders.
#'
#' An important by-product of the construction of OFs are the estimated interval borders \cr
#' a_\{opt,1\},...,a_\{opt,J+1\}. The optimized intervals 
#' ]phi(a_\{opt,1\}),\verb{ }phi(a_\{opt,2\})], \verb{ } ..., \verb{ } \cr
#' ]phi(a_\{opt,J\}),\verb{ } phi(a_\{opt,J+1\})[ can be interpreted
#' vividly as the estimated relative widths of the classes 1,...,J.
#'
#' Further details are given in the sections "Hyperparameters" and "Performance measures".
#' }
#'
#' \subsection{Hyperparameters}{
#' There are several hyperparameters, which do, however, not have to be optimized by the user in general. Expect in the case of
#' \code{nbest} the larger these values are, the better, but the default values should be 
#' large enough in most applications.
#' 
#' These hyperparameters are described in the following:
#' \itemize{
#'   \item \code{ndiv} \verb{   } Default value: 1000. The default value of the number of considered partitions in the estimation of the optimal partition 
#' is quite large. Such a large number of considered partitions is necessary to attain a high chance that some of the partitions are close enough to the optimal partition,
#' that is, the partition that leads to the optimal performance with respect to the considered performance measure (provided with \code{perfmeasure}).
#' \item \code{ntreeperdiv} \verb{   } Default value: 100. A smaller number of trees considered per tried
#' partition might lead to a too strong variability in the assessments of the performances achieved for the individual partitions.
#' \item \code{ntreefinal} \verb{   } Default value: 5000. The number of trees \code{ntreefinal}
#' plays the same role as in conventional regression forests. For very high-dimensional datasets the default number 5000 might be too small.
#' \item \code{npermtrial} \verb{   } Default value: 500. As stated above it is necessary to consider a large number of tried partitions \code{ndiv} in the
#' optimization in order increase the chance that the best of the considered partitions are close to the optimal partition.
#' However, for a larger number of classes it is in addition necessary to use an efficient algorithm for the sampling of the considered partitions.
#' This algorithm should ensure that the partitions are heterogeneous enough to encertain that the best of the tried partitions are close enough to the
#' optimal partition. In OF we consider an algorithm that has the goal of making the rankings of the widths of the intervals corresponding to the respective classes
#' as dissimilar as possible between successive partitions.
#' 
#' Denote d_\{b,j\} := phi(a_\{b,j\}). Then the b-th partition is generated as follows:
#'
#' If b = 1, then J-1 instances of a U(0,1) distributed random variable are drawn and
#' the values are sorted. The sorted values are designated as d_\{1,2\},...,d_\{1,J\}.
#' Moreover, set d_\{1,1\} := 0 and d_\{1,J+1\} := 1. The first considered partition is now \{ d_\{1,1\}, ... ,d_\{1,J+1\} \}.
#'
#' If b > 1, the following procedure is performed: 
#' 
#' 1. \verb{ } Let dist_\{b-1,j\} = d_\{b-1,j+1\} - d_\{b-1,j\} (j in \{1,...J\}) denote the width of the j-th interval of the b-1-th partition and r_\{b-1,j\} the rank of dist_\{b-1,j\} among
#' dist_\{b-1,1\},...,dist_\{b-1,J\}. First, \code{npermtrial} random permutations of 1,...,J are drawn, where r*_\{l,1\},...,r*_\{l,J\} denotes the l-th drawn
#' partition. Subsequently, that permutation \cr
#' r*_\{l*,1\},...,r*_\{l*,J\} (l* in \{1,...,\code{npermtrial}\}) is determined that features the greatest \cr
#' quadratic distance to r_\{b-1,1\},...r_\{b-1,J\},
#' that is, r*_\{l*,1\},...,r*_\{l*,J\} = \cr
#' argmax_\{r*_\{l,1\},...,r*_\{l,J\}\} sum_\{j=1\}^J (r*_\{l,j\} - r_\{b-1,j\})^2. 
#'
#' 2. \verb{ } A partition is generated in the same way as
#' in the case of b = 1. 
#'
#' 3. \verb{ } The intervals of the partition generated in 2. are re-ordered in such a way that the width of the j-th interval (for j in \{1,...,J\}) has rank r*_\{l*,j\}
#' among the widths of the intervals ]d_\{b,1\},\verb{ }d_\{b,2\}], \verb{ } ..., \verb{ } ]d_\{b,J\},\verb{ }d_\{b,J+1\}]. The b-th considered partition is now \{ d_\{b,1\}, ... ,d_\{b,J+1\} \}.
#'
#' The default value 500 for \code{npermtrial} should be large enough to ensure sufficiently heterogeneous partitions - independent from the number of classes J.
#' \item \code{nbest} \verb{   } Default value: 10. After having constructed \code{ndiv} regression forests, each of these using a particular partition, the \code{nbest} partitions associated with the highest
#' levels of prediction performance are considered: For each j in \{1,...,J\} the averages of the phi(a_j) values across these \code{nbest} partitions are taken. Then phi^(-1)(.)
#' is applied to the obtained averages resulting in the estimated optimal borders: a_{opt,1},...,a_{opt,J+1}. The latter are then used to calculate the scores for the OF using the following formula phi^(-1)((phi(a_\{opt,j\}) + phi(a_\{opt,j+1\}))/2) (see also above). \cr
#' It is probably important that the number \code{nbest} of best partitions considered
#' is not strongly misspecified. A too large value of \code{nbest} could lead to averaging over a too heterogeneous set of partitions. Conversely, a too small value of \code{nbest} could lead
#' to a too large variance in the estimation of the optimal partition. For larger numbers of tried partitions \code{ndiv} the results are less sensitive to the choice of \code{nbest},
#' because the number of tried partitions that are close to the optimal partition becomes larger the more partitions are considered. When setting the number of considered partitions \code{ndiv} to 1000, the value 10 for \code{nbest} specified as a default value in \code{ordfor} was seen to lead to a good trade-off
#' between the heterogeneity of the averaged partitions and the variance in the estimation.
#' }
#' }
#'
#' \subsection{Performance measures}{
#' As noted above, the different partitions tried during the estimation of the optimal partition are assessed with respect to their (out-of-bag) prediction performance.
#' The choice of the specific performance measure used here determines the specific kind of performance the ordinal forest should feature:
#' \itemize{
#' \item \code{perfmeasure="equal"} \verb{   } This choice should be made if it is of interest to predict each class with the same precision irrespective of the numbers of observations representing the individual classes. 
#' Youden's J statistic is calculated with respect to each class ("observation/prediction in class j" vs. "observation/prediction NOT in class j" (j=1,...,J))
#' and the simple average of the J results taken.
#' \item \code{perfmeasure="irrespective"} \verb{   } This choice should be made if it is of interest to predict all observations with the same precision independent of their categories. 
#' Youden's J statistic is calculated with respect to each class and subsequently a weighted average of these values is taken - with weights 
#' proportional to the number of observations representing the respective classes in the training data.
#' \item \code{perfmeasure="onecateg"} \verb{   } This choice should be made if it is merely relevant that a particular class of the classes of the ordinal target variable
#' is predicted as precisely as possible.
#' This particular class must be provided via the argument \code{categ}.
#' Youden's J statistic is calculated with respect to class \code{categ} ("observation/prediction in class \code{categ}" vs. "observation/prediction NOT in class \code{categ}").
#' \item \code{perfmeasure="custom"} \verb{   } This choice should be made if there is a particular ranking of the classes with respect to their importance. 
#' Youden's J statistic is calculated with respect to each class. Subsequently, a weighted average
#' with user-specified weights (provided via the argument \code{categweights}) is taken. Classes with 
#' higher weights are to be predicted more precisely than those with smaller weights.
#' }
#' }
#'
#' @return
#' \code{ordfor} returns an object of class \code{ordfor}.
#' An object of class "\code{ordfor}" is a list containing the following components: 
#' \item{forestfinal}{ object of class \code{"ranger"}. Regression forest constructed using the optimized partition of [0,1] (i.e., the OF). Used by \code{\link{predict.ordfor}}.  }
#' \item{bordersbest}{ vector of length J+1. Average over the \code{nbest} best partitions of [0,1]. Used by \code{\link{predict.ordfor}}. }
#' \item{forests}{ list of length \code{ndiv}. The smaller forests constructed during optimizing the partition of [0,1]. }
#' \item{perfmeasurevalues}{ vector of length \code{ndiv}. Performance measure values of the \code{ndiv} smaller forests stored in \code{forests}. }
#' \item{bordersb}{ matrix of dimension ndiv x (J+1). All \code{ndiv} partitions considered. }
#' \item{classes}{ character vector of length J. Classes of the target variable. }
#' \item{ndiv}{ integer. Number of random partitions considered. }
#' \item{ntreeperdiv}{ integer. Number of trees per partition. }
#' \item{ntreefinal}{ integer. Number of trees of OF (constructed after optimizing the partition). }
#' \item{perfmeasure}{ character. Performance measure.  }
#' \item{categimp}{ character. If \code{perfmeasure="onecateg"}: class to priorize, NA else. }
#' \item{nbest}{ integer. Number of forests considered in the averaging of the parititions corresponding to the \code{nbest} forests with the highest levels of prediction performance. }
#' \item{classfreq}{ table. Clas frequencies. }
#' \item{varimp}{ vector of length p. Variable importance for each covariate. }
#'
#' @examples
#' data(hearth)
#'
#' set.seed(123)
#' hearthsubset <- hearth[sort(sample(1:nrow(hearth), size=floor(nrow(hearth)*(1/2)))),]
#' ordforres <- ordfor(depvar="Class", data=hearthsubset, ndiv=80, nbest=5, ntreeperdiv=100, 
#'   ntreefinal=1000, perfmeasure = "equal")
#' # NOTE: ndiv=80 is not enough!! In practice, ndiv=1000 (default value) or a higher
#' # number should be used.
#'
#' ordforres
#'
#' sort(ordforres$varimp, decreasing=TRUE)
#'
#' @references
#' Hothorn, T., Hornik, K., Zeileis, A. (2006) Unbiased recursive partitioning: a conditional inference framework. Journal of Computational and Graphical Statistics, 15, 651--674.
#'
#' @export
ordfor <-
function(depvar, data, ndiv=1000, ntreeperdiv=100, ntreefinal=5000, perfmeasure = c("equal", "irrespective", "onecateg", "custom"), categimp, categweights, npermtrial=500, nbest=10, importance="permutation") {

  # Load package ranger. Required for construction the regression trees:
  
  ## require("ranger")
  
  perfmeasure <- perfmeasure[1]
  
  # Extract the target variable:
  
  y <- eval(parse(text=paste("data$", depvar, sep="")))
  
  if(!is.factor(y))
    stop("Error: dependent variable must be factor valued.")
  
  
  # Number of classes of target variable:
  
  J <- length(levels(y))
  
  ynum <- as.numeric(y)
  
  if(missing(categimp) & (perfmeasure=="onecateg"))
    stop("Class to priorize has to be provided if perfmeasure = 'onecateg'.")
	
  if(missing(categimp) & (perfmeasure!="onecateg"))
    categimp <- levels(y)[1] 
	
  if(missing(categweights)) {
    categweights <- rep(1/J, J)
	if(perfmeasure=="custom")
      warning("perfmeasure = 'custom', but argument 'categweights' was missing. 'categweights' set to 1/J,...,1/J.")
  }	
	
  if(missing(categimp) & (perfmeasure!="onecateg"))
    categimp <- levels(y)[1] 
  
  # Assign tree weight function:
  
  if(perfmeasure=="irrespective")
    perfm <- perfm_irrespective
  
  if(perfmeasure=="equal")
    perfm <- perfm_equal

  if(perfmeasure=="onecateg")
    perfm <- perfm_onecateg

  if(perfmeasure=="custom")
    perfm <- perfm_custom
  
  # Initiate data.frame to be used for constructing the
  # trees. Does not include ordinal target variable:
  
  datait <- data
  eval(parse(text=paste("datait$", depvar, " <- NULL", sep="")))
  datait$ymetric <- NA
  
  
  
  # Construct regression trees constituting the ordinal forest:
  
  # List which will contain the regression trees in the form
  # of ranger-objects:
  forests <- list()
  # Initiate vector of tree weights:
  perfmeasurevalues <- 0
  
  # Initiate matrix that will contain the metric values used in
  # place of the the ordinal classes in the regression trees:
  cb <- matrix(nrow=ndiv, ncol=J)

  # Simulate borders:  
  bordersb <- simulateborders(J=J, ndiv=ndiv, npermtrial=npermtrial)
  
  # Loop constructing the regression trees:
  for(b in 1:ndiv) {

    # Get borders for the b-th forest:
    borders <- bordersb[b,]

    # Get values of the target variable:
    cb[b,] <- (borders[-1] + borders[-length(borders)])/2
    
    # Generate target variable for b-th regression tree:
    datait$ymetric <- qnorm(cb[b,][ynum])
    
    
    # Construct b-th regression tree:
    forests[[b]] <- ranger::ranger(dependent.variable.name = "ymetric", data = datait, 
                           num.trees = ntreeperdiv)#, keep.inbag = TRUE)
    
    # Obtain OOB predictions:
    allpred <- forests[[b]]$predictions
    
    ynumtestpred <- sapply(allpred, function(x) max(which(x >= qnorm(bordersb[b,]))))
    
    keepbool <- !is.infinite(ynumtestpred)
    ynumtestpred <- ynumtestpred[keepbool]
    ynumwithoutinf <- ynum[keepbool]
    
    # Calculate tree weight:
    perfmeasurevalues[b] <- perfm(ynumwithoutinf, ynumtestpred, categimp, categweights)
    
  }
  
  # Take the average over the nbest best borders:
  bordersbest <- c(0, colMeans(bordersb[order(perfmeasurevalues, decreasing=TRUE)[1:nbest],][,c(-1,-ncol(bordersb))]), 1)
  
  # Values of the target variable:
  cbest <- (bordersbest[-1] + bordersbest[-length(bordersbest)])/2
  
  # Generate target variable for the large forest with
  # the optimized target variable:
  datait$ymetric <- qnorm(cbest[ynum])
  
  # Generate final large target variable:
  forestfinal <- ranger::ranger(dependent.variable.name = "ymetric", data = datait, 
                        num.trees = ntreefinal, importance=importance)#, keep.inbag = TRUE)
  
  # Ordinal classes of the target variable:
  classes <- levels(y)
  
  # Class frequencies:
  classfreq <- table(y)
  
  # Output informations:
  res <- list(forestfinal=forestfinal, bordersbest=bordersbest, forests=forests,
    perfmeasurevalues=perfmeasurevalues, bordersb=bordersb, 
	classes=classes, ndiv=ndiv, ntreeperdiv=ntreeperdiv, ntreefinal=ntreefinal, 
	perfmeasure = perfmeasure, categimp=ifelse(perfmeasure=="onecateg", categimp, NA), nbest=nbest, classfreq=classfreq, varimp=forestfinal$variable.importance)
  class(res) <- "ordfor"
  
  # Output results:
  return(res)
  
}
