#' Ordinal forests
#'
#' Constructs ordinal forests (OF) as presented in Hornung et al. (in prep). \cr
#' The following tasks can be performed using OF: 1) Predicting the values of an ordinal target variable for new observations based on covariate values (see \code{\link{predict.ordfor}});
#' 2) Ranking the importances of the covariates with respect to predicting the values of the ordinal target variable. \cr
#' The default values for the following hyperparameters are appropriate for most applications: \code{nsets}, \code{ntreeperdiv}, \code{ntreefinal}, \code{npermtrial}, and \code{nbest}. Thus, these arguments do in general \bold{NOT} have to be specified by hand. \cr
#' For details on OF see the 'Details' section below.
#'
#' @param depvar character. Name of the dependent variable in \code{data}.
#' @param data data.frame. Data frame containing the covariates and a factor-valued ordinal target variable.
#' @param nsets integer. Number of considered score sets in the estimation of the optimal score set.
#' @param ntreeperdiv integer. Number of trees in the smaller regression forests constructed for the \code{nsets} different score sets in the estimation of the optimal score set.
#' @param ntreefinal integer. Number of trees in the larger regression forest constructed using the optimized score set (i.e., the OF).
#' @param perfmeasure character. Performance measure. The default is \code{"equal"}. See 'Details' subsection 'Performance measures' below and \code{\link{perfm}}.
#' @param categimp character. Class to priorize if \code{perfmeasure="onecateg"}.
#' @param categweights numeric. Needed if \code{perfmeasure="custom"}: vector of length equal to the number of classes. Class weights - classes with 
#' higher weights are to be predicted more precisely then those with smaller weights. 
#' @param npermtrial integer. Number of permutations to try for each score set considered during the optimization.
#' @param nbest integer. Number of best score sets used to estimate the optimal score set.
#' @param naive boolean. If set to \code{TRUE}, the score set used for the classes of the target variable will not be optimized, but instead the following (naive) scores
#' will be used: 1,2,3,... Note that we strongly recommend to set \code{naive=FALSE} (default). The only advantage of choosing \code{naive=TRUE} is that the computational burden is reduced. However, the precision of the predictions of a forest using
#' score values 1,2,3,... can be considerably worse than that of a standard ordinal forest.
#' @param num.threads integer. Number of threads. Default is number of CPUs available (passed to the modified \code{ranger} code).
#'
#' @details
#'
#' \subsection{Introduction}{
#' Ordinal forests (OF) are a method for ordinal regression with high-dimensional and low-dimensional 
#' data that is able to predict the values of the ordinal target variable for new observations 
#' based on a training dataset. Using a (permutation-based) variable importance measure it is moreover 
#' possible to rank the covariates with respect to their importances in the prediction of the values
#' of the ordinal target variable. \cr
#' OF will be presented in an upcoming technical report by Hornung et al..
#' }
#'
#' \subsection{Methods}{
#' The concept of OF is based on the following assumption: There exists a (possibly latent) refined continuous 
#' variable y* underlying the observed ordinal target variable y (y in \{1,...,J\}, J number of classes), where y* determines the
#' values of y. The functional relationship between y* and y takes the form of a monotonically increasing
#' step function. Depending on which of J intervals ]c_1,\verb{ }c_2], \verb{ }]c_2,\verb{ }c_3], \verb{ } ..., \verb{ } ]c_J,\verb{ }c_\{J+1\}[
#' contains the value of y*, the ordinal target variable y takes a different value.
#'
#' In situations in which the values of the continuous target variable y* are known, these can be used directly
#' for prediction modeling using a corresponding approach for metric outcome variables.
#' OFs are, however, concerned with settings in which only the values of the classes of the ordinal target variable are given.
#' The main idea of OF is to optimize score values s_1,...,s_J to be used in place of the class values 1,...,J of the ordinal target variable
#' in standard regression forests by maximizing the out-of-bag prediction performance measured by a performance measure g (see section 
#' "Performance measures").
#' 
#' The estimation of the optimal score set consists of two steps:
#' 1) Construct a large number of regression forests (b in 1,...,\code{nsets}) using a limited number of trees, where each of these uses as the values 
#' of the target variable a randomly generated score set \{phi^(-1)((phi(a_\{b,j\}) + phi(a_\{b,j+1\}))/2) : j in 1,...,J\}, where 
#' phi(a_\{b,1\})=0 < phi(a_\{b,2\}) <... < phi(a_\{b,J\}) < phi(a_\{b,J+1\})=1 is a random partition of [0,1]. The intervals used 
#' to obtain the predicted class values from the predicted metric values are as follows: 
#' ]phi(a_\{b,1\}, phi(a_\{b,2\}] --> class 1,..., ]phi(a_\{b,J\}, phi(a_\{b,J+1\}] --> class J. For each forest constructed, calculate the value of
#' the performance measure g using the OOB estimated predictions of the ordinal target variable and the corresponding values of the ordinal target variable. 
#' 2) Calculate the estimated optimal score set as \{phi^(-1)((phi(mean_{b in IND_best} (a_\{b,j\})) + phi(mean_{b in IND_best} (a_\{b,j+1\})))/2) : j in 1,...,J\},
#' where IND_best is the set of indices of the forests with the \code{nbest} highest performance measure values.
#'
#' After estimating the score set, a larger regression forest is constructed using as values of the target variable 
#' this estimated optimal score set s_1,...,s_J. This regression forest is the ordinal forest.
#'
#' Prediction is performed by majority voting of the predictions of the individual trees in the ordinal forest.
#'
#' Further details are given in the sections "Hyperparameters" and "Performance measures".
#' }
#'
#' \subsection{Hyperparameters}{
#' There are several hyperparameters, which do, however, not have to be optimized by the user in general. Except in the case of
#' \code{nbest} the larger these values are, the better, but the default values should be 
#' large enough in most applications.
#' 
#' These hyperparameters are described in the following:
#' \itemize{
#'   \item \code{nsets} \verb{   } Default value: 1000. The default value of the number of considered score sets in the estimation of the optimal score set 
#' is quite large. Such a large number of considered score sets is necessary to attain a high chance that some of the score sets are close enough to the optimal score set,
#' that is, the score set that leads to the optimal performance with respect to the considered performance measure (provided with the argument \code{perfmeasure}).
#' \item \code{ntreeperdiv} \verb{   } Default value: 100. A smaller number of trees considered per tried
#' score set might lead to a too strong variability in the assessments of the performances achieved for the individual score sets.
#' \item \code{ntreefinal} \verb{   } Default value: 5000. The number of trees \code{ntreefinal}
#' plays the same role as in conventional regression forests. For very high-dimensional datasets the default number 5000 might be too small.
#' \item \code{npermtrial} \verb{   } Default value: 500. As stated above it is necessary to consider a large number of tried score sets \code{nsets} in the
#' optimization in order to increase the chance that the best of the considered score sets are close to the optimal score set.
#' However, for a larger number of classes it is in addition necessary to use an efficient algorithm for the sampling of the considered score sets.
#' This algorithm should ensure that the score sets are heterogeneous enough to encertain that the best of the tried score sets are close enough to the
#' optimal score set. In OF we consider an algorithm that has the goal of making the rankings of the widths of the intervals in the corresponding partition 
#' of [0,1] underying the respective score sets as dissimilar as possible between successive score sets.
#' 
#' Denote d_\{b,j\} := phi(a_\{b,j\}). Then the b-th partition of [0,1] for the b-th score set is generated as follows:
#'
#' If b = 1, then J-1 instances of a U(0,1) distributed random variable are drawn and
#' the values are sorted. The sorted values are designated as d_\{1,2\},...,d_\{1,J\}.
#' Moreover, set d_\{1,1\} := 0 and d_\{1,J+1\} := 1. The first considered partition is now \{ d_\{1,1\}, ... ,d_\{1,J+1\} \}.
#'
#' If b > 1, the following procedure is performed: 
#' 
#' 1. \verb{ } Let dist_\{b-1,j\} = d_\{b-1,j+1\} - d_\{b-1,j\} (j in \{1,...J\}) denote the width of the j-th interval of the b-1-th partition and r_\{b-1,j\} the rank of dist_\{b-1,j\} among
#' dist_\{b-1,1\},...,dist_\{b-1,J\}. First, \code{npermtrial} random permutations of 1,...,J are drawn, where r*_\{l,1\},...,r*_\{l,J\} denotes the l-th drawn
#' permutation. Subsequently, that permutation \cr
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
#' The default value 500 for \code{npermtrial} is chosen large enough to ensure sufficiently heterogeneous partitions - independent from the number of classes J.
#' \item \code{nbest} \verb{   } Default value: 10. It is probably important that the number \code{nbest} of best score sets used to estimate the optimal score set is not strongly misspecified. A too large value of \code{nbest} could lead to averaging over a too heterogeneous set of score sets. Conversely, a too small value of \code{nbest} could lead
#' to a too large variance in the estimation of the optimal score set. For larger numbers of tried score sets \code{nsets} the results are less sensitive to the choice of \code{nbest},
#' because the number of tried score sets that are close to the optimal score set becomes larger the more score sets are considered. When setting the number of considered score sets \code{nsets} to 1000, the value 10 for \code{nbest} specified as a default value in \code{ordfor} was seen to lead to a good trade-off
#' between the heterogeneity of the considered score sets and the variance in the estimation.
#' }
#' }
#'
#' \subsection{Performance measures}{
#' As noted above, the different score sets tried during the estimation of the optimal score set are assessed with respect to their (out-of-bag) prediction performance.
#' The choice of the specific performance measure used here determines the specific kind of performance the ordinal forest should feature:
#' \itemize{
#' \item \code{perfmeasure="equal"} \verb{   } This choice should be made if it is of interest to predict each class with the same precision irrespective of the numbers of observations representing the individual classes. 
#' Youden's J statistic is calculated with respect to each class ("observation/prediction in class j" vs. "observation/prediction NOT in class j" (j=1,...,J))
#' and the simple average of the J results taken.
#' \item \code{perfmeasure="irrespective"} \verb{   } This choice should be made if it is of interest to predict all observations with the same precision independent of their class. 
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
#' \item{forestfinal}{ object of class \code{"ranger"}. Regression forest constructed using the optimized score set (i.e., the OF). Used by \code{\link{predict.ordfor}}.  }
#' \item{bordersbest}{ vector of length J+1. Average over the \code{nbest} best partitions of [0,1]. Used by \code{\link{predict.ordfor}}. }
#' \item{forests}{ list of length \code{nsets}. The regression forests constructed for the \code{nsets} different score sets considered in the estimation of the optimal score set. }
#' \item{perfmeasurevalues}{ vector of length \code{nsets}. Performance measure values for all score sets considered in the estimation of the optimal score set. }
#' \item{bordersb}{ matrix of dimension nsets x (J+1). All \code{nsets} partitions of [0,1] considered. }
#' \item{classes}{ character vector of length J. Classes of the target variable. }
#' \item{nsets}{ integer. Number of score sets considered in the optimization. }
#' \item{ntreeperdiv}{ integer. Number of trees per score set considered. }
#' \item{ntreefinal}{ integer. Number of trees of OF (constructed after optimizing the score set). }
#' \item{perfmeasure}{ character. Performance measure.  }
#' \item{categimp}{ character. If \code{perfmeasure="onecateg"}: class to priorize, NA else. }
#' \item{nbest}{ integer. Number of best score sets used to estimate the optimal score set. }
#' \item{classfreq}{ table. Class frequencies. }
#' \item{varimp}{ vector of length p. Permutation variable importance for each covariate. Currently the misclassification error is used as error measure in the variable importance. }
#'
#' @examples
#' data(hearth)
#'
#' set.seed(123)
#' hearthsubset <- hearth[sort(sample(1:nrow(hearth), size=floor(nrow(hearth)*(1/2)))),]
#' ordforres <- ordfor(depvar="Class", data=hearthsubset, nsets=60, nbest=5, ntreeperdiv=100, 
#'   ntreefinal=1000, perfmeasure = "equal")
#' # NOTE: nsets=60 is not enough, because the prediction performance of the resulting 
#' # ordinal forest will be suboptimal!! In practice, nsets=1000 (default value) or a 
#' # number should be used.
#'
#' ordforres
#'
#' sort(ordforres$varimp, decreasing=TRUE)
#'
#' @export
ordfor <-
  function(depvar, data, nsets=1000, ntreeperdiv=100, ntreefinal=5000, perfmeasure = c("equal", "irrespective", "onecateg", "custom"), categimp, categweights, npermtrial=500, nbest=10, naive=FALSE, num.threads = NULL) {
    
    if (is.null(num.threads)) {
      num.threads = 0
    }
    else if (!is.numeric(num.threads) | num.threads < 0) {
      stop("Error: Invalid value for num.threads")
    }
    
    perfmeasure <- perfmeasure[1]
    
    # Extract the target variable:
    y <- eval(parse(text=paste("data$", depvar, sep="")))
    
    if(!is.factor(y))
      stop("Error: dependent variable must be factor valued.")
    
    
    # Number of classes of target variable:
    J <- length(levels(y))
    
    
    # Make a version of the dataset that does not contain the
    # factor-valued target variable (in the iterations numeric
    # target variables are used and the factor-valued target
    # variable is remove here in order to avoid that it is
    # used as a covariate in rangerordfor):
    datait <- data
    eval(parse(text=paste("datait$", depvar, " <- NULL", sep="")))
    datait$ymetric <- NA
    
    ynum <- as.numeric(y)       
    
    
    
    # The partition optimization is only performed if naive=FALSE:	
    
    if(!naive) {
      
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
      
      # Assign performance measure function:
      
      if(perfmeasure=="irrespective")
        perfm <- perfm_irrespective
      
      if(perfmeasure=="equal")
        perfm <- perfm_equal
      
      if(perfmeasure=="onecateg")
        perfm <- perfm_onecateg
      
      if(perfmeasure=="custom")
        perfm <- perfm_custom
      
      
      # Optimization of the partition:
      
      # List which will contain the regression forests in the form
      # of ranger-objects:
      forests <- list()
      # Initiate vector of performance measure values:
      perfmeasurevalues <- 0
      
      # Initiate matrix that will contain the metric values - the scores - used in
      # place of the the ordinal classes in the regression trees:
      cb <- matrix(nrow=nsets, ncol=J)
      
      # Simulate borders:  
      bordersb <- simulateborders(J=J, nsets=nsets, npermtrial=npermtrial)
      
      # Loop constructing a regression forest for each considered partition:
      for(b in 1:nsets) {
        
        # Get borders for the b-th forest:
        borders <- bordersb[b,]
        
        # Get values of the target variable:
        cb[b,] <- (borders[-1] + borders[-length(borders)])/2
        
        # Generate target variable for b-th forest:
        datait$ymetric <- qnorm(cb[b,][ynum])
        
        
        # Construct b-th regression tree:
        forests[[b]] <- rangerordfor(dependent.variable.name = "ymetric", data = datait, 
                                       num.trees = ntreeperdiv, num.threads=num.threads)
        
        # Obtain OOB predictions:
        allpred <- forests[[b]]$predictions
        
        # Get OOB predictions of the classes:
        ynumtestpred <- sapply(allpred, function(x) max(which(x >= qnorm(bordersb[b,]))))
        
        # Remove erratic predictions:
        keepbool <- !is.infinite(ynumtestpred)
        ynumtestpred <- ynumtestpred[keepbool]
        ynumwithoutinf <- ynum[keepbool]
        
        # Calculate value of the performance measure:
        perfmeasurevalues[b] <- perfm(ynumwithoutinf, ynumtestpred, categimp, categweights)
        
      }
      
      # Take the average over the nbest best borders:
      bordersbest <- c(0, colMeans(bordersb[order(perfmeasurevalues, decreasing=TRUE)[1:nbest],][,c(-1,-ncol(bordersb))]), 1)
      
      # Scores of the target variable:
      cbest <- (bordersbest[-1] + bordersbest[-length(bordersbest)])/2
      
      # Generate target variable for the large forest with
      # the optimized target variable:
      datait$ymetric <- qnorm(cbest[ynum])
      
    }
    else {
      
      # For naive=FALSE the scores of the target variable are simply 1,2,3,...:
      cbest <- 1:J
      
      # The information on the optimization of the partitions is 'NA': 
      bordersbest <- NA
      forests <- NA
      perfmeasurevalues <- NA
      bordersb <- NA
      nsets <- NA
      ntreeperdiv <- NA
      perfmeasure <- NA
      nbest <- NA
      
      # Metric target variable:
      datait$ymetric <- cbest[ynum]
      
    }
    
    
    
    # Construct ordinal forest:
    if(!is.na(bordersbest[1]))
     forestfinal <- rangerordfor(dependent.variable.name = "ymetric", data = datait, 
                                   num.trees = ntreefinal, importance="permutation", num.threads=num.threads, borders=qnorm(bordersbest[-c(1,length(bordersbest))]))
    else
    forestfinal <- rangerordfor(dependent.variable.name = "ymetric", data = datait, 
                                   num.trees = ntreefinal, importance="permutation", num.threads=num.threads, borders=(2:J) - 0.5)
   
    # Ordinal classes of the target variable:
    classes <- levels(y)
    
    # Class frequencies:
    classfreq <- table(y)
    
    # Output informations:
    res <- list(forestfinal=forestfinal, bordersbest=bordersbest, forests=forests,
                perfmeasurevalues=perfmeasurevalues, bordersb=bordersb, 
                classes=classes, nsets=nsets, ntreeperdiv=ntreeperdiv, ntreefinal=ntreefinal, 
                perfmeasure = perfmeasure, categimp=ifelse(!is.na(perfmeasure) & perfmeasure=="onecateg", categimp, NA), nbest=nbest, classfreq=classfreq, varimp=forestfinal$variable.importance)
    class(res) <- "ordfor"
    
    # Output results:
    return(res)
    
  }
