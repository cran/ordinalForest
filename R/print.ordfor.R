#' @keywords internal
#' @export
print.ordfor <-
  function(x, ...) {
    
    cat("\n")
    cat("Ordinal forest", "\n")
    
    cat("\n")
    cat(paste("Number of observations: ", x$forestfinal$num.samples, 
              ", number of covariates: ", x$forestfinal$num.independent.variables, sep=""), "\n")
    cat("\n")
    cat("Classes of ordinal target variable:", "\n")
    cat(paste(paste("\"", x$classes, "\" (n = ", x$classfreq, ")", sep=""), collapse=", "), "\n")
    cat("\n")
    cat("Forest setup:", "\n")
    cat(paste("Number of trees in ordinal forest: ", x$ntreefinal, sep=""), "\n")
    cat(paste("Number of considered score sets in total: ", x$nsets, sep=""), "\n")
    cat(paste("Number of best score sets used for estimating the optimal score set: ", x$nbest, sep=""), "\n")
    cat(paste("Number of trees per regression forests constructed in the optimization: ", x$ntreeperdiv, sep=""), "\n")
    if(!is.na(x$perfmeasure))
      cat(paste("Performance measure: \"", x$perfmeasure, "\"", sep=""), "\n")
    else
      cat(paste("Performance measure: ", x$perfmeasure, sep=""), "\n")
    if(!is.na(x$perfmeasure) & x$perfmeasure=="onecateg")
      cat(paste("Class to priorize: \"", x$categimp, "\"", sep=""), "\n")
    
  }
