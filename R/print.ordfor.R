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
    
    borround <- round(x$bordersbest, 2)
    
    if(!any(diff(borround)==0) & !any(diff(borround)[-1] < 0.04)) {
      
      visul <- paste(rep(" ", 100), collapse="")
      
      substring(visul, first=1, last=1) <- "|"
      for(i in 2:length(borround))
        substring(visul, first=borround[i]*100, last=borround[i]*100) <- "|"
      
      labeling <- paste(rep(" ", 100), collapse="")
      substring(labeling, first=1, last=1) <- "0"
      substring(labeling, first=100, last=100) <- "1"
      
      borsub <- as.character(borround[-c(1,length(borround))])
      for(i in seq(along=borsub))
        substring(labeling, first=borround[i+1]*100, last=borround[i+1]*100 + 3) <- borsub[i]
      
      cat("Optimized partition:", "\n")
      cat(labeling, "\n")
	  cat(paste(rep("-", 100), collapse=""), "\n")
      cat(visul, "\n")
	  cat(paste(rep("-", 100), collapse=""), "\n")
      
      cat("\n")
      
    } 
    else {
      
      cat("Optimized partition:", "\n")
      cat(paste("[", paste(borround, collapse=", "), "]", sep=""), "\n")
      
      cat("\n")
      
    }
    
    

    cat("Forest setup:", "\n")
    cat(paste("Number of trees in ordinal forest: ", x$ntreefinal, sep=""), "\n")
    cat(paste("Number of considered partitions: ", x$ndiv, sep=""), "\n")
	cat(paste("Number of partitions used for estimating the optimal partition: ", x$nbest, sep=""), "\n")
    cat(paste("Number of trees per partition: ", x$ntreeperdiv, sep=""), "\n")
    cat(paste("Performance measure: \"", x$perfmeasure, "\"", sep=""), "\n")
    if(x$perfmeasure=="onecateg")
      cat(paste("Category to priorize: \"", x$categimp, "\"", sep=""), "\n")
    
  }
