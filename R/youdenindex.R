youdenindex <-
function(ytest, ytestpred, categ) {
  
  ntp <- sum((ytest==categ) & (ytestpred==categ))
  np <- sum(ytest==categ)
  
  ntn <- sum((ytest!=categ) & (ytestpred!=categ))
  nn <- sum(ytest!=categ)
  
  sens <- ntp/np; spec <- ntn/nn
  
  return(sens + spec - 1)
  
}
