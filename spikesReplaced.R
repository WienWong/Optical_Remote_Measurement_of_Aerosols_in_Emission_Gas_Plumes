spikesReplaced <- function(datColumn, indexVec) {
  
  # This function aims to replace each confirmed individual spike from the vicinity of the spike and 
  # return the replaced column-wised data variable.
  # datColumn: the column-wised data variable to be processed.
  # indexVec: the index vector containing the spikes the indices of the spikes.
  # 2017-06-01, 1st built, 2017-06-19, 2nd modified (mean to median), Wang Weihua
  Vec = c()
  for (idx in indexVec) {
    
    datColumn[idx] <- median( c(datColumn[idx-2],datColumn[idx-3],datColumn[idx-4],datColumn[idx+2],datColumn[idx+3],datColumn[idx+4]) )
    Vec <- c(Vec,datColumn[idx])
  }
  Vec
  newdata <- datColumn
  newdata[indexVec] <- Vec
  return(newdata)
}
