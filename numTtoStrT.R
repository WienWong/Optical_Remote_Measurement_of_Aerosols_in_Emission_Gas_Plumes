numTtoStrT <- function(daytime){
  # Convert numeric time format of xxxxx to string time format of "x:xx:xx". e.g. 85638 to "8:56:38". 
  # 2017-01-11 1st built, Weihua Wang
  
  vec=c()
  for (k in c(1:length(daytime)) ) {
    if ( substr(daytime[k],1,2) > 24 ){
      T <- paste( substr(daytime[k],1,1), ":", substr(daytime[k],2,3), ":", substr(daytime[k],4,5), sep="" )
    } else {
      T <- paste( substr(daytime[k],1,2), ":", substr(daytime[k],3,4), ":", substr(daytime[k],5,6), sep="" )
    }
    vec = c(vec, T)
  }
  return(vec)
}
