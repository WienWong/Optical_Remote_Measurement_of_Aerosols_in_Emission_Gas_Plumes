strVecfromtimeStrvec <- function(time_Strvec) {
  
  # Convert a large charactor vector with string format of "xx:xx:xx" to string format of "xxxxx". e.g. "08:52:01" to "85201". 
  # 2016-12-24, 1st built by Wang Weihua
  
  vec=c()   #  
  for (k in c(1:length(time_Strvec) ) ) {
    if ( substr(time_Strvec[k],1,1) == "0" ){
      T <- paste( substr(time_Strvec[k],2,2), substr(time_Strvec[k],4,5), substr(time_Strvec[k],7,8), sep="" )
    } else {
      T <- paste( substr(time_Strvec[k],1,2), substr(time_Strvec[k],4,5), substr(time_Strvec[k],7,8), sep="" )
    }
    vec = c(vec, T)
  }
  return(vec)
}
