centercal <- function(latv,lonv){
  # Om Mani Padme Hum !
  # center calculation based on longitude vector and latitude vector.
  # 2017-02-02, 1st built, Wang Weihua
  
  center <- c( ( max(lonv) + min(lonv) )/2, ( max(latv) + min(latv) )/2 )  # estimate the Google map center
  return(center)
}
