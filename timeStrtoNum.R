timeStrtoNum <- function(x) {
  
  # x is character vector and has a format of "HH:MM:SS", e.g. "16:58:09" -> 165809; "08:22:34" -> 82234
  # 2016-12-24, 1st built by Weihua Wang
  
  if ( substr(x,1,1) == "0" ){
    T <- paste( substr(x,2,2), substr(x,4,5), substr(x,7,8), sep="" )
  } else {
    T <- paste( substr(x,1,2), substr(x,4,5), substr(x,7,8), sep="" )
  }
  return(as.numeric(T))
}
## test:
# timeStrtoNum("16:58:09")
# timeStrtoNum("08:22:34")
