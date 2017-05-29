breakcheck <- function(tratio,numrows, TimeMatrix) {
  # Check if time break exists during spectra measurement belonging to ( start of time, end of time ).
  # To make sure the measurement time is continue.
  # tratio: time ratio
  # numrows: number of record STD files
  # TimeMatrix: a charactor matrix containing the extracted time information from all STD files.
  # 2016-12-24, 1st built, Wang Weihua
  
  tbvec=c()                   # tbvec means time break vector
  counter=0
  for( k in 1:(numrows-1) ){
    if( TimeMatrix[k,2] == TimeMatrix[k+1,1] ){
      counter = counter + 1
    }else{
      vec = c(vec, k)         # add elements to a list inside the 'for' loop
    }
  }
  print(tratio *counter + 2)  # This can also estimate the total measurement time in seconds 
  return(tbvec)               # List out the break if exists.
}
