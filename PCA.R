
# This bloody crazy R function was created in March 2017. 
# It aims to produce reconstructed Principal Components (PCs) which are used for gas species classification and recognizability. 
# 
# Why so many if-elseif branches? Because I am not so sure how many reconstruction coefficients and PCs should be used when the
# inputs of IRs and I500 are also uncertain. Suggested by Johan, the number of inputs were varied from 2 to 14. Because the PCs
# were expressed with different reconstruction coefficients under different preprocessing methods, many if-elseif branches were
# listed out for time saving purpose!
# The 'log' method (selection branches vary from 2 to 14) was suggested from the Internet but was proved to be not so suitable for this project.
# The 'exp' (2 to 8) and 'yeo' (2 to 12) method were also explored to avoid throwing zeors or negative values away.
# The 'none' method (2 to 9) seemed to be the most proper one.
#
# The Yeо-Johnson conversion is slightly similar to the Box-Cox model, however it may accept predictors with zero or negative 
# values (while predictor values for the Box-Cox transformation have to be strictly positive). The exponential transformation 
# of Manly (1976) can also be used for positive and negative data.

PCA <- function(inputV,prep="log") {
  
  if(prep=="log") {
    
    inputV[inputV <= 0] <- 0.01 
    pca <- prcomp(-1*log(inputV)) 
    #pca <- prcomp(-1*log(inputV), center = TRUE, scale. = TRUE) 
    print(pca)
    #plot(pca, type = "l")
    print( summary(pca) )
    plot(pca, xlab="PCs")
    
    if(dim(inputV)[2]==2) {
      
      PC1_coef <- pca$rotation[,1]   # coef of PC1
      PC2_coef <- pca$rotation[,2]   # coef of PC2
      
      PC1 <- PC1_coef[1]*-log(inputV[,1]) + PC1_coef[2]*-log(inputV[,2])
      #plot(IRI_new$Time, IRI_new$PC1, type = "l", col='deepskyblue4', xlab="Time", ylab="PC1", main="PCA of IR_340T675 and IR_440T675")
      PC2 <- PC2_coef[1]*-log(inputV[,1]) + PC2_coef[2]*-log(inputV[,2])
      #plot(IRI_new$Time, IRI_new$PC2, type = "l", col='deepskyblue4', xlab="Time", ylab="PC2", main="PCA of IR_340T675 and IR_440T675")
      
      PC = cbind(PC1,PC2)  # combine the columns 
      
      return(PC)  
      
    } else if(dim(inputV)[2]==3) {
        
        PC1_coef <- pca$rotation[,1]   # coef of PC1
        PC2_coef <- pca$rotation[,2]   # coef of PC2
        PC3_coef <- pca$rotation[,3]   # coef of PC3
        
        PC1 <- PC1_coef[1]*-log(inputV[,1]) + PC1_coef[2]*-log(inputV[,2]) + PC1_coef[3]*-log(inputV[,3])
        PC2 <- PC2_coef[1]*-log(inputV[,1]) + PC2_coef[2]*-log(inputV[,2]) + PC2_coef[3]*-log(inputV[,3])
        PC3 <- PC3_coef[1]*-log(inputV[,1]) + PC3_coef[2]*-log(inputV[,2]) + PC3_coef[3]*-log(inputV[,3])
        
        PC = cbind(PC1,PC2,PC3)  
        
        return(PC) 
        
    } else if(dim(inputV)[2]==4) {
      
      PC1_coef <- pca$rotation[,1]   # coef of PC1
      PC2_coef <- pca$rotation[,2]   # coef of PC2
      PC3_coef <- pca$rotation[,3]   # coef of PC3
      PC4_coef <- pca$rotation[,4]   # coef of PC4
      
      PC1 <- PC1_coef[1]*-log(inputV[,1]) + PC1_coef[2]*-log(inputV[,2]) + PC1_coef[3]*-log(inputV[,3]) +
        PC1_coef[4]*-log(inputV[,4])
      
      PC2 <- PC2_coef[1]*-log(inputV[,1]) + PC2_coef[2]*-log(inputV[,2]) + PC2_coef[3]*-log(inputV[,3]) +
        PC2_coef[4]*-log(inputV[,4]) 
      
      PC3 <- PC3_coef[1]*-log(inputV[,1]) + PC3_coef[2]*-log(inputV[,2]) + PC3_coef[3]*-log(inputV[,3]) +
        PC3_coef[4]*-log(inputV[,4]) 
      
      PC4 <- PC4_coef[1]*-log(inputV[,1]) + PC4_coef[2]*-log(inputV[,2]) + PC4_coef[3]*-log(inputV[,3]) +
        PC4_coef[4]*-log(inputV[,4])
      
      PC = cbind(PC1,PC2,PC3,PC4)  
      
      return(PC) 
      
    } else if(dim(inputV)[2]==5) {
      
      PC1_coef <- pca$rotation[,1]   # coef of PC1
      PC2_coef <- pca$rotation[,2]   # coef of PC2
      PC3_coef <- pca$rotation[,3]   # coef of PC3
      PC4_coef <- pca$rotation[,4]   # coef of PC4
      PC5_coef <- pca$rotation[,5]   # coef of PC5
      
      PC1 <- PC1_coef[1]*-log(inputV[,1]) + PC1_coef[2]*-log(inputV[,2]) + PC1_coef[3]*-log(inputV[,3]) +
        PC1_coef[4]*-log(inputV[,4]) + PC1_coef[5]*-log(inputV[,5])
      
      PC2 <- PC2_coef[1]*-log(inputV[,1]) + PC2_coef[2]*-log(inputV[,2]) + PC2_coef[3]*-log(inputV[,3]) +
        PC2_coef[4]*-log(inputV[,4]) + PC2_coef[5]*-log(inputV[,5])
      
      PC3 <- PC3_coef[1]*-log(inputV[,1]) + PC3_coef[2]*-log(inputV[,2]) + PC3_coef[3]*-log(inputV[,3]) +
        PC3_coef[4]*-log(inputV[,4]) + PC3_coef[5]*-log(inputV[,5]) 
      
      PC4 <- PC4_coef[1]*-log(inputV[,1]) + PC4_coef[2]*-log(inputV[,2]) + PC4_coef[3]*-log(inputV[,3]) +
        PC4_coef[4]*-log(inputV[,4]) + PC4_coef[5]*-log(inputV[,5]) 
      
      PC5 <- PC5_coef[1]*-log(inputV[,1]) + PC5_coef[2]*-log(inputV[,2]) + PC5_coef[3]*-log(inputV[,3]) + 
        PC5_coef[4]*-log(inputV[,4]) + PC5_coef[5]*-log(inputV[,5]) 
      
      PC = cbind(PC1,PC2,PC3,PC4,PC5)  
      
      return(PC) 
      
    } else if(dim(inputV)[2]==6) {
      
      PC1_coef <- pca$rotation[,1]   # coef of PC1
      PC2_coef <- pca$rotation[,2]   # coef of PC2
      PC3_coef <- pca$rotation[,3]   # coef of PC3
      PC4_coef <- pca$rotation[,4]   # coef of PC4
      PC5_coef <- pca$rotation[,5]   # coef of PC5
      PC6_coef <- pca$rotation[,6]   # coef of PC6
      
      PC1 <- PC1_coef[1]*-log(inputV[,1]) + PC1_coef[2]*-log(inputV[,2]) + PC1_coef[3]*-log(inputV[,3]) +
        PC1_coef[4]*-log(inputV[,4]) + PC1_coef[5]*-log(inputV[,5]) + PC1_coef[6]*-log(inputV[,6])
      
      PC2 <- PC2_coef[1]*-log(inputV[,1]) + PC2_coef[2]*-log(inputV[,2]) + PC2_coef[3]*-log(inputV[,3]) +
        PC2_coef[4]*-log(inputV[,4]) + PC2_coef[5]*-log(inputV[,5]) + PC2_coef[6]*-log(inputV[,6])
      
      PC3 <- PC3_coef[1]*-log(inputV[,1]) + PC3_coef[2]*-log(inputV[,2]) + PC3_coef[3]*-log(inputV[,3]) +
        PC3_coef[4]*-log(inputV[,4]) + PC3_coef[5]*-log(inputV[,5]) + PC3_coef[6]*-log(inputV[,6])
      
      PC4 <- PC4_coef[1]*-log(inputV[,1]) + PC4_coef[2]*-log(inputV[,2]) + PC4_coef[3]*-log(inputV[,3]) +
        PC4_coef[4]*-log(inputV[,4]) + PC4_coef[5]*-log(inputV[,5]) + PC4_coef[6]*-log(inputV[,6])
        
      PC5 <- PC5_coef[1]*-log(inputV[,1]) + PC5_coef[2]*-log(inputV[,2]) + PC5_coef[3]*-log(inputV[,3]) + 
        PC5_coef[4]*-log(inputV[,4]) + PC5_coef[5]*-log(inputV[,5]) + PC5_coef[6]*-log(inputV[,6])
      
      PC6 <- PC6_coef[1]*-log(inputV[,1]) + PC6_coef[2]*-log(inputV[,2]) + PC6_coef[3]*-log(inputV[,3]) +
        PC6_coef[4]*-log(inputV[,4]) + PC6_coef[5]*-log(inputV[,5]) + PC6_coef[6]*-log(inputV[,6])
      
      PC = cbind(PC1,PC2,PC3,PC4,PC5,PC6)  
      
      return(PC) 
      
    } else if(dim(inputV)[2]==7) {
      
      PC1_coef <- pca$rotation[,1]   # coef of PC1
      PC2_coef <- pca$rotation[,2]   # coef of PC2
      PC3_coef <- pca$rotation[,3]   # coef of PC3
      PC4_coef <- pca$rotation[,4]   # coef of PC4
      PC5_coef <- pca$rotation[,5]   # coef of PC5
      PC6_coef <- pca$rotation[,6]   # coef of PC6
      PC7_coef <- pca$rotation[,7]   # coef of PC7
      
      PC1 <- PC1_coef[1]*-log(inputV[,1]) + PC1_coef[2]*-log(inputV[,2]) + PC1_coef[3]*-log(inputV[,3]) +
        PC1_coef[4]*-log(inputV[,4]) + PC1_coef[5]*-log(inputV[,5]) + PC1_coef[6]*-log(inputV[,6]) + PC1_coef[7]*-log(inputV[,7])
      
      PC2 <- PC2_coef[1]*-log(inputV[,1]) + PC2_coef[2]*-log(inputV[,2]) + PC2_coef[3]*-log(inputV[,3]) +
        PC2_coef[4]*-log(inputV[,4]) + PC2_coef[5]*-log(inputV[,5]) + PC2_coef[6]*-log(inputV[,6]) + PC2_coef[7]*-log(inputV[,7])
      
      PC3 <- PC3_coef[1]*-log(inputV[,1]) + PC3_coef[2]*-log(inputV[,2]) + PC3_coef[3]*-log(inputV[,3]) +
        PC3_coef[4]*-log(inputV[,4]) + PC3_coef[5]*-log(inputV[,5]) + PC3_coef[6]*-log(inputV[,6]) + PC3_coef[7]*-log(inputV[,7])
      
      PC4 <- PC4_coef[1]*-log(inputV[,1]) + PC4_coef[2]*-log(inputV[,2]) + PC4_coef[3]*-log(inputV[,3]) +
        PC4_coef[4]*-log(inputV[,4]) + PC4_coef[5]*-log(inputV[,5]) + PC4_coef[6]*-log(inputV[,6]) + PC4_coef[7]*-log(inputV[,7])
      
      PC5 <- PC5_coef[1]*-log(inputV[,1]) + PC5_coef[2]*-log(inputV[,2]) + PC5_coef[3]*-log(inputV[,3]) + 
        PC5_coef[4]*-log(inputV[,4]) + PC5_coef[5]*-log(inputV[,5]) + PC5_coef[6]*-log(inputV[,6]) + PC5_coef[7]*-log(inputV[,7])
      
      PC6 <- PC6_coef[1]*-log(inputV[,1]) + PC6_coef[2]*-log(inputV[,2]) + PC6_coef[3]*-log(inputV[,3]) +
        PC6_coef[4]*-log(inputV[,4]) + PC6_coef[5]*-log(inputV[,5]) + PC6_coef[6]*-log(inputV[,6]) + PC6_coef[7]*-log(inputV[,7])
      
      PC7 <- PC7_coef[1]*-log(inputV[,1]) + PC7_coef[2]*-log(inputV[,2]) + PC7_coef[3]*-log(inputV[,3]) +
        PC7_coef[4]*-log(inputV[,4]) + PC7_coef[5]*-log(inputV[,5]) + PC7_coef[6]*-log(inputV[,6]) + PC7_coef[7]*-log(inputV[,7])
      
      PC = cbind(PC1,PC2,PC3,PC4,PC5,PC6,PC7)  
      
      return(PC) 
      
    } else if(dim(inputV)[2]==8) {
      
      PC1_coef <- pca$rotation[,1]   # coef of PC1
      PC2_coef <- pca$rotation[,2]   # coef of PC2
      PC3_coef <- pca$rotation[,3]   # coef of PC3
      PC4_coef <- pca$rotation[,4]   # coef of PC4
      PC5_coef <- pca$rotation[,5]   # coef of PC5
      PC6_coef <- pca$rotation[,6]   # coef of PC6
      PC7_coef <- pca$rotation[,7]   # coef of PC7
      PC8_coef <- pca$rotation[,8]   # coef of PC8
      
      PC1 <- PC1_coef[1]*-log(inputV[,1]) + PC1_coef[2]*-log(inputV[,2]) + PC1_coef[3]*-log(inputV[,3]) + PC1_coef[4]*-log(inputV[,4]) + 
        PC1_coef[5]*-log(inputV[,5]) + PC1_coef[6]*-log(inputV[,6]) + PC1_coef[7]*-log(inputV[,7]) + PC1_coef[8]*-log(inputV[,8])
      
      PC2 <- PC2_coef[1]*-log(inputV[,1]) + PC2_coef[2]*-log(inputV[,2]) + PC2_coef[3]*-log(inputV[,3]) + PC2_coef[4]*-log(inputV[,4]) +
        PC2_coef[5]*-log(inputV[,5]) + PC2_coef[6]*-log(inputV[,6]) + PC2_coef[7]*-log(inputV[,7]) + PC2_coef[8]*-log(inputV[,8])
      
      PC3 <- PC3_coef[1]*-log(inputV[,1]) + PC3_coef[2]*-log(inputV[,2]) + PC3_coef[3]*-log(inputV[,3]) + PC3_coef[4]*-log(inputV[,4]) + 
        PC3_coef[5]*-log(inputV[,5]) + PC3_coef[6]*-log(inputV[,6]) + PC3_coef[7]*-log(inputV[,7]) + PC3_coef[8]*-log(inputV[,8])
      
      PC4 <- PC4_coef[1]*-log(inputV[,1]) + PC4_coef[2]*-log(inputV[,2]) + PC4_coef[3]*-log(inputV[,3]) + PC4_coef[4]*-log(inputV[,4]) + 
        PC4_coef[5]*-log(inputV[,5]) + PC4_coef[6]*-log(inputV[,6]) + PC4_coef[7]*-log(inputV[,7]) + PC4_coef[8]*-log(inputV[,8])
      
      PC5 <- PC5_coef[1]*-log(inputV[,1]) + PC5_coef[2]*-log(inputV[,2]) + PC5_coef[3]*-log(inputV[,3]) + PC5_coef[4]*-log(inputV[,4]) + 
        PC5_coef[5]*-log(inputV[,5]) + PC5_coef[6]*-log(inputV[,6]) + PC5_coef[7]*-log(inputV[,7]) + PC5_coef[8]*-log(inputV[,8])
      
      PC6 <- PC6_coef[1]*-log(inputV[,1]) + PC6_coef[2]*-log(inputV[,2]) + PC6_coef[3]*-log(inputV[,3]) + PC6_coef[4]*-log(inputV[,4]) + 
        PC6_coef[5]*-log(inputV[,5]) + PC6_coef[6]*-log(inputV[,6]) + PC6_coef[7]*-log(inputV[,7])+ PC6_coef[8]*-log(inputV[,8])
      
      PC7 <- PC7_coef[1]*-log(inputV[,1]) + PC7_coef[2]*-log(inputV[,2]) + PC7_coef[3]*-log(inputV[,3]) + PC7_coef[4]*-log(inputV[,4]) + 
        PC7_coef[5]*-log(inputV[,5]) + PC7_coef[6]*-log(inputV[,6]) + PC7_coef[7]*-log(inputV[,7]) + PC7_coef[8]*-log(inputV[,8])
      
      PC8 <- PC8_coef[1]*-log(inputV[,1]) + PC8_coef[2]*-log(inputV[,2]) + PC8_coef[3]*-log(inputV[,3]) + PC8_coef[4]*-log(inputV[,4]) + 
        PC8_coef[5]*-log(inputV[,5]) + PC8_coef[6]*-log(inputV[,6]) + PC8_coef[7]*-log(inputV[,7]) + PC8_coef[8]*-log(inputV[,8])
      
      PC = cbind(PC1,PC2,PC3,PC4,PC5,PC6,PC7,PC8)  
      
      return(PC) 
      
    } else if(dim(inputV)[2]==9) {
      
      PC1_coef <- pca$rotation[,1]   # coef of PC1
      PC2_coef <- pca$rotation[,2]   # coef of PC2
      PC3_coef <- pca$rotation[,3]   # coef of PC3
      PC4_coef <- pca$rotation[,4]   # coef of PC4
      PC5_coef <- pca$rotation[,5]   # coef of PC5
      PC6_coef <- pca$rotation[,6]   # coef of PC6
      PC7_coef <- pca$rotation[,7]   # coef of PC7
      PC8_coef <- pca$rotation[,8]   # coef of PC8
      PC9_coef <- pca$rotation[,9]   # coef of PC9
      
      PC1 <- PC1_coef[1]*-log(inputV[,1]) + PC1_coef[2]*-log(inputV[,2]) + PC1_coef[3]*-log(inputV[,3]) + PC1_coef[4]*-log(inputV[,4]) + 
        PC1_coef[5]*-log(inputV[,5]) + PC1_coef[6]*-log(inputV[,6]) + PC1_coef[7]*-log(inputV[,7]) + PC1_coef[8]*-log(inputV[,8]) +
        PC1_coef[9]*-log(inputV[,9])
      
      PC2 <- PC2_coef[1]*-log(inputV[,1]) + PC2_coef[2]*-log(inputV[,2]) + PC2_coef[3]*-log(inputV[,3]) + PC2_coef[4]*-log(inputV[,4]) +
        PC2_coef[5]*-log(inputV[,5]) + PC2_coef[6]*-log(inputV[,6]) + PC2_coef[7]*-log(inputV[,7]) + PC2_coef[8]*-log(inputV[,8]) +
        PC2_coef[9]*-log(inputV[,9])
      
      PC3 <- PC3_coef[1]*-log(inputV[,1]) + PC3_coef[2]*-log(inputV[,2]) + PC3_coef[3]*-log(inputV[,3]) + PC3_coef[4]*-log(inputV[,4]) + 
        PC3_coef[5]*-log(inputV[,5]) + PC3_coef[6]*-log(inputV[,6]) + PC3_coef[7]*-log(inputV[,7]) + PC3_coef[8]*-log(inputV[,8]) +
        PC3_coef[9]*-log(inputV[,9])
      
      PC4 <- PC4_coef[1]*-log(inputV[,1]) + PC4_coef[2]*-log(inputV[,2]) + PC4_coef[3]*-log(inputV[,3]) + PC4_coef[4]*-log(inputV[,4]) + 
        PC4_coef[5]*-log(inputV[,5]) + PC4_coef[6]*-log(inputV[,6]) + PC4_coef[7]*-log(inputV[,7]) + PC4_coef[8]*-log(inputV[,8]) +
        PC4_coef[9]*-log(inputV[,9])
      
      PC5 <- PC5_coef[1]*-log(inputV[,1]) + PC5_coef[2]*-log(inputV[,2]) + PC5_coef[3]*-log(inputV[,3]) + PC5_coef[4]*-log(inputV[,4]) + 
        PC5_coef[5]*-log(inputV[,5]) + PC5_coef[6]*-log(inputV[,6]) + PC5_coef[7]*-log(inputV[,7]) + PC5_coef[8]*-log(inputV[,8]) +
        PC5_coef[9]*-log(inputV[,9])
      
      PC6 <- PC6_coef[1]*-log(inputV[,1]) + PC6_coef[2]*-log(inputV[,2]) + PC6_coef[3]*-log(inputV[,3]) + PC6_coef[4]*-log(inputV[,4]) + 
        PC6_coef[5]*-log(inputV[,5]) + PC6_coef[6]*-log(inputV[,6]) + PC6_coef[7]*-log(inputV[,7]) + PC6_coef[8]*-log(inputV[,8]) +
        PC6_coef[9]*-log(inputV[,9])
      
      PC7 <- PC7_coef[1]*-log(inputV[,1]) + PC7_coef[2]*-log(inputV[,2]) + PC7_coef[3]*-log(inputV[,3]) + PC7_coef[4]*-log(inputV[,4]) + 
        PC7_coef[5]*-log(inputV[,5]) + PC7_coef[6]*-log(inputV[,6]) + PC7_coef[7]*-log(inputV[,7]) + PC7_coef[8]*-log(inputV[,8]) +
        PC7_coef[9]*-log(inputV[,9])
      
      PC8 <- PC8_coef[1]*-log(inputV[,1]) + PC8_coef[2]*-log(inputV[,2]) + PC8_coef[3]*-log(inputV[,3]) + PC8_coef[4]*-log(inputV[,4]) + 
        PC8_coef[5]*-log(inputV[,5]) + PC8_coef[6]*-log(inputV[,6]) + PC8_coef[7]*-log(inputV[,7]) + PC8_coef[8]*-log(inputV[,8]) +
        PC8_coef[9]*-log(inputV[,9])
      
      PC9 <- PC9_coef[1]*-log(inputV[,1]) + PC9_coef[2]*-log(inputV[,2]) + PC9_coef[3]*-log(inputV[,3]) + PC9_coef[4]*-log(inputV[,4]) + 
        PC9_coef[5]*-log(inputV[,5]) + PC9_coef[6]*-log(inputV[,6]) + PC9_coef[7]*-log(inputV[,7]) + PC9_coef[8]*-log(inputV[,8]) +
        PC9_coef[9]*-log(inputV[,9])
      
      PC = cbind(PC1,PC2,PC3,PC4,PC5,PC6,PC7,PC8,PC9)  
      
      return(PC) 
      
    } else if(dim(inputV)[2]==10) {
      
      PC1_coef <- pca$rotation[,1]   # coef of PC1
      PC2_coef <- pca$rotation[,2]   # coef of PC2
      PC3_coef <- pca$rotation[,3]   # coef of PC3
      PC4_coef <- pca$rotation[,4]   # coef of PC4
      PC5_coef <- pca$rotation[,5]   # coef of PC5
      PC6_coef <- pca$rotation[,6]   # coef of PC6
      PC7_coef <- pca$rotation[,7]   # coef of PC7
      PC8_coef <- pca$rotation[,8]   # coef of PC8
      PC9_coef <- pca$rotation[,9]   # coef of PC9
      PC10_coef <- pca$rotation[,10]   # coef of PC10
      
      PC1 <- PC1_coef[1]*-log(inputV[,1]) + PC1_coef[2]*-log(inputV[,2]) + PC1_coef[3]*-log(inputV[,3]) + PC1_coef[4]*-log(inputV[,4]) + 
        PC1_coef[5]*-log(inputV[,5]) + PC1_coef[6]*-log(inputV[,6]) + PC1_coef[7]*-log(inputV[,7]) + PC1_coef[8]*-log(inputV[,8]) +
        PC1_coef[9]*-log(inputV[,9]) + PC1_coef[10]*-log(inputV[,10])
      
      PC2 <- PC2_coef[1]*-log(inputV[,1]) + PC2_coef[2]*-log(inputV[,2]) + PC2_coef[3]*-log(inputV[,3]) + PC2_coef[4]*-log(inputV[,4]) +
        PC2_coef[5]*-log(inputV[,5]) + PC2_coef[6]*-log(inputV[,6]) + PC2_coef[7]*-log(inputV[,7]) + PC2_coef[8]*-log(inputV[,8]) +
        PC2_coef[9]*-log(inputV[,9]) + PC2_coef[10]*-log(inputV[,10])
      
      PC3 <- PC3_coef[1]*-log(inputV[,1]) + PC3_coef[2]*-log(inputV[,2]) + PC3_coef[3]*-log(inputV[,3]) + PC3_coef[4]*-log(inputV[,4]) + 
        PC3_coef[5]*-log(inputV[,5]) + PC3_coef[6]*-log(inputV[,6]) + PC3_coef[7]*-log(inputV[,7]) + PC3_coef[8]*-log(inputV[,8]) +
        PC3_coef[9]*-log(inputV[,9]) + PC3_coef[10]*-log(inputV[,10])
      
      PC4 <- PC4_coef[1]*-log(inputV[,1]) + PC4_coef[2]*-log(inputV[,2]) + PC4_coef[3]*-log(inputV[,3]) + PC4_coef[4]*-log(inputV[,4]) + 
        PC4_coef[5]*-log(inputV[,5]) + PC4_coef[6]*-log(inputV[,6]) + PC4_coef[7]*-log(inputV[,7]) + PC4_coef[8]*-log(inputV[,8]) +
        PC4_coef[9]*-log(inputV[,9]) + PC4_coef[10]*-log(inputV[,10])
      
      PC5 <- PC5_coef[1]*-log(inputV[,1]) + PC5_coef[2]*-log(inputV[,2]) + PC5_coef[3]*-log(inputV[,3]) + PC5_coef[4]*-log(inputV[,4]) + 
        PC5_coef[5]*-log(inputV[,5]) + PC5_coef[6]*-log(inputV[,6]) + PC5_coef[7]*-log(inputV[,7]) + PC5_coef[8]*-log(inputV[,8]) +
        PC5_coef[9]*-log(inputV[,9]) + PC5_coef[10]*-log(inputV[,10])
      
      PC6 <- PC6_coef[1]*-log(inputV[,1]) + PC6_coef[2]*-log(inputV[,2]) + PC6_coef[3]*-log(inputV[,3]) + PC6_coef[4]*-log(inputV[,4]) + 
        PC6_coef[5]*-log(inputV[,5]) + PC6_coef[6]*-log(inputV[,6]) + PC6_coef[7]*-log(inputV[,7]) + PC6_coef[8]*-log(inputV[,8]) +
        PC6_coef[9]*-log(inputV[,9]) + PC6_coef[10]*-log(inputV[,10])
      
      PC7 <- PC7_coef[1]*-log(inputV[,1]) + PC7_coef[2]*-log(inputV[,2]) + PC7_coef[3]*-log(inputV[,3]) + PC7_coef[4]*-log(inputV[,4]) + 
        PC7_coef[5]*-log(inputV[,5]) + PC7_coef[6]*-log(inputV[,6]) + PC7_coef[7]*-log(inputV[,7]) + PC7_coef[8]*-log(inputV[,8]) +
        PC7_coef[9]*-log(inputV[,9]) + PC7_coef[10]*-log(inputV[,10])
      
      PC8 <- PC8_coef[1]*-log(inputV[,1]) + PC8_coef[2]*-log(inputV[,2]) + PC8_coef[3]*-log(inputV[,3]) + PC8_coef[4]*-log(inputV[,4]) + 
        PC8_coef[5]*-log(inputV[,5]) + PC8_coef[6]*-log(inputV[,6]) + PC8_coef[7]*-log(inputV[,7]) + PC8_coef[8]*-log(inputV[,8]) +
        PC8_coef[9]*-log(inputV[,9]) + PC8_coef[10]*-log(inputV[,10])
      
      PC9 <- PC9_coef[1]*-log(inputV[,1]) + PC9_coef[2]*-log(inputV[,2]) + PC9_coef[3]*-log(inputV[,3]) + PC9_coef[4]*-log(inputV[,4]) + 
        PC9_coef[5]*-log(inputV[,5]) + PC9_coef[6]*-log(inputV[,6]) + PC9_coef[7]*-log(inputV[,7]) + PC9_coef[8]*-log(inputV[,8]) +
        PC9_coef[9]*-log(inputV[,9]) + PC9_coef[10]*-log(inputV[,10])
      
      PC10 <- PC10_coef[1]*-log(inputV[,1]) + PC10_coef[2]*-log(inputV[,2]) + PC10_coef[3]*-log(inputV[,3]) + PC10_coef[4]*-log(inputV[,4]) + 
        PC10_coef[5]*-log(inputV[,5]) + PC10_coef[6]*-log(inputV[,6]) + PC10_coef[7]*-log(inputV[,7]) + PC10_coef[8]*-log(inputV[,8]) +
        PC10_coef[9]*-log(inputV[,9]) + PC10_coef[10]*-log(inputV[,10])
      
      PC = cbind(PC1,PC2,PC3,PC4,PC5,PC6,PC7,PC8,PC9,PC10)  
      
      return(PC) 
      
    } else if(dim(inputV)[2]==11) {
      
      PC1_coef <- pca$rotation[,1]   # coef of PC1
      PC2_coef <- pca$rotation[,2]   # coef of PC2
      PC3_coef <- pca$rotation[,3]   # coef of PC3
      PC4_coef <- pca$rotation[,4]   # coef of PC4
      PC5_coef <- pca$rotation[,5]   # coef of PC5
      PC6_coef <- pca$rotation[,6]   # coef of PC6
      PC7_coef <- pca$rotation[,7]   # coef of PC7
      PC8_coef <- pca$rotation[,8]   # coef of PC8
      PC9_coef <- pca$rotation[,9]   # coef of PC9
      PC10_coef <- pca$rotation[,10]   # coef of PC10
      PC11_coef <- pca$rotation[,11]   # coef of PC11
      
      PC1 <- PC1_coef[1]*-log(inputV[,1]) + PC1_coef[2]*-log(inputV[,2]) + PC1_coef[3]*-log(inputV[,3]) + PC1_coef[4]*-log(inputV[,4]) + 
        PC1_coef[5]*-log(inputV[,5]) + PC1_coef[6]*-log(inputV[,6]) + PC1_coef[7]*-log(inputV[,7]) + PC1_coef[8]*-log(inputV[,8]) +
        PC1_coef[9]*-log(inputV[,9]) + PC1_coef[10]*-log(inputV[,10]) + PC1_coef[11]*-log(inputV[,11])
      
      PC2 <- PC2_coef[1]*-log(inputV[,1]) + PC2_coef[2]*-log(inputV[,2]) + PC2_coef[3]*-log(inputV[,3]) + PC2_coef[4]*-log(inputV[,4]) +
        PC2_coef[5]*-log(inputV[,5]) + PC2_coef[6]*-log(inputV[,6]) + PC2_coef[7]*-log(inputV[,7]) + PC2_coef[8]*-log(inputV[,8]) +
        PC2_coef[9]*-log(inputV[,9]) + PC2_coef[10]*-log(inputV[,10]) + PC2_coef[11]*-log(inputV[,11])
      
      PC3 <- PC3_coef[1]*-log(inputV[,1]) + PC3_coef[2]*-log(inputV[,2]) + PC3_coef[3]*-log(inputV[,3]) + PC3_coef[4]*-log(inputV[,4]) + 
        PC3_coef[5]*-log(inputV[,5]) + PC3_coef[6]*-log(inputV[,6]) + PC3_coef[7]*-log(inputV[,7]) + PC3_coef[8]*-log(inputV[,8]) +
        PC3_coef[9]*-log(inputV[,9]) + PC3_coef[10]*-log(inputV[,10]) + PC3_coef[11]*-log(inputV[,11])
      
      PC4 <- PC4_coef[1]*-log(inputV[,1]) + PC4_coef[2]*-log(inputV[,2]) + PC4_coef[3]*-log(inputV[,3]) + PC4_coef[4]*-log(inputV[,4]) + 
        PC4_coef[5]*-log(inputV[,5]) + PC4_coef[6]*-log(inputV[,6]) + PC4_coef[7]*-log(inputV[,7]) + PC4_coef[8]*-log(inputV[,8]) +
        PC4_coef[9]*-log(inputV[,9]) + PC4_coef[10]*-log(inputV[,10]) + PC4_coef[11]*-log(inputV[,11])
      
      PC5 <- PC5_coef[1]*-log(inputV[,1]) + PC5_coef[2]*-log(inputV[,2]) + PC5_coef[3]*-log(inputV[,3]) + PC5_coef[4]*-log(inputV[,4]) + 
        PC5_coef[5]*-log(inputV[,5]) + PC5_coef[6]*-log(inputV[,6]) + PC5_coef[7]*-log(inputV[,7]) + PC5_coef[8]*-log(inputV[,8]) +
        PC5_coef[9]*-log(inputV[,9]) + PC5_coef[10]*-log(inputV[,10]) + PC5_coef[11]*-log(inputV[,11])
      
      PC6 <- PC6_coef[1]*-log(inputV[,1]) + PC6_coef[2]*-log(inputV[,2]) + PC6_coef[3]*-log(inputV[,3]) + PC6_coef[4]*-log(inputV[,4]) + 
        PC6_coef[5]*-log(inputV[,5]) + PC6_coef[6]*-log(inputV[,6]) + PC6_coef[7]*-log(inputV[,7]) + PC6_coef[8]*-log(inputV[,8]) +
        PC6_coef[9]*-log(inputV[,9]) + PC6_coef[10]*-log(inputV[,10]) + PC6_coef[11]*-log(inputV[,11])
      
      PC7 <- PC7_coef[1]*-log(inputV[,1]) + PC7_coef[2]*-log(inputV[,2]) + PC7_coef[3]*-log(inputV[,3]) + PC7_coef[4]*-log(inputV[,4]) + 
        PC7_coef[5]*-log(inputV[,5]) + PC7_coef[6]*-log(inputV[,6]) + PC7_coef[7]*-log(inputV[,7]) + PC7_coef[8]*-log(inputV[,8]) +
        PC7_coef[9]*-log(inputV[,9]) + PC7_coef[10]*-log(inputV[,10]) + PC7_coef[11]*-log(inputV[,11])
      
      PC8 <- PC8_coef[1]*-log(inputV[,1]) + PC8_coef[2]*-log(inputV[,2]) + PC8_coef[3]*-log(inputV[,3]) + PC8_coef[4]*-log(inputV[,4]) + 
        PC8_coef[5]*-log(inputV[,5]) + PC8_coef[6]*-log(inputV[,6]) + PC8_coef[7]*-log(inputV[,7]) + PC8_coef[8]*-log(inputV[,8]) +
        PC8_coef[9]*-log(inputV[,9]) + PC8_coef[10]*-log(inputV[,10]) + PC8_coef[11]*-log(inputV[,11])
      
      PC9 <- PC9_coef[1]*-log(inputV[,1]) + PC9_coef[2]*-log(inputV[,2]) + PC9_coef[3]*-log(inputV[,3]) + PC9_coef[4]*-log(inputV[,4]) + 
        PC9_coef[5]*-log(inputV[,5]) + PC9_coef[6]*-log(inputV[,6]) + PC9_coef[7]*-log(inputV[,7]) + PC9_coef[8]*-log(inputV[,8]) +
        PC9_coef[9]*-log(inputV[,9]) + PC9_coef[10]*-log(inputV[,10]) + PC9_coef[11]*-log(inputV[,11])
      
      PC10 <- PC10_coef[1]*-log(inputV[,1]) + PC10_coef[2]*-log(inputV[,2]) + PC10_coef[3]*-log(inputV[,3]) + PC10_coef[4]*-log(inputV[,4]) + 
        PC10_coef[5]*-log(inputV[,5]) + PC10_coef[6]*-log(inputV[,6]) + PC10_coef[7]*-log(inputV[,7]) + PC10_coef[8]*-log(inputV[,8]) +
        PC10_coef[9]*-log(inputV[,9]) + PC10_coef[10]*-log(inputV[,10]) + PC10_coef[11]*-log(inputV[,11])
      
      PC11 <- PC11_coef[1]*-log(inputV[,1]) + PC11_coef[2]*-log(inputV[,2]) + PC11_coef[3]*-log(inputV[,3]) + PC11_coef[4]*-log(inputV[,4]) + 
        PC11_coef[5]*-log(inputV[,5]) + PC11_coef[6]*-log(inputV[,6]) + PC11_coef[7]*-log(inputV[,7]) + PC11_coef[8]*-log(inputV[,8]) +
        PC11_coef[9]*-log(inputV[,9]) + PC11_coef[10]*-log(inputV[,10]) + PC11_coef[11]*-log(inputV[,11])
      
      PC = cbind(PC1,PC2,PC3,PC4,PC5,PC6,PC7,PC8,PC9,PC10,PC11)  
      
      return(PC) 
      
    } else if(dim(inputV)[2]==12) {
      
      PC1_coef <- pca$rotation[,1]   # coef of PC1
      PC2_coef <- pca$rotation[,2]   # coef of PC2
      PC3_coef <- pca$rotation[,3]   # coef of PC3
      PC4_coef <- pca$rotation[,4]   # coef of PC4
      PC5_coef <- pca$rotation[,5]   # coef of PC5
      PC6_coef <- pca$rotation[,6]   # coef of PC6
      PC7_coef <- pca$rotation[,7]   # coef of PC7
      PC8_coef <- pca$rotation[,8]   # coef of PC8
      PC9_coef <- pca$rotation[,9]   # coef of PC9
      PC10_coef <- pca$rotation[,10]   # coef of PC10
      PC11_coef <- pca$rotation[,11]   # coef of PC11
      PC12_coef <- pca$rotation[,12]   # coef of PC12
      
      PC1 <- PC1_coef[1]*-log(inputV[,1]) + PC1_coef[2]*-log(inputV[,2]) + PC1_coef[3]*-log(inputV[,3]) + PC1_coef[4]*-log(inputV[,4]) + 
        PC1_coef[5]*-log(inputV[,5]) + PC1_coef[6]*-log(inputV[,6]) + PC1_coef[7]*-log(inputV[,7]) + PC1_coef[8]*-log(inputV[,8]) +
        PC1_coef[9]*-log(inputV[,9]) + PC1_coef[10]*-log(inputV[,10]) + PC1_coef[11]*-log(inputV[,11]) + PC1_coef[12]*-log(inputV[,12])
      
      PC2 <- PC2_coef[1]*-log(inputV[,1]) + PC2_coef[2]*-log(inputV[,2]) + PC2_coef[3]*-log(inputV[,3]) + PC2_coef[4]*-log(inputV[,4]) +
        PC2_coef[5]*-log(inputV[,5]) + PC2_coef[6]*-log(inputV[,6]) + PC2_coef[7]*-log(inputV[,7]) + PC2_coef[8]*-log(inputV[,8]) +
        PC2_coef[9]*-log(inputV[,9]) + PC2_coef[10]*-log(inputV[,10]) + PC2_coef[11]*-log(inputV[,11]) + PC2_coef[12]*-log(inputV[,12])
      
      PC3 <- PC3_coef[1]*-log(inputV[,1]) + PC3_coef[2]*-log(inputV[,2]) + PC3_coef[3]*-log(inputV[,3]) + PC3_coef[4]*-log(inputV[,4]) + 
        PC3_coef[5]*-log(inputV[,5]) + PC3_coef[6]*-log(inputV[,6]) + PC3_coef[7]*-log(inputV[,7]) + PC3_coef[8]*-log(inputV[,8]) +
        PC3_coef[9]*-log(inputV[,9]) + PC3_coef[10]*-log(inputV[,10]) + PC3_coef[11]*-log(inputV[,11]) + PC3_coef[12]*-log(inputV[,12])
      
      PC4 <- PC4_coef[1]*-log(inputV[,1]) + PC4_coef[2]*-log(inputV[,2]) + PC4_coef[3]*-log(inputV[,3]) + PC4_coef[4]*-log(inputV[,4]) + 
        PC4_coef[5]*-log(inputV[,5]) + PC4_coef[6]*-log(inputV[,6]) + PC4_coef[7]*-log(inputV[,7]) + PC4_coef[8]*-log(inputV[,8]) +
        PC4_coef[9]*-log(inputV[,9]) + PC4_coef[10]*-log(inputV[,10]) + PC4_coef[11]*-log(inputV[,11]) + PC4_coef[12]*-log(inputV[,12])
      
      PC5 <- PC5_coef[1]*-log(inputV[,1]) + PC5_coef[2]*-log(inputV[,2]) + PC5_coef[3]*-log(inputV[,3]) + PC5_coef[4]*-log(inputV[,4]) + 
        PC5_coef[5]*-log(inputV[,5]) + PC5_coef[6]*-log(inputV[,6]) + PC5_coef[7]*-log(inputV[,7]) + PC5_coef[8]*-log(inputV[,8]) +
        PC5_coef[9]*-log(inputV[,9]) + PC5_coef[10]*-log(inputV[,10]) + PC5_coef[11]*-log(inputV[,11]) + PC5_coef[12]*-log(inputV[,12])
      
      PC6 <- PC6_coef[1]*-log(inputV[,1]) + PC6_coef[2]*-log(inputV[,2]) + PC6_coef[3]*-log(inputV[,3]) + PC6_coef[4]*-log(inputV[,4]) + 
        PC6_coef[5]*-log(inputV[,5]) + PC6_coef[6]*-log(inputV[,6]) + PC6_coef[7]*-log(inputV[,7]) + PC6_coef[8]*-log(inputV[,8]) +
        PC6_coef[9]*-log(inputV[,9]) + PC6_coef[10]*-log(inputV[,10]) + PC6_coef[11]*-log(inputV[,11]) + PC6_coef[12]*-log(inputV[,12])
      
      PC7 <- PC7_coef[1]*-log(inputV[,1]) + PC7_coef[2]*-log(inputV[,2]) + PC7_coef[3]*-log(inputV[,3]) + PC7_coef[4]*-log(inputV[,4]) + 
        PC7_coef[5]*-log(inputV[,5]) + PC7_coef[6]*-log(inputV[,6]) + PC7_coef[7]*-log(inputV[,7]) + PC7_coef[8]*-log(inputV[,8]) +
        PC7_coef[9]*-log(inputV[,9]) + PC7_coef[10]*-log(inputV[,10]) + PC7_coef[11]*-log(inputV[,11]) + PC7_coef[12]*-log(inputV[,12])
      
      PC8 <- PC8_coef[1]*-log(inputV[,1]) + PC8_coef[2]*-log(inputV[,2]) + PC8_coef[3]*-log(inputV[,3]) + PC8_coef[4]*-log(inputV[,4]) + 
        PC8_coef[5]*-log(inputV[,5]) + PC8_coef[6]*-log(inputV[,6]) + PC8_coef[7]*-log(inputV[,7]) + PC8_coef[8]*-log(inputV[,8]) +
        PC8_coef[9]*-log(inputV[,9]) + PC8_coef[10]*-log(inputV[,10]) + PC8_coef[11]*-log(inputV[,11]) + PC8_coef[12]*-log(inputV[,12])
      
      PC9 <- PC9_coef[1]*-log(inputV[,1]) + PC9_coef[2]*-log(inputV[,2]) + PC9_coef[3]*-log(inputV[,3]) + PC9_coef[4]*-log(inputV[,4]) + 
        PC9_coef[5]*-log(inputV[,5]) + PC9_coef[6]*-log(inputV[,6]) + PC9_coef[7]*-log(inputV[,7]) + PC9_coef[8]*-log(inputV[,8]) +
        PC9_coef[9]*-log(inputV[,9]) + PC9_coef[10]*-log(inputV[,10]) + PC9_coef[11]*-log(inputV[,11]) + PC9_coef[12]*-log(inputV[,12])
      
      PC10 <- PC10_coef[1]*-log(inputV[,1]) + PC10_coef[2]*-log(inputV[,2]) + PC10_coef[3]*-log(inputV[,3]) + PC10_coef[4]*-log(inputV[,4]) + 
        PC10_coef[5]*-log(inputV[,5]) + PC10_coef[6]*-log(inputV[,6]) + PC10_coef[7]*-log(inputV[,7]) + PC10_coef[8]*-log(inputV[,8]) +
        PC10_coef[9]*-log(inputV[,9]) + PC10_coef[10]*-log(inputV[,10]) + PC10_coef[11]*-log(inputV[,11]) + PC10_coef[12]*-log(inputV[,12])
      
      PC11 <- PC11_coef[1]*-log(inputV[,1]) + PC11_coef[2]*-log(inputV[,2]) + PC11_coef[3]*-log(inputV[,3]) + PC11_coef[4]*-log(inputV[,4]) + 
        PC11_coef[5]*-log(inputV[,5]) + PC11_coef[6]*-log(inputV[,6]) + PC11_coef[7]*-log(inputV[,7]) + PC11_coef[8]*-log(inputV[,8]) +
        PC11_coef[9]*-log(inputV[,9]) + PC11_coef[10]*-log(inputV[,10]) + PC11_coef[11]*-log(inputV[,11]) + PC11_coef[12]*-log(inputV[,12])
      
      PC12 <- PC12_coef[1]*-log(inputV[,1]) + PC12_coef[2]*-log(inputV[,2]) + PC12_coef[3]*-log(inputV[,3]) + PC12_coef[4]*-log(inputV[,4]) + 
        PC12_coef[5]*-log(inputV[,5]) + PC12_coef[6]*-log(inputV[,6]) + PC12_coef[7]*-log(inputV[,7]) + PC12_coef[8]*-log(inputV[,8]) +
        PC12_coef[9]*-log(inputV[,9]) + PC12_coef[10]*-log(inputV[,10]) + PC12_coef[11]*-log(inputV[,11]) + PC12_coef[12]*-log(inputV[,12])
      
      PC = cbind(PC1,PC2,PC3,PC4,PC5,PC6,PC7,PC8,PC9,PC10,PC11,PC12)  
      
      return(PC) 
      
    } else if(dim(inputV)[2]==13) {
      
      PC1_coef <- pca$rotation[,1]   # coef of PC1
      PC2_coef <- pca$rotation[,2]   # coef of PC2
      PC3_coef <- pca$rotation[,3]   # coef of PC3
      PC4_coef <- pca$rotation[,4]   # coef of PC4
      PC5_coef <- pca$rotation[,5]   # coef of PC5
      PC6_coef <- pca$rotation[,6]   # coef of PC6
      PC7_coef <- pca$rotation[,7]   # coef of PC7
      PC8_coef <- pca$rotation[,8]   # coef of PC8
      PC9_coef <- pca$rotation[,9]   # coef of PC9
      PC10_coef <- pca$rotation[,10]   # coef of PC10
      PC11_coef <- pca$rotation[,11]   # coef of PC11
      PC12_coef <- pca$rotation[,12]   # coef of PC12
      PC13_coef <- pca$rotation[,13]   # coef of PC13
      
      PC1 <- PC1_coef[1]*-log(inputV[,1]) + PC1_coef[2]*-log(inputV[,2]) + PC1_coef[3]*-log(inputV[,3]) + PC1_coef[4]*-log(inputV[,4]) + 
        PC1_coef[5]*-log(inputV[,5]) + PC1_coef[6]*-log(inputV[,6]) + PC1_coef[7]*-log(inputV[,7]) + PC1_coef[8]*-log(inputV[,8]) +
        PC1_coef[9]*-log(inputV[,9]) + PC1_coef[10]*-log(inputV[,10]) + PC1_coef[11]*-log(inputV[,11]) + PC1_coef[12]*-log(inputV[,12]) +
        PC1_coef[13]*-log(inputV[,13])
      
      PC2 <- PC2_coef[1]*-log(inputV[,1]) + PC2_coef[2]*-log(inputV[,2]) + PC2_coef[3]*-log(inputV[,3]) + PC2_coef[4]*-log(inputV[,4]) +
        PC2_coef[5]*-log(inputV[,5]) + PC2_coef[6]*-log(inputV[,6]) + PC2_coef[7]*-log(inputV[,7]) + PC2_coef[8]*-log(inputV[,8]) +
        PC2_coef[9]*-log(inputV[,9]) + PC2_coef[10]*-log(inputV[,10]) + PC2_coef[11]*-log(inputV[,11]) + PC2_coef[12]*-log(inputV[,12]) +
        PC2_coef[13]*-log(inputV[,13])
      
      PC3 <- PC3_coef[1]*-log(inputV[,1]) + PC3_coef[2]*-log(inputV[,2]) + PC3_coef[3]*-log(inputV[,3]) + PC3_coef[4]*-log(inputV[,4]) + 
        PC3_coef[5]*-log(inputV[,5]) + PC3_coef[6]*-log(inputV[,6]) + PC3_coef[7]*-log(inputV[,7]) + PC3_coef[8]*-log(inputV[,8]) +
        PC3_coef[9]*-log(inputV[,9]) + PC3_coef[10]*-log(inputV[,10]) + PC3_coef[11]*-log(inputV[,11]) + PC3_coef[12]*-log(inputV[,12]) +
        PC3_coef[13]*-log(inputV[,13])
      
      PC4 <- PC4_coef[1]*-log(inputV[,1]) + PC4_coef[2]*-log(inputV[,2]) + PC4_coef[3]*-log(inputV[,3]) + PC4_coef[4]*-log(inputV[,4]) + 
        PC4_coef[5]*-log(inputV[,5]) + PC4_coef[6]*-log(inputV[,6]) + PC4_coef[7]*-log(inputV[,7]) + PC4_coef[8]*-log(inputV[,8]) +
        PC4_coef[9]*-log(inputV[,9]) + PC4_coef[10]*-log(inputV[,10]) + PC4_coef[11]*-log(inputV[,11]) + PC4_coef[12]*-log(inputV[,12]) +
        PC4_coef[13]*-log(inputV[,13])
      
      PC5 <- PC5_coef[1]*-log(inputV[,1]) + PC5_coef[2]*-log(inputV[,2]) + PC5_coef[3]*-log(inputV[,3]) + PC5_coef[4]*-log(inputV[,4]) + 
        PC5_coef[5]*-log(inputV[,5]) + PC5_coef[6]*-log(inputV[,6]) + PC5_coef[7]*-log(inputV[,7]) + PC5_coef[8]*-log(inputV[,8]) +
        PC5_coef[9]*-log(inputV[,9]) + PC5_coef[10]*-log(inputV[,10]) + PC5_coef[11]*-log(inputV[,11]) + PC5_coef[12]*-log(inputV[,12]) +
        PC5_coef[13]*-log(inputV[,13])
      
      PC6 <- PC6_coef[1]*-log(inputV[,1]) + PC6_coef[2]*-log(inputV[,2]) + PC6_coef[3]*-log(inputV[,3]) + PC6_coef[4]*-log(inputV[,4]) + 
        PC6_coef[5]*-log(inputV[,5]) + PC6_coef[6]*-log(inputV[,6]) + PC6_coef[7]*-log(inputV[,7]) + PC6_coef[8]*-log(inputV[,8]) +
        PC6_coef[9]*-log(inputV[,9]) + PC6_coef[10]*-log(inputV[,10]) + PC6_coef[11]*-log(inputV[,11]) + PC6_coef[12]*-log(inputV[,12]) +
        PC6_coef[13]*-log(inputV[,13])
      
      PC7 <- PC7_coef[1]*-log(inputV[,1]) + PC7_coef[2]*-log(inputV[,2]) + PC7_coef[3]*-log(inputV[,3]) + PC7_coef[4]*-log(inputV[,4]) + 
        PC7_coef[5]*-log(inputV[,5]) + PC7_coef[6]*-log(inputV[,6]) + PC7_coef[7]*-log(inputV[,7]) + PC7_coef[8]*-log(inputV[,8]) +
        PC7_coef[9]*-log(inputV[,9]) + PC7_coef[10]*-log(inputV[,10]) + PC7_coef[11]*-log(inputV[,11]) + PC7_coef[12]*-log(inputV[,12]) +
        PC7_coef[13]*-log(inputV[,13])
      
      PC8 <- PC8_coef[1]*-log(inputV[,1]) + PC8_coef[2]*-log(inputV[,2]) + PC8_coef[3]*-log(inputV[,3]) + PC8_coef[4]*-log(inputV[,4]) + 
        PC8_coef[5]*-log(inputV[,5]) + PC8_coef[6]*-log(inputV[,6]) + PC8_coef[7]*-log(inputV[,7]) + PC8_coef[8]*-log(inputV[,8]) +
        PC8_coef[9]*-log(inputV[,9]) + PC8_coef[10]*-log(inputV[,10]) + PC8_coef[11]*-log(inputV[,11]) + PC8_coef[12]*-log(inputV[,12]) +
        PC8_coef[13]*-log(inputV[,13])
      
      PC9 <- PC9_coef[1]*-log(inputV[,1]) + PC9_coef[2]*-log(inputV[,2]) + PC9_coef[3]*-log(inputV[,3]) + PC9_coef[4]*-log(inputV[,4]) + 
        PC9_coef[5]*-log(inputV[,5]) + PC9_coef[6]*-log(inputV[,6]) + PC9_coef[7]*-log(inputV[,7]) + PC9_coef[8]*-log(inputV[,8]) +
        PC9_coef[9]*-log(inputV[,9]) + PC9_coef[10]*-log(inputV[,10]) + PC9_coef[11]*-log(inputV[,11]) + PC9_coef[12]*-log(inputV[,12]) +
        PC9_coef[13]*-log(inputV[,13])
      
      PC10 <- PC10_coef[1]*-log(inputV[,1]) + PC10_coef[2]*-log(inputV[,2]) + PC10_coef[3]*-log(inputV[,3]) + PC10_coef[4]*-log(inputV[,4]) + 
        PC10_coef[5]*-log(inputV[,5]) + PC10_coef[6]*-log(inputV[,6]) + PC10_coef[7]*-log(inputV[,7]) + PC10_coef[8]*-log(inputV[,8]) +
        PC10_coef[9]*-log(inputV[,9]) + PC10_coef[10]*-log(inputV[,10]) + PC10_coef[11]*-log(inputV[,11]) + PC10_coef[12]*-log(inputV[,12]) + 
        PC10_coef[13]*-log(inputV[,13])
      
      PC11 <- PC11_coef[1]*-log(inputV[,1]) + PC11_coef[2]*-log(inputV[,2]) + PC11_coef[3]*-log(inputV[,3]) + PC11_coef[4]*-log(inputV[,4]) + 
        PC11_coef[5]*-log(inputV[,5]) + PC11_coef[6]*-log(inputV[,6]) + PC11_coef[7]*-log(inputV[,7]) + PC11_coef[8]*-log(inputV[,8]) +
        PC11_coef[9]*-log(inputV[,9]) + PC11_coef[10]*-log(inputV[,10]) + PC11_coef[11]*-log(inputV[,11]) + PC11_coef[12]*-log(inputV[,12]) +
        PC11_coef[13]*-log(inputV[,13])
      
      PC12 <- PC12_coef[1]*-log(inputV[,1]) + PC12_coef[2]*-log(inputV[,2]) + PC12_coef[3]*-log(inputV[,3]) + PC12_coef[4]*-log(inputV[,4]) + 
        PC12_coef[5]*-log(inputV[,5]) + PC12_coef[6]*-log(inputV[,6]) + PC12_coef[7]*-log(inputV[,7]) + PC12_coef[8]*-log(inputV[,8]) +
        PC12_coef[9]*-log(inputV[,9]) + PC12_coef[10]*-log(inputV[,10]) + PC12_coef[11]*-log(inputV[,11]) + PC12_coef[12]*-log(inputV[,12]) +
        PC12_coef[13]*-log(inputV[,13])
      
      PC13 <- PC13_coef[1]*-log(inputV[,1]) + PC13_coef[2]*-log(inputV[,2]) + PC13_coef[3]*-log(inputV[,3]) + PC13_coef[4]*-log(inputV[,4]) + 
        PC13_coef[5]*-log(inputV[,5]) + PC13_coef[6]*-log(inputV[,6]) + PC13_coef[7]*-log(inputV[,7]) + PC13_coef[8]*-log(inputV[,8]) +
        PC13_coef[9]*-log(inputV[,9]) + PC13_coef[10]*-log(inputV[,10]) + PC13_coef[11]*-log(inputV[,11]) + PC13_coef[12]*-log(inputV[,12]) +
        PC13_coef[13]*-log(inputV[,13])
      
      PC = cbind(PC1,PC2,PC3,PC4,PC5,PC6,PC7,PC8,PC9,PC10,PC11,PC12,PC13)  
      
      return(PC) 
      
    } else if(dim(inputV)[2]==14) {
      
      PC1_coef <- pca$rotation[,1]   # coef of PC1
      PC2_coef <- pca$rotation[,2]   # coef of PC2
      PC3_coef <- pca$rotation[,3]   # coef of PC3
      PC4_coef <- pca$rotation[,4]   # coef of PC4
      PC5_coef <- pca$rotation[,5]   # coef of PC5
      PC6_coef <- pca$rotation[,6]   # coef of PC6
      PC7_coef <- pca$rotation[,7]   # coef of PC7
      PC8_coef <- pca$rotation[,8]   # coef of PC8
      PC9_coef <- pca$rotation[,9]   # coef of PC9
      PC10_coef <- pca$rotation[,10]   # coef of PC10
      PC11_coef <- pca$rotation[,11]   # coef of PC11
      PC12_coef <- pca$rotation[,12]   # coef of PC12
      PC13_coef <- pca$rotation[,13]   # coef of PC13
      PC14_coef <- pca$rotation[,14]   # coef of PC14
      
      PC1 <- PC1_coef[1]*-log(inputV[,1]) + PC1_coef[2]*-log(inputV[,2]) + PC1_coef[3]*-log(inputV[,3]) + PC1_coef[4]*-log(inputV[,4]) + 
        PC1_coef[5]*-log(inputV[,5]) + PC1_coef[6]*-log(inputV[,6]) + PC1_coef[7]*-log(inputV[,7]) + PC1_coef[8]*-log(inputV[,8]) +
        PC1_coef[9]*-log(inputV[,9]) + PC1_coef[10]*-log(inputV[,10]) + PC1_coef[11]*-log(inputV[,11]) + PC1_coef[12]*-log(inputV[,12]) +
        PC1_coef[13]*-log(inputV[,13]) + PC1_coef[14]*-log(inputV[,14])
      
      PC2 <- PC2_coef[1]*-log(inputV[,1]) + PC2_coef[2]*-log(inputV[,2]) + PC2_coef[3]*-log(inputV[,3]) + PC2_coef[4]*-log(inputV[,4]) +
        PC2_coef[5]*-log(inputV[,5]) + PC2_coef[6]*-log(inputV[,6]) + PC2_coef[7]*-log(inputV[,7]) + PC2_coef[8]*-log(inputV[,8]) +
        PC2_coef[9]*-log(inputV[,9]) + PC2_coef[10]*-log(inputV[,10]) + PC2_coef[11]*-log(inputV[,11]) + PC2_coef[12]*-log(inputV[,12]) +
        PC2_coef[13]*-log(inputV[,13]) + PC2_coef[14]*-log(inputV[,14])
      
      PC3 <- PC3_coef[1]*-log(inputV[,1]) + PC3_coef[2]*-log(inputV[,2]) + PC3_coef[3]*-log(inputV[,3]) + PC3_coef[4]*-log(inputV[,4]) + 
        PC3_coef[5]*-log(inputV[,5]) + PC3_coef[6]*-log(inputV[,6]) + PC3_coef[7]*-log(inputV[,7]) + PC3_coef[8]*-log(inputV[,8]) +
        PC3_coef[9]*-log(inputV[,9]) + PC3_coef[10]*-log(inputV[,10]) + PC3_coef[11]*-log(inputV[,11]) + PC3_coef[12]*-log(inputV[,12]) +
        PC3_coef[13]*-log(inputV[,13]) + PC3_coef[14]*-log(inputV[,14])
      
      PC4 <- PC4_coef[1]*-log(inputV[,1]) + PC4_coef[2]*-log(inputV[,2]) + PC4_coef[3]*-log(inputV[,3]) + PC4_coef[4]*-log(inputV[,4]) + 
        PC4_coef[5]*-log(inputV[,5]) + PC4_coef[6]*-log(inputV[,6]) + PC4_coef[7]*-log(inputV[,7]) + PC4_coef[8]*-log(inputV[,8]) +
        PC4_coef[9]*-log(inputV[,9]) + PC4_coef[10]*-log(inputV[,10]) + PC4_coef[11]*-log(inputV[,11]) + PC4_coef[12]*-log(inputV[,12]) +
        PC4_coef[13]*-log(inputV[,13]) + PC4_coef[14]*-log(inputV[,14])
      
      PC5 <- PC5_coef[1]*-log(inputV[,1]) + PC5_coef[2]*-log(inputV[,2]) + PC5_coef[3]*-log(inputV[,3]) + PC5_coef[4]*-log(inputV[,4]) + 
        PC5_coef[5]*-log(inputV[,5]) + PC5_coef[6]*-log(inputV[,6]) + PC5_coef[7]*-log(inputV[,7]) + PC5_coef[8]*-log(inputV[,8]) +
        PC5_coef[9]*-log(inputV[,9]) + PC5_coef[10]*-log(inputV[,10]) + PC5_coef[11]*-log(inputV[,11]) + PC5_coef[12]*-log(inputV[,12]) +
        PC5_coef[13]*-log(inputV[,13]) + PC5_coef[14]*-log(inputV[,14])
      
      PC6 <- PC6_coef[1]*-log(inputV[,1]) + PC6_coef[2]*-log(inputV[,2]) + PC6_coef[3]*-log(inputV[,3]) + PC6_coef[4]*-log(inputV[,4]) + 
        PC6_coef[5]*-log(inputV[,5]) + PC6_coef[6]*-log(inputV[,6]) + PC6_coef[7]*-log(inputV[,7]) + PC6_coef[8]*-log(inputV[,8]) +
        PC6_coef[9]*-log(inputV[,9]) + PC6_coef[10]*-log(inputV[,10]) + PC6_coef[11]*-log(inputV[,11]) + PC6_coef[12]*-log(inputV[,12]) +
        PC6_coef[13]*-log(inputV[,13]) + PC6_coef[14]*-log(inputV[,14])
      
      PC7 <- PC7_coef[1]*-log(inputV[,1]) + PC7_coef[2]*-log(inputV[,2]) + PC7_coef[3]*-log(inputV[,3]) + PC7_coef[4]*-log(inputV[,4]) + 
        PC7_coef[5]*-log(inputV[,5]) + PC7_coef[6]*-log(inputV[,6]) + PC7_coef[7]*-log(inputV[,7]) + PC7_coef[8]*-log(inputV[,8]) +
        PC7_coef[9]*-log(inputV[,9]) + PC7_coef[10]*-log(inputV[,10]) + PC7_coef[11]*-log(inputV[,11]) + PC7_coef[12]*-log(inputV[,12]) +
        PC7_coef[13]*-log(inputV[,13]) + PC7_coef[14]*-log(inputV[,14])
      
      PC8 <- PC8_coef[1]*-log(inputV[,1]) + PC8_coef[2]*-log(inputV[,2]) + PC8_coef[3]*-log(inputV[,3]) + PC8_coef[4]*-log(inputV[,4]) + 
        PC8_coef[5]*-log(inputV[,5]) + PC8_coef[6]*-log(inputV[,6]) + PC8_coef[7]*-log(inputV[,7]) + PC8_coef[8]*-log(inputV[,8]) +
        PC8_coef[9]*-log(inputV[,9]) + PC8_coef[10]*-log(inputV[,10]) + PC8_coef[11]*-log(inputV[,11]) + PC8_coef[12]*-log(inputV[,12]) +
        PC8_coef[13]*-log(inputV[,13]) + PC8_coef[14]*-log(inputV[,14])
      
      PC9 <- PC9_coef[1]*-log(inputV[,1]) + PC9_coef[2]*-log(inputV[,2]) + PC9_coef[3]*-log(inputV[,3]) + PC9_coef[4]*-log(inputV[,4]) + 
        PC9_coef[5]*-log(inputV[,5]) + PC9_coef[6]*-log(inputV[,6]) + PC9_coef[7]*-log(inputV[,7]) + PC9_coef[8]*-log(inputV[,8]) +
        PC9_coef[9]*-log(inputV[,9]) + PC9_coef[10]*-log(inputV[,10]) + PC9_coef[11]*-log(inputV[,11]) + PC9_coef[12]*-log(inputV[,12]) +
        PC9_coef[13]*-log(inputV[,13]) + PC9_coef[14]*-log(inputV[,14])
      
      PC10 <- PC10_coef[1]*-log(inputV[,1]) + PC10_coef[2]*-log(inputV[,2]) + PC10_coef[3]*-log(inputV[,3]) + PC10_coef[4]*-log(inputV[,4]) + 
        PC10_coef[5]*-log(inputV[,5]) + PC10_coef[6]*-log(inputV[,6]) + PC10_coef[7]*-log(inputV[,7]) + PC10_coef[8]*-log(inputV[,8]) +
        PC10_coef[9]*-log(inputV[,9]) + PC10_coef[10]*-log(inputV[,10]) + PC10_coef[11]*-log(inputV[,11]) + PC10_coef[12]*-log(inputV[,12]) + 
        PC10_coef[13]*-log(inputV[,13]) + PC10_coef[14]*-log(inputV[,14])
      
      PC11 <- PC11_coef[1]*-log(inputV[,1]) + PC11_coef[2]*-log(inputV[,2]) + PC11_coef[3]*-log(inputV[,3]) + PC11_coef[4]*-log(inputV[,4]) + 
        PC11_coef[5]*-log(inputV[,5]) + PC11_coef[6]*-log(inputV[,6]) + PC11_coef[7]*-log(inputV[,7]) + PC11_coef[8]*-log(inputV[,8]) +
        PC11_coef[9]*-log(inputV[,9]) + PC11_coef[10]*-log(inputV[,10]) + PC11_coef[11]*-log(inputV[,11]) + PC11_coef[12]*-log(inputV[,12]) +
        PC11_coef[13]*-log(inputV[,13]) + PC11_coef[14]*-log(inputV[,14])
      
      PC12 <- PC12_coef[1]*-log(inputV[,1]) + PC12_coef[2]*-log(inputV[,2]) + PC12_coef[3]*-log(inputV[,3]) + PC12_coef[4]*-log(inputV[,4]) + 
        PC12_coef[5]*-log(inputV[,5]) + PC12_coef[6]*-log(inputV[,6]) + PC12_coef[7]*-log(inputV[,7]) + PC12_coef[8]*-log(inputV[,8]) +
        PC12_coef[9]*-log(inputV[,9]) + PC12_coef[10]*-log(inputV[,10]) + PC12_coef[11]*-log(inputV[,11]) + PC12_coef[12]*-log(inputV[,12]) +
        PC12_coef[13]*-log(inputV[,13]) + PC12_coef[14]*-log(inputV[,14]) 
      
      PC13 <- PC13_coef[1]*-log(inputV[,1]) + PC13_coef[2]*-log(inputV[,2]) + PC13_coef[3]*-log(inputV[,3]) + PC13_coef[4]*-log(inputV[,4]) + 
        PC13_coef[5]*-log(inputV[,5]) + PC13_coef[6]*-log(inputV[,6]) + PC13_coef[7]*-log(inputV[,7]) + PC13_coef[8]*-log(inputV[,8]) +
        PC13_coef[9]*-log(inputV[,9]) + PC13_coef[10]*-log(inputV[,10]) + PC13_coef[11]*-log(inputV[,11]) + PC13_coef[12]*-log(inputV[,12]) +
        PC13_coef[13]*-log(inputV[,13]) + PC13_coef[14]*-log(inputV[,14])
      
      PC14 <- PC14_coef[1]*-log(inputV[,1]) + PC14_coef[2]*-log(inputV[,2]) + PC14_coef[3]*-log(inputV[,3]) + PC14_coef[4]*-log(inputV[,4]) + 
        PC14_coef[5]*-log(inputV[,5]) + PC14_coef[6]*-log(inputV[,6]) + PC14_coef[7]*-log(inputV[,7]) + PC14_coef[8]*-log(inputV[,8]) +
        PC14_coef[9]*-log(inputV[,9]) + PC14_coef[10]*-log(inputV[,10]) + PC14_coef[11]*-log(inputV[,11]) + PC14_coef[12]*-log(inputV[,12]) +
        PC14_coef[13]*-log(inputV[,13]) + PC14_coef[14]*-log(inputV[,14])
      
      PC = cbind(PC1,PC2,PC3,PC4,PC5,PC6,PC7,PC8,PC9,PC10,PC11,PC12,PC13,PC14)  
      
      return(PC) 
      
    }
  
  } else if(prep=="exp") {
    
    library(caret)
    
    if(dim(inputV)[2]==2) {
      
      pca <- preProcess(-inputV, method=c("expoTrans", "center", "scale", "pca"), thresh = 1)
      #print( summary(pca) )
      print(pca$rotation)
      
      PC1_coef <- pca$rotation[,1]   # coef of PC1
      PC2_coef <- pca$rotation[,2]   # coef of PC2
      
      PC1 <- PC1_coef[1]*exp(inputV[,1]) + PC1_coef[2]*exp(inputV[,2])
      PC2 <- PC2_coef[1]*exp(inputV[,1]) + PC2_coef[2]*exp(inputV[,2])
      
      PC = cbind(PC1,PC2) 
      
      return(PC)  
      
    } else if(dim(inputV)[2]==3) {
      
      pca <- preProcess(-inputV, method=c("expoTrans", "center", "scale", "pca"), thresh = 1, pcaComp = 3)
      #print( summary(pca) )
      print(pca$rotation)
      
      PC1_coef <- pca$rotation[,1]   # coef of PC1
      PC2_coef <- pca$rotation[,2]   # coef of PC2
      PC3_coef <- pca$rotation[,3]   # coef of PC3
      
      PC1 <- PC1_coef[1]*exp(inputV[,1]) + PC1_coef[2]*exp(inputV[,2]) + PC1_coef[3]*exp(inputV[,3])
      PC2 <- PC2_coef[1]*exp(inputV[,1]) + PC2_coef[2]*exp(inputV[,2]) + PC2_coef[3]*exp(inputV[,3])
      PC3 <- PC3_coef[1]*exp(inputV[,1]) + PC3_coef[2]*exp(inputV[,2]) + PC3_coef[3]*exp(inputV[,3])
      
      PC = cbind(PC1,PC2,PC3) 
      
      return(PC)  
      
    } else if(dim(inputV)[2]==4) {
      
      pca <- preProcess(-inputV, method=c("expoTrans", "center", "scale", "pca"), thresh = 1, pcaComp = 4)
      print(pca$rotation)
      
      PC1_coef <- pca$rotation[,1]   # coef of PC1
      PC2_coef <- pca$rotation[,2]   # coef of PC2
      PC3_coef <- pca$rotation[,3]   # coef of PC3
      PC4_coef <- pca$rotation[,4]   # coef of PC4
      
      PC1 <- PC1_coef[1]*exp(inputV[,1]) + PC1_coef[2]*exp(inputV[,2]) + PC1_coef[3]*exp(inputV[,3]) + PC1_coef[4]*exp(inputV[,4])
      PC2 <- PC2_coef[1]*exp(inputV[,1]) + PC2_coef[2]*exp(inputV[,2]) + PC2_coef[3]*exp(inputV[,3]) + PC2_coef[4]*exp(inputV[,4])
      PC3 <- PC3_coef[1]*exp(inputV[,1]) + PC3_coef[2]*exp(inputV[,2]) + PC3_coef[3]*exp(inputV[,3]) + PC3_coef[4]*exp(inputV[,4])
      PC4 <- PC4_coef[1]*exp(inputV[,1]) + PC4_coef[2]*exp(inputV[,2]) + PC4_coef[3]*exp(inputV[,3]) + PC4_coef[4]*exp(inputV[,4])
      
      PC = cbind(PC1,PC2,PC3,PC4) 
      
      return(PC)  
      
    } else if(dim(inputV)[2]==5) {
      
      pca <- preProcess(-inputV, method=c("expoTrans", "center", "scale", "pca"), thresh = 1, pcaComp = 5)
      print(pca$rotation)
      
      PC1_coef <- pca$rotation[,1]   # coef of PC1
      PC2_coef <- pca$rotation[,2]   # coef of PC2
      PC3_coef <- pca$rotation[,3]   # coef of PC3
      PC4_coef <- pca$rotation[,4]   # coef of PC4
      PC5_coef <- pca$rotation[,5]   # coef of PC5
      
      PC1 <- PC1_coef[1]*exp(inputV[,1]) + PC1_coef[2]*exp(inputV[,2]) + PC1_coef[3]*exp(inputV[,3]) + 
        PC1_coef[4]*exp(inputV[,4]) + PC1_coef[5]*exp(inputV[,5])
      
      PC2 <- PC2_coef[1]*exp(inputV[,1]) + PC2_coef[2]*exp(inputV[,2]) + PC2_coef[3]*exp(inputV[,3]) + 
        PC2_coef[4]*exp(inputV[,4]) + PC2_coef[5]*exp(inputV[,5])
      
      PC3 <- PC3_coef[1]*exp(inputV[,1]) + PC3_coef[2]*exp(inputV[,2]) + PC3_coef[3]*exp(inputV[,3]) + 
        PC3_coef[4]*exp(inputV[,4]) + PC3_coef[5]*exp(inputV[,5])
      
      PC4 <- PC4_coef[1]*exp(inputV[,1]) + PC4_coef[2]*exp(inputV[,2]) + PC4_coef[3]*exp(inputV[,3]) + 
        PC4_coef[4]*exp(inputV[,4]) + PC4_coef[5]*exp(inputV[,5])
      
      PC5 <- PC5_coef[1]*exp(inputV[,1]) + PC5_coef[2]*exp(inputV[,2]) + PC5_coef[3]*exp(inputV[,3]) + 
        PC5_coef[4]*exp(inputV[,4]) + PC5_coef[5]*exp(inputV[,5])
      
      PC = cbind(PC1,PC2,PC3,PC4,PC5) 
      
      return(PC)  
      
    } else if(dim(inputV)[2]==6) {
      
      pca <- preProcess(-inputV, method=c("expoTrans", "center", "scale", "pca"), thresh = 1, pcaComp = 6)
      print(pca$rotation)
      
      PC1_coef <- pca$rotation[,1]   # coef of PC1
      PC2_coef <- pca$rotation[,2]   # coef of PC2
      PC3_coef <- pca$rotation[,3]   # coef of PC3
      PC4_coef <- pca$rotation[,4]   # coef of PC4
      PC5_coef <- pca$rotation[,5]   # coef of PC5
      PC6_coef <- pca$rotation[,6]   # coef of PC6
      
      PC1 <- PC1_coef[1]*exp(inputV[,1]) + PC1_coef[2]*exp(inputV[,2]) + PC1_coef[3]*exp(inputV[,3]) + 
        PC1_coef[4]*exp(inputV[,4]) + PC1_coef[5]*exp(inputV[,5]) + PC1_coef[6]*exp(inputV[,6])
      
      PC2 <- PC2_coef[1]*exp(inputV[,1]) + PC2_coef[2]*exp(inputV[,2]) + PC2_coef[3]*exp(inputV[,3]) + 
        PC2_coef[4]*exp(inputV[,4]) + PC2_coef[5]*exp(inputV[,5]) + PC2_coef[6]*exp(inputV[,6])
      
      PC3 <- PC3_coef[1]*exp(inputV[,1]) + PC3_coef[2]*exp(inputV[,2]) + PC3_coef[3]*exp(inputV[,3]) + 
        PC3_coef[4]*exp(inputV[,4]) + PC3_coef[5]*exp(inputV[,5]) + PC3_coef[6]*exp(inputV[,6])
      
      PC4 <- PC4_coef[1]*exp(inputV[,1]) + PC4_coef[2]*exp(inputV[,2]) + PC4_coef[3]*exp(inputV[,3]) + 
        PC4_coef[4]*exp(inputV[,4]) + PC4_coef[5]*exp(inputV[,5]) + PC4_coef[6]*exp(inputV[,6])
      
      PC5 <- PC5_coef[1]*exp(inputV[,1]) + PC5_coef[2]*exp(inputV[,2]) + PC5_coef[3]*exp(inputV[,3]) + 
        PC5_coef[4]*exp(inputV[,4]) + PC5_coef[5]*exp(inputV[,5]) + PC5_coef[6]*exp(inputV[,6])
      
      PC6 <- PC6_coef[1]*exp(inputV[,1]) + PC6_coef[2]*exp(inputV[,2]) + PC6_coef[3]*exp(inputV[,3]) + 
        PC6_coef[4]*exp(inputV[,4]) + PC6_coef[5]*exp(inputV[,5]) + PC6_coef[6]*exp(inputV[,6])
      
      PC = cbind(PC1,PC2,PC3,PC4,PC5,PC6) 
      
      return(PC)  
      
    } else if(dim(inputV)[2]==7) {
      
      pca <- preProcess(-inputV, method=c("expoTrans", "center", "scale", "pca"), thresh = 1, pcaComp = 7)
      print(pca$rotation)
      
      PC1_coef <- pca$rotation[,1]   # coef of PC1
      PC2_coef <- pca$rotation[,2]   # coef of PC2
      PC3_coef <- pca$rotation[,3]   # coef of PC3
      PC4_coef <- pca$rotation[,4]   # coef of PC4
      PC5_coef <- pca$rotation[,5]   # coef of PC5
      PC6_coef <- pca$rotation[,6]   # coef of PC6
      PC7_coef <- pca$rotation[,7]   # coef of PC7
      
      PC1 <- PC1_coef[1]*exp(inputV[,1]) + PC1_coef[2]*exp(inputV[,2]) + PC1_coef[3]*exp(inputV[,3]) + 
        PC1_coef[4]*exp(inputV[,4]) + PC1_coef[5]*exp(inputV[,5]) + PC1_coef[6]*exp(inputV[,6]) + PC1_coef[7]*exp(inputV[,7])
      
      PC2 <- PC2_coef[1]*exp(inputV[,1]) + PC2_coef[2]*exp(inputV[,2]) + PC2_coef[3]*exp(inputV[,3]) + 
        PC2_coef[4]*exp(inputV[,4]) + PC2_coef[5]*exp(inputV[,5]) + PC2_coef[6]*exp(inputV[,6]) + PC2_coef[7]*exp(inputV[,7])
      
      PC3 <- PC3_coef[1]*exp(inputV[,1]) + PC3_coef[2]*exp(inputV[,2]) + PC3_coef[3]*exp(inputV[,3]) + 
        PC3_coef[4]*exp(inputV[,4]) + PC3_coef[5]*exp(inputV[,5]) + PC3_coef[6]*exp(inputV[,6]) + PC3_coef[7]*exp(inputV[,7])
      
      PC4 <- PC4_coef[1]*exp(inputV[,1]) + PC4_coef[2]*exp(inputV[,2]) + PC4_coef[3]*exp(inputV[,3]) + 
        PC4_coef[4]*exp(inputV[,4]) + PC4_coef[5]*exp(inputV[,5]) + PC4_coef[6]*exp(inputV[,6]) + PC4_coef[7]*exp(inputV[,7])
      
      PC5 <- PC5_coef[1]*exp(inputV[,1]) + PC5_coef[2]*exp(inputV[,2]) + PC5_coef[3]*exp(inputV[,3]) + 
        PC5_coef[4]*exp(inputV[,4]) + PC5_coef[5]*exp(inputV[,5]) + PC5_coef[6]*exp(inputV[,6]) + PC5_coef[7]*exp(inputV[,7])
      
      PC6 <- PC6_coef[1]*exp(inputV[,1]) + PC6_coef[2]*exp(inputV[,2]) + PC6_coef[3]*exp(inputV[,3]) + 
        PC6_coef[4]*exp(inputV[,4]) + PC6_coef[5]*exp(inputV[,5]) + PC6_coef[6]*exp(inputV[,6]) + PC6_coef[7]*exp(inputV[,7])
      
      PC7 <- PC7_coef[1]*exp(inputV[,1]) + PC7_coef[2]*exp(inputV[,2]) + PC7_coef[3]*exp(inputV[,3]) + 
        PC7_coef[4]*exp(inputV[,4]) + PC7_coef[5]*exp(inputV[,5]) + PC7_coef[6]*exp(inputV[,6]) + PC7_coef[7]*exp(inputV[,7])
      
      PC = cbind(PC1,PC2,PC3,PC4,PC5,PC6,PC7) 
      
      return(PC)  
      
    } else if(dim(inputV)[2]==8) {
      
      pca <- preProcess(-inputV, method=c("expoTrans", "center", "scale", "pca"), thresh = 1, pcaComp = 8)
      print(pca$rotation)
      
      PC1_coef <- pca$rotation[,1]   # coef of PC1
      PC2_coef <- pca$rotation[,2]   # coef of PC2
      PC3_coef <- pca$rotation[,3]   # coef of PC3
      PC4_coef <- pca$rotation[,4]   # coef of PC4
      PC5_coef <- pca$rotation[,5]   # coef of PC5
      PC6_coef <- pca$rotation[,6]   # coef of PC6
      PC7_coef <- pca$rotation[,7]   # coef of PC7
      PC8_coef <- pca$rotation[,8]   # coef of PC8
      
      PC1 <- PC1_coef[1]*exp(inputV[,1]) + PC1_coef[2]*exp(inputV[,2]) + PC1_coef[3]*exp(inputV[,3]) + PC1_coef[4]*exp(inputV[,4]) + 
        PC1_coef[5]*exp(inputV[,5]) + PC1_coef[6]*exp(inputV[,6]) + PC1_coef[7]*exp(inputV[,7]) + PC1_coef[8]*exp(inputV[,8])
      
      PC2 <- PC2_coef[1]*exp(inputV[,1]) + PC2_coef[2]*exp(inputV[,2]) + PC2_coef[3]*exp(inputV[,3]) + PC2_coef[4]*exp(inputV[,4]) + 
        PC2_coef[5]*exp(inputV[,5]) + PC2_coef[6]*exp(inputV[,6]) + PC2_coef[7]*exp(inputV[,7]) + PC2_coef[8]*exp(inputV[,8])
      
      PC3 <- PC3_coef[1]*exp(inputV[,1]) + PC3_coef[2]*exp(inputV[,2]) + PC3_coef[3]*exp(inputV[,3]) + PC3_coef[4]*exp(inputV[,4]) + 
        PC3_coef[5]*exp(inputV[,5]) + PC3_coef[6]*exp(inputV[,6]) + PC3_coef[7]*exp(inputV[,7]) + PC3_coef[8]*exp(inputV[,8])
      
      PC4 <- PC4_coef[1]*exp(inputV[,1]) + PC4_coef[2]*exp(inputV[,2]) + PC4_coef[3]*exp(inputV[,3]) + PC4_coef[4]*exp(inputV[,4]) + 
        PC4_coef[5]*exp(inputV[,5]) + PC4_coef[6]*exp(inputV[,6]) + PC4_coef[7]*exp(inputV[,7]) + PC4_coef[8]*exp(inputV[,8])
      
      PC5 <- PC5_coef[1]*exp(inputV[,1]) + PC5_coef[2]*exp(inputV[,2]) + PC5_coef[3]*exp(inputV[,3]) + PC5_coef[4]*exp(inputV[,4]) + 
        PC5_coef[5]*exp(inputV[,5]) + PC5_coef[6]*exp(inputV[,6]) + PC5_coef[7]*exp(inputV[,7]) + PC5_coef[8]*exp(inputV[,8])
      
      PC6 <- PC6_coef[1]*exp(inputV[,1]) + PC6_coef[2]*exp(inputV[,2]) + PC6_coef[3]*exp(inputV[,3]) + PC6_coef[4]*exp(inputV[,4]) + 
        PC6_coef[5]*exp(inputV[,5]) + PC6_coef[6]*exp(inputV[,6]) + PC6_coef[7]*exp(inputV[,7]) + PC6_coef[8]*exp(inputV[,8])
      
      PC7 <- PC7_coef[1]*exp(inputV[,1]) + PC7_coef[2]*exp(inputV[,2]) + PC7_coef[3]*exp(inputV[,3]) + PC7_coef[4]*exp(inputV[,4]) + 
        PC7_coef[5]*exp(inputV[,5]) + PC7_coef[6]*exp(inputV[,6]) + PC7_coef[7]*exp(inputV[,7]) + PC7_coef[8]*exp(inputV[,8])
      
      PC8 <- PC8_coef[1]*exp(inputV[,1]) + PC8_coef[2]*exp(inputV[,2]) + PC8_coef[3]*exp(inputV[,3]) + PC8_coef[4]*exp(inputV[,4]) + 
        PC8_coef[5]*exp(inputV[,5]) + PC8_coef[6]*exp(inputV[,6]) + PC8_coef[7]*exp(inputV[,7]) + PC8_coef[8]*exp(inputV[,8])
      
      PC = cbind(PC1,PC2,PC3,PC4,PC5,PC6,PC7,PC8) 
      
      return(PC)  
      
    }
    
  } else if(prep=="none") {
  
    #pca <- prcomp(inputV, center = TRUE, scale. = TRUE) 
    pca <- prcomp(inputV) 
    print(pca)
    #plot(pca, type = "l")
    print( summary(pca) )
    plot(pca, xlab="PCs")
    
    
    if(dim(inputV)[2]==2) {
      
      PC1_coef <- pca$rotation[,1]   # coef of PC1
      PC2_coef <- pca$rotation[,2]   # coef of PC2
      
      PC1 <- PC1_coef[1]*inputV[,1] + PC1_coef[2]*inputV[,2]
      #PC1 <- PC1_coef[1]*-inputV[,1] + PC1_coef[2]*-inputV[,2]
      PC2 <- PC2_coef[1]*inputV[,1] + PC2_coef[2]*inputV[,2]
      #PC2 <- PC2_coef[1]*-inputV[,1] + PC2_coef[2]*-inputV[,2]
      
      PC = cbind(PC1,PC2)  # combine the columns 
      
      return(PC)  
    
    } else if(dim(inputV)[2]==3) {
      
      PC1_coef <- pca$rotation[,1]   # coef of PC1
      PC2_coef <- pca$rotation[,2]   # coef of PC2
      PC3_coef <- pca$rotation[,3]   # coef of PC3
      
      PC1 <- PC1_coef[1]*inputV[,1] + PC1_coef[2]*inputV[,2] + PC1_coef[3]*inputV[,3]
      PC2 <- PC2_coef[1]*inputV[,1] + PC2_coef[2]*inputV[,2] + PC2_coef[3]*inputV[,3]
      PC3 <- PC3_coef[1]*inputV[,1] + PC3_coef[2]*inputV[,2] + PC3_coef[3]*inputV[,3]
      
      PC = cbind(PC1,PC2,PC3)  
      
      return(PC) 
      
    } else if(dim(inputV)[2]==4) {
      
      PC1_coef <- pca$rotation[,1]   # coef of PC1
      PC2_coef <- pca$rotation[,2]   # coef of PC2
      PC3_coef <- pca$rotation[,3]   # coef of PC3
      PC4_coef <- pca$rotation[,4]   # coef of PC4
      
      PC1 <- PC1_coef[1]*inputV[,1] + PC1_coef[2]*inputV[,2] + PC1_coef[3]*inputV[,3] + PC1_coef[4]*inputV[,4]
      PC2 <- PC2_coef[1]*inputV[,1] + PC2_coef[2]*inputV[,2] + PC2_coef[3]*inputV[,3] + PC2_coef[4]*inputV[,4] 
      PC3 <- PC3_coef[1]*inputV[,1] + PC3_coef[2]*inputV[,2] + PC3_coef[3]*inputV[,3] + PC3_coef[4]*inputV[,4] 
      PC4 <- PC4_coef[1]*inputV[,1] + PC4_coef[2]*inputV[,2] + PC4_coef[3]*inputV[,3] + PC4_coef[4]*inputV[,4]
      
      PC = cbind(PC1,PC2,PC3,PC4)  
      
      return(PC) 
      
    } else if(dim(inputV)[2]==5) {
      
      PC1_coef <- pca$rotation[,1]   # coef of PC1
      PC2_coef <- pca$rotation[,2]   # coef of PC2
      PC3_coef <- pca$rotation[,3]   # coef of PC3
      PC4_coef <- pca$rotation[,4]   # coef of PC4
      PC5_coef <- pca$rotation[,5]   # coef of PC5
      
      PC1 <- PC1_coef[1]*inputV[,1] + PC1_coef[2]*inputV[,2] + PC1_coef[3]*inputV[,3] +
        PC1_coef[4]*inputV[,4] + PC1_coef[5]*inputV[,5]
      
      PC2 <- PC2_coef[1]*inputV[,1] + PC2_coef[2]*inputV[,2] + PC2_coef[3]*inputV[,3] +
        PC2_coef[4]*inputV[,4] + PC2_coef[5]*inputV[,5]
      
      PC3 <- PC3_coef[1]*inputV[,1] + PC3_coef[2]*inputV[,2] + PC3_coef[3]*inputV[,3] +
        PC3_coef[4]*inputV[,4] + PC3_coef[5]*inputV[,5] 
      
      PC4 <- PC4_coef[1]*inputV[,1] + PC4_coef[2]*inputV[,2] + PC4_coef[3]*inputV[,3] +
        PC4_coef[4]*inputV[,4] + PC4_coef[5]*inputV[,5] 
      
      PC5 <- PC5_coef[1]*inputV[,1] + PC5_coef[2]*inputV[,2] + PC5_coef[3]*inputV[,3] + 
        PC5_coef[4]*inputV[,4] + PC5_coef[5]*inputV[,5] 
      
      PC = cbind(PC1,PC2,PC3,PC4,PC5)  
      
      return(PC) 
      
    } else if(dim(inputV)[2]==6) {
      
      PC1_coef <- pca$rotation[,1]   # coef of PC1
      PC2_coef <- pca$rotation[,2]   # coef of PC2
      PC3_coef <- pca$rotation[,3]   # coef of PC3
      PC4_coef <- pca$rotation[,4]   # coef of PC4
      PC5_coef <- pca$rotation[,5]   # coef of PC5
      PC6_coef <- pca$rotation[,6]   # coef of PC6
      
      PC1 <- PC1_coef[1]*inputV[,1] + PC1_coef[2]*inputV[,2] + PC1_coef[3]*inputV[,3] +
        PC1_coef[4]*inputV[,4] + PC1_coef[5]*inputV[,5] + PC1_coef[6]*inputV[,6]
      
      PC2 <- PC2_coef[1]*inputV[,1] + PC2_coef[2]*inputV[,2] + PC2_coef[3]*inputV[,3] +
        PC2_coef[4]*inputV[,4] + PC2_coef[5]*inputV[,5] + PC2_coef[6]*inputV[,6]
      
      PC3 <- PC3_coef[1]*inputV[,1] + PC3_coef[2]*inputV[,2] + PC3_coef[3]*inputV[,3] +
        PC3_coef[4]*inputV[,4] + PC3_coef[5]*inputV[,5] + PC3_coef[6]*inputV[,6]
      
      PC4 <- PC4_coef[1]*inputV[,1] + PC4_coef[2]*inputV[,2] + PC4_coef[3]*inputV[,3] +
        PC4_coef[4]*inputV[,4] + PC4_coef[5]*inputV[,5] + PC4_coef[6]*inputV[,6]
      
      PC5 <- PC5_coef[1]*inputV[,1] + PC5_coef[2]*inputV[,2] + PC5_coef[3]*inputV[,3] + 
        PC5_coef[4]*inputV[,4] + PC5_coef[5]*inputV[,5] + PC5_coef[6]*inputV[,6]
      
      PC6 <- PC6_coef[1]*inputV[,1] + PC6_coef[2]*inputV[,2] + PC6_coef[3]*inputV[,3] +
        PC6_coef[4]*inputV[,4] + PC6_coef[5]*inputV[,5] + PC6_coef[6]*inputV[,6]
      
      PC = cbind(PC1,PC2,PC3,PC4,PC5,PC6)  
      
      return(PC) 
      
    } else if(dim(inputV)[2]==7) {
      
      PC1_coef <- pca$rotation[,1]   # coef of PC1
      PC2_coef <- pca$rotation[,2]   # coef of PC2
      PC3_coef <- pca$rotation[,3]   # coef of PC3
      PC4_coef <- pca$rotation[,4]   # coef of PC4
      PC5_coef <- pca$rotation[,5]   # coef of PC5
      PC6_coef <- pca$rotation[,6]   # coef of PC6
      PC7_coef <- pca$rotation[,7]   # coef of PC7
      
      PC1 <- PC1_coef[1]*inputV[,1] + PC1_coef[2]*inputV[,2] + PC1_coef[3]*inputV[,3] +
        PC1_coef[4]*inputV[,4] + PC1_coef[5]*inputV[,5] + PC1_coef[6]*inputV[,6] + PC1_coef[7]*inputV[,7]
      
      PC2 <- PC2_coef[1]*inputV[,1] + PC2_coef[2]*inputV[,2] + PC2_coef[3]*inputV[,3] +
        PC2_coef[4]*inputV[,4] + PC2_coef[5]*inputV[,5] + PC2_coef[6]*inputV[,6] + PC2_coef[7]*inputV[,7]
      
      PC3 <- PC3_coef[1]*inputV[,1] + PC3_coef[2]*inputV[,2] + PC3_coef[3]*inputV[,3] +
        PC3_coef[4]*inputV[,4] + PC3_coef[5]*inputV[,5] + PC3_coef[6]*inputV[,6] + PC3_coef[7]*inputV[,7]
      
      PC4 <- PC4_coef[1]*inputV[,1] + PC4_coef[2]*inputV[,2] + PC4_coef[3]*inputV[,3] +
        PC4_coef[4]*inputV[,4] + PC4_coef[5]*inputV[,5] + PC4_coef[6]*inputV[,6] + PC4_coef[7]*inputV[,7]
      
      PC5 <- PC5_coef[1]*inputV[,1] + PC5_coef[2]*inputV[,2] + PC5_coef[3]*inputV[,3] + 
        PC5_coef[4]*inputV[,4] + PC5_coef[5]*inputV[,5] + PC5_coef[6]*inputV[,6] + PC5_coef[7]*inputV[,7]
      
      PC6 <- PC6_coef[1]*inputV[,1] + PC6_coef[2]*inputV[,2] + PC6_coef[3]*inputV[,3] +
        PC6_coef[4]*inputV[,4] + PC6_coef[5]*inputV[,5] + PC6_coef[6]*inputV[,6] + PC6_coef[7]*inputV[,7]
      
      PC7 <- PC7_coef[1]*inputV[,1] + PC7_coef[2]*inputV[,2] + PC7_coef[3]*inputV[,3] +
        PC7_coef[4]*inputV[,4] + PC7_coef[5]*inputV[,5] + PC7_coef[6]*inputV[,6] + PC7_coef[7]*inputV[,7]
      
      PC = cbind(PC1,PC2,PC3,PC4,PC5,PC6,PC7)  
      
      return(PC) 
      
    } else if(dim(inputV)[2]==8) {
      
      PC1_coef <- pca$rotation[,1]   # coef of PC1
      PC2_coef <- pca$rotation[,2]   # coef of PC2
      PC3_coef <- pca$rotation[,3]   # coef of PC3
      PC4_coef <- pca$rotation[,4]   # coef of PC4
      PC5_coef <- pca$rotation[,5]   # coef of PC5
      PC6_coef <- pca$rotation[,6]   # coef of PC6
      PC7_coef <- pca$rotation[,7]   # coef of PC7
      PC8_coef <- pca$rotation[,8]   # coef of PC8
      
      PC1 <- PC1_coef[1]*inputV[,1] + PC1_coef[2]*inputV[,2] + PC1_coef[3]*inputV[,3] + PC1_coef[4]*inputV[,4] + 
        PC1_coef[5]*inputV[,5] + PC1_coef[6]*inputV[,6] + PC1_coef[7]*inputV[,7] + PC1_coef[8]*inputV[,8]
      
      PC2 <- PC2_coef[1]*inputV[,1] + PC2_coef[2]*inputV[,2] + PC2_coef[3]*inputV[,3] + PC2_coef[4]*inputV[,4] + 
        PC2_coef[5]*inputV[,5] + PC2_coef[6]*inputV[,6] + PC2_coef[7]*inputV[,7] + PC2_coef[8]*inputV[,8]
      
      PC3 <- PC3_coef[1]*inputV[,1] + PC3_coef[2]*inputV[,2] + PC3_coef[3]*inputV[,3] + PC3_coef[4]*inputV[,4] + 
        PC3_coef[5]*inputV[,5] + PC3_coef[6]*inputV[,6] + PC3_coef[7]*inputV[,7] + PC3_coef[8]*inputV[,8]
      
      PC4 <- PC4_coef[1]*inputV[,1] + PC4_coef[2]*inputV[,2] + PC4_coef[3]*inputV[,3] + PC4_coef[4]*inputV[,4] + 
        PC4_coef[5]*inputV[,5] + PC4_coef[6]*inputV[,6] + PC4_coef[7]*inputV[,7] + PC4_coef[8]*inputV[,8]
      
      PC5 <- PC5_coef[1]*inputV[,1] + PC5_coef[2]*inputV[,2] + PC5_coef[3]*inputV[,3] + PC5_coef[4]*inputV[,4] + 
        PC5_coef[5]*inputV[,5] + PC5_coef[6]*inputV[,6] + PC5_coef[7]*inputV[,7] + PC5_coef[8]*inputV[,8]
      
      PC6 <- PC6_coef[1]*inputV[,1] + PC6_coef[2]*inputV[,2] + PC6_coef[3]*inputV[,3] + PC6_coef[4]*inputV[,4] + 
        PC6_coef[5]*inputV[,5] + PC6_coef[6]*inputV[,6] + PC6_coef[7]*inputV[,7] + PC6_coef[8]*inputV[,8]
      
      PC7 <- PC7_coef[1]*inputV[,1] + PC7_coef[2]*inputV[,2] + PC7_coef[3]*inputV[,3] + PC7_coef[4]*inputV[,4] + 
        PC7_coef[5]*inputV[,5] + PC7_coef[6]*inputV[,6] + PC7_coef[7]*inputV[,7] + PC7_coef[8]*inputV[,8]
      
      PC8 <- PC8_coef[1]*inputV[,1] + PC8_coef[2]*inputV[,2] + PC8_coef[3]*inputV[,3] + PC8_coef[4]*inputV[,4] + 
        PC8_coef[5]*inputV[,5] + PC8_coef[6]*inputV[,6] + PC8_coef[7]*inputV[,7] + PC8_coef[8]*inputV[,8]
      
      PC = cbind(PC1,PC2,PC3,PC4,PC5,PC6,PC7,PC8)  
      
      return(PC) 
      
    } else if(dim(inputV)[2]==9) {
      
      PC1_coef <- pca$rotation[,1]   # coef of PC1
      PC2_coef <- pca$rotation[,2]   # coef of PC2
      PC3_coef <- pca$rotation[,3]   # coef of PC3
      PC4_coef <- pca$rotation[,4]   # coef of PC4
      PC5_coef <- pca$rotation[,5]   # coef of PC5
      PC6_coef <- pca$rotation[,6]   # coef of PC6
      PC7_coef <- pca$rotation[,7]   # coef of PC7
      PC8_coef <- pca$rotation[,8]   # coef of PC8
      PC9_coef <- pca$rotation[,9]   # coef of PC9
      
      PC1 <- PC1_coef[1]*inputV[,1] + PC1_coef[2]*inputV[,2] + PC1_coef[3]*inputV[,3] + PC1_coef[4]*inputV[,4] + 
        PC1_coef[5]*inputV[,5] + PC1_coef[6]*inputV[,6] + PC1_coef[7]*inputV[,7] + PC1_coef[8]*inputV[,8] + PC1_coef[9]*inputV[,9]
      
      PC2 <- PC2_coef[1]*inputV[,1] + PC2_coef[2]*inputV[,2] + PC2_coef[3]*inputV[,3] + PC2_coef[4]*inputV[,4] + 
        PC2_coef[5]*inputV[,5] + PC2_coef[6]*inputV[,6] + PC2_coef[7]*inputV[,7] + PC2_coef[8]*inputV[,8] + PC2_coef[9]*inputV[,9]
      
      PC3 <- PC3_coef[1]*inputV[,1] + PC3_coef[2]*inputV[,2] + PC3_coef[3]*inputV[,3] + PC3_coef[4]*inputV[,4] + 
        PC3_coef[5]*inputV[,5] + PC3_coef[6]*inputV[,6] + PC3_coef[7]*inputV[,7] + PC3_coef[8]*inputV[,8] + PC3_coef[9]*inputV[,9]
      
      PC4 <- PC4_coef[1]*inputV[,1] + PC4_coef[2]*inputV[,2] + PC4_coef[3]*inputV[,3] + PC4_coef[4]*inputV[,4] + 
        PC4_coef[5]*inputV[,5] + PC4_coef[6]*inputV[,6] + PC4_coef[7]*inputV[,7] + PC4_coef[8]*inputV[,8] + PC4_coef[9]*inputV[,9]
      
      PC5 <- PC5_coef[1]*inputV[,1] + PC5_coef[2]*inputV[,2] + PC5_coef[3]*inputV[,3] + PC5_coef[4]*inputV[,4] + 
        PC5_coef[5]*inputV[,5] + PC5_coef[6]*inputV[,6] + PC5_coef[7]*inputV[,7] + PC5_coef[8]*inputV[,8] + PC5_coef[9]*inputV[,9]
      
      PC6 <- PC6_coef[1]*inputV[,1] + PC6_coef[2]*inputV[,2] + PC6_coef[3]*inputV[,3] + PC6_coef[4]*inputV[,4] + 
        PC6_coef[5]*inputV[,5] + PC6_coef[6]*inputV[,6] + PC6_coef[7]*inputV[,7] + PC6_coef[8]*inputV[,8] + PC6_coef[9]*inputV[,9]
      
      PC7 <- PC7_coef[1]*inputV[,1] + PC7_coef[2]*inputV[,2] + PC7_coef[3]*inputV[,3] + PC7_coef[4]*inputV[,4] + 
        PC7_coef[5]*inputV[,5] + PC7_coef[6]*inputV[,6] + PC7_coef[7]*inputV[,7] + PC7_coef[8]*inputV[,8] + PC7_coef[9]*inputV[,9]
      
      PC8 <- PC8_coef[1]*inputV[,1] + PC8_coef[2]*inputV[,2] + PC8_coef[3]*inputV[,3] + PC8_coef[4]*inputV[,4] + 
        PC8_coef[5]*inputV[,5] + PC8_coef[6]*inputV[,6] + PC8_coef[7]*inputV[,7] + PC8_coef[8]*inputV[,8] + PC8_coef[9]*inputV[,9]
      
      PC9 <- PC9_coef[1]*inputV[,1] + PC9_coef[2]*inputV[,2] + PC9_coef[3]*inputV[,3] + PC9_coef[4]*inputV[,4] + 
        PC9_coef[5]*inputV[,5] + PC9_coef[6]*inputV[,6] + PC9_coef[7]*inputV[,7] + PC9_coef[8]*inputV[,8] + PC9_coef[9]*inputV[,9]
      
      PC = cbind(PC1,PC2,PC3,PC4,PC5,PC6,PC7,PC8,PC9)  
      
      return(PC) 
      
    }
  
  } else if(prep=="yeo") {
    
    library(caret)
    
    if(dim(inputV)[2]==2) {
      
      pca <- preProcess(-inputV, method=c("YeoJohnson", "center", "scale", "pca"), thresh = 1)
      #print( summary(pca) )
      print(pca$rotation)
      
      PC1_coef <- pca$rotation[,1]   # coef of PC1
      PC2_coef <- pca$rotation[,2]   # coef of PC2
      
      PC1 <- PC1_coef[1]*exp(inputV[,1]) + PC1_coef[2]*exp(inputV[,2])
      PC2 <- PC2_coef[1]*exp(inputV[,1]) + PC2_coef[2]*exp(inputV[,2])
      
      PC = cbind(PC1,PC2) 
      
      return(PC)  
      
    } else if(dim(inputV)[2]==3) {
      
      pca <- preProcess(-inputV, method=c("YeoJohnson", "center", "scale", "pca"), thresh = 1, pcaComp = 3)
      #print( summary(pca) )
      print(pca$rotation)
      
      PC1_coef <- pca$rotation[,1]   # coef of PC1
      PC2_coef <- pca$rotation[,2]   # coef of PC2
      PC3_coef <- pca$rotation[,3]   # coef of PC3
      
      PC1 <- PC1_coef[1]*exp(inputV[,1]) + PC1_coef[2]*exp(inputV[,2]) + PC1_coef[3]*exp(inputV[,3])
      PC2 <- PC2_coef[1]*exp(inputV[,1]) + PC2_coef[2]*exp(inputV[,2]) + PC2_coef[3]*exp(inputV[,3])
      PC3 <- PC3_coef[1]*exp(inputV[,1]) + PC3_coef[2]*exp(inputV[,2]) + PC3_coef[3]*exp(inputV[,3])
      
      PC = cbind(PC1,PC2,PC3) 
      
      return(PC)  
      
    } else if(dim(inputV)[2]==4) {
      
      pca <- preProcess(-inputV, method=c("YeoJohnson", "center", "scale", "pca"), thresh = 1, pcaComp = 4)
      print(pca$rotation)
      
      PC1_coef <- pca$rotation[,1]   # coef of PC1
      PC2_coef <- pca$rotation[,2]   # coef of PC2
      PC3_coef <- pca$rotation[,3]   # coef of PC3
      PC4_coef <- pca$rotation[,4]   # coef of PC4
      
      PC1 <- PC1_coef[1]*exp(inputV[,1]) + PC1_coef[2]*exp(inputV[,2]) + PC1_coef[3]*exp(inputV[,3]) + PC1_coef[4]*exp(inputV[,4])
      PC2 <- PC2_coef[1]*exp(inputV[,1]) + PC2_coef[2]*exp(inputV[,2]) + PC2_coef[3]*exp(inputV[,3]) + PC2_coef[4]*exp(inputV[,4])
      PC3 <- PC3_coef[1]*exp(inputV[,1]) + PC3_coef[2]*exp(inputV[,2]) + PC3_coef[3]*exp(inputV[,3]) + PC3_coef[4]*exp(inputV[,4])
      PC4 <- PC4_coef[1]*exp(inputV[,1]) + PC4_coef[2]*exp(inputV[,2]) + PC4_coef[3]*exp(inputV[,3]) + PC4_coef[4]*exp(inputV[,4])
      
      PC = cbind(PC1,PC2,PC3,PC4) 
      
      return(PC)  
      
    } else if(dim(inputV)[2]==5) {
      
      pca <- preProcess(-inputV, method=c("YeoJohnson", "center", "scale", "pca"), thresh = 1, pcaComp = 5)
      print(pca$rotation)
      
      PC1_coef <- pca$rotation[,1]   # coef of PC1
      PC2_coef <- pca$rotation[,2]   # coef of PC2
      PC3_coef <- pca$rotation[,3]   # coef of PC3
      PC4_coef <- pca$rotation[,4]   # coef of PC4
      PC5_coef <- pca$rotation[,5]   # coef of PC5
      
      PC1 <- PC1_coef[1]*exp(inputV[,1]) + PC1_coef[2]*exp(inputV[,2]) + PC1_coef[3]*exp(inputV[,3]) + 
        PC1_coef[4]*exp(inputV[,4]) + PC1_coef[5]*exp(inputV[,5])
      
      PC2 <- PC2_coef[1]*exp(inputV[,1]) + PC2_coef[2]*exp(inputV[,2]) + PC2_coef[3]*exp(inputV[,3]) + 
        PC2_coef[4]*exp(inputV[,4]) + PC2_coef[5]*exp(inputV[,5])
      
      PC3 <- PC3_coef[1]*exp(inputV[,1]) + PC3_coef[2]*exp(inputV[,2]) + PC3_coef[3]*exp(inputV[,3]) + 
        PC3_coef[4]*exp(inputV[,4]) + PC3_coef[5]*exp(inputV[,5])
      
      PC4 <- PC4_coef[1]*exp(inputV[,1]) + PC4_coef[2]*exp(inputV[,2]) + PC4_coef[3]*exp(inputV[,3]) + 
        PC4_coef[4]*exp(inputV[,4]) + PC4_coef[5]*exp(inputV[,5])
      
      PC5 <- PC5_coef[1]*exp(inputV[,1]) + PC5_coef[2]*exp(inputV[,2]) + PC5_coef[3]*exp(inputV[,3]) + 
        PC5_coef[4]*exp(inputV[,4]) + PC5_coef[5]*exp(inputV[,5])
      
      PC = cbind(PC1,PC2,PC3,PC4,PC5) 
      
      return(PC)  
      
    } else if(dim(inputV)[2]==6) {
      
      pca <- preProcess(-inputV, method=c("YeoJohnson", "center", "scale", "pca"), thresh = 1, pcaComp = 6)
      print(pca$rotation)
      
      PC1_coef <- pca$rotation[,1]   # coef of PC1
      PC2_coef <- pca$rotation[,2]   # coef of PC2
      PC3_coef <- pca$rotation[,3]   # coef of PC3
      PC4_coef <- pca$rotation[,4]   # coef of PC4
      PC5_coef <- pca$rotation[,5]   # coef of PC5
      PC6_coef <- pca$rotation[,6]   # coef of PC6
      
      PC1 <- PC1_coef[1]*exp(inputV[,1]) + PC1_coef[2]*exp(inputV[,2]) + PC1_coef[3]*exp(inputV[,3]) + 
        PC1_coef[4]*exp(inputV[,4]) + PC1_coef[5]*exp(inputV[,5]) + PC1_coef[6]*exp(inputV[,6])
      
      PC2 <- PC2_coef[1]*exp(inputV[,1]) + PC2_coef[2]*exp(inputV[,2]) + PC2_coef[3]*exp(inputV[,3]) + 
        PC2_coef[4]*exp(inputV[,4]) + PC2_coef[5]*exp(inputV[,5]) + PC2_coef[6]*exp(inputV[,6])
      
      PC3 <- PC3_coef[1]*exp(inputV[,1]) + PC3_coef[2]*exp(inputV[,2]) + PC3_coef[3]*exp(inputV[,3]) + 
        PC3_coef[4]*exp(inputV[,4]) + PC3_coef[5]*exp(inputV[,5]) + PC3_coef[6]*exp(inputV[,6])
      
      PC4 <- PC4_coef[1]*exp(inputV[,1]) + PC4_coef[2]*exp(inputV[,2]) + PC4_coef[3]*exp(inputV[,3]) + 
        PC4_coef[4]*exp(inputV[,4]) + PC4_coef[5]*exp(inputV[,5]) + PC4_coef[6]*exp(inputV[,6])
      
      PC5 <- PC5_coef[1]*exp(inputV[,1]) + PC5_coef[2]*exp(inputV[,2]) + PC5_coef[3]*exp(inputV[,3]) + 
        PC5_coef[4]*exp(inputV[,4]) + PC5_coef[5]*exp(inputV[,5]) + PC5_coef[6]*exp(inputV[,6])
      
      PC6 <- PC6_coef[1]*exp(inputV[,1]) + PC6_coef[2]*exp(inputV[,2]) + PC6_coef[3]*exp(inputV[,3]) + 
        PC6_coef[4]*exp(inputV[,4]) + PC6_coef[5]*exp(inputV[,5]) + PC6_coef[6]*exp(inputV[,6])
      
      PC = cbind(PC1,PC2,PC3,PC4,PC5,PC6) 
      
      return(PC)  
      
    } else if(dim(inputV)[2]==7) {
      
      pca <- preProcess(-inputV, method=c("YeoJohnson", "center", "scale", "pca"), thresh = 1, pcaComp = 7)
      print(pca$rotation)
      
      PC1_coef <- pca$rotation[,1]   # coef of PC1
      PC2_coef <- pca$rotation[,2]   # coef of PC2
      PC3_coef <- pca$rotation[,3]   # coef of PC3
      PC4_coef <- pca$rotation[,4]   # coef of PC4
      PC5_coef <- pca$rotation[,5]   # coef of PC5
      PC6_coef <- pca$rotation[,6]   # coef of PC6
      PC7_coef <- pca$rotation[,7]   # coef of PC7
      
      PC1 <- PC1_coef[1]*exp(inputV[,1]) + PC1_coef[2]*exp(inputV[,2]) + PC1_coef[3]*exp(inputV[,3]) + 
        PC1_coef[4]*exp(inputV[,4]) + PC1_coef[5]*exp(inputV[,5]) + PC1_coef[6]*exp(inputV[,6]) + PC1_coef[7]*exp(inputV[,7])
      
      PC2 <- PC2_coef[1]*exp(inputV[,1]) + PC2_coef[2]*exp(inputV[,2]) + PC2_coef[3]*exp(inputV[,3]) + 
        PC2_coef[4]*exp(inputV[,4]) + PC2_coef[5]*exp(inputV[,5]) + PC2_coef[6]*exp(inputV[,6]) + PC2_coef[7]*exp(inputV[,7])
      
      PC3 <- PC3_coef[1]*exp(inputV[,1]) + PC3_coef[2]*exp(inputV[,2]) + PC3_coef[3]*exp(inputV[,3]) + 
        PC3_coef[4]*exp(inputV[,4]) + PC3_coef[5]*exp(inputV[,5]) + PC3_coef[6]*exp(inputV[,6]) + PC3_coef[7]*exp(inputV[,7])
      
      PC4 <- PC4_coef[1]*exp(inputV[,1]) + PC4_coef[2]*exp(inputV[,2]) + PC4_coef[3]*exp(inputV[,3]) + 
        PC4_coef[4]*exp(inputV[,4]) + PC4_coef[5]*exp(inputV[,5]) + PC4_coef[6]*exp(inputV[,6]) + PC4_coef[7]*exp(inputV[,7])
      
      PC5 <- PC5_coef[1]*exp(inputV[,1]) + PC5_coef[2]*exp(inputV[,2]) + PC5_coef[3]*exp(inputV[,3]) + 
        PC5_coef[4]*exp(inputV[,4]) + PC5_coef[5]*exp(inputV[,5]) + PC5_coef[6]*exp(inputV[,6]) + PC5_coef[7]*exp(inputV[,7])
      
      PC6 <- PC6_coef[1]*exp(inputV[,1]) + PC6_coef[2]*exp(inputV[,2]) + PC6_coef[3]*exp(inputV[,3]) + 
        PC6_coef[4]*exp(inputV[,4]) + PC6_coef[5]*exp(inputV[,5]) + PC6_coef[6]*exp(inputV[,6]) + PC6_coef[7]*exp(inputV[,7])
      
      PC7 <- PC7_coef[1]*exp(inputV[,1]) + PC7_coef[2]*exp(inputV[,2]) + PC7_coef[3]*exp(inputV[,3]) + 
        PC7_coef[4]*exp(inputV[,4]) + PC7_coef[5]*exp(inputV[,5]) + PC7_coef[6]*exp(inputV[,6]) + PC7_coef[7]*exp(inputV[,7])
      
      PC = cbind(PC1,PC2,PC3,PC4,PC5,PC6,PC7) 
      
      return(PC)  
      
    } else if(dim(inputV)[2]==8) {
      
      pca <- preProcess(-inputV, method=c("YeoJohnson", "center", "scale", "pca"), thresh = 1, pcaComp = 8)
      print(pca$rotation)
      
      PC1_coef <- pca$rotation[,1]   # coef of PC1
      PC2_coef <- pca$rotation[,2]   # coef of PC2
      PC3_coef <- pca$rotation[,3]   # coef of PC3
      PC4_coef <- pca$rotation[,4]   # coef of PC4
      PC5_coef <- pca$rotation[,5]   # coef of PC5
      PC6_coef <- pca$rotation[,6]   # coef of PC6
      PC7_coef <- pca$rotation[,7]   # coef of PC7
      PC8_coef <- pca$rotation[,8]   # coef of PC8
      
      PC1 <- PC1_coef[1]*exp(inputV[,1]) + PC1_coef[2]*exp(inputV[,2]) + PC1_coef[3]*exp(inputV[,3]) + PC1_coef[4]*exp(inputV[,4]) + 
        PC1_coef[5]*exp(inputV[,5]) + PC1_coef[6]*exp(inputV[,6]) + PC1_coef[7]*exp(inputV[,7]) + PC1_coef[8]*exp(inputV[,8]) 
      
      PC2 <- PC2_coef[1]*exp(inputV[,1]) + PC2_coef[2]*exp(inputV[,2]) + PC2_coef[3]*exp(inputV[,3]) + PC2_coef[4]*exp(inputV[,4]) + 
        PC2_coef[5]*exp(inputV[,5]) + PC2_coef[6]*exp(inputV[,6]) + PC2_coef[7]*exp(inputV[,7]) + PC2_coef[8]*exp(inputV[,8]) 
      
      PC3 <- PC3_coef[1]*exp(inputV[,1]) + PC3_coef[2]*exp(inputV[,2]) + PC3_coef[3]*exp(inputV[,3]) + PC3_coef[4]*exp(inputV[,4]) + 
        PC3_coef[5]*exp(inputV[,5]) + PC3_coef[6]*exp(inputV[,6]) + PC3_coef[7]*exp(inputV[,7]) + PC3_coef[8]*exp(inputV[,8]) 
      
      PC4 <- PC4_coef[1]*exp(inputV[,1]) + PC4_coef[2]*exp(inputV[,2]) + PC4_coef[3]*exp(inputV[,3]) + PC4_coef[4]*exp(inputV[,4]) + 
        PC4_coef[5]*exp(inputV[,5]) + PC4_coef[6]*exp(inputV[,6]) + PC4_coef[7]*exp(inputV[,7]) + PC4_coef[8]*exp(inputV[,8]) 
      
      PC5 <- PC5_coef[1]*exp(inputV[,1]) + PC5_coef[2]*exp(inputV[,2]) + PC5_coef[3]*exp(inputV[,3]) + PC5_coef[4]*exp(inputV[,4]) + 
        PC5_coef[5]*exp(inputV[,5]) + PC5_coef[6]*exp(inputV[,6]) + PC5_coef[7]*exp(inputV[,7]) + PC5_coef[8]*exp(inputV[,8]) 
      
      PC6 <- PC6_coef[1]*exp(inputV[,1]) + PC6_coef[2]*exp(inputV[,2]) + PC6_coef[3]*exp(inputV[,3]) + PC6_coef[4]*exp(inputV[,4]) + 
        PC6_coef[5]*exp(inputV[,5]) + PC6_coef[6]*exp(inputV[,6]) + PC6_coef[7]*exp(inputV[,7]) + PC6_coef[8]*exp(inputV[,8]) 
      
      PC7 <- PC7_coef[1]*exp(inputV[,1]) + PC7_coef[2]*exp(inputV[,2]) + PC7_coef[3]*exp(inputV[,3]) + PC7_coef[4]*exp(inputV[,4]) + 
        PC7_coef[5]*exp(inputV[,5]) + PC7_coef[6]*exp(inputV[,6]) + PC7_coef[7]*exp(inputV[,7]) + PC7_coef[8]*exp(inputV[,8])
      
      PC8 <- PC8_coef[1]*exp(inputV[,1]) + PC8_coef[2]*exp(inputV[,2]) + PC8_coef[3]*exp(inputV[,3]) + PC8_coef[4]*exp(inputV[,4]) + 
        PC8_coef[5]*exp(inputV[,5]) + PC8_coef[6]*exp(inputV[,6]) + PC8_coef[7]*exp(inputV[,7]) + PC8_coef[8]*exp(inputV[,8]) 
      
      PC = cbind(PC1,PC2,PC3,PC4,PC5,PC6,PC7,PC8) 
      
      return(PC)  
      
    } else if(dim(inputV)[2]==9) {
      
      pca <- preProcess(-inputV, method=c("YeoJohnson", "center", "scale", "pca"), thresh = 1, pcaComp = 9)
      print(pca$rotation)
      
      PC1_coef <- pca$rotation[,1]   # coef of PC1
      PC2_coef <- pca$rotation[,2]   # coef of PC2
      PC3_coef <- pca$rotation[,3]   # coef of PC3
      PC4_coef <- pca$rotation[,4]   # coef of PC4
      PC5_coef <- pca$rotation[,5]   # coef of PC5
      PC6_coef <- pca$rotation[,6]   # coef of PC6
      PC7_coef <- pca$rotation[,7]   # coef of PC7
      PC8_coef <- pca$rotation[,8]   # coef of PC8
      PC9_coef <- pca$rotation[,9]   # coef of PC9
      
      PC1 <- PC1_coef[1]*exp(inputV[,1]) + PC1_coef[2]*exp(inputV[,2]) + PC1_coef[3]*exp(inputV[,3]) + PC1_coef[4]*exp(inputV[,4]) + 
        PC1_coef[5]*exp(inputV[,5]) + PC1_coef[6]*exp(inputV[,6]) + PC1_coef[7]*exp(inputV[,7]) + PC1_coef[8]*exp(inputV[,8]) + 
        PC1_coef[9]*exp(inputV[,9]) 
      
      PC2 <- PC2_coef[1]*exp(inputV[,1]) + PC2_coef[2]*exp(inputV[,2]) + PC2_coef[3]*exp(inputV[,3]) + PC2_coef[4]*exp(inputV[,4]) + 
        PC2_coef[5]*exp(inputV[,5]) + PC2_coef[6]*exp(inputV[,6]) + PC2_coef[7]*exp(inputV[,7]) + PC2_coef[8]*exp(inputV[,8]) + 
        PC2_coef[9]*exp(inputV[,9]) 
      
      PC3 <- PC3_coef[1]*exp(inputV[,1]) + PC3_coef[2]*exp(inputV[,2]) + PC3_coef[3]*exp(inputV[,3]) + PC3_coef[4]*exp(inputV[,4]) + 
        PC3_coef[5]*exp(inputV[,5]) + PC3_coef[6]*exp(inputV[,6]) + PC3_coef[7]*exp(inputV[,7]) + PC3_coef[8]*exp(inputV[,8]) + 
        PC3_coef[9]*exp(inputV[,9]) 
      
      PC4 <- PC4_coef[1]*exp(inputV[,1]) + PC4_coef[2]*exp(inputV[,2]) + PC4_coef[3]*exp(inputV[,3]) + PC4_coef[4]*exp(inputV[,4]) + 
        PC4_coef[5]*exp(inputV[,5]) + PC4_coef[6]*exp(inputV[,6]) + PC4_coef[7]*exp(inputV[,7]) + PC4_coef[8]*exp(inputV[,8]) + 
        PC4_coef[9]*exp(inputV[,9]) 
      
      PC5 <- PC5_coef[1]*exp(inputV[,1]) + PC5_coef[2]*exp(inputV[,2]) + PC5_coef[3]*exp(inputV[,3]) + PC5_coef[4]*exp(inputV[,4]) + 
        PC5_coef[5]*exp(inputV[,5]) + PC5_coef[6]*exp(inputV[,6]) + PC5_coef[7]*exp(inputV[,7]) + PC5_coef[8]*exp(inputV[,8]) + 
        PC5_coef[9]*exp(inputV[,9]) 
      
      PC6 <- PC6_coef[1]*exp(inputV[,1]) + PC6_coef[2]*exp(inputV[,2]) + PC6_coef[3]*exp(inputV[,3]) + PC6_coef[4]*exp(inputV[,4]) + 
        PC6_coef[5]*exp(inputV[,5]) + PC6_coef[6]*exp(inputV[,6]) + PC6_coef[7]*exp(inputV[,7]) + PC6_coef[8]*exp(inputV[,8]) + 
        PC6_coef[9]*exp(inputV[,9]) 
      
      PC7 <- PC7_coef[1]*exp(inputV[,1]) + PC7_coef[2]*exp(inputV[,2]) + PC7_coef[3]*exp(inputV[,3]) + PC7_coef[4]*exp(inputV[,4]) + 
        PC7_coef[5]*exp(inputV[,5]) + PC7_coef[6]*exp(inputV[,6]) + PC7_coef[7]*exp(inputV[,7]) + PC7_coef[8]*exp(inputV[,8]) + 
        PC7_coef[9]*exp(inputV[,9]) 
      
      PC8 <- PC8_coef[1]*exp(inputV[,1]) + PC8_coef[2]*exp(inputV[,2]) + PC8_coef[3]*exp(inputV[,3]) + PC8_coef[4]*exp(inputV[,4]) + 
        PC8_coef[5]*exp(inputV[,5]) + PC8_coef[6]*exp(inputV[,6]) + PC8_coef[7]*exp(inputV[,7]) + PC8_coef[8]*exp(inputV[,8]) + 
        PC8_coef[9]*exp(inputV[,9]) 
      
      PC9 <- PC9_coef[1]*exp(inputV[,1]) + PC9_coef[2]*exp(inputV[,2]) + PC9_coef[3]*exp(inputV[,3]) + PC9_coef[4]*exp(inputV[,4]) + 
        PC9_coef[5]*exp(inputV[,5]) + PC9_coef[6]*exp(inputV[,6]) + PC9_coef[7]*exp(inputV[,7]) + PC9_coef[8]*exp(inputV[,8]) + 
        PC9_coef[9]*exp(inputV[,9])
      
      PC = cbind(PC1,PC2,PC3,PC4,PC5,PC6,PC7,PC8,PC9) 
      
      return(PC)  
      
    } else if(dim(inputV)[2]==10) {
      
      pca <- preProcess(-inputV, method=c("YeoJohnson", "center", "scale", "pca"), thresh = 1, pcaComp = 10)
      print(pca$rotation)
      
      PC1_coef <- pca$rotation[,1]   # coef of PC1
      PC2_coef <- pca$rotation[,2]   # coef of PC2
      PC3_coef <- pca$rotation[,3]   # coef of PC3
      PC4_coef <- pca$rotation[,4]   # coef of PC4
      PC5_coef <- pca$rotation[,5]   # coef of PC5
      PC6_coef <- pca$rotation[,6]   # coef of PC6
      PC7_coef <- pca$rotation[,7]   # coef of PC7
      PC8_coef <- pca$rotation[,8]   # coef of PC8
      PC9_coef <- pca$rotation[,9]   # coef of PC9
      PC10_coef <- pca$rotation[,10]   # coef of PC10
      
      PC1 <- PC1_coef[1]*exp(inputV[,1]) + PC1_coef[2]*exp(inputV[,2]) + PC1_coef[3]*exp(inputV[,3]) + PC1_coef[4]*exp(inputV[,4]) + 
        PC1_coef[5]*exp(inputV[,5]) + PC1_coef[6]*exp(inputV[,6]) + PC1_coef[7]*exp(inputV[,7]) + PC1_coef[8]*exp(inputV[,8]) + 
        PC1_coef[9]*exp(inputV[,9]) + PC1_coef[10]*exp(inputV[,10]) 
      
      PC2 <- PC2_coef[1]*exp(inputV[,1]) + PC2_coef[2]*exp(inputV[,2]) + PC2_coef[3]*exp(inputV[,3]) + PC2_coef[4]*exp(inputV[,4]) + 
        PC2_coef[5]*exp(inputV[,5]) + PC2_coef[6]*exp(inputV[,6]) + PC2_coef[7]*exp(inputV[,7]) + PC2_coef[8]*exp(inputV[,8]) + 
        PC2_coef[9]*exp(inputV[,9]) + PC2_coef[10]*exp(inputV[,10]) 
      
      PC3 <- PC3_coef[1]*exp(inputV[,1]) + PC3_coef[2]*exp(inputV[,2]) + PC3_coef[3]*exp(inputV[,3]) + PC3_coef[4]*exp(inputV[,4]) + 
        PC3_coef[5]*exp(inputV[,5]) + PC3_coef[6]*exp(inputV[,6]) + PC3_coef[7]*exp(inputV[,7]) + PC3_coef[8]*exp(inputV[,8]) + 
        PC3_coef[9]*exp(inputV[,9]) + PC3_coef[10]*exp(inputV[,10]) 
      
      PC4 <- PC4_coef[1]*exp(inputV[,1]) + PC4_coef[2]*exp(inputV[,2]) + PC4_coef[3]*exp(inputV[,3]) + PC4_coef[4]*exp(inputV[,4]) + 
        PC4_coef[5]*exp(inputV[,5]) + PC4_coef[6]*exp(inputV[,6]) + PC4_coef[7]*exp(inputV[,7]) + PC4_coef[8]*exp(inputV[,8]) + 
        PC4_coef[9]*exp(inputV[,9]) + PC4_coef[10]*exp(inputV[,10]) 
      
      PC5 <- PC5_coef[1]*exp(inputV[,1]) + PC5_coef[2]*exp(inputV[,2]) + PC5_coef[3]*exp(inputV[,3]) + PC5_coef[4]*exp(inputV[,4]) + 
        PC5_coef[5]*exp(inputV[,5]) + PC5_coef[6]*exp(inputV[,6]) + PC5_coef[7]*exp(inputV[,7]) + PC5_coef[8]*exp(inputV[,8]) + 
        PC5_coef[9]*exp(inputV[,9]) + PC5_coef[10]*exp(inputV[,10]) 
      
      PC6 <- PC6_coef[1]*exp(inputV[,1]) + PC6_coef[2]*exp(inputV[,2]) + PC6_coef[3]*exp(inputV[,3]) + PC6_coef[4]*exp(inputV[,4]) + 
        PC6_coef[5]*exp(inputV[,5]) + PC6_coef[6]*exp(inputV[,6]) + PC6_coef[7]*exp(inputV[,7]) + PC6_coef[8]*exp(inputV[,8]) + 
        PC6_coef[9]*exp(inputV[,9]) + PC6_coef[10]*exp(inputV[,10]) 
      
      PC7 <- PC7_coef[1]*exp(inputV[,1]) + PC7_coef[2]*exp(inputV[,2]) + PC7_coef[3]*exp(inputV[,3]) + PC7_coef[4]*exp(inputV[,4]) + 
        PC7_coef[5]*exp(inputV[,5]) + PC7_coef[6]*exp(inputV[,6]) + PC7_coef[7]*exp(inputV[,7]) + PC7_coef[8]*exp(inputV[,8]) + 
        PC7_coef[9]*exp(inputV[,9]) + PC7_coef[10]*exp(inputV[,10]) 
      
      PC8 <- PC8_coef[1]*exp(inputV[,1]) + PC8_coef[2]*exp(inputV[,2]) + PC8_coef[3]*exp(inputV[,3]) + PC8_coef[4]*exp(inputV[,4]) + 
        PC8_coef[5]*exp(inputV[,5]) + PC8_coef[6]*exp(inputV[,6]) + PC8_coef[7]*exp(inputV[,7]) + PC8_coef[8]*exp(inputV[,8]) + 
        PC8_coef[9]*exp(inputV[,9]) + PC8_coef[10]*exp(inputV[,10]) 
      
      PC9 <- PC9_coef[1]*exp(inputV[,1]) + PC9_coef[2]*exp(inputV[,2]) + PC9_coef[3]*exp(inputV[,3]) + PC9_coef[4]*exp(inputV[,4]) + 
        PC9_coef[5]*exp(inputV[,5]) + PC9_coef[6]*exp(inputV[,6]) + PC9_coef[7]*exp(inputV[,7]) + PC9_coef[8]*exp(inputV[,8]) + 
        PC9_coef[9]*exp(inputV[,9]) + PC9_coef[10]*exp(inputV[,10]) 
      
      PC10 <- PC10_coef[1]*exp(inputV[,1]) + PC10_coef[2]*exp(inputV[,2]) + PC10_coef[3]*exp(inputV[,3]) + PC10_coef[4]*exp(inputV[,4]) + 
        PC10_coef[5]*exp(inputV[,5]) + PC10_coef[6]*exp(inputV[,6]) + PC10_coef[7]*exp(inputV[,7]) + PC10_coef[8]*exp(inputV[,8]) + 
        PC10_coef[9]*exp(inputV[,9]) + PC10_coef[10]*exp(inputV[,10]) 
      
      PC = cbind(PC1,PC2,PC3,PC4,PC5,PC6,PC7,PC8,PC9,PC10) 
      
      return(PC)  
      
    } else if(dim(inputV)[2]==11) {
      
      pca <- preProcess(-inputV, method=c("YeoJohnson", "center", "scale", "pca"), thresh = 1, pcaComp = 11)
      print(pca$rotation)
      
      PC1_coef <- pca$rotation[,1]   # coef of PC1
      PC2_coef <- pca$rotation[,2]   # coef of PC2
      PC3_coef <- pca$rotation[,3]   # coef of PC3
      PC4_coef <- pca$rotation[,4]   # coef of PC4
      PC5_coef <- pca$rotation[,5]   # coef of PC5
      PC6_coef <- pca$rotation[,6]   # coef of PC6
      PC7_coef <- pca$rotation[,7]   # coef of PC7
      PC8_coef <- pca$rotation[,8]   # coef of PC8
      PC9_coef <- pca$rotation[,9]   # coef of PC9
      PC10_coef <- pca$rotation[,10]   # coef of PC10
      PC11_coef <- pca$rotation[,11]   # coef of PC11
      
      PC1 <- PC1_coef[1]*exp(inputV[,1]) + PC1_coef[2]*exp(inputV[,2]) + PC1_coef[3]*exp(inputV[,3]) + PC1_coef[4]*exp(inputV[,4]) + 
        PC1_coef[5]*exp(inputV[,5]) + PC1_coef[6]*exp(inputV[,6]) + PC1_coef[7]*exp(inputV[,7]) + PC1_coef[8]*exp(inputV[,8]) + 
        PC1_coef[9]*exp(inputV[,9]) + PC1_coef[10]*exp(inputV[,10]) + PC1_coef[11]*exp(inputV[,11]) 
      
      PC2 <- PC2_coef[1]*exp(inputV[,1]) + PC2_coef[2]*exp(inputV[,2]) + PC2_coef[3]*exp(inputV[,3]) + PC2_coef[4]*exp(inputV[,4]) + 
        PC2_coef[5]*exp(inputV[,5]) + PC2_coef[6]*exp(inputV[,6]) + PC2_coef[7]*exp(inputV[,7]) + PC2_coef[8]*exp(inputV[,8]) + 
        PC2_coef[9]*exp(inputV[,9]) + PC2_coef[10]*exp(inputV[,10]) + PC2_coef[11]*exp(inputV[,11])
      
      PC3 <- PC3_coef[1]*exp(inputV[,1]) + PC3_coef[2]*exp(inputV[,2]) + PC3_coef[3]*exp(inputV[,3]) + PC3_coef[4]*exp(inputV[,4]) + 
        PC3_coef[5]*exp(inputV[,5]) + PC3_coef[6]*exp(inputV[,6]) + PC3_coef[7]*exp(inputV[,7]) + PC3_coef[8]*exp(inputV[,8]) + 
        PC3_coef[9]*exp(inputV[,9]) + PC3_coef[10]*exp(inputV[,10]) + PC3_coef[11]*exp(inputV[,11]) 
      
      PC4 <- PC4_coef[1]*exp(inputV[,1]) + PC4_coef[2]*exp(inputV[,2]) + PC4_coef[3]*exp(inputV[,3]) + PC4_coef[4]*exp(inputV[,4]) + 
        PC4_coef[5]*exp(inputV[,5]) + PC4_coef[6]*exp(inputV[,6]) + PC4_coef[7]*exp(inputV[,7]) + PC4_coef[8]*exp(inputV[,8]) + 
        PC4_coef[9]*exp(inputV[,9]) + PC4_coef[10]*exp(inputV[,10]) + PC4_coef[11]*exp(inputV[,11]) 
      
      PC5 <- PC5_coef[1]*exp(inputV[,1]) + PC5_coef[2]*exp(inputV[,2]) + PC5_coef[3]*exp(inputV[,3]) + PC5_coef[4]*exp(inputV[,4]) + 
        PC5_coef[5]*exp(inputV[,5]) + PC5_coef[6]*exp(inputV[,6]) + PC5_coef[7]*exp(inputV[,7]) + PC5_coef[8]*exp(inputV[,8]) + 
        PC5_coef[9]*exp(inputV[,9]) + PC5_coef[10]*exp(inputV[,10]) + PC5_coef[11]*exp(inputV[,11]) 
      
      PC6 <- PC6_coef[1]*exp(inputV[,1]) + PC6_coef[2]*exp(inputV[,2]) + PC6_coef[3]*exp(inputV[,3]) + PC6_coef[4]*exp(inputV[,4]) + 
        PC6_coef[5]*exp(inputV[,5]) + PC6_coef[6]*exp(inputV[,6]) + PC6_coef[7]*exp(inputV[,7]) + PC6_coef[8]*exp(inputV[,8]) + 
        PC6_coef[9]*exp(inputV[,9]) + PC6_coef[10]*exp(inputV[,10]) + PC6_coef[11]*exp(inputV[,11])
      
      PC7 <- PC7_coef[1]*exp(inputV[,1]) + PC7_coef[2]*exp(inputV[,2]) + PC7_coef[3]*exp(inputV[,3]) + PC7_coef[4]*exp(inputV[,4]) + 
        PC7_coef[5]*exp(inputV[,5]) + PC7_coef[6]*exp(inputV[,6]) + PC7_coef[7]*exp(inputV[,7]) + PC7_coef[8]*exp(inputV[,8]) + 
        PC7_coef[9]*exp(inputV[,9]) + PC7_coef[10]*exp(inputV[,10]) + PC7_coef[11]*exp(inputV[,11]) 
      
      PC8 <- PC8_coef[1]*exp(inputV[,1]) + PC8_coef[2]*exp(inputV[,2]) + PC8_coef[3]*exp(inputV[,3]) + PC8_coef[4]*exp(inputV[,4]) + 
        PC8_coef[5]*exp(inputV[,5]) + PC8_coef[6]*exp(inputV[,6]) + PC8_coef[7]*exp(inputV[,7]) + PC8_coef[8]*exp(inputV[,8]) + 
        PC8_coef[9]*exp(inputV[,9]) + PC8_coef[10]*exp(inputV[,10]) + PC8_coef[11]*exp(inputV[,11]) 
      
      PC9 <- PC9_coef[1]*exp(inputV[,1]) + PC9_coef[2]*exp(inputV[,2]) + PC9_coef[3]*exp(inputV[,3]) + PC9_coef[4]*exp(inputV[,4]) + 
        PC9_coef[5]*exp(inputV[,5]) + PC9_coef[6]*exp(inputV[,6]) + PC9_coef[7]*exp(inputV[,7]) + PC9_coef[8]*exp(inputV[,8]) + 
        PC9_coef[9]*exp(inputV[,9]) + PC9_coef[10]*exp(inputV[,10]) + PC9_coef[11]*exp(inputV[,11])
      
      PC10 <- PC10_coef[1]*exp(inputV[,1]) + PC10_coef[2]*exp(inputV[,2]) + PC10_coef[3]*exp(inputV[,3]) + PC10_coef[4]*exp(inputV[,4]) + 
        PC10_coef[5]*exp(inputV[,5]) + PC10_coef[6]*exp(inputV[,6]) + PC10_coef[7]*exp(inputV[,7]) + PC10_coef[8]*exp(inputV[,8]) + 
        PC10_coef[9]*exp(inputV[,9]) + PC10_coef[10]*exp(inputV[,10]) + PC10_coef[11]*exp(inputV[,11]) 
      
      PC11 <- PC11_coef[1]*exp(inputV[,1]) + PC11_coef[2]*exp(inputV[,2]) + PC11_coef[3]*exp(inputV[,3]) + PC11_coef[4]*exp(inputV[,4]) + 
        PC11_coef[5]*exp(inputV[,5]) + PC11_coef[6]*exp(inputV[,6]) + PC11_coef[7]*exp(inputV[,7]) + PC11_coef[8]*exp(inputV[,8]) + 
        PC11_coef[9]*exp(inputV[,9]) + PC11_coef[10]*exp(inputV[,10]) + PC11_coef[11]*exp(inputV[,11])
      
      PC = cbind(PC1,PC2,PC3,PC4,PC5,PC6,PC7,PC8,PC9,PC10,PC11) 
      
      return(PC)  
      
    } else if(dim(inputV)[2]==12) {
      
      pca <- preProcess(-inputV, method=c("YeoJohnson", "center", "scale", "pca"), thresh = 1, pcaComp = 12)
      print(pca$rotation)
      
      PC1_coef <- pca$rotation[,1]   # coef of PC1
      PC2_coef <- pca$rotation[,2]   # coef of PC2
      PC3_coef <- pca$rotation[,3]   # coef of PC3
      PC4_coef <- pca$rotation[,4]   # coef of PC4
      PC5_coef <- pca$rotation[,5]   # coef of PC5
      PC6_coef <- pca$rotation[,6]   # coef of PC6
      PC7_coef <- pca$rotation[,7]   # coef of PC7
      PC8_coef <- pca$rotation[,8]   # coef of PC8
      PC9_coef <- pca$rotation[,9]   # coef of PC9
      PC10_coef <- pca$rotation[,10]   # coef of PC10
      PC11_coef <- pca$rotation[,11]   # coef of PC11
      PC12_coef <- pca$rotation[,12]   # coef of PC12
      
      PC1 <- PC1_coef[1]*exp(inputV[,1]) + PC1_coef[2]*exp(inputV[,2]) + PC1_coef[3]*exp(inputV[,3]) + PC1_coef[4]*exp(inputV[,4]) + 
        PC1_coef[5]*exp(inputV[,5]) + PC1_coef[6]*exp(inputV[,6]) + PC1_coef[7]*exp(inputV[,7]) + PC1_coef[8]*exp(inputV[,8]) + 
        PC1_coef[9]*exp(inputV[,9]) + PC1_coef[10]*exp(inputV[,10]) + PC1_coef[11]*exp(inputV[,11]) + PC1_coef[12]*exp(inputV[,12])
      
      PC2 <- PC2_coef[1]*exp(inputV[,1]) + PC2_coef[2]*exp(inputV[,2]) + PC2_coef[3]*exp(inputV[,3]) + PC2_coef[4]*exp(inputV[,4]) + 
        PC2_coef[5]*exp(inputV[,5]) + PC2_coef[6]*exp(inputV[,6]) + PC2_coef[7]*exp(inputV[,7]) + PC2_coef[8]*exp(inputV[,8]) + 
        PC2_coef[9]*exp(inputV[,9]) + PC2_coef[10]*exp(inputV[,10]) + PC2_coef[11]*exp(inputV[,11]) + PC2_coef[12]*exp(inputV[,12])
      
      PC3 <- PC3_coef[1]*exp(inputV[,1]) + PC3_coef[2]*exp(inputV[,2]) + PC3_coef[3]*exp(inputV[,3]) + PC3_coef[4]*exp(inputV[,4]) + 
        PC3_coef[5]*exp(inputV[,5]) + PC3_coef[6]*exp(inputV[,6]) + PC3_coef[7]*exp(inputV[,7]) + PC3_coef[8]*exp(inputV[,8]) + 
        PC3_coef[9]*exp(inputV[,9]) + PC3_coef[10]*exp(inputV[,10]) + PC3_coef[11]*exp(inputV[,11]) + PC3_coef[12]*exp(inputV[,12])
      
      PC4 <- PC4_coef[1]*exp(inputV[,1]) + PC4_coef[2]*exp(inputV[,2]) + PC4_coef[3]*exp(inputV[,3]) + PC4_coef[4]*exp(inputV[,4]) + 
        PC4_coef[5]*exp(inputV[,5]) + PC4_coef[6]*exp(inputV[,6]) + PC4_coef[7]*exp(inputV[,7]) + PC4_coef[8]*exp(inputV[,8]) + 
        PC4_coef[9]*exp(inputV[,9]) + PC4_coef[10]*exp(inputV[,10]) + PC4_coef[11]*exp(inputV[,11]) + PC4_coef[12]*exp(inputV[,12])
      
      PC5 <- PC5_coef[1]*exp(inputV[,1]) + PC5_coef[2]*exp(inputV[,2]) + PC5_coef[3]*exp(inputV[,3]) + PC5_coef[4]*exp(inputV[,4]) + 
        PC5_coef[5]*exp(inputV[,5]) + PC5_coef[6]*exp(inputV[,6]) + PC5_coef[7]*exp(inputV[,7]) + PC5_coef[8]*exp(inputV[,8]) + 
        PC5_coef[9]*exp(inputV[,9]) + PC5_coef[10]*exp(inputV[,10]) + PC5_coef[11]*exp(inputV[,11]) + PC5_coef[12]*exp(inputV[,12])
      
      PC6 <- PC6_coef[1]*exp(inputV[,1]) + PC6_coef[2]*exp(inputV[,2]) + PC6_coef[3]*exp(inputV[,3]) + PC6_coef[4]*exp(inputV[,4]) + 
        PC6_coef[5]*exp(inputV[,5]) + PC6_coef[6]*exp(inputV[,6]) + PC6_coef[7]*exp(inputV[,7]) + PC6_coef[8]*exp(inputV[,8]) + 
        PC6_coef[9]*exp(inputV[,9]) + PC6_coef[10]*exp(inputV[,10]) + PC6_coef[11]*exp(inputV[,11]) + PC6_coef[12]*exp(inputV[,12])
      
      PC7 <- PC7_coef[1]*exp(inputV[,1]) + PC7_coef[2]*exp(inputV[,2]) + PC7_coef[3]*exp(inputV[,3]) + PC7_coef[4]*exp(inputV[,4]) + 
        PC7_coef[5]*exp(inputV[,5]) + PC7_coef[6]*exp(inputV[,6]) + PC7_coef[7]*exp(inputV[,7]) + PC7_coef[8]*exp(inputV[,8]) + 
        PC7_coef[9]*exp(inputV[,9]) + PC7_coef[10]*exp(inputV[,10]) + PC7_coef[11]*exp(inputV[,11]) + PC7_coef[12]*exp(inputV[,12])
      
      PC8 <- PC8_coef[1]*exp(inputV[,1]) + PC8_coef[2]*exp(inputV[,2]) + PC8_coef[3]*exp(inputV[,3]) + PC8_coef[4]*exp(inputV[,4]) + 
        PC8_coef[5]*exp(inputV[,5]) + PC8_coef[6]*exp(inputV[,6]) + PC8_coef[7]*exp(inputV[,7]) + PC8_coef[8]*exp(inputV[,8]) + 
        PC8_coef[9]*exp(inputV[,9]) + PC8_coef[10]*exp(inputV[,10]) + PC8_coef[11]*exp(inputV[,11]) + PC8_coef[12]*exp(inputV[,12])
      
      PC9 <- PC9_coef[1]*exp(inputV[,1]) + PC9_coef[2]*exp(inputV[,2]) + PC9_coef[3]*exp(inputV[,3]) + PC9_coef[4]*exp(inputV[,4]) + 
        PC9_coef[5]*exp(inputV[,5]) + PC9_coef[6]*exp(inputV[,6]) + PC9_coef[7]*exp(inputV[,7]) + PC9_coef[8]*exp(inputV[,8]) + 
        PC9_coef[9]*exp(inputV[,9]) + PC9_coef[10]*exp(inputV[,10]) + PC9_coef[11]*exp(inputV[,11]) + PC9_coef[12]*exp(inputV[,12])
      
      PC10 <- PC10_coef[1]*exp(inputV[,1]) + PC10_coef[2]*exp(inputV[,2]) + PC10_coef[3]*exp(inputV[,3]) + PC10_coef[4]*exp(inputV[,4]) + 
        PC10_coef[5]*exp(inputV[,5]) + PC10_coef[6]*exp(inputV[,6]) + PC10_coef[7]*exp(inputV[,7]) + PC10_coef[8]*exp(inputV[,8]) + 
        PC10_coef[9]*exp(inputV[,9]) + PC10_coef[10]*exp(inputV[,10]) + PC10_coef[11]*exp(inputV[,11]) + PC10_coef[12]*exp(inputV[,12])
      
      PC11 <- PC11_coef[1]*exp(inputV[,1]) + PC11_coef[2]*exp(inputV[,2]) + PC11_coef[3]*exp(inputV[,3]) + PC11_coef[4]*exp(inputV[,4]) + 
        PC11_coef[5]*exp(inputV[,5]) + PC11_coef[6]*exp(inputV[,6]) + PC11_coef[7]*exp(inputV[,7]) + PC11_coef[8]*exp(inputV[,8]) + 
        PC11_coef[9]*exp(inputV[,9]) + PC11_coef[10]*exp(inputV[,10]) + PC11_coef[11]*exp(inputV[,11]) + PC11_coef[12]*exp(inputV[,12])
      
      PC12 <- PC12_coef[1]*exp(inputV[,1]) + PC12_coef[2]*exp(inputV[,2]) + PC12_coef[3]*exp(inputV[,3]) + PC12_coef[4]*exp(inputV[,4]) + 
        PC12_coef[5]*exp(inputV[,5]) + PC12_coef[6]*exp(inputV[,6]) + PC12_coef[7]*exp(inputV[,7]) + PC12_coef[8]*exp(inputV[,8]) + 
        PC12_coef[9]*exp(inputV[,9]) + PC12_coef[10]*exp(inputV[,10]) + PC12_coef[11]*exp(inputV[,11]) + PC12_coef[12]*exp(inputV[,12])
      
      PC = cbind(PC1,PC2,PC3,PC4,PC5,PC6,PC7,PC8,PC9,PC10,PC11,PC12) 
      
      return(PC)  
      
    }
    
  }
  
}
