
# This function helps draw the wind arrow on the google map -- Weihua Wang
# 2017-05-08, 1st built, 'Another correct way'
# 2017-05-10, 2st visit, Xoffset should be e.g. 0.01*sin(wind), Yoffset should be e.g. 0.01*cos(wind)
# Input of windir is the direction where the wind is coming from (in degrees).
# Outputs of c(latend,lonend) is the ending points of the wind arrow.
# 0.01 or alike just helps calculate the arrow ending points

windArrow <- function(windir,zoomfactor=13){
  
  wind <- windir*pi/180     # changes into radians
  
  if(zoomfactor==13){
    offx = 0.005*sin(wind+pi)
    offy = 0.005*cos(wind+pi) 
  } else if(zoomfactor==12){
    offx = 0.01*sin(wind+pi)
    offy = 0.01*cos(wind+pi)
  } else if(zoomfactor==11){
    offx = 0.03*sin(wind+pi)
    offy = 0.03*cos(wind+pi)
  }
  
  latend = center[2]+offy  
  lonend = center[1]+offx
  
  return(c(latend,lonend))
}



# One correct way
# windArrow <- function(windir){
#   
#   if(windir>=0 & windir<90){
#     wind <- windir*pi/180     # changes into radians
#     offx = -0.01*sin(wind)     
#     offy = -0.01*cos(wind)
#     latend = center[2]+offy   # maps to No.4 Quadrant
#     lonend = center[1]+offx
#   } else if(windir>=90 & windir<180){
#     wind <- windir*pi/180     # changes into radians
#     offx = -0.01*sin(wind)
#     offy = -0.01*cos(wind)
#     latend = center[2]+offy
#     lonend = center[1]+offx
#   } else if(windir>=180 & windir<270){
#     wind <- windir*pi/180 # in radians
#     offx = -0.01*sin(wind)
#     offy = -0.01*cos(wind)
#     latend = center[2]+offy
#     lonend = center[1]+offx
#   } else if(windir>=270 & windir<360){
#     wind <- windir*pi/180
#     offx = -0.01*sin(wind)
#     offy = -0.01*cos(wind)
#     latend = center[2]+offy
#     lonend = center[1]+offx
#   }
#   return(c(latend,lonend))
# }
# 
# Another correct way -- here I adjust all windir angle to be ACUTE angle! 
# windArrow <- function(windir){
#   if(windir>=0 & windir<90){
#     wind <- windir*pi/180 # in radians
#     offx = 0.01*cos(wind)
#     offy = 0.01*sin(wind)
#     latend = center[2]-offy
#     lonend = center[1]-offx
#   } else if(windir>=90 & windir<180){
#     wind <- windir*pi/180 # in radians
#     offx = 0.01*cos(wind-pi/2)
#     offy = 0.01*sin(wind-pi/2)
#     latend = center[2]+offy
#     lonend = center[1]-offx
#   } else if(windir>=180 & windir<270){
#     wind <- windir*pi/180 # in radians
#     offx = 0.01*cos(wind-pi)
#     offy = 0.01*sin(wind-pi)
#     latend = center[2]+offy
#     lonend = center[1]+offx
#   } else if(windir>=270 & windir<360){
#     wind <- windir*pi/180
#     offy = 0.01*sin(wind-3*pi/2)
#     offx = 0.01*cos(wind-3*pi/2)
#     latend = center[2]-offy
#     lonend = center[1]+offx
#   }
#   return(c(latend,lonend))
# }

# Test code
# center = c(0,0)
# windArrow(20);windArrow(120);windArrow(240);windArrow(320)
