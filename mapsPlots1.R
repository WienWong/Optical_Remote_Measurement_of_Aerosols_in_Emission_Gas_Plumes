
# This function works for 'Log Ratio' 
# switch==1: SO2, NO2, HCHO, OpticalDepth
# switch==2: SO2, NO2, HCHO, Alkane, OpticalDepth
# switch==3: SO2, NO2, HCHO, NH3, Alkane, OpticalDepth
# switch==4: SO2, NO2, HCHO, NH3, Alkane, Alkene
# 2017-06-01, 1st built, Wang Weihua

mapsPlots1 <- function(appro="nature",switch=1) {
  
  library(grid)            # for function "textGrob"
  library(gridExtra)
  title1 = textGrob(title, gp=gpar(fontsize=18,fontface="bold"))
  
  if(appro=="nature"){
    
    p0 <- mp + geom_point(data=datmrg.seg, aes(x=longitude, y=latitude, colour=OpticalDepth, size=size   )) + 
      scale_colour_gradientn(colors = c("blue", "cyan", "green", "yellow", "orange", "magenta", "red")) +
      geom_segment(data = datmrg.seg, aes(y = lat, x = lon, yend = latend, xend = lonend), arrow = arrow() )
    
    if(switch==1){         # SO2, NO2, HCHO, OpticalDepth
      
      p1 <- mp + geom_point(data=datmrgSO2, aes(x=longitude, y=latitude, colour = SO2conc, size = size ) ) + 
        scale_colour_gradientn(colors = c("blue", "cyan", "green", "yellow", "orange", "magenta", "red"))
      
      p2 <- mp + geom_point(data=datmrgNO2, aes(x=longitude, y=latitude, colour = NO2aconc, size = size ) ) + 
        scale_colour_gradientn(colors = c("blue", "cyan", "green", "yellow", "orange", "magenta", "red"))
      
      p3 <- mp + geom_point(data=datmrgHCHO, aes(x=longitude, y=latitude, colour = HCHOaconc, size = size ) ) + 
        scale_colour_gradientn(colors = c("blue", "cyan", "green", "yellow", "orange", "magenta", "red"))
      
      grid.arrange(p1,p2,p3,p0,top=title1,layout_matrix = matrix(c(1,2,3,4),ncol=2,byrow=TRUE))
    } else if(switch==2){  # SO2, NO2, HCHO, Alkane, OpticalDepth
      
      p1 <- mp + geom_point(data=datmrgSO2, aes(x=longitude, y=latitude, colour = SO2conc, size = size ) ) + 
        scale_colour_gradientn(colors = c("blue", "cyan", "green", "yellow", "orange", "magenta", "red"))
      
      p2 <- mp + geom_point(data=datmrgNO2, aes(x=longitude, y=latitude, colour = NO2aconc, size = size ) ) + 
        scale_colour_gradientn(colors = c("blue", "cyan", "green", "yellow", "orange", "magenta", "red"))
      
      p3 <- mp + geom_point(data=datmrgHCHO, aes(x=longitude, y=latitude, colour = HCHOaconc, size = size ) ) + 
        scale_colour_gradientn(colors = c("blue", "cyan", "green", "yellow", "orange", "magenta", "red"))
      
      p8 <- mp + geom_point(data=datmrgAlkane, aes(x=longitude, y=latitude, colour = Alkaneconc, size = size ) ) + 
        scale_colour_gradientn(colors = c("blue", "cyan", "green", "yellow", "orange", "magenta", "red"))
      
      grid.arrange(p1,p2,p3,p8,p0,top=title1,layout_matrix = matrix(c(1,2,3,4,5,5),ncol=3,byrow=TRUE))
    } else if(switch==3){  # SO2, NO2, HCHO, NH3, Alkane, OpticalDepth
      
      p1 <- mp + geom_point(data=datmrgSO2, aes(x=longitude, y=latitude, colour = SO2conc, size = size ) ) + 
        scale_colour_gradientn(colors = c("blue", "cyan", "green", "yellow", "orange", "magenta", "red"))
      
      p2 <- mp + geom_point(data=datmrgNO2, aes(x=longitude, y=latitude, colour = NO2aconc, size = size ) ) + 
        scale_colour_gradientn(colors = c("blue", "cyan", "green", "yellow", "orange", "magenta", "red"))
      
      p3 <- mp + geom_point(data=datmrgHCHO, aes(x=longitude, y=latitude, colour = HCHOaconc, size = size ) ) + 
        scale_colour_gradientn(colors = c("blue", "cyan", "green", "yellow", "orange", "magenta", "red"))
      
      p5 <- mp + geom_point(data=datmrgNH3, aes(x=longitude, y=latitude, colour = NH3conc, size = size ) ) + 
        scale_colour_gradientn(colors = c("blue", "cyan", "green", "yellow", "orange", "magenta", "red"))
      
      p8 <- mp + geom_point(data=datmrgAlkane, aes(x=longitude, y=latitude, colour = Alkaneconc, size = size ) ) + 
        scale_colour_gradientn(colors = c("blue", "cyan", "green", "yellow", "orange", "magenta", "red"))
      
      grid.arrange(p1,p2,p3,p5,p8,p0,top=title1,layout_matrix = matrix(c(1,2,3,4,5,6),ncol=3,byrow=TRUE))
    } else if(switch==4){  # SO2, NO2, HCHO, NH3, Alkane, Alkene
      
      p1 <- mp + geom_point(data=datmrgSO2, aes(x=longitude, y=latitude, colour = SO2conc, size = size ) ) + 
        scale_colour_gradientn(colors = c("blue", "cyan", "green", "yellow", "orange", "magenta", "red"))
      
      p2 <- mp + geom_point(data=datmrgNO2, aes(x=longitude, y=latitude, colour = NO2aconc, size = size ) ) + 
        scale_colour_gradientn(colors = c("blue", "cyan", "green", "yellow", "orange", "magenta", "red"))
      
      p3 <- mp + geom_point(data=datmrgHCHO, aes(x=longitude, y=latitude, colour = HCHOaconc, size = size ) ) + 
        scale_colour_gradientn(colors = c("blue", "cyan", "green", "yellow", "orange", "magenta", "red"))
      
      p5 <- mp + geom_point(data=datmrgNH3, aes(x=longitude, y=latitude, colour = NH3conc, size = size ) ) + 
        scale_colour_gradientn(colors = c("blue", "cyan", "green", "yellow", "orange", "magenta", "red"))
      p6 <- mp + geom_point(data=datmrgNH3, aes(x=longitude, y=latitude, colour = log10(NH3conc), size = log10(size) ) ) + 
        scale_colour_gradientn(colors = c("blue", "cyan", "green", "yellow", "orange", "magenta", "red"))
      
      p8 <- mp + geom_point(data=datmrgAlkane, aes(x=longitude, y=latitude, colour = Alkaneconc, size = size ) ) + 
        scale_colour_gradientn(colors = c("blue", "cyan", "green", "yellow", "orange", "magenta", "red"))
      p9 <- mp + geom_point(data=datmrgAlkane, aes(x=longitude, y=latitude, colour = log10(Alkaneconc), size = log10(size) ) ) + 
        scale_colour_gradientn(colors = c("blue", "cyan", "green", "yellow", "orange", "magenta", "red"))
      
      p11 <- mp + geom_point(data=datmrgAlkene, aes(x=longitude, y=latitude, colour = Alkeneconc, size = size ) ) + 
        scale_colour_gradientn(colors = c("blue", "cyan", "green", "yellow", "orange", "magenta", "red"))
      p12 <- mp + geom_point(data=datmrgAlkene, aes(x=longitude, y=latitude, colour = log10(Alkeneconc), size = log10(size) ) ) + 
        scale_colour_gradientn(colors = c("blue", "cyan", "green", "yellow", "orange", "magenta", "red"))
      
      grid.arrange(p1,p2,p3,p8,p11,top=title1,layout_matrix = matrix(c(1,2,3,4,5,6),ncol=3,byrow=TRUE))
    }
  } else if(appro=="Log"){
    
    p0 <- mp + geom_point(data=datmrg.seg, aes(x=longitude, y=latitude, colour=Log.OpticalDepth, size=size   )) + 
      scale_colour_gradientn(colors = c("blue", "cyan", "green", "yellow", "orange", "magenta", "red")) +
      geom_segment(data = datmrg.seg, aes(y = lat, x = lon, yend = latend, xend = lonend), arrow = arrow() )
    
    if(switch==1){         # SO2, NO2, HCHO, OpticalDepth
      
      p1 <- mp + geom_point(data=datmrgSO2, aes(x=longitude, y=latitude, colour = SO2conc, size = size ) ) + 
        scale_colour_gradientn(colors = c("blue", "cyan", "green", "yellow", "orange", "magenta", "red"))
      
      p2 <- mp + geom_point(data=datmrgNO2, aes(x=longitude, y=latitude, colour = NO2aconc, size = size ) ) + 
        scale_colour_gradientn(colors = c("blue", "cyan", "green", "yellow", "orange", "magenta", "red"))
      
      p3 <- mp + geom_point(data=datmrgHCHO, aes(x=longitude, y=latitude, colour = HCHOaconc, size = size ) ) + 
        scale_colour_gradientn(colors = c("blue", "cyan", "green", "yellow", "orange", "magenta", "red"))
      
      grid.arrange(p1,p2,p3,p0,top=title1,layout_matrix = matrix(c(1,2,3,4),ncol=2,byrow=TRUE))
    } else if(switch==2){  # SO2, NO2, HCHO, Alkane, OpticalDepth
      
      p1 <- mp + geom_point(data=datmrgSO2, aes(x=longitude, y=latitude, colour = SO2conc, size = size ) ) + 
        scale_colour_gradientn(colors = c("blue", "cyan", "green", "yellow", "orange", "magenta", "red"))
      
      p2 <- mp + geom_point(data=datmrgNO2, aes(x=longitude, y=latitude, colour = NO2aconc, size = size ) ) + 
        scale_colour_gradientn(colors = c("blue", "cyan", "green", "yellow", "orange", "magenta", "red"))
      
      p3 <- mp + geom_point(data=datmrgHCHO, aes(x=longitude, y=latitude, colour = HCHOaconc, size = size ) ) + 
        scale_colour_gradientn(colors = c("blue", "cyan", "green", "yellow", "orange", "magenta", "red"))
      
      p8 <- mp + geom_point(data=datmrgAlkane, aes(x=longitude, y=latitude, colour = Alkaneconc, size = size ) ) + 
        scale_colour_gradientn(colors = c("blue", "cyan", "green", "yellow", "orange", "magenta", "red"))
      
      grid.arrange(p1,p2,p3,p8,p0,top=title1,layout_matrix = matrix(c(1,2,3,4,5,5),ncol=3,byrow=TRUE))
    } else if(switch==3){  # SO2, NO2, HCHO, NH3, Alkane, OpticalDepth
      
      p1 <- mp + geom_point(data=datmrgSO2, aes(x=longitude, y=latitude, colour = SO2conc, size = size ) ) + 
        scale_colour_gradientn(colors = c("blue", "cyan", "green", "yellow", "orange", "magenta", "red"))
      
      p2 <- mp + geom_point(data=datmrgNO2, aes(x=longitude, y=latitude, colour = NO2aconc, size = size ) ) + 
        scale_colour_gradientn(colors = c("blue", "cyan", "green", "yellow", "orange", "magenta", "red"))
      
      p3 <- mp + geom_point(data=datmrgHCHO, aes(x=longitude, y=latitude, colour = HCHOaconc, size = size ) ) + 
        scale_colour_gradientn(colors = c("blue", "cyan", "green", "yellow", "orange", "magenta", "red"))
      
      p5 <- mp + geom_point(data=datmrgNH3, aes(x=longitude, y=latitude, colour = NH3conc, size = size ) ) + 
        scale_colour_gradientn(colors = c("blue", "cyan", "green", "yellow", "orange", "magenta", "red"))
      
      p8 <- mp + geom_point(data=datmrgAlkane, aes(x=longitude, y=latitude, colour = Alkaneconc, size = size ) ) + 
        scale_colour_gradientn(colors = c("blue", "cyan", "green", "yellow", "orange", "magenta", "red"))
      
      grid.arrange(p1,p2,p3,p5,p8,p0,top=title1,layout_matrix = matrix(c(1,2,3,4,5,6),ncol=3,byrow=TRUE))
    } else if(switch==4){  # SO2, NO2, HCHO, NH3, Alkane, Alkene
      
      p1 <- mp + geom_point(data=datmrgSO2, aes(x=longitude, y=latitude, colour = SO2conc, size = size ) ) + 
        scale_colour_gradientn(colors = c("blue", "cyan", "green", "yellow", "orange", "magenta", "red"))
      
      p2 <- mp + geom_point(data=datmrgNO2, aes(x=longitude, y=latitude, colour = NO2aconc, size = size ) ) + 
        scale_colour_gradientn(colors = c("blue", "cyan", "green", "yellow", "orange", "magenta", "red"))
      
      p3 <- mp + geom_point(data=datmrgHCHO, aes(x=longitude, y=latitude, colour = HCHOaconc, size = size ) ) + 
        scale_colour_gradientn(colors = c("blue", "cyan", "green", "yellow", "orange", "magenta", "red"))
      
      p5 <- mp + geom_point(data=datmrgNH3, aes(x=longitude, y=latitude, colour = NH3conc, size = size ) ) + 
        scale_colour_gradientn(colors = c("blue", "cyan", "green", "yellow", "orange", "magenta", "red"))
      p6 <- mp + geom_point(data=datmrgNH3, aes(x=longitude, y=latitude, colour = log10(NH3conc), size = log10(size) ) ) + 
        scale_colour_gradientn(colors = c("blue", "cyan", "green", "yellow", "orange", "magenta", "red"))
      
      p8 <- mp + geom_point(data=datmrgAlkane, aes(x=longitude, y=latitude, colour = Alkaneconc, size = size ) ) + 
        scale_colour_gradientn(colors = c("blue", "cyan", "green", "yellow", "orange", "magenta", "red"))
      p9 <- mp + geom_point(data=datmrgAlkane, aes(x=longitude, y=latitude, colour = log10(Alkaneconc), size = log10(size) ) ) + 
        scale_colour_gradientn(colors = c("blue", "cyan", "green", "yellow", "orange", "magenta", "red"))
      
      p11 <- mp + geom_point(data=datmrgAlkene, aes(x=longitude, y=latitude, colour = Alkeneconc, size = size ) ) + 
        scale_colour_gradientn(colors = c("blue", "cyan", "green", "yellow", "orange", "magenta", "red"))
      p12 <- mp + geom_point(data=datmrgAlkene, aes(x=longitude, y=latitude, colour = log10(Alkeneconc), size = log10(size) ) ) + 
        scale_colour_gradientn(colors = c("blue", "cyan", "green", "yellow", "orange", "magenta", "red"))
      
      grid.arrange(p1,p2,p3,p8,p11,top=title1,layout_matrix = matrix(c(1,2,3,4,5,6),ncol=3,byrow=TRUE))
    }
  }
  
}
