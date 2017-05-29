# Wavelength matching and convoluted cross section of NO2 on May 08.
# 2017-01-05, 1st built, Wang Weihua 

library(ggplot2)
dirpath = "/home/wien/Octave/flameDOAS/CrossSections/"

options(digits=10)                      # extend default digits in numeric value
filename7 = "NO2_Voigt_293K_vac"
filepath = paste(dirpath, filename7, ".xs", sep = "")

dat = read.table(filepath, header = FALSE, sep = "")
names(dat) <- c("Wavelength", "CrossSection")

qplot(data = dat, x = Wavelength, y = CrossSection) + ggtitle("Reference CrossSection of NO2 at 293K")


chwl = read.table('chwlmp0508.dat', header = FALSE, sep = "")
names(chwl) <- c("Wavelength")

wl_a = dat$Wavelength[1]
wl_x = dat$Wavelength[dim(dat)[1]]
wl_a;wl_x
# [1] 800.009
# [1] 250

# shrink the wavelength scope for matching 
subchwl <- chwl[ (chwl$Wavelength >= wl_x-0.5) & (chwl$Wavelength <= wl_a+0.5) , ]
subchwl[1:5];subchwl[(length(subchwl)-5):length(subchwl)]
chwlsub <- subset(chwl, (chwl$Wavelength >= wl_x-0.5) & (chwl$Wavelength <= wl_a+0.5))

# mg1 <- merge.data.frame(chwl, dat, by.x = "Wavelength", by.y = "Wavelength",  all.x = TRUE )
# mg2 <- merge.data.frame(dat, chwl, by.x = "Wavelength", by.y = "Wavelength",  all.x = TRUE )
# above two lines of code give different results.
# So I use data.table package to solve this problem
library(data.table)
dat1 <- data.table(dat) # convert from data.frame to data.table
chwlsub1 <- data.table(chwlsub)

# matching by 'nearest' observation
setkey(dat1, Wavelength, CrossSection)
datmatch <- dat1[chwlsub1, roll="nearest"]
class(datmatch)
qplot(data = datmatch, x = Wavelength, y = CrossSection) + ggtitle("Sampled NO2 CrossSection at 293K")

# # If apply data.table to un-subset data frame 
# chwl_table <- data.table(chwl)
# setkey(dat1, Wavelength, CrossSection)
# datmatch22 <- dat1[chwl_table, roll="nearest"]
# qplot(data = datmatch22, x = Wavelength, y = CrossSection) + ggtitle("Sampled NO2 CrossSection at 293K")
# # from the result, we should subset the chwl first. 

# lineshape function convolute with sampled cross section of NO2
linefuc = read.table('lineshapeFuc0508.dat', header = FALSE, sep = "")
names(linefuc) <- c("distTocenter","intensity")
class(linefuc)
datconv <- convolve(datmatch$CrossSection, linefuc$intensity,type="o") # see convolve function
idxvec <- 1:length(datconv)
df.cov <- data.frame(idxvec, datconv)
names(df.cov) <- c("Index","ConvCrossSection")
qplot(data = df.cov, x = Index, y = ConvCrossSection) + ggtitle("Convoluted CrossSection of NO2 at 293K")

# 2017-01-04 wavelength matching and convoluted cross section of SO2 on May 08.
library(ggplot2)
dirpath = "/home/wien/Octave/flameDOAS/CrossSections/"

options(digits=10)                      # extend default digits in numeric value
filename = "SO2_Bogumil_293K_vac_SCIAMACHY"
filepath = paste(dirpath, filename, ".xs", sep = "")

dat = read.table(filepath, header = FALSE, sep = "")
names(dat) <- c("Wavelength", "CrossSection")

qplot(data = dat, x = Wavelength, y = CrossSection) + ggtitle("Reference CrossSection of SO2 at 293K")


chwl = read.table('chwlmp0508.dat', header = FALSE, sep = "")
names(chwl) <- c("Wavelength")

wl_a = dat$Wavelength[1]
wl_x = dat$Wavelength[dim(dat)[1]]
wl_a;wl_x

# shrink the wavelength scope for matching 
subchwl <- chwl[ (chwl$Wavelength >= wl_a-0.5) & (chwl$Wavelength <= wl_x+0.5) , ]
subchwl[1:5];subchwl[(length(subchwl)-5):length(subchwl)]
chwlsub <- subset(chwl, (chwl$Wavelength >= wl_a-0.5) & (chwl$Wavelength <= wl_x+0.5))

# mg1 <- merge.data.frame(chwl, dat, by.x = "Wavelength", by.y = "Wavelength",  all.x = TRUE )
# mg2 <- merge.data.frame(dat, chwl, by.x = "Wavelength", by.y = "Wavelength",  all.x = TRUE )
# above two lines of code give different results.
# So I use data.table package to solve this problem
library(data.table)
dat1 <- data.table(dat) # convert from data.frame to data.table
chwlsub1 <- data.table(chwlsub)

# matching by 'nearest' observation
setkey(dat1, Wavelength, CrossSection)
datmatch <- dat1[chwlsub1, roll="nearest"]
class(datmatch)
qplot(data = datmatch, x = Wavelength, y = CrossSection) + ggtitle("Sampled SO2 CrossSection at 293K")

# If apply data.table to un-subset data frame 
chwl_table <- data.table(chwl)
setkey(dat1, Wavelength, CrossSection)
datmatch22 <- dat1[chwl_table, roll="nearest"]
qplot(data = datmatch22, x = Wavelength, y = CrossSection) + ggtitle("Sampled SO2 CrossSection at 293K")
# from the result, we should subset the chwl first. 

# lineshape function convolute with sampled cross section of SO2
linefuc = read.table('lineshapeFuc0508.dat', header = FALSE, sep = "")
names(linefuc) <- c("distTocenter","intensity")
class(linefuc)
datconv <- convolve(datmatch$CrossSection, linefuc$intensity,type="o") # see convolve function
idxvec <- 1:length(datconv)
df.cov <- data.frame(idxvec, datconv)
names(df.cov) <- c("Index","ConvCrossSection")
qplot(data = df.cov, x = Index, y = ConvCrossSection) + ggtitle("Convoluted CrossSection of SO2 at 293K")
#

library(ggplot2)
dirpath = "/home/wien/Octave/flameDOAS/CrossSections/"

options(digits=10)                      # extend default digits in numeric value
filename = "SO2_Bogumil_293K_vac_SCIAMACHY"
filepath = paste(dirpath, filename, ".xs", sep = "")

dat = read.table(filepath, header = FALSE, sep = "")
names(dat) <- c("Wavelength", "CrossSection")

qplot(data = dat, x = Wavelength, y = CrossSection) + ggtitle("SO2 crossSection at 293K")

  #geom_jitter() + 
  #stat_smooth(method="lm")
##
filename2 = "BrO_Wil298_Air"
filepath = paste(dirpath, filename2, ".XS", sep = "")

dat2 = read.table(filepath, header = FALSE, sep = "")
names(dat2) <- c("Wavelength", "CrossSection")

qplot(data = dat2, x = Wavelength, y = CrossSection) + ggtitle("BrO crossSection at 298K")

##
filename3 = "ClO_Burrows_270K"
filepath = paste(dirpath, filename3, ".xs", sep = "")

dat3 = read.table(filepath, header = FALSE, sep = "")
names(dat3) <- c("Wavelength", "CrossSection")

qplot(data = dat3, x = Wavelength, y = CrossSection) + ggtitle("CLO crossSection at 270K")

##
filename4 = "CS2_Schneider_294K"
filepath = paste(dirpath, filename4, ".xs", sep = "")

dat4 = read.table(filepath, header = FALSE, sep = "")
names(dat4) <- c("Wavelength", "CrossSection")

qplot(data = dat4, x = Wavelength, y = CrossSection) + ggtitle("CS2 crossSection at 294K")

##
filename5 = "glyoxal_296K_air"
filepath = paste(dirpath, filename5, ".xs", sep = "")

dat5 = read.table(filepath, header = FALSE, sep = "")
names(dat5) <- c("Wavelength", "CrossSection")

qplot(data = dat5, x = Wavelength, y = CrossSection) + ggtitle("Glyoxal crossSection at 296K")

##
filename6 = "H2CO_290K"
filepath = paste(dirpath, filename6, ".xs", sep = "")

dat6 = read.table(filepath, header = FALSE, sep = "")
names(dat6) <- c("Wavelength", "CrossSection")

qplot(data = dat6, x = Wavelength, y = CrossSection) + ggtitle("H2CO crossSection at 296K")

##
filename7 = "NO2_Voigt_293K_vac"
filepath = paste(dirpath, filename7, ".xs", sep = "")

dat7 = read.table(filepath, header = FALSE, sep = "")
names(dat7) <- c("Wavelength", "CrossSection")

qplot(data = dat7, x = Wavelength, y = CrossSection) + ggtitle("NO2 crossSection at 293K")

##
filename8 = "O3_Burrows_GOME_293K_air"
filepath = paste(dirpath, filename8, ".xs", sep = "")

dat8 = read.table(filepath, header = FALSE, sep = "")
names(dat8) <- c("Wavelength", "CrossSection")

qplot(data = dat8, x = Wavelength, y = CrossSection) + ggtitle("O3 crossSection at 293K")

##
filename9 = "OBrO_Burrows_10cm-1_298K_vac"
filepath = paste(dirpath, filename9, ".xs", sep = "")

dat9 = read.table(filepath, header = FALSE, sep = "")
names(dat9) <- c("Wavelength", "CrossSection")

qplot(data = dat9, x = Wavelength, y = CrossSection) + ggtitle("OBrO crossSection at 298K")

##
filename10 = "OClO_Kromminga_1cm-1_293K_vac"
filepath = paste(dirpath, filename10, ".xs", sep = "")

dat10 = read.table(filepath, header = FALSE, sep = "")
names(dat10) <- c("Wavelength", "CrossSection")

qplot(data = dat10, x = Wavelength, y = CrossSection) + ggtitle("OClO crossSection at 293K")

##
filename11 = "Water_EvenInterpolation"
filepath = paste(dirpath, filename11, ".xs", sep = "")

dat11 = read.table(filepath, header = FALSE, sep = "")
names(dat11) <- c("Wavelength", "CrossSection")

qplot(data = dat11, x = Wavelength, y = CrossSection) + ggtitle("Water crossSection EvenInterpolation")

