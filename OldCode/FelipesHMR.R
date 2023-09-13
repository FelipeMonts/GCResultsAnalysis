###############################################################################################################
#
#  Calculation of flux rates based on the paper:
# 
# Pedersen, A. R., S. O. Petersen, and K. Schelde. “A Comprehensive Approach to Soil-Atmosphere Trace-Gas Flux 
# 
# Estimation with Static Chambers.” European Journal of Soil Science 61, no. 6 (2010): 888–902. 
# 
# https://doi.org/10.1111/j.1365-2389.2010.01291.x.
# 
# 
# and the r package : 
#
# Pedersen, Asger R. “HMR: Flux Estimation with Static Chamber Data,” May 20, 2020. https://CRAN.R-project.org/package=HMR.
#
#
#  The code in the package only uses "," or ";" separated values read from a text file as inputs. This version will read
# 
#   the input from data frame and do the calculations all with in R without any external code in C or C++
#
#   which allows to study the fitted model thoroughly.
# 
#
#############################################################################################################


###############################################################################################################
#                            Install the packages that are needed                       
###############################################################################################################

# install.packages("HMR",  dependencies = T)

# install.packages("nlstools",  dependencies = T)

# install.packages("nls2",  dependencies = T)

###############################################################################################################
#                           load the libraries that are needed   
###############################################################################################################


library(HMR)

library(nlstools)

library(nls2)

###############################################################################################################

# readClipboard() 


###############################################################################################################
#                             Setting up working directory  Loading Packages and Setting up working directory                        
###############################################################################################################


#      set the working directory

# readClipboard() 

setwd(paste0("D:\\Felipe\\Current_Projects\\CCC Based Experiments\\" ,
              
              "StrategicTillage_NitrogenLosses_OrganicCoverCrops\\DataAnalysis\\RCode\\GCResultsAnalysis\\"))

getwd()


###############################################################################################################
#                           load the working data from GCANalysis 
###############################################################################################################


load("GCAnalysis2022.RData")

########### Ad chamber dimensions for the calculations

str(Chamber.Dimensions)



V <- Chamber.Dimensions[Chamber.Dimensions$DIMENSION == "Volume" , c("VALUE")]

A <- Chamber.Dimensions[Chamber.Dimensions$DIMENSION == "Surface.Area" , c("VALUE")] 

h = V / A

# Series.1 <- HMR.Test.Data[1:11, c( "Time" , "Concentration")];
# 
# plot(Concentration ~ Time , data = Series.1)

#  Ct = phi + f0(exp(-kt) / -kh ) 


##############################################################################################################
# 
#    Pre-filtering for discarding no fluxes based on the variance of the t0 concentration measurements
# 
###############################################################################################################


########## Max flux data ##########

str(Flux.Data)

######## converting fo from character to numeric

Flux.Data$f0 <- as.numeric(Flux.Data$f0) ;

Flux.Data$LR.f0<- as.numeric(Flux.Data$LR.f0) ;

max(as.numeric(Flux.Data$f0) , na.rm = T)

max(as.numeric(Flux.Data$LR.f0) , na.rm = T)

Flux.Data[which(Flux.Data$f0 == 43.39) , c("Series")]

Flux.Data[which(Flux.Data$LR.f0 == 21.35) , c("Series")]

Max.Flux.Data <- Flux.Data[Flux.Data$Series == "20220615_2_Clover_C" ,]

str(Max.Flux.Data)




# qchisq( p = c(0.25) , df = 15, lower.tail = F) 

sigma02 <- var(Flux.Data[Flux.Data$Sampling.Time == 0, c("CO2.ppm")])

sigma2 <- var(Max.Flux.Data$CO2.ppm) 

n <-dim(Max.Flux.Data)[1]

ChiSqr <- dchisq( x = (dim(Max.Flux.Data)[1]-1 ) * (sigma2 / sigma02), df = dim(Max.Flux.Data)[1]-1 ) 


P.Noflux <- ChiSqr ;

plot(CO2.ppm ~ Sampling.Time ,  data = Max.Flux.Data, col = "blue")

########## No flux data ##########

str(Flux.Data)

Flux.Data[which(Flux.Data$Prefilter == "Noise") , c("Series")]

No.Flux.data <- Flux.Data[which(Flux.Data$Series == "20220609_3_Clover_D") , ]

str(No.Flux.data)


# qchisq( p = c(0.25) , df = 15, lower.tail = F) 

sigma02 <- var(Flux.Data[Flux.Data$Sampling.Time == 0, c("CO2.ppm")])

sigma2 <- var(No.Flux.data$CO2.ppm) 

n <-dim(No.Flux.data)[1]

ChiSqr <- dchisq( x = (dim(Max.Flux.Data)[1]-1 ) * (sigma2 / sigma02), df = dim(Max.Flux.Data)[1]-1 ) 


P.Noflux <- ChiSqr ;

points(CO2.ppm ~ Sampling.Time ,  data = No.Flux.data , col ='red')

##############################################################################################################
# 
#    Calculate the time to saturation based on the highest flux obtained in the HMR results
# 
###############################################################################################################



#### Max flux data 

f0.max <- max(Flux.Data$Flux , na.rm = T) ;

str(Flux.Data)

Flux.Data[Flux.Data$Series == "20220615_2_Clover_C" ,]

plot(CO2.ppm ~ Sampling.Time , data = Flux.Data[Flux.Data$Series == "20220615_2_Clover_C" ,])

Max.Flux.Data <- Flux.Data[Flux.Data$Series == "20220615_2_Clover_C" , c("Series" ,"Sampling.Time" , "CO2.ppm")] ;

names(Max.Flux.Data) <- c("Series" , "Time" , "Concentration")


#### no flux data

no.flux <- Flux.Data[Flux.Data$Method == "No flux" , c("Series")]

Flux.Data[Flux.Data$Series == "20221005_3_Trit_A" ,]

plot(CO2.ppm ~ Sampling.Time , data = Flux.Data[Flux.Data$Series == "20221005_3_Trit_A" ,])

No.Flux.data <- Flux.Data[Flux.Data$Series == "20221005_3_Trit_A" , c("Series" ,"Sampling.Time" , "CO2.ppm") ]

names(No.Flux.data) <- c("Series" , "Time" , "Concentration")


###############################################################################################################
# 
#    Testing the fitting of the k parameter with the 
# 
###############################################################################################################

########### Doing K MSE minimization ######################

# k <- seq(0.01,0.1, by = 0.001) ;

# k <- seq(0.02,0.06, by = 0.001) ;

k <- seq(0.03,0.04, by = 0.0001) 


length(k)
str(Max.Flux.Data)


######## initializing the dataframe to collect the results from k

HMR.k.0 <- data.frame( k.i = double() , MSQE = double() , phi = double() , f0 = double()) ;

str(HMR.k.0)

# i = 1

for (i in seq(1 : length(k) ) ) {
  
 # as.formula(Concentration ~ phi + f0*(exp(-k*Time)/-k*h))
 #  
 #  HMR.nls1 <- nls(formula = Concentration ~ phi + f0*(exp(-i*Time)/-i*h) ,  start = list(phi = 19, f0 = 0.003 ),
 #                  
 #                  data = Series.1 , trace = T);
 #  
  
  Max.Flux.Data$xi <- exp(-k[[i]] * Max.Flux.Data$Time) / (-k[[i]]*h)
  
  
 #  plot(Concentration ~ xi , data = Max.Flux.Data)
  
  HMR.lm.xi <- lm(Concentration ~ xi , data = Max.Flux.Data )
  
  deviance(HMR.lm.xi)
  
  
  HMR.k.1 <- data.frame( k.i = k[[i]], MSQE = deviance(HMR.lm.xi) , phi = coef(HMR.lm.xi)[1] , f0 = coef(HMR.lm.xi)[2] )
  
  HMR.k.2 <- rbind(HMR.k.0 , HMR.k.1)
  
  HMR.k.0 <-  HMR.k.2
  
}

str(HMR.k.0)

plot(MSQE ~ k.i , data = HMR.k.0, log = "x")

plot(MSQE ~ k.i , data = HMR.k.0)

lm(Concentration ~ xi , data = Max.Flux.Data )

# used ki = 0.035 for the calulation of the time t to 50% saturation

# -0.5*10244.04 
# 
# -5122.02 / 60.32
# 
# -84.91412
# 
# 0.035 * 0.1016
# 
# -0.003556 * -84.91412
# 
# 0.3019546
# 
# log(0.3019546) 
# 
# -1.197479 / -0.035
# 
# 34.21369 min 
# 
###############################################################################################################
# 
#    With the max flux data , the optimum ki = 0.035 and the 50% sat time is 34 min
# 
###############################################################################################################

p = 0.5 

T = 30

k.top <- 1/T * (log(1/(1-p)))



######### Using nls to estimate the parameters of the HMR model ######


Model.Equation <- as.formula(Concentration ~ phi + f0*(exp(-k*Time)/-k*h)) ;

preview(formula = Model.Equation , 
        
        data = data.matrix(frame = as.matrix(Max.Flux.Data[, c("Time" , "Concentration")] , rownames.force = F)), 
        
        start = list(phi = 10000, k = 0.03 , f0 = 4000), variable = 1 )

str(Max.Flux.Data)

HMR.nls1 <- nls(formula = Model.Equation ,  start = list(phi = 5000, k = 0.03, f0 = 60),
                
                data = Max.Flux.Data[, c("Time" , "Concentration")] , trace = T) ;

plotfit(HMR.nls1)

overview(HMR.nls1)

plotfit(HMR.nls1 , smooth = T)

HMR.res1 <- nlsResiduals(HMR.nls1) ;

plot(HMR.res1)


HMR.cont1 <- nlsContourRSS(HMR.nls1)

plot(HMR.cont1)

HMR.conf1 <- nlsConfRegions(HMR.nls1, exp = 2, length = 2000)

plot(HMR.conf1, bounds = TRUE)

HMR.jack1 <-nlsJack(HMR.nls1)

plot(HMR.jack1)


HMR.boot1 <- nlsBoot(HMR.nls1, niter = 200)

plot(HMR.boot1, type = "boxplot")

plotfit(HMR.nls1)

pred.HMR <- nlsBootPredict( nlsBoot = HMR.boot1 , interval = c("confidence") )

lines(HMR.Test.Data[1:11, c("Time")] , pred.HMR[,2] , col = "red" , lty = 2) 

lines(HMR.Test.Data[1:11, c("Time")] , pred.HMR[,3] , col = "red" , lty = 2) 



