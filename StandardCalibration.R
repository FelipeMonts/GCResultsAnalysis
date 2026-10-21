##############################################################################################################
# 
# 
# Program to form a calibration curve from all the GC standards collected from tha analysis of the 2021 and 2022
# 
# sampling seasons and analyzed using from Professor Lauren McPhillips Agilent 8890 Gas Chromatograph
# 
#    
#  Felipe Montes 2026/18/26
# 
# 
# 
# 
############################################################################################################### 


###############################################################################################################
#                            Install the packages that are needed                       
###############################################################################################################

# install.packages("openxlsx",  dependencies = T)

# install.packages("Rtools",  dependencies = T)

# install.packages("pdftools",  dependencies = T)

# install.packages("askpass",  dependencies = T)

# install.packages("cli",  dependencies = T)

# install.packages("utf8",  dependencies = T)

# install.packages("quantreg",  dependencies = T)

# install.packages("HMR",  dependencies = T)

# install.packages("Viridis",  dependencies = T)

# install.packages("paletteer", dependencies = T)

# install.packages("MASS", dependencies = T) 

###############################################################################################################
#                           load the libraries that are needed   
###############################################################################################################

library(openxlsx)

library(lattice)

library(quantreg)

library(paletteer)

library(MASS) 

###############################################################################################################
#                             Setting up working directory  Loading Packages and Setting up working directory                        
###############################################################################################################


#      set the working directory

# readClipboard() 

Working.Directory.File.Path <- file.path("C:\\Users\\frm10\\OneDrive - The Pennsylvania State University\\",

"Current_Projects\\CCC Based Experiments\\StrategicTillage_NitrogenLosses_OrganicCoverCrops\\DataAnalysis\\",

"RCode\\GCResultsAnalysis\\FluxDataAnalysisResults");

setwd(Working.Directory.File.Path)

###############################################################################################################
#               Read curated GC Standards data from 2021 and 20222                        
###############################################################################################################


C.GC.Standards.2021 <- read.csv(file = "StandardsDataset2021.csv" , header = T);

str(C.GC.Standards.2021)

head(C.GC.Standards.2021, 20)

tail(C.GC.Standards.2021, 20)


C.GC.Standards.2022 <- read.csv(file = "StandardsDataset2022.csv" , header = T);

str(C.GC.Standards.2022)

head(C.GC.Standards.2022, 20)

tail(C.GC.Standards.2022, 20)

names(C.GC.Standards.2021)

names(C.GC.Standards.2022)

######### C.GC.Standards.2022 have two column names that do not match C.GC.Standards.2021 #####

######### "X" and "Factor.Name" ########

names(C.GC.Standards.2022)

str(C.GC.Standards.2022$Factor.Name)

######## Change column name from "X" to "Rownames"#######

colnames(C.GC.Standards.2022) [colnames(C.GC.Standards.2022) == "X" ] <- "Rownames" ;

######## Remove the "Factor.Name" column #########

C.GC.Standards.2022 <- subset(C.GC.Standards.2022, select = - Factor.Name)

colnames(C.GC.Standards.2022)

###### Add a factor name that identifies the year of sampling ########

C.GC.Standards.2021$YEAR <- 2021 ;

C.GC.Standards.2022$YEAR <- 2022 ;




###############################################################################################################
#                           
#        Merge the two standards data set to get a universal calibration line
#
###############################################################################################################


C.GC.Standards.All <- rbind(C.GC.Standards.2021,C.GC.Standards.2022 ) ;

str(C.GC.Standards.All)

C.GC.Standards.All$YEAR <- as.factor(C.GC.Standards.All$YEAR) ;

###### Plot Standards  #####

xyplot(CO2 ~ CO2.ppm,
       
       data = C.GC.Standards.All, 
       
       main = "CO2",
       
       groups = YEAR,
       
       pch = c(16, 1),
       
       cex = c(1, 1.5),
       
       col = c("red" , "blue"),
       
       key = list(
         
         space = "top",
         
         columns = 2,
         
         text = list(c("2021" , "2022")),
         
         points = list(pch = c(16, 1),
                       
                       col = c("red" , "blue")
                       
         )
       ))


xyplot(N2O ~ N2O.ppm,
       
       data = C.GC.Standards.All, 
       
       main = "N2O",
       
       groups = YEAR,
       
       pch = c(16, 1),
       
       cex = c(1, 1.5),
       
       col = c("red" , "blue"),
       
       key = list(
         
         space = "top",
         
         columns = 2,
         
         text = list(c("2021" , "2022")),
         
         points = list(pch = c(16, 1),
                       
                       col = c("red" , "blue")
                       
         )
       ))


xyplot(CH4 ~ CH4.ppm,
       
       data = C.GC.Standards.All, 
       
       main = "CH4",
       
       groups = YEAR,
       
       pch = c(16, 1),
       
       cex = c(1, 1.5),
       
       col = c("red" , "blue"),
       
       key = list(
         
         space = "top",
         
         columns = 2,
         
         text = list(c("2021" , "2022")),
         
         points = list(pch = c(16, 1),
                       
                       col = c("red" , "blue")
                       
         )
       ))



###############################################################################################################
#                           
#        Do a regression with Ordinary least squares OLS 
#
###############################################################################################################



##############################################  CO2  ########################################################

OLS.regression.CO2 <- lm(CO2 ~ CO2.ppm, 
                     
                     data = C.GC.Standards.All) ;

summary(OLS.regression.CO2)

#plot(OLS.regression.CO2)

str(OLS.regression.CO2)

plot(OLS.regression.CO2$fitted.values, OLS.regression.CO2$residuals)

plot(C.GC.Standards.All$CO2.ppm , OLS.regression.CO2$residuals ) 

##### it seems that the residuals grow as the level of CO2 increases. Heteroscedasticity.  Try log transformation

C.GC.Standards.All$log.CO2 <- log(C.GC.Standards.All$CO2) ; 

C.GC.Standards.All$log.CO2.ppm <- log(C.GC.Standards.All$CO2.ppm) ; 


#### CO2 ~ log.CO2.ppm  ####


xyplot(log.CO2 ~ CO2.ppm,
       
       data = C.GC.Standards.All, 
       
       main = "CO2",
       
       groups = YEAR,
       
       pch = c(16, 1),
       
       cex = c(1, 1.5),
       
       col = c("red" , "blue"),
       
       key = list(
         
         space = "top",
         
         columns = 2,
         
         text = list(c("2021" , "2022")),
         
         points = list(pch = c(16, 1),
                       
                       col = c("red" , "blue")
                       
         )
       ))

OLS.regression.Log <- lm(log.CO2 ~ CO2.ppm , 
                     
                     data = C.GC.Standards.All) ;

summary(OLS.regression.Log)

#plot(OLS.regression.Log)



#### Not Good fit, residuals are all over the place ###

##### the residuals still grow as the level of CO2 increases. Heteroscedasticity.  Try log log transformation

str(C.GC.Standards.All)

OLS.regression.Log.log <- lm(log.CO2 ~ log.CO2.ppm , 
                             
                             data = C.GC.Standards.All) ;

# Error in lm.fit(x, y, offset = offset, singular.ok = singular.ok, ...) : 
#   NA/NaN/Inf in 'x'

# There are zeros in the standards , CO2.ppm therefore log transform does not work for the CO2.ppm 

# and option is to try log(CO2.ppm +1) to bypass the zeroes. 

#Another option is the inverse hyperbolic sine function IHS asinh(), but it has also disadvantages, 
# especially in the back transformation


C.GC.Standards.All$CO2.ppm.plus.1 <- C.GC.Standards.All$CO2.ppm +1 ;

C.GC.Standards.All$log.CO2.ppm.plus.1 <- log(C.GC.Standards.All$CO2.ppm.plus.1) ;

OLS.regression.Log.plus.1 <- lm(log.CO2 ~ log.CO2.ppm.plus.1 , 
                         
                         data = C.GC.Standards.All) ;


summary(OLS.regression.Log.plus.1)

# plot(OLS.regression.Log.plus.1)

##### Not very good fit either, the the residuals are all over the place and the fit is worst.



# ###############################################################################################################
# 
# 
#    Using the Box-Cox transformation analysis to find the data transformation that best reduces Heteroscedasticity 
#  
#     Draper and Smith, 1998. Applied regression Analysis, Page 280. Using the R package MASS from the Book
#
#     Venables and Ripley, "Modern Applied Statistics with S" (4th edition, 2002), page 170
#
#    https://cran.r-project.org/web/packages/MASS/index.html  
#
#    https://cran.r-project.org/web/packages/MASS/refman/MASS.html
#
#    https://cran.r-project.org/web/packages/MASS/MASS.pdf
# 
# 
# ################################################################################################################


anyNA(C.GC.Standards.All$CO2)

anyNA(C.GC.Standards.All$CO2.ppm)





#### ratio of the largest to smallest response data  ####

max(C.GC.Standards.All$CO2)/min(C.GC.Standards.All$CO2)



### Transformation is granted and recommended!!!! #####

boxcox(OLS.regression.CO2)

boxcox(CO2 ~ CO2.ppm , data = C.GC.Standards.All , lambda = seq(from = -2, to = 2 , by = 0.25),
       
       plotit = T )

boxcox(OLS.regression.CO2 , data = C.GC.Standards.All , lambda = seq(from = -2, to = 2 , by = 0.25),
       
       plotit = T )


boxcox(OLS.regression.CO2 , data = C.GC.Standards.All , lambda = seq(from = 0, to = 1 , by = 0.01),
       
       plotit = T )

boxcox(OLS.regression.CO2 , data = C.GC.Standards.All , lambda = seq(from = 0.3, to = 0.6 , by = 0.01),
       
       plotit = T )


OLS.regression.CO2.BoxCox <- boxcox(OLS.regression.CO2 , lambda = seq(from = 0.3, to = 0.6 , by = 0.01),
       
       plotit = T) ;

str(OLS.regression.CO2.BoxCox)

max(OLS.regression.CO2.BoxCox$y)

which(OLS.regression.CO2.BoxCox$y == max(OLS.regression.CO2.BoxCox$y))

OLS.regression.CO2.BoxCox$x[59]

##### The best lambda is OLS.regression.CO2.BoxCox$x[59] = 0.4757576 , which is very close to 0.5  ######

##### Considering the transformation Yt = Log(Y+alpha])  the function logtrans of the pakage MASS helps determine the 
##### optimal alpha. Venables and Ripley, "Modern Applied Statistics with S" (4th edition, 2002), page 170

logtrans(OLS.regression.CO2)

logtrans(OLS.regression.CO2, alpha = seq(from = 0, to = 10, by = 1 ) )

logtrans(OLS.regression.CO2, alpha = seq(from = 0, to = 100, by = 10) )

logtrans(OLS.regression.CO2, alpha = seq(from = 0, to = 1000, by = 100) )

##### The likelihood function explodes as alpha increases... best not to use that transformation ######

#### The optimal transformation according to the Box-Cox transformation analysis is sqrt(y) = a + X1...

C.GC.Standards.All$sqrt_CO2 <- sqrt(C.GC.Standards.All$CO2) ;

str(C.GC.Standards.All)


OLS.regression.sqrt_CO2 <- lm(sqrt_CO2 ~ CO2.ppm, 
                              
                              data = C.GC.Standards.All) ;

summary(OLS.regression.sqrt_CO2)  
  
# plot(OLS.regression.sqrt_CO2) 

str(OLS.regression.sqrt_CO2)

#### The best model for calibration of the CO2 GC data is 

summary(OLS.regression.sqrt_CO2) 

# Call:
#   lm(formula = sqrt_CO2 ~ CO2.ppm, data = C.GC.Standards.All)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -39.204  -5.497   0.577   5.345  36.231 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 3.556e+01  5.495e-01   64.72   <2e-16 ***
#   CO2.ppm     2.012e-02  2.395e-04   84.02   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 10.93 on 645 degrees of freedom
# Multiple R-squared:  0.9163,	Adjusted R-squared:  0.9162 
# F-statistic:  7060 on 1 and 645 DF,  p-value: < 2.2e-16
# 

# sqrt_CO2 = 3.556e+01 + 2.012e-02 * CO2.ppm
# 
# sqrt_CO2 = 35.56 + 0.0201 * CO2.ppm 
# 
# The calibration curve is then 
# 
# (sqrt_CO2 - 35.56) / 0.0201 = CO2.ppm 
#
#  CO2.ppm = (sqrt_CO2 - 35.56) / 0.0201 



plot(CO2 ~ CO2.ppm,
         
         data = C.GC.Standards.All, 
         
         main = "CO2",
         
         pch = 16,
         
         cex = 1,
         
         col = "red")

points(predict(OLS.regression.CO2)  ~  C.GC.Standards.All$CO2.ppm  , pch = 17 , cex = 1.5 , col = "green")

points((predict(OLS.regression.sqrt_CO2))^2  ~  C.GC.Standards.All$CO2.ppm  , pch = 16 , cex = 1.5 , col = "blue")

points(exp(predict(OLS.regression.Log)) ~ C.GC.Standards.All$CO2.ppm  , pch = 17 , cex = 1.5 , col = "cyan")

       
legend("topleft", legend = c("CO2.ppm", "OLS-CO2", "OLS-Sqrt(CO2)"), col = c("red" , "blue", "green"), pch = c(16, 17,16))


###############################################################################################################
#                           
#        Use quantile regression for the standards
#
###############################################################################################################


############## quantile regression  CO2 ###########

Quantile.Reg <- rq( CO2 ~ CO2.ppm, data = C.GC.Standards.All, tau = seq(from = 0.1, to = 0.9 , by = 0.1) ) ;

str(Quantile.Reg )

plot(Quantile.Reg)

summary(Quantile.Reg)

str(dimnames(coefficients(Quantile.Reg)))

coefficients(Quantile.Reg)


xyplot(CO2 ~ CO2.ppm ,
       
       data = C.GC.Standards.All, 
       
       main = "CO2",
       
       groups = YEAR,
       
       pch = c(16, 1),
       
       cex = c(1, 1.5),
       
       col = c("red" , "blue"),
       
       key = list(
         
         space = "top",
         
         columns = 2,
         
         text = list(c("2021" , "2022")),
         
         points = list(pch = c(16, 1),
                       
                       col = c("red" , "blue"))),
       
       panel = function(x,y,...) {
         
         panel.xyplot(x,y, ...)
         
         panel.xyplot(C.GC.Standards.All$CO2.ppm , C.GC.Standards.All$CO2, col = "black", lwd =2)
         
         panel.abline(a = coefficients(Quantile.Reg)[1,1] , b = coefficients(Quantile.Reg)[2,1],
                      
                      col = "gray" , lwd = 1.5)
         
         panel.abline(a = coefficients(Quantile.Reg)[1,2] , b = coefficients(Quantile.Reg)[2,2],
                      
                      col = "gray" , lwd = 1.5)
        
         panel.abline(a = coefficients(Quantile.Reg)[1,3] , b = coefficients(Quantile.Reg)[2,3],
                      
                      col = "gray" , lwd = 1.5)
         
         panel.abline(a = coefficients(Quantile.Reg)[1,4] , b = coefficients(Quantile.Reg)[2,4],
                      
                      col = "gray" , lwd = 1.5)
         
         panel.abline(a = coefficients(Quantile.Reg)[1,5] , b = coefficients(Quantile.Reg)[2,5],
                      
                      col = "gray" , lwd = 1.5)
         
         panel.abline(a = coefficients(Quantile.Reg)[1,6] , b = coefficients(Quantile.Reg)[2,6],
                      
                      col = "gray" , lwd = 1.5)
         
         panel.abline(a = coefficients(Quantile.Reg)[1,7] , b = coefficients(Quantile.Reg)[2,7],
                      
                      col = "gray" , lwd = 1.5)
         
         panel.abline(a = coefficients(Quantile.Reg)[1,8] , b = coefficients(Quantile.Reg)[2,8],
                      
                      col = "gray" , lwd = 1.5)
         
         panel.abline(a = coefficients(Quantile.Reg)[1,9] , b = coefficients(Quantile.Reg)[2,9],
                      
                      col = "gray" , lwd = 1.5)
         
         panel.text(x = 1000,  y = 10000, 
                    
                    labels = paste0("OLS: ",round(OLS.regression.CO2$coefficients[1],3), "+" ,
                                   
                                   round(OLS.regression.CO2$coefficients[2],3), "CO2"),
                    
                    col = "black" ,
                    
                    cex = 2)
         
         panel.text(x = 1000,  y = 20000, 
                    
                    labels = paste0("QUANT: " ,round(Quantile.Reg$coefficients[1,5],3), "+" ,
                                   
                                   round(Quantile.Reg$coefficients[2,5],3), "CO2"),
                    
                    col = "black" ,
                    
                    cex = 2)
         
       }
       
)




Quantile.Reg.05 <- rq( CO2 ~ CO2.ppm, data = C.GC.Standards.All, tau = 0.5 )

summary(Quantile.Reg.05)



plot(CO2 ~ CO2.ppm,
     
     data = C.GC.Standards.All, 
     
     main = "CO2",
     
     pch = 16,
     
     cex = 1,
     
     col = "red")

points(predict(OLS.regression.CO2)  ~  C.GC.Standards.All$CO2.ppm  , pch = 17 , cex = 1.5 , col = "green")

abline(a = OLS.regression.CO2$coefficients[1], b = OLS.regression.CO2$coefficients[2], col = "green")

points((predict(OLS.regression.sqrt_CO2))^2  ~  C.GC.Standards.All$CO2.ppm  , pch = 16 , cex = 1.5 , col = "blue")


points(exp(predict(OLS.regression.Log)) ~ C.GC.Standards.All$CO2.ppm  , pch = 17 , cex = 1.5 , col = "cyan")

points(predict(Quantile.Reg.05) ~ C.GC.Standards.All$CO2.ppm  , pch = 18 , cex = 1.5 , col = "magenta") 

abline(a = Quantile.Reg.05$coefficients[1], b = Quantile.Reg.05$coefficients[2], col = "magenta")


legend("topleft", legend = c("CO2.ppm", "OLS-CO2", "OLS-Sqrt(CO2)", "Quant 50"), 
       
       col = c("red" , "blue", "green", "magenta" ), pch = c(16, 17, 16 , 18))



###############################################################################################################
#                           
#       The best regression analysis for the GC calibration is the Quantile regression at 0.5
#     
#       But the errors grow with the values of the standards. Heteroscedasticity.
#
#       It is very similar to the regular OLS regression, but is less affected  by extreme values.
#
#       Quantile regression is a more robust and conservative option
#
###############################################################################################################




# ###############################################################################################################
# 
#                      Best calibration equation for CO2
# 
# ###############################################################################################################
# 
# 
# Call: rq(formula = CO2 ~ CO2.ppm, tau = 0.5, data = C.GC.Standards.All)
# 
# tau: [1] 0.5
# 
# Coefficients:
#   coefficients lower bd  upper bd 
# (Intercept) 797.91013    740.63252 845.75477
# CO2.ppm       3.30048      3.22693   3.47683
# 
# 
# ###############################################################################################################


##############################################  N2O  ########################################################


anyNA(C.GC.Standards.All$N2O)

anyNA(C.GC.Standards.All$N2O.ppm)


##### There are NA records in N2O data   ####

which(is.na(C.GC.Standards.All$N2O))

C.GC.Standards.All[which(is.na(C.GC.Standards.All$N2O)),]

##### All records with NA are from 2022   #####

##### Do the calibration without NA  records ###


C.GC.Standards.no.NA <- C.GC.Standards.All[which(!is.na(C.GC.Standards.All$N2O)),]  ;

which(is.na(C.GC.Standards.no.NA$N2O))


OLS.regression.N2O <- lm(N2O ~ N2O.ppm, 
                         
                         data = C.GC.Standards.no.NA) ;

summary(OLS.regression.N2O)

# plot(OLS.regression.N2O)


plot(OLS.regression.N2O$fitted.values, OLS.regression.N2O$residuals)

plot(C.GC.Standards.no.NA$N2O.ppm , OLS.regression.N2O$residuals ) 


##### Residuals grow as the level of CO2 increases. Heteroscedasticity . Do Box-Cox method like CO2 


# ###############################################################################################################
# 
# 
#    Using the Box-Cox transformation analysis to find the data transformation that best reduces Heteroscedasticity 
#  
#     Draper and Smith, 1998. Applied regression Analysis, Page 280. Using the R package MASS from the Book
#
#     Venables and Ripley, "Modern Applied Statistics with S" (4th edition, 2002), page 170
#
#    https://cran.r-project.org/web/packages/MASS/index.html  
#
#    https://cran.r-project.org/web/packages/MASS/refman/MASS.html
#
#    https://cran.r-project.org/web/packages/MASS/MASS.pdf
# 
# 
# ################################################################################################################



 
#### ratio of the largest to smallest response data  ####

max(C.GC.Standards.no.NA$N2O)/min(C.GC.Standards.no.NA$N2O)
  
  
boxcox(OLS.regression.N2O)

boxcox(N2O ~ N2O.ppm , data = C.GC.Standards.no.NA , lambda = seq(from = 0, to = 1 , by = 0.01),
       
       plotit = T )
 

OLS.regression.N2O.BoxCox <-boxcox(N2O ~ N2O.ppm , data = C.GC.Standards.no.NA , lambda = seq(from = 0.2, to = 0.4 , by = 0.01),
       
       plotit = T )

str(OLS.regression.N2O.BoxCox)

max(OLS.regression.N2O.BoxCox$y)

which(OLS.regression.N2O.BoxCox$y == max(OLS.regression.N2O.BoxCox$y))

OLS.regression.N2O.BoxCox$x[53]

##### The best lambda is OLS.regression.N2O.BoxCox$x[53] = 0.3050505 , which is very close to 0.3  ######

#### The optimal transformation according to the Box-Cox transformation analysis is y^0.3 = a + X1...

C.GC.Standards.no.NA$N2O.pwr.0_3 <- (C.GC.Standards.no.NA$N2O)^0.3 ;

str(C.GC.Standards.no.NA)


OLS.regression.N2O.pwr.0_3 <- lm(C.GC.Standards.no.NA$N2O.pwr.0_3 ~ N2O.ppm, 
                              
                              data = C.GC.Standards.no.NA) ;

summary(OLS.regression.N2O.pwr.0_3)  

# plot(OLS.regression.N2O.pwr.0_3) 



#### The best model for N2O GC data is 

summary(OLS.regression.N2O.pwr.0_3)

# Call:
#   lm(formula = C.GC.Standards.no.NA$N2O.pwr.0_3 ~ N2O.ppm, data = C.GC.Standards.no.NA)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -5.0528 -1.3603 -0.1184  0.9312  6.4156 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 6.203021   0.095474   64.97   <2e-16 ***
#   N2O.ppm     0.351243   0.004174   84.15   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 1.991 on 641 degrees of freedom
# Multiple R-squared:  0.917,	Adjusted R-squared:  0.9169 
# F-statistic:  7082 on 1 and 641 DF,  p-value: < 2.2e-16



plot(N2O ~ N2O.ppm,
     
     data = C.GC.Standards.no.NA, 
     
     main = "N2O",
     
     pch = 16,
     
     cex = 1,
     
     col = "red")

points(predict(OLS.regression.N2O) ~ C.GC.Standards.no.NA$N2O.ppm, pch = 16, cex = 1.5 , col = "blue")

abline( a = OLS.regression.N2O$coefficients[1] , b = OLS.regression.N2O$coefficients[2] , col = "blue")

points((predict(OLS.regression.N2O.pwr.0_3))^(10/3) ~ C.GC.Standards.no.NA$N2O.ppm, pch = 16, cex = 1.5 , col = "green")

legend("topleft", legend = c("N2O.ppm", "OLS" , "OLS 0.3 power"), col = c("red" , "blue" , "green"), pch = c(16, 16 , 16))



################## TESTING SQUARE ROOT FOR N2O #####################

C.GC.Standards.no.NA$N2O.sqrt <- sqrt(C.GC.Standards.no.NA$N2O) ;


OLS.regression.sqrt_N2O <- lm(C.GC.Standards.no.NA$N2O.sqrt ~ N2O.ppm, 
                                 
                                 data = C.GC.Standards.no.NA) ;

summary(OLS.regression.sqrt_N2O)

# plot(OLS.regression.sqrt_N2O)

plot(OLS.regression.sqrt_N2O$fitted.values, OLS.regression.sqrt_N2O$residuals)

plot(C.GC.Standards.no.NA$N2O.ppm , OLS.regression.sqrt_N2O$residuals ) 

plot(C.GC.Standards.no.NA$N2O.ppm , OLS.regression.sqrt_N2O$residuals ) 


plot(C.GC.Standards.no.NA$N2O.ppm , OLS.regression.sqrt_N2O$fitted.values ) 


plot(N2O ~ N2O.ppm,
     
     data = C.GC.Standards.no.NA, 
     
     main = "N2O",
     
     pch = 16,
     
     cex = 1,
     
     col = "red")

points(predict(OLS.regression.N2O) ~ C.GC.Standards.no.NA$N2O.ppm, pch = 16, cex = 1.5 , col = "blue")

points(predict(OLS.regression.sqrt_N2O)^2 ~ C.GC.Standards.no.NA$N2O.ppm, pch = 16, cex = 1.5 , col = "green")


legend("topleft", legend = c("N2O.ppm", "OLS" , "SQRT OLS"), col = c("red" , "blue" , "green"), pch = c(16, 16 , 16))



############## quantile regression  N2O ###########

Quantile.Reg.N2O <- rq( N2O ~ N2O.ppm, data = C.GC.Standards.no.NA, tau = seq(from = 0.1, to = 0.9 , by = 0.1) ) ;

str(Quantile.Reg.N2O )

plot(Quantile.Reg.N2O)

summary(Quantile.Reg.N2O)

coefficients(Quantile.Reg.N2O)


xyplot(N2O ~ N2O.ppm ,
       
       data = C.GC.Standards.no.NA, 
       
       main = "N2O",
       
       groups = YEAR,
       
       pch = c(16, 1),
       
       cex = c(1, 1.5),
       
       col = c("red" , "blue"),
       
       key = list(
         
         space = "top",
         
         columns = 2,
         
         text = list(c("2021" , "2022")),
         
         points = list(pch = c(16, 1),
                       
                       col = c("red" , "blue"))),
       
       panel = function(x,y,...) {
         
         panel.xyplot(x,y, ...)
         
         panel.xyplot(C.GC.Standards.no.NA$N2O.ppm ,C.GC.Standards.no.NA$N2O, col = "black", lwd =2)
         
         panel.abline(a = coefficients(Quantile.Reg.N2O)[1,1] , b = coefficients(Quantile.Reg.N2O)[2,1],
                      
                      col = "gray" , lwd = 1.5)
         
         panel.abline(a = coefficients(Quantile.Reg.N2O)[1,2] , b = coefficients(Quantile.Reg.N2O)[2,2],
                      
                      col = "gray" , lwd = 1.5)
         
         panel.abline(a = coefficients(Quantile.Reg.N2O)[1,3] , b = coefficients(Quantile.Reg.N2O)[2,3],
                      
                      col = "gray" , lwd = 1.5)
         
         panel.abline(a = coefficients(Quantile.Reg.N2O)[1,4] , b = coefficients(Quantile.Reg.N2O)[2,4],
                      
                      col = "gray" , lwd = 1.5)
         
         panel.abline(a = coefficients(Quantile.Reg.N2O)[1,5] , b = coefficients(Quantile.Reg.N2O)[2,5],
                      
                      col = "gray" , lwd = 1.5)
         
         panel.abline(a = coefficients(Quantile.Reg.N2O)[1,6] , b = coefficients(Quantile.Reg.N2O)[2,6],
                      
                      col = "gray" , lwd = 1.5)
         
         panel.abline(a = coefficients(Quantile.Reg.N2O)[1,7] , b = coefficients(Quantile.Reg.N2O)[2,7],
                      
                      col = "gray" , lwd = 1.5)
         
         panel.abline(a = coefficients(Quantile.Reg.N2O)[1,8] , b = coefficients(Quantile.Reg.N2O)[2,8],
                      
                      col = "gray" , lwd = 1.5)
         
         panel.abline(a = coefficients(Quantile.Reg.N2O)[1,9] , b = coefficients(Quantile.Reg.N2O)[2,9],
                      
                      col = "gray" , lwd = 1.5)
         
         panel.text(x = 10,  y = 20000, 
                    
                    labels = paste0("OLS: ",round(OLS.regression.N2O$coefficients[1],3), "+" ,
                                    
                                    round(OLS.regression.N2O$coefficients[2],3), "N2O"),
                    
                    col = "black" ,
                    
                    cex = 2)
         
         panel.text(x = 10,  y = 30000, 
                    
                    labels = paste0("QUANT: " ,round(Quantile.Reg.N2O$coefficients[1,5],3), "+" ,
                                    
                                    round(Quantile.Reg.N2O$coefficients[2,5],3), "N2O"),
                    
                    col = "black" ,
                    
                    cex = 2)
         
       }
       
)




Quantile.Reg.N2O.05 <- rq( N2O ~ N2O.ppm, data = C.GC.Standards.no.NA, tau = 0.5 )

summary(Quantile.Reg.N2O.05)



plot(N2O ~ N2O.ppm,
     
     data = C.GC.Standards.no.NA, 
     
     main = "N2O",
     
     pch = 16,
     
     cex = 1,
     
     col = "red")

points(predict(OLS.regression.N2O)  ~  C.GC.Standards.no.NA$N2O.ppm  , pch = 17 , cex = 1.5 , col = "green")

abline(a = OLS.regression.N2O$coefficients[1], b = OLS.regression.N2O$coefficients[2], col = "green")

points(predict(OLS.regression.sqrt_N2O)^2 ~ C.GC.Standards.no.NA$N2O.ppm, pch = 16 , cex = 1.5 , col = "blue")

points(predict(Quantile.Reg.N2O.05) ~ C.GC.Standards.no.NA$N2O.ppm  , pch = 18 , cex = 1.5 , col = "magenta") 

abline(a = Quantile.Reg.N2O.05$coefficients[1], b = Quantile.Reg.N2O.05$coefficients[2], col = "magenta")


legend("topleft", legend = c("N2O.ppm", "OLS-N2O", "OLS-Sqrt(N2O)", "Quant 50"), 
       
       col = c("red" , "blue", "green", "magenta" ), pch = c(16, 17, 16 , 18))



###############################################################################################################
#                           
#       The best regression analysis for the GC calibration is the Quantile regression at 0.5
#     
#       But the errors grow with the values of the standards. Heteroscedasticity.
#
#       It is very similar to the regular OLS regression, but is less affected  by extreme values.
#
#       Quantile regression is a more robust and conservative option
#
###############################################################################################################




# ###############################################################################################################
# 
#                      Best calibration equation for N2O
# 
# ###############################################################################################################
# 
# 
# Call: rq(formula = N2O ~ N2O.ppm, tau = 0.5, data = C.GC.Standards.no.NA)
# 
# tau: [1] 0.5
# 
# Coefficients:
#   coefficients lower bd upper bd
# (Intercept) 134.9825     127.2636 149.5451
# N2O.ppm     624.6675     608.9579 634.0715 
# 
# ###############################################################################################################

range( C.GC.Standards.no.NA$N2O)
