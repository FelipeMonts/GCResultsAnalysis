








###############################################################################################################
#                           
#        Regression for standards that were obtained on the same day of a particular GC analysis
#
###############################################################################################################




str(GC.standards)

levels(GC.standards$ANAL.DATE)[[1]]

GC.standards[GC.standards$ANAL.DATE == levels(GC.standards$ANAL.DATE)[[8]], ]

plot(CO2~CO2.ppm, data = GC.standards[GC.standards$ANAL.DATE == levels(GC.standards$ANAL.DATE)[[8]], ], col= "blue") ;


### Ordinary Least Square Regression OLS

OLS.regression <- lm(CO2~CO2.ppm, data = GC.standards[GC.standards$ANAL.DATE == levels(GC.standards$ANAL.DATE)[[8]], ] ) ;

summary(OLS.regression)

str(OLS.regression)

OLS.regression$coefficients[[1]]


abline(a = OLS.regression$coefficients[[1]], b = OLS.regression$coefficients[[2]] , col="red")



### quantile regression

Quantile.Reg <- rq( CO2~CO2.ppm, data = GC.standards[GC.standards$ANAL.DATE == levels(GC.standards$ANAL.DATE)[[8]], ], tau = c(0.25, 0.50, 0.75) )

summary(Quantile.Reg)


str(Quantile.Reg)

abline(a = Quantile.Reg$coefficients[1,1], b = Quantile.Reg$coefficients[2,1] , col="cyan")

abline(a = Quantile.Reg$coefficients[1,2], b = Quantile.Reg$coefficients[2,2] , col="green")

abline(a = Quantile.Reg$coefficients[1,3], b = Quantile.Reg$coefficients[2,3] , col="magenta")

# xyplot(CO2 ~ CO2.ppm | GC.Date , groups = Series , data = GC.standards, 
#        
#        type="b",main="CO2", auto.key = T, col = c("BLACK" , "RED" , "BLUE", "CYAN", "MAGENTA"),  lwd=3); 


###############################################################################################################
#                           
#        Comparison between ordinary least squares regression and quantile regresson for the standards
#
###############################################################################################################


####   CO2  ####

xyplot(CO2.ppm ~ CO2 | GC.Date , groups = ANAL.DATE , data=GC.standards, 
       
       panel = function(x, y) { panel.xyplot(x, y)
         
         panel.xyplot(x, y) 
         
         panel.abline(lm(y ~ x), col = "BLACK", lwd = 2)
         
         panel.abline(rq(y ~ x), col="RED" , lwd = 2)
         
         panel.text(10000, 4000,labels = signif(rq(y ~ x)$coefficients[2], digits = 3), col = "red")
         
         panel.text(10000, 3500,labels = signif(rq(y ~ x)$coefficients[1], digits = 3), col = "red")
         
         panel.text(20000, 2000,labels = signif(lm(y ~ x)$coefficients[1], digits = 3), col = 'black' )
         
         panel.text(20000, 2500,labels = signif(lm(y ~ x)$coefficients[2], digits = 3), col = 'black' )
       },
       
       type="b",main="CO2", auto.key = T)


xyplot(CO2.ppm ~ CO2 , data=GC.standards, 
       
       panel = function(x, y) { panel.xyplot(x, y)
         
         panel.xyplot(x, y) 
         
         panel.abline(lm(y ~ x), col = "BLACK", lwd = 2)
         
         panel.abline(rq(y ~ x), col="RED" , lwd = 2)
         
         panel.text(10000, 4000,labels = signif(rq(y ~ x)$coefficients[2], digits = 3), col = "red")
         
         panel.text(10000, 3500,labels = signif(rq(y ~ x)$coefficients[1], digits = 3), col = "red")
         
         panel.text(20000, 2000,labels = signif(lm(y ~ x)$coefficients[1], digits = 3), col = 'black' )
         
         panel.text(20000, 2500,labels = signif(lm(y ~ x)$coefficients[2], digits = 3), col = 'black' )
       },
       
       type="b",main="N2O", auto.key = T)


####   N2O  ####

xyplot(N2O.ppm ~ N2O | GC.Date , groups = ANAL.DATE , data=GC.standards, 
       
       panel = function(x, y) { panel.xyplot(x, y)
         
         panel.xyplot(x, y) 
         
         panel.abline(lm(y ~ x), col = "BLACK", lwd = 2)
         
         panel.abline(rq(y ~ x), col="RED" , lwd = 2)
         
         panel.text(10000, 40,labels = signif(rq(y ~ x)$coefficients[2], digits = 3), col = "red")
         
         panel.text(10000, 35,labels = signif(rq(y ~ x)$coefficients[1], digits = 3), col = "red")
         
         panel.text(20000, 20,labels = signif(lm(y ~ x)$coefficients[1], digits = 3), col = 'black' )
         
         panel.text(20000, 25,labels = signif(lm(y ~ x)$coefficients[2], digits = 3), col = 'black' )
       },
       
       type="b",main="N2O", auto.key = T)


xyplot(N2O.ppm ~ N2O , data=GC.standards, 
       
       panel = function(x, y) { panel.xyplot(x, y)
         
         panel.xyplot(x, y) 
         
         panel.abline(lm(y ~ x), col = "BLACK", lwd = 2)
         
         panel.abline(rq(y ~ x), col="RED" , lwd = 2)
         
         panel.text(10000, 40,labels = signif(rq(y ~ x)$coefficients[2], digits = 3), col = "red")
         
         panel.text(10000, 35,labels = signif(rq(y ~ x)$coefficients[1], digits = 3), col = "red")
         
         panel.text(20000, 20,labels = signif(lm(y ~ x)$coefficients[1], digits = 3), col = 'black' )
         
         panel.text(20000, 25,labels = signif(lm(y ~ x)$coefficients[2], digits = 3), col = 'black' )
       },
       
       type="b",main="N2O", auto.key = T)



####   CH4  ####

xyplot(CH4.ppm ~ CH4 | GC.Date , groups = ANAL.DATE , data=GC.standards, 
       
       panel = function(x, y) { panel.xyplot(x, y)
         
         panel.xyplot(x, y) 
         
         panel.abline(lm(y ~ x), col = "BLACK", lwd = 2)
         
         panel.abline(rq(y ~ x), col="RED" , lwd = 2)
         
         panel.text(200, 40,labels = signif(rq(y ~ x)$coefficients[2], digits = 3), col = "red")
         
         panel.text(200, 35,labels = signif(rq(y ~ x)$coefficients[1], digits = 3), col = "red")
         
         panel.text(400, 20,labels = signif(lm(y ~ x)$coefficients[1], digits = 3), col = 'black' )
         
         panel.text(400, 25,labels = signif(lm(y ~ x)$coefficients[2], digits = 3), col = 'black' )
       },
       
       type="b",main="CH4", auto.key = T)


xyplot(CH4.ppm ~ CH4 , data=GC.standards, 
       
       panel = function(x, y) { panel.xyplot(x, y)
         
         panel.xyplot(x, y) 
         
         panel.abline(lm(y ~ x), col = "BLACK", lwd = 2)
         
         panel.abline(rq(y ~ x), col="RED" , lwd = 2)
         
         panel.text(200, 40,labels = signif(rq(y ~ x)$coefficients[2], digits = 3), col = "red")
         
         panel.text(200, 35,labels = signif(rq(y ~ x)$coefficients[1], digits = 3), col = "red")
         
         panel.text(400, 20,labels = signif(lm(y ~ x)$coefficients[1], digits = 3), col = 'black' )
         
         panel.text(400, 25,labels = signif(lm(y ~ x)$coefficients[2], digits = 3), col = 'black' )
       },
       
       type="b",main="CH4", auto.key = T)


#################################################################################################################
# 
# the best model for the standard calibration is the one using all the standards data in a linear regression
# 
# 
################################################################################################################## 


#### CO2 standards calibration ####


plot(CO2.ppm ~ CO2 , data = GC.standards)

# ##### There is a point with 2500 ppm that has GC CO2 area greater than 2500
# 
# GC.standards[GC.standards$CO2 >= 25000,]
# 
# GC.standards.2 <- GC.standards[GC.standards$CO2<= 25000,] ;
# 
# # Sample.Name Position Vial     CH4      CO2      N2O                      File Sampling.Day Sampling.Date    GC.Date
# # 1768    50PerSTD        5    1 583.846 25807.62 44608.26 20210720B3B4peakareas.pdf     20210720    2021-07-20 2021-08-06
# # AnalysisName Factor.Name CH4.ppm CO2.ppm N2O.ppm  ANAL.DATE
# # 1768 20210720B3B4peakareas.pdf    50PerSTD      25    2500      25 2021-08-06
# # This point appears in all of the gases as an outlier, therefore it is bets to remove it.
# 
# GC.standards <- GC.standards.2 ;

#################### Regression without forcing the intercept through 0 ####################

# CO2.Calibration <- lm(CO2.ppm ~ CO2 , data = GC.standards) ;
# 
# 
# summary(CO2.Calibration)

# Call:
#   lm(formula = CO2.ppm ~ CO2, data = GC.standards)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -2556.7  -272.6  -147.0   123.9  3928.5 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -31.605388  39.270520  -0.805    0.421    
# CO2           0.269938   0.004337  62.242   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 696.6 on 599 degrees of freedom
# Multiple R-squared:  0.8661,	Adjusted R-squared:  0.8659 
# F-statistic:  3874 on 1 and 599 DF,  p-value: < 2.2e-16

#################### Regression without forcing the intercept through 0 ####################

CO2.Calibration <- lm(CO2.ppm ~ CO2 , data = GC.standards) ;

summary(CO2.Calibration)

# Call:
#   lm(formula = CO2.ppm ~ CO2 + 0, data = GC.standards)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -2505.3  -301.8  -139.0  1954.6  4774.0 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# CO2  0.33033    0.02177   15.18   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 1837 on 197 degrees of freedom
# Multiple R-squared:  0.5389,	Adjusted R-squared:  0.5366 
# F-statistic: 230.3 on 1 and 197 DF,  p-value: < 2.2e-16
# 

abline(lm(CO2.ppm ~ CO2  + 0 ,data = GC.standards), col = "red" , lwd = 2 )

abline(lm(CO2.ppm ~ CO2  + 0 ,data = GC.standards.2), col = "blue" , lwd = 2)

abline(rq(CO2.ppm ~ CO2  + 0 ,data = GC.standards), col = "magenta" , lwd = 2)

abline(rq(CO2.ppm ~ CO2  + 0 ,data = GC.standards.2), col = "green" , lwd = 2)



#### N20 standards calibration ####


plot(N2O.ppm ~ N2O , data = GC.standards)

#################### Regression without forcing the intercept through 0 ####################

# N2O.Calibration <- lm(N2O.ppm ~ N2O  , data = GC.standards) ;
# 
# summary(N2O.Calibration)

# Call:
#   lm(formula = N2O.ppm ~ N2O, data = GC.standards)
# 
# Residuals:
#   Min     1Q Median     3Q    Max 
# -13.17 -12.57 -12.25  12.01  37.29 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 1.250e+01  1.372e+00   9.114  < 2e-16 ***
#   N2O         1.171e-03  1.403e-04   8.346 1.25e-14 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 17.28 on 196 degrees of freedom
# Multiple R-squared:  0.2622,	Adjusted R-squared:  0.2584 
# F-statistic: 69.65 on 1 and 196 DF,  p-value: 1.253e-14


#################### Regression forcing the intercept through 0 ####################


N2O.Calibration <- lm(N2O.ppm ~ N2O + 0 , data = GC.standards) ;

summary(N2O.Calibration)



# Call:
#   lm(formula = N2O.ppm ~ N2O, data = GC.standards)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -23.093  -1.763  -1.525  -0.551  41.403 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 1.425e+00  3.382e-01   4.214  2.9e-05 ***
#   N2O         1.494e-03  2.159e-05  69.213  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 6.605 on 595 degrees of freedom
# (4 observations deleted due to missingness)
# Multiple R-squared:  0.8895,	Adjusted R-squared:  0.8893 
# F-statistic:  4790 on 1 and 595 DF,  p-value: < 2.2e-16
# 

abline(lm(N2O.ppm ~ N2O, data = GC.standards) , col = "red" , lwd = 2) 


abline(lm(N2O.ppm ~ N2O + 0 , data = GC.standards) , col = "magenta" , lwd = 2) 




#### CH4 standards calibration ####

plot(CH4.ppm ~ CH4 , data = GC.standards)

#################### Regression without forcing the intercept through 0 ####################


# CH4.Calibration <- lm(CH4.ppm ~ CH4 , data = GC.standards) ;
# 
# summary(CH4.Calibration)

# Call:
#   lm(formula = CH4.ppm ~ CH4, data = GC.standards)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -14.642 -12.402  -7.507   9.523  35.063 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 14.29260    1.30325  10.967  < 2e-16 ***
#   CH4          0.08576    0.01027   8.352 1.21e-14 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 15.92 on 196 degrees of freedom
# Multiple R-squared:  0.2625,	Adjusted R-squared:  0.2587 
# F-statistic: 69.75 on 1 and 196 DF,  p-value: 1.209e-14



#################### Regression forcing the intercept through 0 ####################


CH4.Calibration <- lm(CH4.ppm ~ CH4 + 0, data = GC.standards) ;

summary(CH4.Calibration)


# Call:
#   lm(formula = CH4.ppm ~ CH4 + 0, data = GC.standards)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -21.755   0.263   1.768  23.038  48.936 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# CH4  0.14168    0.01129   12.54   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 20.17 on 197 degrees of freedom
# Multiple R-squared:  0.4441,	Adjusted R-squared:  0.4413 
# F-statistic: 157.4 on 1 and 197 DF,  p-value: < 2.2e-16

abline(lm(CH4.ppm ~ CH4, data = GC.standards) , col = "red" , lwd = 2) 


abline(lm(CH4.ppm ~ CH4 + 0 , data = GC.standards) , col = "magenta" , lwd = 2) 



# ###############################################################################################################
#                           
#                              Exploring the data without standards
#
###############################################################################################################

##### Data with no standards included

str(GC.standards)

str(PeakArea.results)

levels(GC.standards$Factor.Name)

GC.Data.NoSTD<-PeakArea.results[!(PeakArea.results$Sample.Name %in% levels(GC.standards$Factor.Name)),];

str(GC.Data.NoSTD)


plot.CH4.hist.dat<-hist(GC.Data.NoSTD$CH4)
plot.CH4.density.dat<-density(GC.Data.NoSTD$CH4, na.rm=T)

plot.CO2.hist.dat<-hist(GC.Data.NoSTD$CO2)
plot.CO2.density.dat<-density(GC.Data.NoSTD$CO2, na.rm=T)


plot.N2O.hist.dat<-hist(GC.Data.NoSTD$N2O)
plot.N2O.density.dat<-density(GC.Data.NoSTD$N2O, na.rm=T)


###############################################################################################################
#                          
#            Calculation of concentration based on the Standard gas concentrations
#  
#
###############################################################################################################


str(GC.Data.NoSTD)


##### CO2 ######

GC.Data.NoSTD$CO2.Intercept <- 0 ;

GC.Data.NoSTD$CO2.Slope <- coefficients(CO2.Calibration)[1] ;

GC.Data.NoSTD$CO2.ppm <- (GC.Data.NoSTD$CO2 * GC.Data.NoSTD$CO2.Slope) + GC.Data.NoSTD$CO2.Intercept ;

plot(GC.Data.NoSTD$CO2.ppm )


##### N2O ######

GC.Data.NoSTD$N2O.Intercept <- 0 ;

GC.Data.NoSTD$N2O.Slope <- coefficients(N2O.Calibration)[1] ;

GC.Data.NoSTD$N2O.ppm <- (GC.Data.NoSTD$N2O * GC.Data.NoSTD$N2O.Slope) + GC.Data.NoSTD$N2O.Intercept ;



##### CH4 ######

GC.Data.NoSTD$CH4.Intercept <- 0 ;

GC.Data.NoSTD$CH4.Slope <- coefficients(CH4.Calibration)[1] ;

GC.Data.NoSTD$CH4.ppm <- (GC.Data.NoSTD$CH4 * GC.Data.NoSTD$CH4.Slope) + GC.Data.NoSTD$CH4.Intercept ;


###############################################################################################################
#                           
#                               Organizing the data for visualization
#
###############################################################################################################

str(GC.Data.NoSTD)



### Organizing the data according to Treatments

unique(GC.Data.NoSTD$Sample.Name)

GC.Data.NoSTD$Treatment<-c("NONE");


GC.Data.NoSTD[grep("AT",GC.Data.NoSTD$Sample.Name , ignore.case = T), c("Treatment")]<-c("A");

GC.Data.NoSTD[grep("BT",GC.Data.NoSTD$Sample.Name , ignore.case = T), c("Treatment")]<-c("B");

GC.Data.NoSTD[grep("CT",GC.Data.NoSTD$Sample.Name , ignore.case = T) , c("Treatment")]<-c("C");

GC.Data.NoSTD[grep("DT",GC.Data.NoSTD$Sample.Name , ignore.case = T) , c("Treatment")]<-c("D");

### Check if there was any treatment left with "NONE" label

GC.Data.NoSTD[which(GC.Data.NoSTD$Treatment == "NONE"), ];

GC.Data.NoSTD[GC.Data.NoSTD$Sample.Name == "B4TritC30",]

GC.Data.NoSTD[GC.Data.NoSTD$Sample.Name == "B4TritC30", c("Treatment")] <- c("C") ;


GC.Data.NoSTD[which(GC.Data.NoSTD$Treatment == "NONE"), ];

### Organizing the data according to Blocks

grep("B1",GC.Data.NoSTD$Sample.Name)

GC.Data.NoSTD$BLOCK<-c(9999);

GC.Data.NoSTD[grep("B1",GC.Data.NoSTD$Sample.Name), c("BLOCK")]<-c(1);

GC.Data.NoSTD[grep("B2",GC.Data.NoSTD$Sample.Name), c("BLOCK")]<-c(2);

GC.Data.NoSTD[grep("B3",GC.Data.NoSTD$Sample.Name), c("BLOCK")]<-c(3);

GC.Data.NoSTD[grep("B4",GC.Data.NoSTD$Sample.Name), c("BLOCK")]<-c(4);

### Check if there was any BLOCK labeled 9999

GC.Data.NoSTD[which(GC.Data.NoSTD$BLOCK == 9999 ), ];



### Organizing the data according to CoverCrop

grep("3Spp",GC.Data.NoSTD$Sample.Name)

GC.Data.NoSTD$CoverCrop<-c("NONE");

GC.Data.NoSTD[grep("3Spp",GC.Data.NoSTD$Sample.Name), c("CoverCrop")]<-c("3Spp");

GC.Data.NoSTD[grep("Clover",GC.Data.NoSTD$Sample.Name), c("CoverCrop")]<-c("Clover");

GC.Data.NoSTD[grep("Trit",GC.Data.NoSTD$Sample.Name), c("CoverCrop")]<-c("Trit");


### Check if there was any  CoverCrop labeled "NONE"

GC.Data.NoSTD[which(GC.Data.NoSTD$CoverCrop == "NONE" ), ];


### Organizing the data according to Sampling Time Order

grep("T0",GC.Data.NoSTD$Sample.Name)

GC.Data.NoSTD$Sampling.Time<-c(9999);

GC.Data.NoSTD[grep("T0",GC.Data.NoSTD$Sample.Name), c("Sampling.Time")]<-c(0);

GC.Data.NoSTD[grep("T15",GC.Data.NoSTD$Sample.Name), c("Sampling.Time")]<-c(15);

GC.Data.NoSTD[grep("T30",GC.Data.NoSTD$Sample.Name), c("Sampling.Time")]<-c(30);

GC.Data.NoSTD[grep("T45",GC.Data.NoSTD$Sample.Name), c("Sampling.Time")]<-c(45);


### Check if there was any Sampling.Time left with "NONE" label

GC.Data.NoSTD[which(GC.Data.NoSTD$Sampling.Time==9999),];


GC.Data.NoSTD[GC.Data.NoSTD$Sample.Name == "B4TritC30",] ;

GC.Data.NoSTD[GC.Data.NoSTD$Sample.Name == "B4TritC30", c("Sampling.Time")] <- c(30) ;


GC.Data.NoSTD[which(GC.Data.NoSTD$Sampling.Time==9999),];



### Converting experimental designations into factors

GC.Data.NoSTD$Treatment.F<-as.factor(GC.Data.NoSTD$Treatment) ;

GC.Data.NoSTD$BLOCK.F<-as.factor(GC.Data.NoSTD$BLOCK) ;

GC.Data.NoSTD$CoverCrop.F<-as.factor(GC.Data.NoSTD$CoverCrop) ;









###############################################################################################################
#                           
#                               Exploratory Data visualization
#
###############################################################################################################

GC.Data.NoSTD$Series <- paste( GC.Data.NoSTD$Sampling.Day , GC.Data.NoSTD$BLOCK.F , 
                               
                               GC.Data.NoSTD$CoverCrop.F , GC.Data.NoSTD$Treatment.F, sep = "_") ;

head(GC.Data.NoSTD)



str(GC.Data.NoSTD)

levels(GC.Data.NoSTD$Treatment.F)

levels(GC.Data.NoSTD$BLOCK.F)

levels(GC.Data.NoSTD$CoverCrop.F)

GC.Data.NoSTD[GC.Data.NoSTD$CoverCrop.F == "Clover" ,]

xyplot(CO2.ppm ~ Sampling.Time | Treatment.F * BLOCK.F * CoverCrop.F, 
       
       data = GC.Data.NoSTD , xlim=c(0,45), ylim = c(0, max(GC.Data.NoSTD$CO2.ppm)) ,   
       
       type="o", auto.key = T, main = "CO2");


xyplot(N2O.ppm ~ Sampling.Time | Treatment.F * BLOCK.F * CoverCrop.F, 
       
       data = GC.Data.NoSTD , xlim=c(0,45), ylim = c(0, max(GC.Data.NoSTD$N2O.ppm)) ,   
       
       type="o", auto.key = T , main = "N2O");


xyplot(CH4.ppm ~ Sampling.Time | Treatment.F * BLOCK.F * CoverCrop.F, 
       
       data = GC.Data.NoSTD , xlim=c(0,45), ylim = c(0, max(GC.Data.NoSTD$CH4.ppm)) , 
       
       type="o", auto.key = T , main = "CH4");





###############################################################################################################
#
#               Reference data taken from Allison Kohele's Calculations Excel Files
#
###############################################################################################################





Chamber.Dimensions<-data.frame(DIMENSION=c("Length", "Width" , "Height", "Volume" , "Surface.Area"),UNITS=c("m"), VALUE=c(0.52705, 0.32385, 0.1016, 9999, 9999));

Chamber.Dimensions[Chamber.Dimensions$DIMENSION =="Volume", c("VALUE")]<-Chamber.Dimensions[1,3]*Chamber.Dimensions[2,3]*Chamber.Dimensions[3,3] ;


Chamber.Dimensions[Chamber.Dimensions$DIMENSION =="Surface.Area", c("VALUE")]<-Chamber.Dimensions[1,3]*Chamber.Dimensions[2,3] ;

Molar.Mass<-data.frame(GAS=c("CH4" , "CO2" , "N2O"), UNITS=c("g/mol"), VALUE=c(16.04, 44.01, 44.013));

Gas.Law<-data.frame(UNITS=c("L-atm/Mol-K", "J/K-Mol", "m3-Pa/K-Mol", "Kg-m2-s2/K-Mol", "m3-atm/K-Mol"), VALUE=c(0.08205736, 8.314462,8.314462, 8.314462, 8.205736e-5 ))  ;


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

# Pedersen, Asger R. “HMR: Flux Estimation with Static Chamber Data,” May 20, 2020. https://CRAN.R-project.org/package=HMR.
# 
#
###############################################################################################################

str(GC.Data.NoSTD)

unique(GC.Data.NoSTD$Sampling.Day)


####### Creating series data names for the HMR analysis ############

str(GC.Data.NoSTD)
unique(GC.Data.NoSTD$Sampling.Date)
unique(GC.Data.NoSTD$Sampling.Day)


# CO2.ppm : Sampling.Day - BLOCK. - CoverCrop.F - Treatment.F  
# 
# N2O.ppm : Sampling.Day - BLOCK. - CoverCrop.F - Treatment.F  
# 
# CH4.ppm : Sampling.Day - BLOCK. - CoverCrop.F - Treatment.F  
# 
# names(Test.data.HMR.CO2.1) <- c("Series" , "V" , "A" , "Time" , "Concentration")
# 
# write.table(x = Test.data.HMR.CO2.1, sep = ";", dec = "." ,file = "TEST_DATA.csv", row.names = F)

GC.Data.NoSTD$Series <- paste( GC.Data.NoSTD$Sampling.Day , GC.Data.NoSTD$BLOCK.F , 
                               
                               GC.Data.NoSTD$CoverCrop.F , GC.Data.NoSTD$Treatment.F, sep = "_") ;

head(GC.Data.NoSTD)

#### there is  duplicated data for a Sample day 20220615, need to get rid of the duplicated data


GC.Data.NoSTD$Series.Sampling.Time <- paste(GC.Data.NoSTD$Series , GC.Data.NoSTD$Sampling.Time , sep = "_" )

str(GC.Data.NoSTD)

duplicated(GC.Data.NoSTD$Series.Sampling.Time)

GC.Data.NoSTD.1 <- GC.Data.NoSTD[ duplicated(GC.Data.NoSTD$Series.Sampling.Time),]

