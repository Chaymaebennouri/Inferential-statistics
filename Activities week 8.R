# Load packages:

library(tidyverse)
library(broom)
library(sp)
library(raster)
library(rgl)
library(ggplot2)
library(maps)
library(scatterplot3d)

#loading the data

clim <- read.csv("https://userpage.fu-berlin.de/soga/data/raw-data/Climfrance.csv", sep = ";")

str(clim)
#The dataset we are exploring is a climate data table for 36 stations across France. The table below includes various information about each station's climate, such as the station's name, altitude, latitude, and longitude.
# Convert the variables "altitude" and "p mean" from character to numeric

clim$altitude <- as.numeric(gsub(",", ".", clim$altitude))

clim$p_mean <- as.numeric(gsub(",", ".", clim$p_mean))

str(clim)
#plot the stations on a map to get an idea of their locations
G1 <- raster::getData(country = "France", level = 1)

ggplot() +
  geom_polygon(data = G1,
    aes(x = long, y = lat, group = group),
    colour = "black", fill = "#fab64c"
  ) +
  geom_point(
    data = clim,
    aes(x = lon, y = lat),
    alpha = .5,
    size = 2
  ) +
  theme_bw() +
  labs(title = "Stations location in france",
       x = "Longitude",
       y = "Latitude")+
  coord_map()
########################################################################
#######################################################################

#Exercise 1

climfrar <- clim[1:34,]
climfrar

#linear modle
model <- lm(t_mean ~ altitude + lat+ lon, data = climfrar)
coef(model)
# (Intercept)     altitude          lat          lon 
#37.265036409 -0.006413946 -0.533960332  0.032101008 
#From this figure we can extract the linear model coefficients: 
#  Mean Annual Temperature = 37.26 + (-0.0064) * Altitude + (-0.534) * Latitude + 0.032 * Longitude


summary(model)
#Call:
#  lm(formula = t_mean ~ altitude + lat + lon, data = climfrar)

#Residuals:
#  Min       1Q   Median       3Q      Max 
#-1.76492 -0.32755  0.04337  0.24787  2.58927 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 37.2650364  2.6220099  14.212 7.29e-15 ***
#  altitude    -0.0064139  0.0008688  -7.383 3.17e-08 ***
#  lat         -0.5339603  0.0557546  -9.577 1.24e-10 ***
#  lon          0.0321010  0.0395728   0.811    0.424    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 0.7308 on 30 degrees of freedom
#Multiple R-squared:  0.8329,	Adjusted R-squared:  0.8162 
#F-statistic: 49.84 on 3 and 30 DF,  p-value: 9.112e-12

#Interpretation of results
#The summary output of the model shows that altitude and latitude variables are significant (p-value < 0.001).
#•	Assuming that the altitude and latitude slope is 0, the observed slope (-0.0064, -0.534) is unexpected.
#•	Altitude was negatively associated with mean temperature (-0.0064, SE=0.00086, p<0.001).
#•	For each decrease in latitude by one standard deviation, mean temperature decreases by -0.534 (b = -0.534, SE = 0.05, p<0.001).

#Exercise 2

model_reduced <- step(model)
coef(model_reduced)
#(Intercept)     altitude          lat 
#37.914756654 -0.006264281 -0.546532463 

summary(model_reduced)

#Call:
#  lm(formula = t_mean ~ altitude + lat, data = climfrar)

#Residuals:
#  Min       1Q   Median       3Q      Max 
#-1.79206 -0.27571 -0.00556  0.30536  2.71871 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 37.9147567  2.4828724   15.27 5.68e-16 ***
#  altitude    -0.0062643  0.0008443   -7.42 2.34e-08 ***
#  lat         -0.5465325  0.0532610  -10.26 1.72e-11 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 0.7268 on 31 degrees of freedom
#Multiple R-squared:  0.8292,	Adjusted R-squared:  0.8182 
#F-statistic: 75.26 on 2 and 31 DF,  p-value: 1.268e-12



# get the predicted temperature values for Mont-Ventoux and Pic-du-midi
pred_temp <- predict(model_reduced,
                     newdata = list("altitude" = c(1212, 2860), 
                                    "lat" = c(44.2, 42.9)), 
                     interval = "p", level = 0.95)
pred_temp

#fit       lwr      upr
#1  6.165713  3.792341 8.539085
#2 -3.447331 -8.347964 1.453302
#The predicted values show the lower temperature (3.8, -8.3) for both Mont-Ventoux and Pic-du-midi and upper temperatures (8.5, 1.4) for Mont-Ventoux and Pic-du-midi.
#The fitted temperature for Mont-Ventoux is 6.1and for Pic-du-midi temperature is -3.5.


#Exercise 3
scatter_3d <- with(climfrar, 
                   scatterplot3d(main = "3D scatterplot of the model results", xlab = "Altitude", 
                                 ylab = "latitude", zlab = "Mean temperature" ,altitude, lat, t_mean,
                                           pch = 16, highlight.3d = TRUE,
                                           angle = 45,)
)
scatter_3d$plane3d(model_reduced)
#•	The 3D scatterplot displays the distribution of the model.
#•	The point's color represents the mean temperature of that specific altitude and latitude.
#•	In this scatterplot, we can observe that the predicted temperature values closely follow the mean temperature values. This indicates that the model has successfully predicted the temperatures at different altitude and latitude combinations for Mont-Ventoux and Pic-du-midi.
