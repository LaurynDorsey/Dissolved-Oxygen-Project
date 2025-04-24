#' ---
#' title: "Dissolved Oxygen Project "
#' author: "Lauryn Dorsey"
#' date: '`r Sys.Date()`'
#' output: html_document
#' ---
  
#Load these Dependencies
library(ggplot2)
library(gridExtra)
library(scatterplot3d)
library(MASS)
library(marginaleffects)
library(GGally)
library(ggcorrplot)
library(mand)
library(marginaleffects)
library(tidyverse)
library(nnet)
require(graphics)


#Load data
water_data <- read.csv(file= "./Final Project/data/waterdata.csv")

#drop Sample.ID because it does not pertain to this project (no lat or long)
water <- subset(water_data, select = -c(Sample.ID))


#check data
head(water)
summary(water)


#Rename the columns for an easier time of coding
colnames(water)[2] ="TEMP"
colnames(water)[3] ="TURB"
colnames(water)[4] ="DO"
colnames(water)[5] ="COND"
names(water)

#check if renaming worked properly
head(water)

#We run this code chunk to make the plots look more appealing.
panelwater2panel.cor <- function(x, y, digits = 2, prefix = "", 
                                              cex.cor, ...)
{
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}



#Run the pairs function to visualize the relationships present in the data and 
#to get the Pearson correlation coefficient for each of the features.
#pdf('./figs/pairs_plot.pdf')
pairs( ~ DO + TEMP + TURB + pH +
         COND, data = water, lower.panel = panelwater2panel.cor,
       upper.panel = panel.smooth)
#dev.off()


#pdf('./figs/ggpairs_plot.pdf')
ggpairs(water)
#dev.off()

#These two plots can be difficult to understand so we will make a few more plots
#to see which will display the results appropriately. 



#This set of plots shows the relationship between DO and the four other features 
#in separate color coordinated plots making it easier for interpretation.

#pdf('./figs/plot.pdf')
par(mfrow = c(2,2))
plot(DO ~ TEMP, data = water, 
     xlab = 'Temperature', ylab = 'DO')
points(DO ~ TEMP, data = water, 
       pch = 16, col = 'red')
lines(lowess(water$TEMP, water$DO), 
      lty = 1,lwd = 2, col = 'black')

plot(DO ~ pH, data = water,
     xlab = 'pH', ylab = 'DO')
points(DO ~ pH, data = water,
       pch = 16, col = 'blue')
lines(lowess(water$pH,
             water$DO),
      lty = 1, lwd = 2, col = 'black')

plot(DO ~ TURB, data = water,
     xlab = 'Turbidity', ylab = 'DO')
points(DO ~ TURB, data = water,
       pch = 16, col = 'green')
lines(lowess(water$TURB,
             water$DO),
      lty = 1, lwd = 2, col = 'black')

plot(DO ~ COND, data = water,
     xlab = 'Conductivity', ylab = 'DO')
points(DO ~ COND, data = water,
       pch = 16, col = '6')
lines(lowess(water$COND,
             water$DO),
      lty = 1, lwd = 2, col = 'black')
#dev.off()




#pdf('./figs/corr_plot.pdf')
corr <- round(cor(water), 2)
ggcorrplot(corr, hc.order = TRUE,
           type = "lower",
           lab = TRUE,
           lab_size = 4,
           method="circle",
           colors = c("1", "white", "4"),
           title="Correlogram of water",
           ggtheme=theme_bw())
#dev.off()
#This correlogram displays the correlation between the different features and 
#DO with color and size showing the significance of the relationship.



#pdf('./figs/lm_plot.pdf')
par(mfrow = c(1,1))
null_mod <- lm(DO ~ 1, data = water)
null_mod
mean(water$DO)
plot(DO ~ 1, data = water)
abline(null_mod, lwd = 2)
abline(h = mean(water$DO), col = 'red', lty = 2, lwd = 2)
#dev.off()



#Here we will make linear regression models for all of the features.
model_all <- lm(DO ~ pH + TEMP + TURB + COND, data=water)

#Then we will use the summary and anova function to get an overview of the statistics and 
#structure of the data
summary(model_all)
anova(model_all)


#This plot shows the Residuals vs Fitted, Q-Q Residuals, Scale-Location, 
#and Residuals vs Leverage for the whole water data set
#pdf('./figs/model_all_plot.pdf')
par(mfrow=c(2,2))
plot(model_all)
#dev.off()



#This term plot will plot regression terms against predictors with partial 
#residuals present 
#pdf('./figs/termplot.pdf')
termplot(model_all, se = TRUE, partial.resid = TRUE)
#dev.off()



#generate prediction interval for multiple regression model:

#as a function of pH, other features are set to mean values
newdata_pH <- with(water, data.frame(pH = seq(min(pH), max(pH), .01),
                        TEMP = mean(TEMP),
                        TURB = mean(TURB),
                        COND = mean(COND)))

#as a function of Temperature, other features are set to mean values
newdata_TEMP <- with(water, data.frame(TEMP = 
                        seq(min(TEMP), max(TEMP), .01),
                        pH = mean(pH),
                        TURB = mean(TURB),
                        COND = mean(COND)))

#as a function of Conductivity, other features are set to mean values
newdata_Cond <- with(water, data.frame(COND = 
                seq(min(COND), max(COND), .01),
                TEMP = mean(TEMP),
                TURB = mean(TURB),
                pH = mean(pH)))

#as a function of Turbidity, other features are set to mean values
newdata_Turb <- with(water, data.frame(TURB = 
                          seq(min(TURB), max(TURB), .01),
                          TEMP = mean(TEMP),
                          pH = mean(pH),
                          COND = mean(COND)))


#predictions for all four features
phPred_p <- predict(model_all, newdata_pH, se.fit = TRUE, 
                    interval = 'prediction')
TEMPPred <- predict(model_all, newdata_TEMP, 
                                se.fit = TRUE, interval = 'prediction')
CondPred <- predict(model_all, newdata_Cond, se.fit = TRUE, 
                    interval = 'prediction')
TurbPred <- predict(model_all, newdata_Turb, se.fit = TRUE, 
                    interval = 'prediction')



#Now we will plot the predictions for all of the features.
#pdf('./figs/All_Prediction_plots.pdf')
par(mfrow = c(2,2))

#Temperature prediction plot
plot(newdata_TEMP$TEMP, TEMPPred$fit[,1], type = 'l', lty = "dashed", 
     col='black', lwd=2, ylim = c(5, 12), xlab = 'Temperature', ylab = 'DO')
polygon(c(newdata_TEMP$TEMP, rev(newdata_TEMP$TEMP)), 
        c(TEMPPred$fit[,2], rev(TEMPPred$fit[,3])), col = "#87CEFA30")
lines(newdata_TEMP$TEMP, TEMPPred$fit[,2], lty = "solid", col='red', lwd=3 )
lines(newdata_TEMP$TEMP, TEMPPred$fit[,3], lty = "solid", col='red', lwd=3)

#pH prediction plot  
plot(newdata_pH$pH, phPred_p$fit[,1], type = 'l', lty = "dashed", 
     col='black', lwd=2, ylim = c(5, 12),
     xlab = 'pH', ylab = 'DO')
polygon(c(newdata_pH$pH, rev(newdata_pH$pH)), 
        c(phPred_p$fit[,2], rev(phPred_p$fit[,3])), col = "#87CEFA30")
lines(newdata_pH$pH, phPred_p$fit[,2], lty = "solid", col='red', lwd=3)
lines(newdata_pH$pH, phPred_p$fit[,3], lty = "solid", col='red', lwd=3)
legend("topright", inset = 0, legend=c("Upper/lower bound of prediction", 
                            "local constant fit"), col=c("red", "black"), 
       lty=1:2, cex=1)

#Turbidity prediction plot
plot(newdata_Turb$TURB, TurbPred$fit[,1], type = 'l', lty = "dashed", 
     col='black', lwd=2, ylim = c(5, 12), xlab = 'Turbidity', ylab = 'DO')
polygon(c(newdata_Turb$TURB, rev(newdata_Turb$TURB)), 
        c(TurbPred$fit[,2], rev(TurbPred$fit[,3])), col = "#87CEFA30")
lines(newdata_Turb$TURB, TurbPred$fit[,2], lty = "solid", col='red', lwd=3)
lines(newdata_Turb$TURB, TurbPred$fit[,3], lty = "solid", col='red', lwd=3)


#Conductivity prediction plot
plot(newdata_Cond$COND, CondPred$fit[,1], type = 'l',lty = "dashed", 
  col='black', lwd=2, ylim = c(5, 12), xlab = 'Conductivity', ylab = 'DO')
polygon(c(newdata_Cond$COND, rev(newdata_Cond$COND)), 
        c(CondPred$fit[,2], rev(CondPred$fit[,3])), col = "#87CEFA30")
lines(newdata_Cond$COND, CondPred$fit[,2], lty = "solid", col='red', lwd=3 )
lines(newdata_Cond$COND, CondPred$fit[,3], lty = "solid", col='red', lwd=3)
#dev.off()

#These plots show the prediction interval for DO levels of a given feature.


