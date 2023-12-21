# analysis of calibration data

# relevant packages
install.packages("ggplot2")
library(ggplot2)
install.packages("gridExtra")
library('gridExtra')
install.packages('lme4')
library(lme4)
install.packages('MuMIn')
library(MuMIn)
install.packages('lmerTest')
library('lmerTest')

####################################################################
# Mixed Model

# initial mixed model testing the effect of fixed terms of the FDR signal and
# wood-density + the random effects of species and individuals

m0 <- lmer(VWC_exp ~ VWC_FDR_exp + meandensity +(1|species/sample), data = VWC)
summary(m0)
anova(m0)                      #meandensity non-significant -> drop in next step
ranova(m0, reduce.terms = TRUE)#random effects highly significant  
r.squaredGLMM(m0)              #random effects improve model goodness-of-fit
par(mfrow=c(2,2))
plot(m0)

# retrieve species specific sd for WD measurements
tapply(VWC$density, VWC$species, sd, na.rm = TRUE)

# mixed model without using WD 
m1 <- lmer(VWC_exp ~ VWC_FDR_exp  + (1|species/sample), data = VWC)

summary(m1)
anova(m1)
ranova(m1, reduce.terms = TRUE)
r.squaredGLMM(m1)


##############################################################################
# Linear Calibration Models


#TTCM (Tropical Tree Calibration Model)
TTCM <- lm(VWC_exp ~ ep_exp.sqrt, data = VWC)
summary(TTCM)                # slope = 0.2227; intercept = -0.396
anova(TTCM)
confint(TTCM, level = 0.95)


#FDR based model (based not on sqrt EP but the processed VWC from sensor)
FDR_m <- lm(VWC_exp ~ VWC_FDR_exp, data = VWC)
summary(FDR_m)               # slope = 1.837; intercept = -0.082
anova(FDR_m)


#Testing slope differences in species
#Interaction with species
int <- lm(VWC_exp ~ ep_exp.sqrt*species, data = VWC)
summary(int)
anova(int)  # significant but small variance explained by slope



# Building a One slope model
# First-order differencing

#ONE SLOPE using diff() and time series data (experiment was a temporaliy 
# explicit drydown/wetup appraoch)
#relate temporal changes of VWC signal to FDR signal
VWC$diffSWC = NA
VWC$diff_sqrt.ep= NA
VWC$id = paste(VWC$species, VWC$sample)
for(id in unique(VWC$id)){
  rows = VWC$id == id
  workTable = VWC[rows,]
  VWC$diffSWC[rows] = c(NA,diff(workTable$VWC_exp))
  VWC$diff_sqrt.ep[rows] = c(NA,diff(workTable$ep_exp.sqrt))
}
plot(diffSWC ~ diff_sqrt.ep, VWC, col = as.factor(VWC$species))
fit = lm(diffSWC ~ diff_sqrt.ep, VWC)
abline(fit)
summary(fit)

# OSM model based on differenced values
OSM <- lm(VWC$diffSWC ~ VWC$diff_sqrt.ep)
summary(OSM)    # slope coefficient = 0.2254
anova(OSM)
confint(OSM, level = 0.95)


#########
# Quantifying Error (Accuracy)
# MAE, RMSE, RAE

fun1 <- function(response = ep_exp.sqrt, 
                 predictor = VWC_exp, 
                 dt = VWC){
  
  if(is.null(response)){
    stop("specify response argument")
  }
  if(is.null(predictor)){
    stop("specify response argument")
  }
  if(is.null(dt)){
    stop("specify response argument")
  }
  
  frml  <- paste0(response, " ~ ", predictor)
  
  mod <- lm(as.formula(frml), data = dt)
  
  predicted_values <- predict(mod, newdata = dt)
  residuals <- VWC[,response] - predicted_values
  sqr.res <- residuals^2
  mean.sqr.res <- mean(sqr.res, na.rm = T)
  data.rae <- data.frame(y_pred = predicted_values,
                         y_actual = VWC[,response])
  data.rae <- na.omit(data.rae)
  abs_diff <- abs(data.rae$y_pred - data.rae$y_actual)
  mean_actual <- mean(data.rae$y_actual)
  mean_abs_diff <- mean(abs(data.rae$y_actual - mean_actual))
  
  
  mae <- mean(abs(VWC[, response] - predicted_values), na.rm = TRUE)
  mse <- mean((VWC[, response] - predicted_values)^2, na.rm = TRUE)
  rmse <- sqrt(mean.sqr.res)
  rae <- (mean(abs_diff) / mean_abs_diff)
  
  rslts <- list(data.frame('mae' = mae, 'rmse' = rmse, 'rae' = rae), 'summary' = summary(mod))
  return(rslts)
}


rsltTCCM  <- fun1(response = "VWC_exp", 
                  predictor  = "ep_exp.sqrt", 
                  dt = VWC) 

rsltOSM  <- fun1(response = "vwc_centralized", 
                 predictor  = "ep.sqrt_cent", 
                 dt = VWC) 

rsltFDR <- fun1(response = "VWC_exp", 
                predictor  = "VWC_FDR_exp", 
                dt = VWC) 

rsltOSMDiff <- fun1(response = 'diffSWC',
                    predictor = 'diff_sqrt.ep',
                    dt = VWC)


rsltTCCM
rsltFDR
rsltOSMDiff
