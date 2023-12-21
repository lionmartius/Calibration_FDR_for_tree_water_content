# Test the effect of temperature on uncalibrated Teros 12 loggers
# WOOD block and WATER BUCKET experiment

# Get calibration data in one table, long format --------------------------
data.cal <- read.csv('tmp_cal.csv')

# Fit calibration model ---------------------------------------------------
# water experiment
str(data.cal)
plot(swc ~ temperature, data.cal[data.cal$treatment == "water",], col = as.factor(data.cal$replicate[data.cal$treatment == "water"]))
fit = lm(swc ~ temperature, data.cal[data.cal$treatment == "water",])
summary(fit)
anova(fit)       #slope = -0.000974
abline(fit)
confint(fit, level = 0.95)


library(ggplot2)
library(grid)

# Fig 3a Temperature effect of water experiment
ggplot(data = data.cal[data.cal$treatment == "water",], 
       mapping = aes(y=swc, x= temperature))+
  geom_point(shape = 21, size = 3, fill = "#E7B800")+
  geom_smooth(method = 'lm', se = T, colour = 'black' )+
  ylab(expression(paste('FDR WC [m'^'3','/m'^'3',']')))+
  xlab(substitute(paste(italic('T '), '[°C]')))+
  theme_bw()
ggsave('temp_effect.png', dpi = 300, width = 6, height = 4)


# first wood experiment
plot(swc ~ temperature, data.cal[data.cal$treatment == "wood",], type = "o")
fit = lm(swc ~ temperature, data.cal[data.cal$treatment == "wood",])
summary(fit)
abline(fit)

# wood treatment first part of the data is probably not in equilibrium, greatly affecitng the slope
fit = lm(swc ~ temperature, data.cal[data.cal$treatment == "wood" & data.cal$swc <= 0.291,])
summary(fit)
abline(fit, col = "red")

# not a huge effect, but its there!
# the temperature variation was also very low, which reduces our capacity to get the slope well (it becomes sensitive to random variation)
# informed by this we redid the wood experiment over a larger temperature difference

## NEW WOOD + AIR experiment
woodCalibration3 <- read.csv('temp_wood_air3.csv')

wood3 <- lm(swc3~temperature3, data=woodCalibration3)
summary(wood3) # The slope of the T effect in wood is similar to water bucket.
               # model performed less well - shift of water content in the 
              # warm up phase (possible condensation of unsealed sensor - real WC
              # changes rather than T effect)
anova(wood3)

air2 <- lm(swc4~temperature4, woodCalibration3)
summary(air2)   # The temperature effect for sensors left in air is low
air1 <- lm(swc5~temperature5, woodCalibration3)
summary(air1)


# Fig 3b Temperature effect of Wood experiment
ggplot(data = woodCalibration3, 
       mapping = aes(y=swc3, x= temperature3))+
  geom_point(shape = 21, size = 3, fill = "#E7B800")+
  geom_smooth(method = 'lm', se = T, colour = 'black' )+
  ylab(expression(paste('FDR WC [m'^'3','/m'^'3',']')))+
  xlab(substitute(paste(italic('T '), '[°C]')))+
  theme_bw()
ggsave('temp_effect.png', dpi = 300, width = 6, height = 4)

