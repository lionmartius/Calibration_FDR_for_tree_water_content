# Initial Data Processing


# read VWC Calibration Data
VWC <- read.csv(file = 'VWC_Calibration_ALL.csv')
str(VWC)
summary(VWC)


# Add species specific mean density to dataframe
species_mean_density <- with(VWC, tapply(density, species, mean, na.rm=TRUE))
VWC$meandensity <- species_mean_density[match(VWC$species, names(species_mean_density))]


# Temp outlier remove (409C is clearly impossible and a typing mistake)
outlier_index <- which(VWC$Temp_f == 409.75)

# Remove the outlier by assigning NA to the corresponding value
VWC$Temp_f[outlier_index] <- NA


##Create Species Groups / Taxonomic Units

Manilkara <- VWC[VWC$species == 'Manilkara_bidentata',]
Protium <- VWC[VWC$species == 'Protium_tenuifolium',]
Voucapoua <- VWC[VWC$species == 'Voucapoua_americana',]  
Licania <- VWC[VWC$species == 'Licania_octandra',]
Jacaranda <- VWC[VWC$species == 'Jacaranda_copaia',]
Oenocarpus <- VWC[VWC$species == 'Oenocarpus_distichus',]
Euterpe <- VWC[VWC$species == 'Euterpe_oleraceae',]
Astrocaryum <-VWC[VWC$species == 'Astrocaryum_vulgare',]                


Palms <- subset(VWC, species %in% c('Euterpe_oleraceae','Oenocarpus_distichus', 
                                    'Astrocaryum_vulgare'))
Dicots <- subset(VWC, species %in% c('Manilkara_bidentata','Protium_tenuifolium',
                                     'Voucapoua_americana',
                                     'Licania_octandra','Jacaranda_copaia'))


### FILLING RAW data gaps and remove outliers which are likely typing errors
# since RAW and VWC data are linear - outliers were identified
# due to local collaboration and the remoteness of the site, data was written
# down by hand - 
# non- treatment of outliers would lead to biases in electric permittivity values 
# later on

### Predict RAW values from model (fill missing RAW values)
# RAW EXP
plot(VWC$VWC_FDR_exp ~ VWC$RAW_VWC_exp, data = VWC)

df_exp <- data.frame(VWC$RAW_VWC_exp, VWC$VWC_FDR_exp)
fit <- lm(df_exp)
summary(lm(df_exp))
fit
df_exp$predicted <- predict(fit, newdata = df_exp)

plot(df_exp$VWC.RAW_VWC_exp, df_exp$VWC.VWC_FDR_exp)

par(mfrow= c(1,1))
plot(df_exp$VWC.VWC_FDR_exp, df_exp$VWC.RAW_VWC_exp, col = 'blue',
     points(df_exp$VWC.VWC_FDR_exp, df_exp$predicted, col = 'red'))
plot(df_exp$VWC.VWC_FDR_exp, df_exp$predicted)
VWC$RAW_predict_exp <- df_exp$predicted

# RAW FRESH
plot(VWC$VWC_FDR_f ~ VWC$RAW_VWC_f, data = VWC)

df_f <- data.frame(VWC$RAW_VWC_f, VWC$VWC_FDR_f)
fit_f <- lm(df_f)
summary(lm(df_f))
fit_f
df_f$predicted <- predict(fit_f, newdata = df_f)

plot(df_f$VWC.RAW_VWC_f, df_f$VWC.VWC_FDR_f)

par(mfrow= c(1,1))
plot(df_f$VWC.VWC_FDR_f, df_f$VWC.RAW_VWC_f, col = 'blue',
     points(df_f$VWC.VWC_FDR_f, df_f$predicted, col = 'red'))
plot(df_f$VWC.VWC_FDR_f, df_f$predicted)
VWC$RAW_predict_f <- df_f$predicted

# ε (electric permittivity) can be calculted from RAW (voltage) values
# measured by the FDR sensor
# Note: Using measured RAW values and applying the METER (Teros) generated 
# calibtration to calculate ε is NOT correct
# RAW values need to be divided by 10 initially - This has been discussed with 
# METER's technical support 
# Hence, equation 8 of the Teros11-12 Manual is used, and RAW values were divided 
# by 10
##############################################################################
#####################
# USE PREDICTED RAW VALUES
VWC$RAW_VWC_exp10p <- VWC$RAW_predict_exp/10 
VWC$RAW_VWC_f10p <- VWC$RAW_predict_f/10

# CALCULATE ε (ep) from RAW10 values
VWC$ep_f <- (2.887 * 10^-9 * VWC$RAW_VWC_f10p^3 - 2.080 
             * 10^-5 * VWC$RAW_VWC_f10p^2 + 5.276 * 10^-2 * VWC$RAW_VWC_f10p -43.39 )^2
VWC$ep_exp <- (2.887 * 10^-9 * VWC$RAW_VWC_exp10p^3 - 2.080 
               * 10^-5 * VWC$RAW_VWC_exp10p^2 + 5.276 * 10^-2 * VWC$RAW_VWC_exp10p -43.39 )^2

# Apply sqrt-transformation of ep 

VWC$ep_exp.sqrt <- sqrt(VWC$ep_exp)
VWC$ep_f.sqrt <- sqrt(VWC$ep_f)


## DATA exploration using pair plots

#########################################
# Correlation Pair Plots
#########################################
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  usr <- par("usr")
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y, use = "complete.obs"))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}
panel.hist <- function(x, ...)
{
  usr <- par("usr")
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "cyan", ...)
}

pairs(VWC[, c("VWC_exp", "VWC_FDR_exp", "ep_exp", 
              "density", "RAW_VWC_exp10p","Temp_exp")],
      lower.panel = panel.cor,
      diag.panel = panel.hist,
      upper.panel = panel.smooth) 


pairs(VWC[, c("VWC_grav", "VWC_FDR_f", "ep_f", 
              "density", "RAW_VWC_f10p","Temp_f")],
      lower.panel = panel.cor,
      diag.panel = panel.hist,
      upper.panel = panel.smooth)
