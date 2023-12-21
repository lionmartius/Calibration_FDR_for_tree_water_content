# Validation Excercise:
# Implications of intercept differences on accuracy of OSM and TTCM

# species-specific plot 
ggplot(mapping = aes(y = VWC_exp, x = ep_exp.sqrt, colour = species2), data = VWC) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_abline(intercept = coef(m_epHE)[1], 
              slope = coef(m_epHE)[2], color = "blue3",
              size = 1.1)+
  geom_abline(intercept = coef(TTCM)[1],
              slope= coef(TTCM)[2], color = 'orange',
              size = 1.1) +
  theme_grey() +
  theme(legend.position = c(0.8, 0.25),
        legend.key.size = unit(0.01, "cm"),
        legend.text = element_text(face = "italic")
  ) +
  labs(colour = 'species') +
  xlab(expression(paste('',phantom(' '),sqrt('ε'))))+
  ylab(expression(paste('StWC grav m'^'3', '/m'^'3'))) +
  scale_colour_manual(
    values = c("Astrocaryum vulgare" = 'darkolivegreen3', "Euterpe oleraceae" = 'green3', "Oenocarpus distichus" = 'forestgreen',
               "Jacaranda copaia" = 'blue4', "Licania octandra" = 'cyan', "Manilkara bidentata" = "blue",
               "Protium tenuifolium" = "plum3", "Voucapoua americana" = "blueviolet" ),
    breaks = c("Astrocaryum vulgare", "Euterpe oleraceae", "Oenocarpus distichus",
               "Jacaranda copaia", "Licania octandra", "Manilkara bidentata",
               "Protium tenuifolium", "Voucapoua americana")
  )

# retrieve species specific intercepts

# Load the broom package for tidying model results
install.packages("broom")
library(broom)
library(dplyr)

# Your existing ggplot code

# Fit separate linear models for each species
models <- by(VWC, VWC$species, function(subset) {
  lm(VWC_exp ~ ep_exp.sqrt, data = subset)
})

# Extract coefficients and tidy the results
coefficients_df <- bind_rows(lapply(names(models), function(species) {
  tidy(models[[species]]) %>%
    mutate(species = species)
}))

# Print or use coefficients_df as needed
print(coefficients_df)


# Create new intercept of one species (Licania) to simulate a new unknown species.

# Assuming you have already fitted the models and stored coefficients in coefficients_df

# Create a new data frame to store modified coefficients
modified_coefficients_df <- coefficients_df

# Modify intercepts for 'Licania octandra' and 'Jacaranda copaia'
modified_coefficients_df$estimate[modified_coefficients_df$term == "(Intercept)" & 
                                    modified_coefficients_df$species == "Licania_octandra"] <- 0.5

modified_coefficients_df$estimate[modified_coefficients_df$term == "(Intercept)" & 
                                    modified_coefficients_df$species == "Jacaranda_copaia"] <- 0.55
require(gridExtra)
# Testing introduction of new intercept on TTCM

# Assuming you have the 'new_df' dataframe

# Update the intercept for Jacaranda_copaia in the new dataframe
new_df <- VWC
new_df$intercept[new_df$species == 'Jacaranda_copaia'] <- 0.527

# Plot all species
plot(VWC_exp ~ ep_exp.sqrt, VWC, col = as.factor(species))

# Assuming you have the 'data' dataframe

# Create a new variable with the adjusted predicted values
new_df$adjusted_predictedVwc <- new_df$ep_exp.sqrt * 0.22 - 0.396


# Move intercept of Jacaranda_copaia from -0.396 to -0.527 (He - model)
new_df$adjusted_predictedVwc[new_df$species == 'Jacaranda_copaia'] <- new_df$adjusted_predictedVwc[new_df$species == 'Jacaranda_copaia'] + 0.131
new_df$adjusted_predictedVwc[new_df$species == 'Licania_octandra'] <- new_df$adjusted_predictedVwc[new_df$species == 'Licania_octandra'] + 0.200
new_df$adjusted_predictedVwc[new_df$species == 'Euterpe_oleraceae'] <- new_df$adjusted_predictedVwc[new_df$species == 'Euterpe_oleraceae'] + 0.131
# Plot the adjusted values
plot(VWC_exp ~ adjusted_predictedVwc, new_df, col = as.factor(new_df$species))

# Repeat each step
# 1) Predict values with TTCM
# 2) Shift intercept of Jacaranda, then plot and model
# 3) Shift intercept of Licania, then plot again and model again
p3 <- ggplot(mapping = aes(y = VWC_exp, x = adjusted_predictedVwc, colour = species2), data = new_df) +
  geom_point() +
  geom_abline(intercept = coef(fit_adjusted)[1], 
              slope = coef(fit_adjusted)[2], color = "gold",
              size = 1.1)+
  geom_smooth(method = "lm", se = FALSE) +
  theme_grey() +
  theme(legend.position = c(0.8, 0.25),
        legend.key.size = unit(0.01, "cm"),
        legend.text = element_text(face = "italic")
  ) +
  annotate('text', x = 0.15, y = 0.65, label = expression(paste(italic('RMSE'),'= 0.097')))+
  labs(colour = 'species') +
  xlab(expression(paste('FDR WC m'^'3', '/m'^'3'))) +
  ylab(expression(paste('StWC grav m'^'3', '/m'^'3'))) +
  scale_colour_manual(
    values = c("Astrocaryum vulgare" = 'darkolivegreen3', "Euterpe oleraceae" = 'green3', "Oenocarpus distichus" = 'forestgreen',
               "Jacaranda copaia" = 'blue4', "Licania octandra" = 'cyan', "Manilkara bidentata" = "blue",
               "Protium tenuifolium" = "plum3", "Voucapoua americana" = "blueviolet" ),
    breaks = c("Astrocaryum vulgare", "Euterpe oleraceae", "Oenocarpus distichus",
               "Jacaranda copaia", "Licania octandra", "Manilkara bidentata",
               "Protium tenuifolium", "Voucapoua americana")
  )
p3
ggsave('interc_shift3.png', plot = p3, height = 4, width = 6)

fit_adjusted <- lm(VWC_exp ~ adjusted_predictedVwc, new_df)
#abline(fit_adjusted)
summary(fit_adjusted)


#### rmse
predicted_values <- predict(fit_adjusted, newdata = new_df, na.rm = T)

# Calculate residuals (observed - predicted)
residuals <- new_df$VWC_exp - predicted_values

# Calculate RMSE
rmse <- sqrt(mean(residuals^2,na.rm = T))

# Print or use the RMSE value as needed
print(rmse)


# Repeating with Differenced OSM


new_df$diff_sqrt.ep_adjusted= NA
new_df$id = paste(new_df$species, new_df$sample)
for(id in unique(new_df$id)){
  rows = new_df$id == id
  workTable = new_df[rows,]
  new_df$diff_sqrt.ep_adjusted[rows] = c(NA,diff(workTable$adjusted_predictedVwc))
}

p4 <- ggplot(mapping = aes(y = diffSWC, x = diff_sqrt.ep_adjusted, colour = species2), data = new_df) +
  geom_point() +
  geom_abline(intercept = coef(fitOSM)[1], 
              slope = coef(fitOSM)[2], color = "gold",
             size = 1.1)+
  # geom_smooth(method = "lm", se = FALSE) +
  theme_grey() +
  theme(legend.position = c(0.8, 0.25),
        legend.key.size = unit(0.01, "cm"),
        legend.text = element_text(face = "italic")
  ) +
  annotate('text', x = -0.3, y = 0.35, label = expression(paste(italic('RMSE'),'= 0.07')))+
  labs(colour = 'species') +
  ylab(expression(paste('rate of change in StWC [m'^'3','/m'^'3',']')))+
  xlab(expression(paste('rate of change in FDR WC [m'^'3','/m'^'3',']')))+
  scale_colour_manual(
    values = c("Astrocaryum vulgare" = 'darkolivegreen3', "Euterpe oleraceae" = 'green3', "Oenocarpus distichus" = 'forestgreen',
               "Jacaranda copaia" = 'blue4', "Licania octandra" = 'cyan', "Manilkara bidentata" = "blue",
               "Protium tenuifolium" = "plum3", "Voucapoua americana" = "blueviolet" ),
    breaks = c("Astrocaryum vulgare", "Euterpe oleraceae", "Oenocarpus distichus",
               "Jacaranda copaia", "Licania octandra", "Manilkara bidentata",
               "Protium tenuifolium", "Voucapoua americana"))
  
p4
  ggsave('interc_shift4.png', plot = p4, height = 4, width = 6)


plot(diffSWC ~ diff_sqrt.ep_adjusted, new_df, col = as.factor(species))
fitOSM <- lm(diffSWC ~ diff_sqrt.ep_adjusted, new_df)
summary(fitOSM)
# Despite shifting a species on the intercept the OSM accuracy remains 
# unaffected in contrast to the TTCM's accuracy.

