# Plots and figures

# Figure 2a

# PLOT VWC vs FDR signal ALL species
# Wood Density 

ggplot(mapping = aes(y = VWC_exp, 
                     x = VWC_FDR_exp, 
                     colour = density), 
       data = VWC) +
  scale_color_gradient(low = "darkgoldenrod2", 
                       high = "brown") +
  geom_point() +
  labs(color = expression(paste('WD (g/cm'^'3',')'))) +
  theme(legend.position = c(0.88, 0.32),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10)) +
  geom_smooth(data = Dicots, method = 'lm', se = FALSE, color = "forestgreen", size = 0.8) +
  geom_smooth(data = Palms, method = 'lm', se = FALSE, color = "burlywood4", size = 0.8) +
  geom_smooth(method = 'lm', se = T, color = 'black', size = 1.25) +
  #geom_encircle(aes(group = num > 173), color = "gray", linetype = 'dotted') +  # Draw encircle around points of the same species
  xlab(expression(paste('FDR WC m'^'3', '/m'^'3'))) +
  ylab(expression(paste('StWC grav m'^'3', '/m'^'3'))) +
  geom_text(data = Dicots, aes(label = "Dicots"), x = 0.35, y = 0.45, hjust = 1, vjust = 1, color = "forestgreen", size = 3.5) +
  geom_text(data = Palms, aes(label = "Palms"), x = 0.28, y = 0.5, hjust = 1, vjust = 0, color = "burlywood4", size = 3.5) +
  geom_text(aes(label = "All species"), x = 0.47, y = 0.85, hjust = 1, vjust = 1, color = "black", size = 3.5)
ggsave("density.png", dpi = 300, width = 6, height = 4)

# Fig 2b Individual species curve

VWC$species2 <- gsub("_", " ", VWC$species)
VWC$species2 <- as.factor(VWC$species2)

ggplot(mapping = aes(y = VWC_exp, x = VWC_FDR_exp, colour = species2), data = VWC) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_grey() +
  theme(legend.position = c(0.8, 0.25),
        legend.key.size = unit(0.01, "cm"),
        legend.text = element_text(face = "italic")
  ) +
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
ggsave("density.png", dpi = 300, width = 6, height = 4)


### Fig 3 will be found in seperate script dealing with the T experiment

# PLOT centred data (electric permittivity)
# TCCM (2023) vs He et al
#####  He et al
m_epHE <- lm(VWC_exp ~ ep_exp.sqrt, data = VWC)
m_epHE$coefficients[1] <- -0.527    # coefficient values are taken from He et al. 2021
m_epHE$coefficients[2] <- 0.2233


ggplot(mapping = aes(y = VWC_exp, x = ep_exp.sqrt), data = VWC)+
  geom_point(shape = 21, size = 2, fill = 'darkgoldenrod2')+
  geom_smooth(method = 'lm', se = T, colour = 'brown', size = 1.2)+
  geom_abline(intercept = coef(m_epHE)[1], 
              slope = coef(m_epHE)[2], color = "blue3",
              size = 1.1)+
  geom_text(data = VWC, aes(label = "He et al.(2021)"), 
            x = 5.5, y = 0.4, hjust = 1, vjust = 0, color = "blue3", size = 4)+
  geom_text(data = VWC, aes(label = "TTCM"), 
            x = 3, y = 0.77, hjust = 1, vjust = 0, color = "brown", size = 4)+
  annotate("text", x = 6, y = 0.32,
           label = expression(italic(theta) == 0.2233 ~ sqrt(epsilon) ~ "- 0.527" ~ (italic(R)^2 == 0.864)),
           hjust = 1, vjust = 0, color = "blue3", size = 3.8) +
  annotate("text", x = 4, y = 0.7,
           label = expression(italic(theta) == 0.2227 ~ sqrt(epsilon) ~ "- 0.396" ~ (italic(R)^2 == 0.844)),
           hjust = 1, vjust = 0, color = "brown", size = 3.8) +
  xlim(1.8, 6)+ ylim(0.04, 0.83)+
  ylab(expression(paste('StWC [m'^'3','/m'^'3',']')))+
  xlab(expression(sqrt('ε')))+
  theme_bw()
ggsave('TTCM_he.png',dpi = 300, width = 6, height = 4 )


# Fig. 5 One-slope model

ggplot(mapping = aes(y = diffSWC, x = diff_sqrt.ep), data = VWC)+
  geom_point(shape = 21, size = 2, fill = 'brown')+
  geom_smooth(method = 'lm', se = T, colour = 'darkgoldenrod2', size = 1.2)+
  annotate("text", x = 1.5, y = 0.4,
           label = expression(paste(italic('slope'),' = 0.2254' ~ (italic(R)^2 == 0.852))),
           hjust = 2.02, vjust = 2.7, color = "brown", size = 4) +
  ylab(expression(paste('rate of change in StWC [m'^'3','/m'^'3',']')))+
  xlab(expression(paste('rate of change in',phantom(' '),sqrt('ε'))))+
  theme_bw()
ggsave('differencing.png',dpi = 300, width = 6, height = 4 )


# Figure 1: Relationship between the FDR WC output and the 
# square root of electric permittivity from field data of 2 palm and 3 dicots. 

fd_dicot = read.csv("field_dt_dicot.csv", na.strings = c("#N/A"))
fd_palm = read.csv("field_dt_palm.csv", na.strings = c("#N/A"))
# Define color palette with distinct colorblind-friendly colors
pdf('sqrt_ep.pdf', width = 6, height = 4)
par(mar = c(6, 6, 4, 6))
custom_colors <- c("#1F77B4", "brown", "#2CA02C", "darkgreen", "blue", "#8C564B", "gold")
species_names <- c(expression(italic('Astrocaryum vulgare')), expression(italic('Oenocarpus distichus')), 
                   expression(italic('Manilkara bidentata')), expression(italic('Voucapoua americana')), 
                   expression(italic('Licania octandra')))


plot(VWC$VWC_FDR_exp ~ sqrt(VWC$ep_exp), pch = 19, cex = .5,
     xlab = expression(sqrt(epsilon)), ylab = expression(paste('FDR WC [m'^'3','/m'^'3',']')))
ep_fit <- lm(VWC$VWC_FDR_exp ~ VWC$ep_exp.sqrt)
abline(ep_fit, lwd = 1.5, col = custom_colors[5])
points(fd_palm$swcT ~ fd_palm$epT.sqrt, col = custom_colors[3], pch = 20, cex = 1.1)
points(fd_palm$swcB ~ fd_palm$epB.sqrt, col = 'darkolivegreen3', pch = 20, cex = 1.1)
points(fd_dicot$swc3 ~ fd_dicot$ep3.sqrt, col = custom_colors[7], pch = 20, cex = 1.1)
points(fd_dicot$swc1 ~ fd_dicot$ep1.sqrt, col = 'darkorchid1', pch = 20, cex = 1.1)
points(fd_dicot$swc2 ~ fd_dicot$ep2.sqrt, col = 'coral3', pch = 20, cex = 1.1)

legend('bottomright', legend = species_names, 
       col = c(custom_colors[3],'darkolivegreen3', custom_colors[7],'darkorchid1','coral3'), pch = 20, cex = 0.55)
dev.off()


# other plots tbf in other scripts
# these are the main result plots