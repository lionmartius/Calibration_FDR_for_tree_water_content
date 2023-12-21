# Applying temperature effect on field data using FDR sensors in
# Manilkara bidentata 
fd_dicot_T <-read.csv('fd_dicot_T.csv')
########################## relative wc from intial value
# centre around initial value
# FDR
initial_FDR <- fd_dicot_T$wc_FDRAllM[1]
fd_dicot_T$FDR1CENT <- fd_dicot_T$wc_FDRAllM - initial_FDR

fd_dicot_T$date <- as.POSIXct(fd_dicot_T$date, tz = "GMT")
 

fd_dicot_T$wc_FDRAllM <- 1.83658 *as.numeric(fd_dicot_T$swc1) - 0.08189

sub_fd_dicot_T <- fd_dicot_T[1:1500,]
sub2_fd_dicot_T <- fd_dicot_T[1:4500,]

# ADD T effect

fd_dicot_T$tDiffM25 = as.numeric(fd_dicot_T$temperature1) - 25.24236
tEffect = -0.000974

fd_dicot_T$slope1_tmp = 
  as.numeric(fd_dicot_T$FDR1CENT) - fd_dicot_T$tDiffM25*tEffect

library(ggplot2)
library(gridExtra)
library(ggExtra)
library(patchwork)

tempplot <- ggplot(data = sub2_fd_dicot_T, aes(x = date)) +
  theme_bw() +
  geom_line(aes(y = FDR1CENT, color = "FDR1CENT"), lwd = 1) +
  geom_line(aes(y = slope1_tmp, color = "slope1_tmp"), lwd = 1) +
  scale_y_continuous(sec.axis = sec_axis(~ . * 100, name = expression('ΔT'['(25°C)']))) +
  geom_line(aes(y = tDiffM25 / 100, color = "tDiffM25 / 100")) +
  geom_line(aes(y = 0), color = 'black', lty = 'dashed', lwd = 1) +
labs(x = '',y=expression(paste('ΔStWC [m'^'3','/m'^'3',']'))) +
  scale_color_manual(name = NULL,values = c("FDR1CENT" = "#56B4E9", 
                                "slope1_tmp" = "#009E73", 
                               "tDiffM25 / 100" = "#D55E00", 
                               "0" = "black"),
                     labels = c("raw ΔStWC", "T-adjusted ΔStWC", "ΔT"))+
  theme(legend.position = 'top')


tempplot
ggsave("tempplot2.png", tempplot, width = 6, height = 4, dpi = 300)
  

precipplot <- ggplot(data = subset_metTorre[1:2249,], aes(x = TIMESTAMP))+
  geom_line(aes(y=Rain_mm_Tot))+
  theme_bw()+
  labs(y = 'precipitation [mm]', x = '')
precipplot

combined_plots <- tempplot / precipplot + plot_layout(
  heights=c(2.5,1),ncol = 1)
combined_plots
ggsave("temp_precip_plots1.png", combined_plots, width = 7, height = 6, dpi = 300)


write.csv(fd_dicot_T, file = 'fd_dicot_T.csv', row.names = F)
