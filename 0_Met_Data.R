#--------------------------------------------------------------
# Processing precipitation Data
# RAW Met Data
all_lines <- readLines('TORRE_LBA_Table.dat')

# Skip the first two lines
data_lines <- all_lines[-c(1, 3,4)]

# Create a text connection to read the remaining lines as a table
data_text <- textConnection(data_lines)
metTorre <- read.table(data_text, header = T, sep = ',')
# Close the text connection
close(data_text)


metTorre$TIMESTAMP <- as.POSIXct(metRAW$TIMESTAMP, tryFormats= c("%Y-%m-%d %H:%M:%OS", 
                                               "%Y/%m/%d %H:%M:%OS",
                                               "%Y-%m-%d %H:%M",
                                               "%Y/%m/%d %H:%M",
                                               "%Y-%m-%d",
                                               "%Y/%m/%d"), tz = 'GMT')
as.numeric(metTorre$TIMESTAMP[1])
as.POSIXct(1603193400, origin = "1970-01-01", tz = "GMT")
time = as.POSIXlt(met$time)
date$mon


par(mfrow = c(1,1))
plot(Rain_mm_Tot ~ TIMESTAMP, data = metRAW, ylim = c(0,40), type = 'l',
     ylab='precipitation [mm]', xlab='INFRA PA', col = 'blue', lwd = 1.5)
plot(Rain_mm_Tot ~ TIMESTAMP, data = metTorre, ylim = c(0,40), type = 'l',
     ylab='precipitation [mm]', xlab='TORRE LBA')

subset_metTorre <- metTorre[64324:67323, ]

# Create the plot using the subsetted data
plot(Rain_mm_Tot ~ TIMESTAMP, data = subset_metTorre, ylim = c(0, 40), type = 'l',
     ylab = 'precipitation [mm]', xlab = 'TORRE LBA')
plot(Rain_mm_Tot ~ TIMESTAMP, data = subset_metTorre, ylim = c(0, 40), type = 'l',
     ylab = 'precipitation [mm]', xlab = 'TORRE LBA', xaxt = 'n',
     col = 'blue', lwd = 1.5)

# Add custom x-axis tick positions and labels for every month
axis(1, 
     at = subset_metTorre$TIMESTAMP,
     labels = ifelse(format(subset_metTorre$TIMESTAMP, "%d") == "01", format(subset_metTorre$TIMESTAMP, "%b %Y"), ""),
     cex.axis = 0.7, las = 2
)


