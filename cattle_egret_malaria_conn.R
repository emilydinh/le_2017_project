# Mapping lineage locations


# Step 1 - set up data
# Import lineages & GPS data
lin_data <- read.csv("blast_lineages_neg.csv")
gps_loc <- read.csv("Location GPS.csv")

# Change AY-2014 lineage to Haemosporidia novel & remove species name associated with it
# First convert Lineage to character from factor
lin_data$Lineage <- as.character(lin_data$Lineage)
lin_data$Lineage[lin_data$Lineage=="AY-2014"] <- "Haemosporidia novel"
lin_data$Species[lin_data$Lineage=="Haemosporidia novel"] <- ""

# Rename "Location" column in lin_data to "General.Capture.Location" to match to gps_loc for merging
library(plyr)
lin_data <- rename(x=lin_data, replace=c("Location"="General.Capture.Location"))

# Merge "Species" & "Lineage" columns in lin_data
lin_data$Sp.Lin <- paste(lin_data$Species, lin_data$Lineage)

# Merge lin_data & gps_loc by General.Capture.Location to get lineages by location
lin_loc_data <- merge(x=lin_data, y=gps_loc, by="General.Capture.Location")

# Count the number of each lineage for each location
lin_count <- count(lin_loc_data, vars = c("Sp.Lin","General.Capture.Location"))

# Merge the count data frame to gps_loc to get number of each lineage/location
lin_loc <- merge(lin_count, gps_loc, by="General.Capture.Location")




# Step 2 - Create map

# Load packages
library(raster)
library(rgdal)
library(ggplot2)
library(ggrepel)

# Import state map of FL
FL_map <- readOGR(dsn=".", layer="Florida")

# Remove unneeded locations from gps_loc to make repelled labels on map
gps_loc <- gps_loc[-c(2,8:17), ]

# Create map
ggplot() +
  theme_bw() +
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.line.x = element_line(color = "black", size = 0.25),
    axis.line.y = element_line(color = "black", size = 0.25)) +
  scale_x_continuous(limits = c(-88, -80)) +
  scale_y_continuous(limits = c(24, 32)) +
  coord_fixed() +
  # Draw FL border
  geom_polygon(
    data = FL_map,
    aes(x = long, y = lat, group = group),
    fill = NA,
    col = "black") +
  # Plot the sites, make points & point labels blue, bold for now
  geom_point(
    data = lin_loc,
    aes(x = Long, y = Lat), 
    size = 3.0, pch = 16, color="blue") +
  geom_label_repel(
    data=gps_loc, aes(x=Long, y=Lat, label=General.Capture.Location), fontface="bold",
    col="blue", size=4, force=15, segment.color = "blue", segment.size = 1,
    box.padding = unit(0.1, "lines"), point.padding = unit(0.3, "lines")
    )
  # geom_text(data=lin_loc,aes(x=Long, y=Lat, label=General.Capture.Location), color="blue", nudge_x=0.1, nudge_y=-0.15, fontface="bold")
  # Save plot. I can't figure out how to put pie charts + map together, so I'll do them separately for now.
  ggsave("final_map.jpg", plot = last_plot(), dpi = 300, width = 20, height = 20, units = "cm")

  
# Color values, breaks, and labels for each lineage - colors in legend change with each pie chart using default settings
col_val <- c("pink","grey", "red","orange", "orange3","khaki", "yellow", "lightgreen",
             "green4", "lightblue","blue","navy", "mediumorchid", "purple4")
col_breaks <- unique(lin_loc$Sp.Lin)
col_labs <- col_breaks


  # Plot the count of each lineage for each site, changing name of General.Capture.Location to match
  bar <- ggplot(data=lin_loc, aes(x=General.Capture.Location, y=freq, fill=Sp.Lin)) +
    geom_bar(data=subset(lin_loc, General.Capture.Location=="Valparaiso"), width=1, stat="identity", color="black") +
    labs(fill= "Species & Lineage") +
    scale_fill_manual(values=col_val, breaks=col_breaks, labels=col_labs)

  pie <- bar + coord_polar(theta="y") + theme_minimal() +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      panel.grid=element_blank(),
      plot.title = element_text(hjust=0.5)) +
    ggtitle("Valparaiso")
  pie
  # Save plot.
  ggsave("final_pie_Valparaiso.jpg", plot=last_plot(), dpi=100, width=20, height=20, units="cm")



###########################################################################################################################

# read in cattle egret avian malaria sequences FASTA file
library("ShortRead")
caeg_avimal <- readFasta("CaEg_AviMal_seq.fasta")
# read sequences from FASTA file
seqs <- sread(caeg_avimal)

# Possible way of calculating sequence divergence:
# https://www.bioconductor.org/packages/devel/bioc/vignettes/seqcombo/inst/doc/seqcombo.html