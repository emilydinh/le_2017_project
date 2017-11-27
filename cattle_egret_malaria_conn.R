# Mapping lineage locations


# Step 1 - set up data
# Import lineages & GPS data
lineages_data <- read.csv("blast_lineages_updated.csv")
gps_loc <- read.csv("Location GPS.csv")

# Change AY-2014 lineage to Haemosporidia novel
# First convert Lineage to character from factor
lineages_data$Lineage <- as.character(lineages_data$Lineage)
lineages_data$Lineage[lineages_data$Lineage=="AY-2014"] <- "Haemosporidia novel"

# Rename "Location" column in lineages_data to "General.Capture.Location" to match to gps_loc for merging
library(plyr)
lineages_data <- rename(x=lineages_data, replace=c("Location"="General.Capture.Location"))

# Merge "Species" & "Lineage" columns in lineages_data
lineages_data$Sp.Lin <- paste(lineages_data$Species, lineages_data$Lineage)

# Merge lineages_data & gps_loc by General.Capture.Location to get sequence lineages by location
seq_loc_data <- merge(x=lineages_data, y=gps_loc, by="General.Capture.Location")

# Count the number of each lineage for each location
lineage_count <- count(seq_loc_data, vars = c("Sp.Lin","General.Capture.Location"))

# Merge the count data frame to gps_loc for number of each lineage/location
lineage_loc <- merge(lineage_count, gps_loc, by="General.Capture.Location")




# Step 2 - Create map

# Load packages
library(raster)
library(rgdal)
library(ggplot2)

#Import state map of FL
FL_map <- readOGR(dsn=".", layer="Florida")

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
    data = lineage_loc,
    aes(x = Long, y = Lat), 
    size = 3.0, pch = 16, color="blue") +
  geom_text(data=lineage_loc,aes(x=Long, y=Lat, label=General.Capture.Location), color="blue", nudge_x=0.1, nudge_y=-0.15, fontface="bold")
  # Save plot. I can't fugure out how to put pie charts + map together, so I'll do them separately for now.
  ggsave("prelim_map.jpg", plot = last_plot(), dpi = 300, width = 20, height = 20, units = "cm")



  # Plot the count of each lineage for each site; doing 1 sample pie chart (Homestead) for now
  bar <- ggplot(data=lineage_loc, aes(x=General.Capture.Location, y=freq, fill=Sp.Lin)) +
  geom_bar(data=subset(lineage_loc, General.Capture.Location=="Homestead"), width=1, stat="identity") +
  labs(fill= "Species & Lineage")

  pie <- bar + coord_polar(theta="y") + theme_minimal() +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      panel.grid=element_blank(),
      plot.title = element_text(hjust=0.5)) +
    ggtitle("Homestead")
  pie
  # Save plot.
  ggsave("prelim_pie_Homestead.jpg", plot=last_plot(), dpi=100, width=20, height=20, units="cm")



###########################################################################################################################

# read in cattle egret avian malaria sequences FASTA file
library("ShortRead")
caeg_avimal <- readFasta("CaEg_AviMal_seq.fasta")
# read sequences from FASTA file
seqs <- sread(caeg_avimal)

# Possible way of calculating sequence divergence:
https://www.bioconductor.org/packages/devel/bioc/vignettes/seqcombo/inst/doc/seqcombo.html