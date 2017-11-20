# Mapping lineage locations


# Step 1 - set up data
# Import lineages & GPS data
lineages_data <- read.csv("blast_lineages_updated.csv")
gps_loc <- read.csv("Location GPS.csv")

# Rename "Location" column in lineages_data to "General.Capture.Location" to match to gps_loc for merging
library(plyr)
lineages_data <- rename(x=lineages_data, replace=c("Location"="General.Capture.Location"))

# Merge lineages_data & gps_loc by General.Capture.Location to get sequence lineages by location
seq_loc_data <- merge(x=lineages_data, y=gps_loc, by="General.Capture.Location")

# Count the number of each lineage for each location
lineage_count <- count(seq_loc_data, vars = c("Lineage","General.Capture.Location"))

# Merge the count data frame to gps_loc for number of each lineage/location
lineage_loc <- merge(lineage_count, gps_loc, by="General.Capture.Location")




# Step 2 - Create map

# Load packages
library(raster)
library(rgdal)
library(ggplot2)
library(ggrepel)

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
  scale_x_continuous(limits = c(-90, -74)) +
  scale_y_continuous(limits = c(24, 35)) +
  coord_fixed() +
  # Draw FL border
  geom_polygon(
    data = FL_map,
    aes(x = long, y = lat, group = group),
    fill = NA,
    col = "black") +
  # Plot the sites, make grey points for now
  geom_point(
    data = lineage_loc,
    aes(x = Long, y = Lat, fill="General.Capture.Location"), 
    size = 3.0, pch = 16, color="grey") +
  # Reformat the legend from having multiple points to 1 point representing capture locations. NOTE: can't figure out how to properly change
  # label of point from General.Capture.Location to Capture Location
  scale_fill_manual(name="Legend", values=c("Capture Location" = "grey")) +
  # Plot the count of each lineage for each site with at least one sample
  bar <- ggplot(lineage_loc, aes(x=General.Capture.Location, y=freq, fill=Lineage)) +
  geom_bar(width=1, stat="identity")

  pie <- bar + coord_polar(theta="y", start=0) + theme(axis.text = element_blank())
  plot(pie)
  
  
  
  geom_text_repel(
    data = lineage_loc,
    aes(x = Long, y = Lat, label = freq),
    fontface = "bold",
    col = "black",
    size = 4,
    segment.color = "blue",
    box.padding = unit(0.3, "lines"))

# Playing around with how to make a pie chart of lineages for each location
  bar <- ggplot(lineage_loc, aes(x=General.Capture.Location, y=freq, fill=Lineage)) +
    geom_bar(width=1, stat="identity")
  
  pie <- bar + coord_polar(theta="y", start=0) + theme(axis.text = element_blank())
  plot(pie)




###########################################################################################################################
lineage_loc$General.Capture.Location[1]

# read in cattle egret avian malaria sequences FASTA file
library("ShortRead")
caeg_avimal <- readFasta("CaEg_AviMal_seq.fasta")
# read sequences from FASTA file
seqs <- sread(caeg_avimal)

# Possible way of calculating sequence divergence:
https://www.bioconductor.org/packages/devel/bioc/vignettes/seqcombo/inst/doc/seqcombo.html