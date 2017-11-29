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
  # Save plot. I can't figure out how to put pie charts + map together, so I'll do them separately for now.
  ggsave("final_map.jpg", plot = last_plot(), dpi = 300, width = 20, height = 20, units = "cm")

# List of lineages to refer to in specifying color values & limits below. Strings
# must match for things to work
lin_list <- unique(lin_data$Sp.Lin)

# Specify color values and limits for each lineage - colors in legend
# change with each pie chart using default settings
col_val <- c(" Haemosporidia novel" = "white", 
             " Negative" = "grey50", 
             "Leucocytozoon sp. Isolate B30" = "lightpink",
             "Plasmodium elongatum Clone Sd6022A" = "red",
             "Plasmodium elongatum Isolate DENVID02" = "orange",
             "Plasmodium paranucleophilum Strain CPCT57" = "darkorange3",
             "Plasmodium sp. Clone G18" = "yellow",
             "Plasmodium sp. Cluster D Isolate CRAM 2278" = "green",
             "Plasmodium sp. Cluster I Isolate IPRAM-ES114" = "forestgreen",
             "Plasmodium sp. G21" = "lightblue",
             "Plasmodium sp. Isolate ERU-375P" = "royalblue", 
             "Plasmodium sp. Isolate PIPCHL01" = "navy",
             "Plasmodium sp. NYCNYC01" = "purple4")

Homestead_lim <- c(" Haemosporidia novel", " Negative", "Leucocytozoon sp. Isolate B30",
                   "Plasmodium elongatum Clone Sd6022A", "Plasmodium elongatum Isolate DENVID02",
                   "Plasmodium sp. Cluster I Isolate IPRAM-ES114", "Plasmodium sp. G21")

Milton_lim <- c(" Haemosporidia novel", " Negative", "Leucocytozoon sp. Isolate B30",
                "Plasmodium elongatum Isolate DENVID02", "Plasmodium paranucleophilum Strain CPCT57",
                "Plasmodium sp. Clone G18", "Plasmodium sp. Cluster D Isolate CRAM 2278",
                "Plasmodium sp. G21", "Plasmodium sp. NYCNYC01")

Jax_lim <- c(" Negative", "Leucocytozoon sp. Isolate B30",
             "Plasmodium elongatum Isolate DENVID02", "Plasmodium sp. Clone G18",
             "Plasmodium sp. Cluster D Isolate CRAM 2278", "Plasmodium sp. G21",
             "Plasmodium sp. Isolate ERU-375P")
  

Tampa_lim <- c(" Negative", "Leucocytozoon sp. Isolate B30",
               "Plasmodium elongatum Isolate DENVID02", "Plasmodium sp. Isolate PIPCHL01")

KW_lim <- c(" Negative", "Leucocytozoon sp. Isolate B30", "Plasmodium sp. G21",
            "Plasmodium sp. Isolate ERU-375P")

V_lim <- c(" Negative", "Leucocytozoon sp. Isolate B30",
           "Plasmodium paranucleophilum Strain CPCT57",
           "Plasmodium sp. Clone G18",
           "Plasmodium sp. Cluster D Isolate CRAM 2278",
           "Plasmodium sp. Isolate PIPCHL01")

  # Plot the count of each lineage for each site, changing name of General.Capture.Location to match
  # Homestead
  Homestead_bar <- ggplot(data=lin_loc, aes(x=General.Capture.Location, y=freq, fill=Sp.Lin)) +
    geom_bar(data=subset(lin_loc, General.Capture.Location=="Homestead"), width=1, stat="identity", color="black") +
    labs(fill= "Species & Lineage") +
    scale_fill_manual(values=col_val, limits=Homestead_lim)
  
  Homestead_pie <- Homestead_bar + coord_polar(theta="y") + theme_minimal() +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      panel.grid=element_blank(),
      plot.title = element_text(hjust=0.5)) +
    ggtitle("Homestead")
  Homestead_pie
  # Save plot.
  ggsave("final_pie_Homestead.jpg", plot=last_plot(), dpi=100, width=20, height=20, units="cm")

  
  
  # Milton
  Milton_bar <- ggplot(data=lin_loc, aes(x=General.Capture.Location, y=freq, fill=Sp.Lin)) +
    geom_bar(data=subset(lin_loc, General.Capture.Location=="Milton"), width=1, stat="identity", color="black") +
    labs(fill= "Species & Lineage") +
    scale_fill_manual(values=col_val, limits=Milton_lim)
  
  Milton_pie <- Milton_bar + coord_polar(theta="y") + theme_minimal() +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      panel.grid=element_blank(),
      plot.title = element_text(hjust=0.5)) +
    ggtitle("Milton")
  Milton_pie
  # Save plot.
  ggsave("final_pie_Milton.jpg", plot=last_plot(), dpi=100, width=20, height=20, units="cm")
  
  
  
  # Jacksonville
  Jax_bar <- ggplot(data=lin_loc, aes(x=General.Capture.Location, y=freq, fill=Sp.Lin)) +
    geom_bar(data=subset(lin_loc, General.Capture.Location=="Jacksonville"), width=1, stat="identity", color="black") +
    labs(fill= "Species & Lineage") +
    scale_fill_manual(values=col_val, limits=Jax_lim)

  Jax_pie <- Jax_bar + coord_polar(theta="y") + theme_minimal() +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      panel.grid=element_blank(),
      plot.title = element_text(hjust=0.5)) +
    ggtitle("Jacksonville")
  Jax_pie
  # Save plot.
  ggsave("final_pie_Jacksonville.jpg", plot=last_plot(), dpi=100, width=20, height=20, units="cm")
  
  
  # Tampa
  Tampa_bar <- ggplot(data=lin_loc, aes(x=General.Capture.Location, y=freq, fill=Sp.Lin)) +
    geom_bar(data=subset(lin_loc, General.Capture.Location=="Tampa"), width=1, stat="identity", color="black") +
    labs(fill= "Species & Lineage") +
    scale_fill_manual(values=col_val, limits=Tampa_lim)
  
  Tampa_pie <- Tampa_bar + coord_polar(theta="y") + theme_minimal() +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      panel.grid=element_blank(),
      plot.title = element_text(hjust=0.5)) +
    ggtitle("Tampa")
  Tampa_pie
  # Save plot.
  ggsave("final_pie_Tampa.jpg", plot=last_plot(), dpi=100, width=20, height=20, units="cm")
  
  
  # Key West
  KW_bar <- ggplot(data=lin_loc, aes(x=General.Capture.Location, y=freq, fill=Sp.Lin)) +
    geom_bar(data=subset(lin_loc, General.Capture.Location=="Key West"), width=1, stat="identity", color="black") +
    labs(fill= "Species & Lineage") +
    scale_fill_manual(values=col_val, limits=KW_lim)
  
  KW_pie <- KW_bar + coord_polar(theta="y") + theme_minimal() +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      panel.grid=element_blank(),
      plot.title = element_text(hjust=0.5)) +
    ggtitle("Key West")
  KW_pie
  # Save plot.
  ggsave("final_pie_Key_West.jpg", plot=last_plot(), dpi=100, width=20, height=20, units="cm")
  
  
  # Valparaiso
  V_bar <- ggplot(data=lin_loc, aes(x=General.Capture.Location, y=freq, fill=Sp.Lin)) +
    geom_bar(data=subset(lin_loc, General.Capture.Location=="Valparaiso"), width=1, stat="identity", color="black") +
    labs(fill= "Species & Lineage") +
    scale_fill_manual(values=col_val, limits=V_lim)
  
  V_pie <- V_bar + coord_polar(theta="y") + theme_minimal() +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      panel.grid=element_blank(),
      plot.title = element_text(hjust=0.5)) +
    ggtitle("Valparaiso")
  V_pie 
  # Save plot.
  ggsave("final_pie_Valparaiso.jpg", plot=last_plot(), dpi=100, width=20, height=20, units="cm")
  