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




###########################################################################################################################


# read in cattle egret avian malaria sequences FASTA file
library("ShortRead")
caeg_avimal <- readFasta("CaEg_AviMal_seq.fasta")
# read sequences from FASTA file
seqs <- sread(caeg_avimal)

# Possible way of calculating sequence divergence:
https://www.bioconductor.org/packages/devel/bioc/vignettes/seqcombo/inst/doc/seqcombo.html