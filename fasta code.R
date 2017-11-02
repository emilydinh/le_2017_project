setwd("~/Classes/Landscape Ecology/Group Project")

#install.packages("seqinr")
library(seqinr)

install.packages("phangorn")
library(phangorn)

AMseq<-read.fasta("CaEg_AviMal_seq.fasta")

AMseq<-data.frame()
AMseq<-read.fasta("CaEg_AviMal_trimmed.fasta")
AMseq_df <- data.frame(AMseq)

phyDat(AMseq, type="DNA")
