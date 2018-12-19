source("https://bioconductor.org/biocLite.R")
#biocLite("DESeq2")
#biocLite("pasilla")

if (!requireNamespace("BiocManager", quietly = TRUE)) 
  install.packages("BiocManager")
BiocManager::install("DESeq", version = "3.8")

browseVignettes("DESeq")

if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("pasilla", version = "3.8")

library(pasilla)
library(DESeq2) 
library(Biobase) 
library(ggplot2)

# http://bioconductor.org/packages/release/data/experiment/html/pasilla.html

# Bioconductor package DESeq http://bioconductor.org/packages/release/bioc/html/DESeq.html needs to be installed on your computer countData<-counts(pasillaGenes)
data("pasillaGenes")

countData <-counts(pasillaGenes)
colData <- pData(pasillaGenes)[,c("condition","type")]

str( fitInfo(pasillaGenes) )
pasillaGenes$condition

datafile = system.file( "extdata/pasilla_gene_counts.tsv", package="pasilla" )
# Exercise 1: DESeqDataSet

# browseVignettes("pasilla")

# sampleGroup <- sub(".$", "", colnames(countData))

  pasillaDesign = data.frame(
    row.names = colnames( pasillaCountTable ),
    condition = c( "untreated", "untreated", "untreated",
                   "untreated", "treated", "treated", "treated" ),
    libType = c( "single-end", "single-end", "paired-end",
                 "paired-end", "single-end", "paired-end", "paired-end" ) );
  
  
pairedSamples = pasillaDesign$libType == "paired-end"
countTable = pasillaCountTable[ , pairedSamples ]
condition = pasillaDesign$condition[ pairedSamples ]


dds <- DESeqDataSetFromMatrix(countData = countData, colData = coldata, design = ~ batch + condition)
