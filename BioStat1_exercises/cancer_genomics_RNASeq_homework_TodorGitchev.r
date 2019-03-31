source("https://bioconductor.org/biocLite.R")
#biocLite("DESeq2")
#biocLite("pasilla")

if (!requireNamespace("BiocManager", quietly = TRUE)) 
  install.packages("BiocManager")
#BiocManager::install("DESeq", version = "3.8")

#browseVignettes("DESeq")
browseVignettes("DESeq2")

if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
#BiocManager::install("pasilla", version = "3.8")

library(pasilla)
library(DESeq2) 
library(DESeq) 
library(Biobase) 
library(ggplot2)

(datafile = system.file( "extdata/pasilla_gene_counts.tsv", package="pasilla" ))
pasillaCountTable = read.table( datafile, header=TRUE, row.names=1 )
head( pasillaCountTable )

pasillaDesign = data.frame(row.names = colnames( pasillaCountTable ), condition = c( "untreated", "untreated", "untreated", "untreated", "treated", "treated", "treated" ), libType = c( "single-end", "single-end", "paired-end", "paired-end", "single-end", "paired-end", "paired-end" ) )
pasillaDesign
pairedSamples = pasillaDesign$libType == "paired-end"
countTable = pasillaCountTable[ , pairedSamples ]
condition = pasillaDesign$condition[ pairedSamples ]

# http://bioconductor.org/packages/release/data/experiment/html/pasilla.html

# Bioconductor package DESeq http://bioconductor.org/packages/release/bioc/html/DESeq.html needs to be installed on your computer countData<-counts(pasillaGenes)
data("pasillaGenes")

(countData <- counts(pasillaGenes))
(colData <- pData(pasillaGenes)[,c("condition","type")])

# Exercise 1: Create a DESeqDataSet object 
# and run the DESeq2 analysis with ???condition??? as your explanatory variable (for the moment, we ignore ???type???)

colnames(countData)
# the same as colData$condition
#sampleGroup<-sub("...$", "", colnames(countData)) # remove the last 3 character (= replicate id) from the sample name to end up with subtype

length(colnames(countData))
length(rownames(countData))

dds<-DESeqDataSetFromMatrix(countData, colData, formula(~condition))
dds<-DESeq(dds, betaPrior = TRUE)
(res <-results(dds))

sizeFactors( estimateSizeFactors( dds ) )

str(countData)

countData["FBgn0261552",]

normNountData <- counts(dds, normalized=TRUE)
head(normNountData)
normNountData["FBgn0261552",]

str( fitInfo(estimateDispersions( dds )) )

(rld <-rlog(dds, blind=TRUE))
p<-plotPCA(rld, intgroup=c("condition"))
print(p, ntop=500)

plotMA(res, alpha=0.05)

library("RColorBrewer")
library("gplots")
select <- order(rowMeans(counts(dds,normalized=TRUE)),decreasing=TRUE)[1:50]
hmcol <- colorRampPalette(brewer.pal(9, "GnBu"))(100)
heatmap.2(assay(rld)[select,], col = hmcol, trace="none", margin=c(10,6),
          labCol=colnames(dds), cexRow = 1/log10(length(select)))

head(res)
head(counts(dds, normalized=TRUE))

citation("pasilla")


untreated_treated <-results(dds, contrast=c('condition', 'untreated', 'treated' ))

normNountData["FBgn0261552",]
untreated_treated["FBgn0261552",]

cds = newCountDataSet( countTable, condition )
head( fData(dds) )
resNbintest = nbinomTest( dds, "untreated", "treated" )

fc <- b/a
fc
log2fc <- log2(fc)

head(untreated_treated)
head(res)

head(colData)

plot(res$log2FoldChange, untreated_treated$log2FoldChange, xlab='first analysis', ylab='second analysis')


#Plot 2: Mean expression against log-fold change. Genes with p-adjusted below alpha will be shown in red, all others in grey

plotMA(res, alpha=0.05)

#Plot 3: Heatmap using the 50 most highly expressed genes

select <- order(rowMeans(counts(dds,normalized=TRUE)),decreasing=TRUE)[1:50]
hmcol <- colorRampPalette(brewer.pal(9, "GnBu"))(100)
heatmap.2(assay(rld)[select,], col = hmcol, trace="none", margin=c(10,6),
          labCol=colnames(dds), cexRow = 1/log10(length(select)))

#### Pairwise contrasts

#Now take a closer look at the results and the normalised count data:
  
head(res)
head(counts(dds, normalized=TRUE))

#We can explicitly specify which levels we want to compare, as follows:
  
#TNBC_HER2 <-results(dds, contrast=c('condition', 'TNBC', 'HER2' ))

#Let???s double-check that this really produces exactly the same results as before by comparing the log2 fold changes:
  
#plot(res$log2FoldChange, TNBC_HER2$log2FoldChange, xlab='first analysis', ylab='second analysis')

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
head(countTable)
condition
cds = newCountDataSet( countTable, condition )
cds = estimateSizeFactors( cds )
sizeFactors( cds )
head( counts( cds, normalized=TRUE ) )
cds = estimateDispersions( cds )
str( fitInfo(cds) )
plotDispEsts( cds )
head( fData(cds) )
res = nbinomTest( cds, "untreated", "treated" )
head(res)
plotMA(res)
hist(res$pval, breaks=100, col="skyblue", border="slateblue", main="")
resSig = res[ res$padj < 0.1, ]
head( resSig[ order(resSig$pval), ] )
head( resSig[ order( resSig$foldChange, -resSig$baseMean ), ] )
head( resSig[ order( -resSig$foldChange, -resSig$baseMean ), ] )
ncu = counts( cds, normalized=TRUE )[ , conditions(cds)=="untreated" ]
head(ncu)
head(cds)
plotMA(data.frame(baseMean = rowMeans(ncu),
                  log2FoldChange = log2( ncu[,2] / ncu[,1] )),
       col = "black")
head(ncu[,2])
dds <- DESeqDataSetFromMatrix(countData = countData, colData = coldata, design = ~ batch + condition)
