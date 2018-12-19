source("https://bioconductor.org/biocLite.R")
biocLite("DESeq2")
library(DESeq2)

#library("DESeq2")

countData<-read.table('counts.txt', header=TRUE)
rownames(countData)<-countData$Geneid


countData<-countData[, -1]
colnames(countData)<-sub(".*\\.bamfiles\\.([A-Za-z1-3]*)\\.coordSorted\\.bam", "\\1", colnames(countData))

sampleGroup<-sub(".$", "", colnames(countData)) # remove the last character (= replicate number) from the sample name to end up with subtype
colData<-data.frame(condition=factor(sampleGroup))

dds<-DESeqDataSetFromMatrix(countData, colData, formula(~condition))

dds<-DESeq(dds, betaPrior = TRUE)

res <-results(dds)

rld <-rlog(dds, blind=TRUE) # apply a regularized log transformation, ignoring information about experimental groups
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

TNBC_HER2 <-results(dds, contrast=c('condition', 'TNBC', 'HER2' ))

plot(res$log2FoldChange, TNBC_HER2$log2FoldChange, xlab='first analysis', ylab='second analysis')
