load("oscc.rda")

library(pheatmap)

corMat <- cor(countMatrix, method = "spearman")
#png(filename = "pheatmap.png", width = 500, height = 500)
svg("pheatmap.svg", width = 4, height = 3.3)
pheatmap(corMat, show_rownames = F, show_colnames = F, border_color = NA)
dev.off()

colsums <- colSums(countMatrix)
normCounts <- t(log10(t(countMatrix)/colsums * 10^6 + 0.1))

set.seed(123)
normCounts <- normCounts[sample(nrow(normCounts), 8000), ]

xSample <- "PG123-N"
ySample <- "PG123-D"

#png(filename = "scatter.png", width = 500, height = 500)
svg("scatter.svg", width = 5, height = 5)
plot(normCounts[, xSample], normCounts[, ySample], cex = 0.5, ylab = "", xlab = "")
dev.off()

library(jsonlite)

writeLines(
  paste0("var hData = ",
    toJSON(list(corrMat = corMat, 
              countMatrix = normCounts, 
              sampleNames = colnames(normCounts), 
              geneNames = rownames(normCounts))), ";"),
  "heatmapData.js")
