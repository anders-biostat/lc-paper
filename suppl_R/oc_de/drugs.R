library(tidyverse)
library(rlc)

load("drugScreen.RData")

openPage(useViewer = FALSE, layout = "table1x3")

selDrugs <- colnames(scoreMatrix)[1:2]
selCellLine <- rownames(scoreMatrix)[1]

getCurve <- function(drug, cellLine, x) {
  row <- paste(cellLine, drug, sep = "_")
  curves[row, "min"] + (curves[row, "max"] - curves[row, "min"])/
    (1 + 10^(-(x - log10(curves[row, "IC50"]/curves[row, "minConc"])) * curves[row, "Slope"]))
}

lc_heatmap(value = cor(scoreMatrix),
  paddings = list(left = 40, top = 40, right = 50),
  clusterRows = TRUE,
  clusterCols = TRUE,
  showLegend = FALSE,
  on_click = function(d) {
    selDrugs <<-  colnames(scoreMatrix)[d]
    updateCharts()
  },
  place = "A1")

lc_scatter(dat(
  x = scoreMatrix[, selDrugs[1]],
  y = scoreMatrix[, selDrugs[2]],
  colour = ifelse(rownames(scoreMatrix) == selCellLine, "black", "grey"),
  axisTitleX = selDrugs[1],
  axisTitleY = selDrugs[2]), 
  width = 300,
  height = 250,
  title = "DSS value",
  domainX = c(0, 45),
  domainY = c(0, 45),
  paddings = list(bottom = 20),
  label = rownames(scoreMatrix),
  on_click = function(d) {
    selCellLine <<- rownames(scoreMatrix)[d]
    updateCharts()
  },
  place = "A2", chartId = "dssPlot"
)

lc_abLine(
  a = 1, b = 0,
  chartId = "dssPlot",
  addLayer = TRUE
)

x <- seq(0, 4, length.out = 50)
lc_line(dat(
  x = x,
  y = cbind(getCurve(selDrugs[1], selCellLine, x),
            getCurve(selDrugs[2], selCellLine, x)),
  colourValue = selDrugs),
  addColourScaleToLegend = FALSE,
  width = 300,
  height = 150,
  axisTitleX = "Concentration order",
  axisTitleY = "Cell viability, %",
  paddings = list(bottom = 20),  
  legend_container = "#A2",
  legend_width = 300,
  chartId = "viability", place = "A2"
)

lc_scatter(dat(
  x = c(0:4, 0:4),
  y = c(unlist(curves[paste(selCellLine, selDrugs[1], sep = "_"), paste0("D", 1:5)]),
        unlist(curves[paste(selCellLine, selDrugs[2], sep = "_"), paste0("D", 1:5)])),
  title = selCellLine,
  colourValue = rep(selDrugs, each = 5)),
  on_mouseover = function(d) {
    well <- curves[paste(selCellLine, selDrugs[(d > 5) + 1], sep = "_"), paste0("W", d %% 5 + 1)]
    row <- which(LETTERS %in% str_sub(well, 1, 1))
    col <- str_sub(well, 2)
    mark(c(row, col), str_c("plate", (d > 5) + 1), clear = TRUE)
  },
  on_mouseout = function() {
    mark(clear = TRUE, chartId = c("plate1", "plate2"))
  },
  addLayer = TRUE, chartId = "viability"
)

getInfo <- function(drug) {
  plates %>% 
    filter(Barcode == barcodes[paste(selCellLine, drug, sep = "_")]) %>%
    transmute(DRow, DCol, text = str_c(
      "<b>", ProductName, "</b><br>",
      "Concentration: ", str_replace_na(Concentration), "<br>",
      "Value: ", rawIntensity
    )) %>%
    pivot_wider(names_from = DCol, values_from = text) %>%
    select(-DRow) %>%
    as.matrix()
}

getPlate <- function(drug) {
  plates %>% 
    filter(Barcode == barcodes[paste(selCellLine, drug, sep = "_")]) %>%
    select(DRow, DCol, rawIntensity) %>%
    pivot_wider(names_from = DCol, values_from = rawIntensity) %>%
    select(-DRow) %>%
    as.matrix()
}

placeHeatmap <- function(i) {
  lc_heatmap(dat(
    title = paste0("Plate ", barcodes[paste(selCellLine, selDrugs[ind], sep = "_")]),
    value = getPlate(selDrugs[ind]),
    informText = getInfo(selDrugs[ind])),
  width = 300,
  height = 225,
  showPanel = FALSE,
  titleSize = 15,
  paddings = list(top = 40, left = 15, bottom = 5, right = 5),
  rowLabel = LETTERS[1:16],
  showLegend = FALSE,
  place = "A3", chartId = paste0("plate", i),
  with = list(ind = i)
  )
}

for(i in 1:2)
  placeHeatmap(i)


#minimal code

openPage(useViewer = FALSE, layout = "table1x3")

selDrugs <- colnames(scoreMatrix)[1:2]
selCellLine <- rownames(scoreMatrix)[1]

getCurve <- function(drug, cellLine, x) {
  row <- paste(cellLine, drug, sep = "_")
  curves[row, "min"] + (curves[row, "max"] - curves[row, "min"])/
    (1 + 10^(-(x - log10(curves[row, "IC50"]/curves[row, "minConc"])) * curves[row, "Slope"]))
}

lc_heatmap(value = cor(scoreMatrix),
           on_click = function(d) {
             selDrugs <<-  colnames(scoreMatrix)[d]
             updateCharts()
           },
           place = "A1")

lc_scatter(dat(
  x = scoreMatrix[, selDrugs[1]],
  y = scoreMatrix[, selDrugs[2]]), 
  width = 300,
  height = 250,
  paddings = list(bottom = 20),
  label = rownames(scoreMatrix),
  on_click = function(d) {
    selCellLine <<- rownames(scoreMatrix)[d]
    updateCharts()
  },
  place = "A2"
)

lc_scatter(dat(
  x = c(0:4, 0:4),
  y = c(curves[paste(selCellLine, selDrugs[1], sep = "_"), paste0("D", 1:5)],
        curves[paste(selCellLine, selDrugs[2], sep = "_"), paste0("D", 1:5)]),
  colourValue = rep(selDrugs, each = 5)),
  width = 300,
  height = 150,
  place = "A2", chartId = "viability"
)

x <- seq(0, 4, length.out = 50)
lc_line(dat(
  x = x,
  y = cbind(getCurve(selDrugs[1], selCellLine, x),
            getCurve(selDrugs[2], selCellLine, x)),
  colourValue = selDrugs),
  chartId = "viability", addLayer = TRUE
)

getInfo <- function(drug) {
  plates %>% 
    filter(Barcode == barcodes[paste(selCellLine, drug, sep = "_")]) %>%
    transmute(DRow, DCol, text = str_c(
      "<b>", ProductName, "</b><br>",
      "Concentration: ", str_replace_na(Concentration), "<br>",
      "Value: ", rawIntensity
    )) %>%
    pivot_wider(names_from = DCol, values_from = text) %>%
    select(-DRow) %>%
    as.matrix()
}

getPlate <- function(drug) {
  plates %>% 
    filter(Barcode == barcodes[paste(selCellLine, drug, sep = "_")]) %>%
    select(DRow, DCol, rawIntensity) %>%
    pivot_wider(names_from = DCol, values_from = rawIntensity) %>%
    select(-DRow) %>%
    as.matrix()
}

placeHeatmap <- function(i) {
  lc_heatmap(dat(
    value = getPlate(selDrugs[ind]),
    informText = getInfo(selDrugs[ind])),
    width = 300,
    height = 225,
    paddings = list(top = 40, left = 15, bottom = 5, right = 5),
    rowLabel = LETTERS[1:16],
    place = "A3", chartId = paste0("plate", i),
    with = list(ind = i)
  )
}

for(i in 1:2)
  placeHeatmap(i)
