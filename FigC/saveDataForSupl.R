library(tidyverse)
library(jsonlite)

read_delim("dssTable_RTG", ";") %>%
  filter(MainDrug == "DMSO") %>%
  mutate(Drug = str_replace_all(Drug, "-", "_")) %>%
  mutate(Drug = str_replace_all(Drug, " ", "")) -> drugTable

drugTable %>%
  group_by(Drug) %>%
  summarise(mean = mean(DSS)) %>%
  arrange(desc(mean)) %>%
  head(n = 50) %>%
  pull(Drug) -> drugs

drugTable %>%
  filter(Drug %in% drugs) %>%
  mutate(Screen = tolower(Screen)) %>%
  mutate(Screen = str_replace_all(Screen, "-", "_")) %>%
  mutate(Screen = str_remove(Screen, "^mda_")) -> fullTable

fullTable %>%
  select(CellLine, Drug, DSS) %>%
  pivot_wider(names_from = Drug, values_from = DSS) %>%
  column_to_rownames("CellLine") -> scoreMatrix
scoreMatrix <- scoreMatrix[, drugs]

read_csv("all_raw.csv") %>%
  mutate(screen_id = tolower(screen_id)) %>%
  mutate(screen_id = str_replace_all(screen_id, "-", "_")) %>%
  filter(screen_id %in% fullTable$Screen) %>%
  distinct() %>% 
  mutate(ProductName = str_replace_all(ProductName, "-", "_")) %>%
  mutate(ProductName = str_replace_all(ProductName, " ", "")) -> rd


select(rd, Screen = screen_id, Drug = ProductName, Barcode, well = DWell, Concentration) %>%
  filter(!is.na(Concentration), !(Drug %in% c("BzCl", "dmso"))) %>%
  group_by(Drug, Screen) %>%
  mutate(conc_rank = str_c("W", rank(Concentration))) %>%
  select(-Concentration) %>%
  pivot_wider(names_from = conc_rank, values_from = well) -> wells

fullTable %>%
  left_join(wells) %>%
  select(CellLine, Drug, minConc, D1:D5, IC50:W1) -> curves

rd %>%
  select(DWell, DRow, DCol, ProductName, Concentration, Barcode, rawIntensity) -> plates

corMat <- cor(scoreMatrix)
cellLines <- rownames(scoreMatrix)

rownames(corMat) <- NULL
colnames(corMat) <- NULL
rownames(scoreMatrix) <- NULL
colnames(scoreMatrix) <- NULL

writeLines(c(paste0("drugs = ", toJSON(drugs), ";"),
             paste0("cellLines = ", toJSON(cellLines), ";"),
             paste0("corMat = ", toJSON(corMat), ";"),
             paste0("scoreMat = ", toJSON(scoreMatrix), ";")), "scoreData.js")

writeLines(c(paste0("curves = ", toJSON(curves), ";"),
             paste0("plates = ", toJSON(plates), ";")), "rawData.js")

rownames(scoreMatrix) <- str_replace_all(rownames(scoreMatrix), "-", "_")

curves %>%
  mutate(CellLine = str_replace_all(CellLine, "-", "_")) %>%
  unite(id, CellLine, Drug) %>%
  column_to_rownames("id") %>%
  as.data.frame() -> curves

barcodes <- curves$Barcode
names(barcodes) <- rownames(curves)
curves$Barcode <- NULL
curves <- as.matrix(curves)

save(plates, barcodes, curves, scoreMatrix, file = "drugScreen.RData")
