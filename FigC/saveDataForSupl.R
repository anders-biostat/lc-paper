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


fullTable %>%
  left_join(select(rd, Screen = screen_id, Drug = ProductName, Barcode) %>% distinct()) %>%
  select(CellLine, Drug, minConc, D1:D5, IC50:Barcode) -> curves

rd %>%
  select(DWell, DRow, DCol, ProductName, Concentration, Barcode, rawIntensity) -> plates

corMat <- cor(scoreMatrix)
rownames(corMat) <- NULL
colnames(corMat) <- NULL
rownames(scoreMatrix) <- NULL
colnames(scoreMatrix) <- NULL
writeLines(c(paste0("drugs = ", toJSON(drugs), ";"),
             paste0("cellLines = ", toJSON(rownames(scoreMatrix)), ";"),
             paste0("corMat = ", toJSON(corMat), ";"),
             paste0("scoreMat = ", toJSON(scoreMatrix), ";")), "scoreData.js")

writeLines(c(paste0("curves = ", toJSON(curves), ";"),
             paste0("plates = ", toJSON(plates), ";")), "rawData.js")

drugTable %>%
  select(DRUG_NAME, D1:D5, Max.Conc.tested) %>%
  pivot_longer(D1:D5, values_to = "y") %>%
  separate(name, c("tmp", "x"), 1, convert = T) %>%
  select(-tmp) %>%
  mutate(x = Max.Conc.tested * 10^(x - 4)) %>%
  select(-Max.Conc.tested) -> points


get_curve <- function(data) {
  x <- seq(0, 4, length.out = 30)
  y <- data$MIN + (data$MAX - data$MIN)/ (1 + data$SLOPE * 10^(-x + log10(data$IC50/data$Max.Conc.tested) + 4))
  tibble(DRUG_NAME = data$DRUG_NAME, y = y, x = data$Max.Conc.tested * 10^(x - 3))
}
