library(tidyverse)

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
  select(CellLine, Drug, DSS) -> fullTable

fullTable %>%
  pivot_wider(names_from = Drug, values_from = DSS) %>%
  column_to_rownames("CellLine") -> scoreMatrix

