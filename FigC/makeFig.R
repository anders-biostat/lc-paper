library(tidyverse)
library(cowplot)
library(scales)

read_tsv("Raw_Data.txt") %>%
  filter(readout == "CTX") %>%
  select(Barcode, DRow, DCol, rawIntensity) -> rd

set.seed(123)
barcodes <- sample(rd$Barcode, 4)

rd %>%
  filter(Barcode %in% barcodes) %>%
  ggplot() + geom_raster(aes(DCol, DRow, fill = squish(rawIntensity, c(0, 150000)))) + facet_wrap(~Barcode) + 
  scale_fill_gradient2(low = "red", high = "blue", mid = "white", midpoint = 90000) +
  coord_fixed(0.75) + theme_void() + guides(fill = FALSE) -> plates

read_tsv("curves.csv") %>%
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

read_tsv("curves.csv") %>%
  select(DRUG_NAME, IC50, SLOPE, MAX, MIN, Max.Conc.tested, DSS) %>%
  filter(DSS > 20) %>%
  rowwise() %>%
  do(get_curve(.)) %>%
  ungroup() -> curves


n_plots <- 16 #let it be even
set.seed(1000)
drugs <- sample(unique(curves$DRUG_NAME), n_plots)

curves %>%
  filter(DRUG_NAME %in% drugs) %>%
  group_split(DRUG_NAME) %>%
  map(~ggplot(., aes(x = x, y = y)) + 
        geom_line(size = 1.5) + 
        geom_point(data = filter(points, DRUG_NAME %in% .$DRUG_NAME), size = 4, colour = "blue") +
        scale_x_log10() + 
        ylim(-30, 140) + 
        theme(panel.background = element_rect(fill = rgb(runif(1, 0.9), runif(1, 0.9), runif(1, 0.9), 1)),
              plot.background = element_rect(fill = "transparent"),
              axis.text.x = element_text(angle = 45, hjust = 1),
              panel.grid = element_blank()) +
        ggtitle(.$DRUG_NAME[1])) -> plotList


coords <- seq(0, 0.3, length.out = (n_plots + 2)) * rep(c(1, -1), times = n_plots/2 + 1) + rep(c(0, 0.6), times = n_plots/2 + 1) +
  rnorm(n_plots + 2, sd = 0.04)

curvePlots <- ggdraw()
for(i in 2:(n_plots + 1))
  curvePlots <- curvePlots + draw_plot(plotList[[i - 1]], coords[i], coords[i + sample(-1:1, 1)], 0.4, 0.4)

read_delim("dssTable_RTG", ";") -> fullTable

set.seed(500)
drugs <- sample(unique(fullTable$Drug), 50)

fullTable %>%
  filter(MainDrug == "DMSO", Drug %in% drugs) %>%
  select(CellLine, Drug, DSS) -> fullTable

