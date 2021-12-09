library(tidyverse)
library(cowplot)
library(scales)

read_tsv("Raw_Data.txt") %>%
  filter(readout == "CTX") %>%
  select(Barcode, DRow, DCol, rawIntensity) -> rd

set.seed(123)
barcodes <- sample(unique(rd$Barcode), 6)

rd %>%
  filter(Barcode %in% barcodes) %>% 
  ggplot() + geom_raster(aes(DCol, DRow, fill = squish(rawIntensity, c(0, 150000)))) + facet_wrap(~Barcode, nrow = 3) + 
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


n_plots <- 10 #let it be even
set.seed(1000)
drugs <- sample(unique(curves$DRUG_NAME), n_plots)

curves %>%
  filter(DRUG_NAME %in% drugs) %>%
  group_split(DRUG_NAME) %>%
  map(~ggplot(., aes(x = x, y = y)) + 
        geom_line() + 
        geom_point(data = filter(points, DRUG_NAME %in% .$DRUG_NAME), size = 3, colour = "#1a2c79") +
        scale_x_log10() + 
        ylim(-30, 140) + 
        theme(panel.background = element_rect(fill = rgb(runif(1, 0.9), runif(1, 0.9), runif(1, 0.9), 1)),
              #plot.background = element_rect(fill = "transparent"),
              axis.text.x = element_text(angle = 30, hjust = 1),
              panel.grid = element_blank(),
              axis.title.x = element_blank(),
              axis.title.y = element_blank()) +
        ggtitle(.$DRUG_NAME[1])) -> plotList


coords_x <- sample(seq(0, 0.6, length.out = 2 * n_plots), n_plots)
coords_y <- sample(seq(0, 0.6, length.out = 2 * n_plots), n_plots)
ord <- order(abs(coords_x - 0.3) + abs(coords_y - 0.3))
coords_x <- coords_x[ord]
coords_y <- coords_y[ord]

curvePlots <- ggdraw()
for(i in 1:n_plots)
  curvePlots <- curvePlots + draw_plot(plotList[[i]], coords_x[i], coords_y[i], 0.4, 0.4)

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

set.seed(600)
pairDrugs <- t(replicate(4, {sample(drugs, 2)}))

apply(pairDrugs, 1, function(pair) {
  fullTable %>%
    filter(Drug %in% pairDrugs) %>%
    pivot_wider(names_from = Drug, values_from = DSS) %>%
    ggplot() + geom_point(aes_string(x = pair[1], y = pair[2])) +
    geom_abline(slope = 1, intercept = 0) + ylim(0, 35) + xlim(0, 35)
}) %>% plot_grid(plotlist = .) -> cellLines


library(pheatmap)

fullTable %>%
  pivot_wider(names_from = Drug, values_from = DSS) %>%
  column_to_rownames("CellLine") %>%
  cor() %>%
  pheatmap() -> heatmap

plot_grid(plates, curvePlots, heatmap[[4]], cellLines, scale = 0.85, 
          labels = c("Raw readout", "Estimating drug scores", "Correlation heatmap", "Drug vs. Drug comparison"),
          nrow = 2, ncol = 2, label_fontface = "plain", label_x = c(0.3, 0.15, 0.15, 0.15), label_size = 19) + 
  draw_line(c(.48, .53), c(.77, .77), size = 2, arrow = arrow(), colour = "#1a2c79") + 
  draw_line(c(.77, .77), c(.55, .51), size = 2, arrow = arrow(), colour = "#1a2c79") +
  draw_line(c(.53, .48), c(.27, .27), size = 2, arrow = arrow(), colour = "#1a2c79") +
  draw_line(c(.53, .48), c(.73, .73), size = 2, arrow = arrow(), colour = "#fe4a49") + 
  draw_line(c(.73, .73), c(.51, .55), size = 2, arrow = arrow(), colour = "#fe4a49") +
  draw_line(c(.48, .53), c(.23, .23), size = 2, arrow = arrow(), colour = "#fe4a49") +
  draw_text(c("A", "D", "B", "C"), c(.05, .05, .55, .55), c(.99, .49, .99, .49), size = 25, fontface = "bold") -> plots

ggdraw(ylim = c(0.45, 0.55)) +
  draw_line(c(0.05, 0.1), c(0.5, 0.5), size = 2, arrow = arrow(), colour = "#1a2c79") +
  draw_line(c(0.55, 0.6), c(0.5, 0.5), size = 2, arrow = arrow(), colour = "#fe4a49") +
  draw_text("Data generation", .13, 0.5, hjust = 0, colour = "#1a2c79", size = 19) +
  draw_text("Data visualization", .63, 0.5, hjust = 0, colour = "#fe4a49", size = 19) -> legend
  
plot_grid(plots, legend, nrow = 2, rel_heights = c(0.95, 0.05)) -> figC

png("figC.png", width = 1500, height = 1500)
print(figC)
dev.off()

