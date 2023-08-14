library(ggplot2)

png("ggplot.png", width = 900, height = 750)
ggplot(iris) + geom_point(aes(x = Sepal.Length, y = Petal.Length,
                              size = Sepal.Width * 2, colour = Petal.Width, shape = Species)) +
  theme(text = element_text(size = 27))
dev.off()
