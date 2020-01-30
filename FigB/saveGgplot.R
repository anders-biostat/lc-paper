library(ggplot2)

png("ggplot.png", width = 600, height = 500)
ggplot(iris) + geom_point(aes(x = Sepal.Length, y = Petal.Length,
                              size = Sepal.Width * 2, colour = Petal.Width, shape = Species))
dev.off()
