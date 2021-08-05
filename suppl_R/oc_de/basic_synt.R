data("iris")
library("rlc")

#full code
lc_scatter(dat(
    x = Sepal.Length,
    y = Petal.Length,
    size = Sepal.Width * 2,
    colourValue = Petal.Width,
    symbolValue = Species),
  with = iris)

#minimal code
lc_scatter(dat(
    x = Sepal.Length,
    y = Petal.Length),
  with = iris)
