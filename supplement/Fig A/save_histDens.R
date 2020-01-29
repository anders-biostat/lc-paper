library(rlc)

d <- rnorm(1000)

lc_hist(value = d)
lc_dens(value = d)
app <- getPage()
debugonce(app$.__enclos_env__$private$charts$Chart1$.__enclos_env__$private$sendProperties)
debugonce(app$.__enclos_env__$private$charts$Chart2$.__enclos_env__$private$sendProperties)
updateCharts()

