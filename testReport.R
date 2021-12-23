source("pdfReport.r")
load("../config/grids.rda")

records <- read.csv("../results/test02/test02_simulation_p24d2.csv", stringsAsFactors = FALSE)
locs   <- grids$p24d2[,c("x", "y")]

pdfReport(file = "test.pdf", records, locs, ps = 10)