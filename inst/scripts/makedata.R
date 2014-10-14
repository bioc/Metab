##### Examples #####

exampleMSLfile <- data.frame(read.csv("../extdata/ref_sol2_200513.msl", sep = "\t", header = FALSE))

exampleIonLib <-  buildLib("../extdata/ref_sol2_200513.msl", save = FALSE)

exampleAMDISReport <- data.frame(read.csv("../extdata/AMDIS_Report.TXT", sep = "\t"))

exampleMetReport <- MetReport( inputData = unzip(system.file("extdata/130513_REF_SOL2_2_50_50_1.CDF.zip", package = "Metab")), singleFile = TRUE, AmdisReport = exampleAMDISReport, abundance = "Area", save = FALSE )

exampleBiomass <- read.csv("../extdata/Biomass.csv", colClasses = "character")

exampleHtest  <- htest(exampleMetReport, save = FALSE, StatTest = "A")