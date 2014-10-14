######## Test MetReportNames ###########
test_MetReport <- function (){
    data(exampleAMDISReport)
    f <- unzip(system.file("extdata/130513_REF_SOL2_2_50_50_1.CDF.zip", package = "Metab"))
    results <- MetReport(f, singleFile = TRUE, AmdisReport = exampleAMDISReport, abundance = "Area", save = FALSE)
    checkEqualsNumeric(as.numeric(results[2,2]), 2801759237)
}
