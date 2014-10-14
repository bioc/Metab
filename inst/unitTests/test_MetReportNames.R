######## Test MetReportNames ###########
test_MetReportNames <- function (){
	data(exampleAMDISReport)
    results <- MetReportNames("130513_REF_SOL2_2_100_1", exampleAMDISReport, save = FALSE, TimeWindow = 0.5, base.peak = TRUE)
   	checkEqualsNumeric(as.numeric(results[1,2]), 162705504)
}
