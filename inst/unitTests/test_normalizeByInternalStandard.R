######## Test normalizeByInternalStandard ###########
test_normalizeByInternalStandard <- function (){
    data(exampleMetReport)
    results <- normalizeByInternalStandard(exampleMetReport, internalStandard = "1-butanol", save = FALSE)
    checkEqualsNumeric(as.numeric(results[which(results[,1] == "1-butanol"),2]), 1)
}
