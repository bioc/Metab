######## Test normalizeByBiomass ###########
test_normalizeByBiomass <- function (){
	data(exampleMetReport)
	data(exampleBiomass)
    results <- normalizeByBiomass(exampleMetReport, biomass = exampleBiomass, save = FALSE)
   	checkEqualsNumeric(as.numeric(results[2,2]), as.numeric(exampleMetReport[2,2])/0.5)
}
