######## Test removeFalsePositives ###########
test_removeFalsePositives <- function (){
    data(exampleMetReport)
    results <- removeFalsePositives(exampleMetReport, 50, save = FALSE)
    checkEqualsNumeric(nrow(results), 13)
}
