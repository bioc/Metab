htest <-
function(inputData, signif.level = 0.05, log.transform = TRUE, save = TRUE, folder, StatTest, output, adjust.pValue = TRUE, method = "bonferroni") {
	## Function to check if file is CSV ##
	isCSVdlg <- function(titleMSG, errorMSG) {
		t = 0
		while (t == 0) {
			checkIfCsv <- dlgOpen(title = titleMSG, multiple = FALSE)$res
			checkIfCsv2 <- basename(checkIfCsv)
			checkIfCsv3 <- unlist(strsplit(checkIfCsv2, "\\."))
			checkIfCsv4 <- checkIfCsv3[length(checkIfCsv3)]
			if (checkIfCsv4 %in% c("CSV", "csv")) {
				t = 1
				return(checkIfCsv)
			} else {
				dlgMessage(errorMSG)
			}
		}
	}
	#########################################################
	
	#### No dialog box ###
	isCSV <- function(pathFile, errorMSG) {
		t = 0
		checkIfCsv <- pathFile
		checkIfCsv2 <- basename(checkIfCsv)
		checkIfCsv3 <- unlist(strsplit(checkIfCsv2, "\\."))
		checkIfCsv4 <- checkIfCsv3[length(checkIfCsv3)]
		if (checkIfCsv4 %in% c("CSV", "csv")) {
			t = 1
			return(t)
		} else {
			#dlgMessage(errorMSG)
			return(t)
		}
	}
	#########################################################
    
	#### t-test ##########
	test.t <- function() {
		message(paste("t-test is in progress..", "\n"))
		htest <- HtestData[-1]
		missing <- t(apply(htest, 1, function(y) tapply(as.numeric(y), reps, function(x) sum(!is.na(x)))))
		missing2 <- apply(t(apply(missing, 1, function(x) x < 2)), 1, sum)
		htest2 <- htest[missing2 == 0, ]
		rest <- htest[missing2 == 1, ]
		if (nrow(rest) != 0) {
			rest$pvalues <- 0
		}
		if (log.transform) {
			negative <- htest2
			negative[is.na(negative)] = 0
			negative <- data.frame(apply(negative, 1, function(x) sum(x < 0)))
			negative <- apply(negative, 2, function(x) sum(x > 0))
			if (negative > 0) {
				message("log.transform was set to TRUE. However, you have negative values in your input data and it is not possible to calculate the log of a negative value. Thus, the test will be performed with no log transformation.")
			} else {
				htest2[is.na(htest2)] <- 0
				htest2 <- data.matrix(htest2)
				htest2 <- log10(htest2)
				htest2[!is.finite(htest2)] <- NA
			}
		}
		list.p <- apply(htest2, 1, function(x) t.test(as.numeric(x) ~ reps))
		list.p <- unlist(list.p)
		t <- 3
		htest2 <- data.frame(htest2)
		for (i in 1:nrow(htest2)) {
			htest2$pvalues[i] <- list.p[t]
			t <- t + 11
		}
		if (nrow(rest) != 0) {
		  names(htest2) <- names(rest)
		}
		htest3 <- rbind(htest2, rest)
		HtestData <- merge(HtestData, htest3[ncol(htest3)], by = 0)
		#HtestData <- subset(HtestData, as.numeric(HtestData$pvalues) < signif.level)
		HtestData[1] <- NULL
		return(HtestData)
		message("Done.\n")
	}

	#### ANOVA ###########
	anova.t <- function() {
		message(paste("ANOVA is in progress..", "\n"))
		htest <- HtestData[-1]
		missing <- t(apply(htest, 1, function(y) tapply(as.numeric(y), reps, function(x) sum(!is.na(x)))))
		missing2 <- apply(t(apply(missing, 1, function(x) x < 2)), 1, sum)
		htest2 <- htest[missing2 < (length(levels(reps)) - 1), ]
		rest <- htest[missing2 %in% c((length(levels(reps)) - 1):(length(levels(reps)) - 1)), ]
		if (nrow(rest) != 0) {
			rest$pvalues <- 0
		}
		if (log.transform) {
			negative <- htest2
			negative[is.na(negative)] = 0
			negative <- data.frame(apply(negative, 1, function(x) sum(x < 0)))
			negative <- apply(negative, 2, function(x) sum(x > 0))
			if (negative > 0) {
				message("log.transform was set to TRUE. However, you have negative values in your input data and it is not possible to calculate the log of a negative value. Thus, the test will be performed with no log transformation.")
			} else {
				htest2 <- data.matrix(htest2)
				htest2 <- log10(htest2)
			}
		}
		lm.list <- apply(htest2, 1, function(x) lm(as.numeric(x) ~ reps))
		anovaLM.list <- lapply(lm.list, anova)
		pvalues <- data.frame(anovaLM.list)
		t <- 5
		htest2 <- data.frame(htest2)
		for (i in 1:nrow(htest2)) {
			htest2$pvalues[i] <- pvalues[1, t]
			t <- t + 5
		}
		if (nrow(rest) != 0) {
		  names(htest2) <- names(rest)
		}
		htest3 <- rbind.data.frame(htest2, rest)
		HtestData <- merge(HtestData, htest3[ncol(htest3)], by = 0)
		#HtestData <- subset(HtestData, as.numeric(HtestData$pvalues) < signif.level)
		HtestData[1] <- NULL
		return(HtestData)
		message("Done.\n")
	}

	## Begin collecting arguments
	if (missing(inputData)) {
		inputData <- isCSVdlg("Select the CSV file containing the input data", "The input file MUST be in the format of comma-separated value (csv). Please, choose an input file showing the extension .csv.")
		HtestData <- read.csv(inputData, colClasses = "character", check.names = FALSE)
		message("Input file loaded...")
	} else {
		if (is.data.frame(inputData)) {
			HtestData <- inputData
			message("Data frame loaded...")
		} else {
			if (is.character(inputData)) {
				checkIfCsv <- isCSV(inputData)
				if (checkIfCsv == 1) {
					inputTest <- file.access(inputData, 0)
					if (inputTest == 0) {
						HtestData = read.csv(inputData, colClasses = "character", check.names = FALSE)
					} else {
						inputData <- isCSVdlg("Select the CSV file containing the input data", "The input file MUST be in the format of comma-separated value (csv). Please, choose an input file showing the extension .csv.")
						HtestData <- read.csv(inputData, colClasses = "character", check.names = FALSE)
						message("Input file loaded...")
					}
				} else {
					inputData <- isCSVdlg("Select the CSV file containing the input data", "The input file MUST be in the format of comma-separated value (csv). Please, choose an input file showing the extension .csv.")
					HtestData <- read.csv(inputData, colClasses = "character", check.names = FALSE)
					message("Input file loaded...")
				}
			} else {
				inputData <- isCSVdlg("Select the CSV file containing the input data", "The input file MUST be in the format of comma-separated value (csv). Please, choose an input file showing the extension .csv.")
				HtestData <- read.csv(inputData, colClasses = "character", check.names = FALSE)
				message("Input file loaded...")
			}
		}
	}

	if (save) {
		if (missing(folder)) {
			folder = dlgDir(title = "Select the folder where the output file will be saved.")$res
		} else {
			if (is.character(folder)) {
				isFolder <- file.access(as.character(folder), 0)
				if (isFolder == 0) {
					isFolder <- file.info(folder)
					if (!isFolder$isdir) {
						message("The folder defined to save the results is not a valid path.")
						folder = dlgDir(title = "Select the folder where the output file will be saved.")$res
					}
				} else {
					message("The folder defined to save the results is not a valid path.")
					folder = dlgDir(title = "Select the folder where the output file will be saved.")$res
				}
			} else {
				message("The path to the folder where the results will be saved must be specified as character.")
				folder = dlgDir(title = "Select the folder where the output file will be saved.")$res
			}
		}
	}

	if (toupper(HtestData[1, 1]) == "REPLICATES") {
		replicates <- as.character(HtestData[1, ])
		HtestData <- HtestData[-1, ]
		rep <- 1
		reps <- factor(replicates[-1])
	} else {
		# Indicate replicates
		message("For a faster process of pipeLine, the first row of the input data can be used to indicate which experimental condition each sample bellongs to. For this, in the inputData you must add a new row below the header and fill it with the following information: the word 'Replicates' in the first column and the name of the experiemntal condition correponding to each sample (or replicate) in the following columns. See data(papiResults) for an example of input data.")
		HtestData <- rbind(c("Replicates", 2:ncol(HtestData)), HtestData)
		conditions <- as.numeric(dlgList(1:1000, title = "How many experimental conditions?")$res)
		samples <- names(HtestData)
		for (k in 1:conditions) {
			cond <- dlgList(samples, multiple = TRUE, title = paste("Which samples bellong to experimental condition", k))$res
			cond <- which(samples %in% cond)
			HtestData[1, cond] <- k
		}
		replicates <- as.character(HtestData[1, ])
		HtestData <- HtestData[-1, ]
		rep <- 1
		reps <- factor(replicates[-1])
	}

	if (length(levels(reps)) == 1) {
		stop("ANOVA and t-test require more than one experimental condition.")
	}

	if (missing(StatTest)) {
		if (length(levels(reps)) > 2) {
			HtestData <- anova.t()
			testDone <- "ANOVA"
		} else {
			HtestData <- test.t()
			testDone <- "ttest"
		}
	} else {
		if (StatTest %in% c("ANOVA", "Anova", "anova", "A", "a")) {
			HtestData <- anova.t()
			testDone <- "ANOVA"
		} else {
			if (StatTest %in% c("T-TEST", "T-test", "t-test", "t-TEST", "t", "T")) {
				HtestData <- test.t()
				testDone <- "ttest"
			} else {
				message(paste("Sorry, ", StatTest, " is not an option for htest. Please select one of the options that will appear to you. Next time, you can use one of the following entries: ANOVA, Anova, anova, A and a for ANOVA; or T-TEST, T-test, t-test, t-TEST, t and T for a t-test",
					sep = ""))
				option.test <- dlgList(c("ANOVA", "t-test"))$res
				if (option.test == "ANOVA") {
					HtestData <- anova.t()
					testDone <- "ANOVA"
				} else {
					HtestData <- test.t()
					testDone <- "ttest"
				}
			}
		}
	}
	if (adjust.pValue){
		HtestData[ncol(HtestData)] <- p.adjust(as.numeric(HtestData[,ncol(HtestData)]), method = method)
		adjustMethod <- method
	} else {
		adjustMethod <- "Not-adjusted"
	}
	HtestData <- subset(HtestData, as.numeric(HtestData$pvalues) < signif.level)
	if (rep == 1) {
		HtestData <- rbind(c(replicates, adjustMethod), HtestData)
	}
	if (save) {
		if (missing(output)){
			sheet <- testDone
		} else {
			sheet <- output
		}
		store <- file.path(folder, paste(sheet, ".csv", sep = ""))
      	inputTest <- file.access(store, 0)
      	if (inputTest == 0) {
      		addFile <- 1
      		while(inputTest == 0){
      			store <- file.path(folder, paste(sheet, addFile, ".csv", sep = ""))
      			inputTest <- file.access(store, 0)
      			addFile <- addFile + 1
      		}
      	}
      	write.csv(HtestData, file = store, row.names = FALSE)
		message("File saved: ", store, "\n")
	}
	return(HtestData)
}
