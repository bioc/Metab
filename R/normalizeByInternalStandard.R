normalizeByInternalStandard <-
function (inputData, internalStandard, save = TRUE, folder, output = "normalizedByInternalStandard") 
{
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
    
		## Begin collecting arguments
	if (missing(inputData)) {
		inputData <- isCSVdlg("Select the CSV file containing the input data", "The input file MUST be in the format of comma-separated value (csv). Please, choose an input file showing the extension .csv.")
		inputData <- read.csv(inputData, colClasses = "character", check.names = FALSE)
		message("Input file loaded...")
	} else {
		if (is.data.frame(inputData)) {
			message("Data frame loaded...")
		} else {
			if (is.character(inputData)) {
				checkIfCsv <- isCSV(inputData)
				if (checkIfCsv == 1) {
					inputTest <- file.access(inputData, 0)
					if (inputTest == 0) {
						inputData = read.csv(inputData, colClasses = "character", check.names = FALSE)
					} else {
						inputData <- isCSVdlg("Select the CSV file containing the input data", "The input file MUST be in the format of comma-separated value (csv). Please, choose an input file showing the extension .csv.")
						inputData <- read.csv(inputData, colClasses = "character", check.names = FALSE)
						message("Input file loaded...")
					}
				} else {
					inputData <- isCSVdlg("Select the CSV file containing the input data", "The input file MUST be in the format of comma-separated value (csv). Please, choose an input file showing the extension .csv.")
					inputData <- read.csv(inputData, colClasses = "character", check.names = FALSE)
					message("Input file loaded...")
				}
			} else {
				inputData <- isCSVdlg("Select the CSV file containing the input data", "The input file MUST be in the format of comma-separated value (csv). Please, choose an input file showing the extension .csv.")
				inputData <- read.csv(inputData, colClasses = "character", check.names = FALSE)
				message("Input file loaded...")
			}
		}
	}

	if (save) {
		if (missing(folder)) {
			folder = dlgDir(title = "Select the folder where results will be saved.")$res
		} else {
			if (is.character(folder)) {
				isFolder <- file.access(as.character(folder), 0)
				if (isFolder == 0) {
					isFolder <- file.info(folder)
					if (!isFolder$isdir) {
						folder = dlgDir(title = "Select the folder where results will be saved.")$res
					}
				} else {
					folder = dlgDir(title = "Select the folder where results will be saved.")$res
				}
			} else {
				folder = dlgDir(title = "Select the folder where results will be saved.")$res
			}
		}
	}
    if (toupper(inputData[1, 1]) == "REPLICATES") {
        replicates <- inputData[1, ]
        inputData <- inputData[-1, ]
        rep <- 1
    }
    if (missing(internalStandard)) {
            internal <- select.list(inputData[, 1], title = "Select the internal standard:")
            internal <- grep(internal, inputData[, 1], value = FALSE, fixed = TRUE)
            for (i in 2:ncol(inputData)) {
	            if (is.na(inputData[internal, i])) {
         	         inputData[internal, i] <- "Not-detected"
                  } else {
                    inputData[i] <- as.numeric(inputData[, i])/as.numeric(inputData[internal, i])
                  }
            }
    } else {
        internal <- grep(internalStandard, inputData[, 1], value = FALSE)
        if (length(internal) == 0) {
            dlgMessage("Internal standard not found.\n")
		internal <- select.list(inputData[, 1], title = "Select the internal standard:")
            internal <- grep(internal, inputData[, 1], value = FALSE, fixed = TRUE)
            for (i in 2:ncol(inputData)) {
	            if (is.na(inputData[internal, i])) {
         	         inputData[internal, i] <- "Not-detected"
                  } else {
                    inputData[i] <- as.numeric(inputData[, i])/as.numeric(inputData[internal, i])
                  }
            }
		internal <- c()
        }
        if (length(internal) > 1) {
            dlgMessage("There is more one compound with the name specified.\n")
		internal <- select.list(inputData[, 1], title = "Select the internal standard:")
            internal <- grep(internal, inputData[, 1], value = FALSE, fixed = TRUE)
            for (i in 2:ncol(inputData)) {
	            if (is.na(inputData[internal, i])) {
         	         inputData[internal, i] <- "Not-detected"
                  } else {
                    inputData[i] <- as.numeric(inputData[, i])/as.numeric(inputData[internal, i])
                  }
            }
		internal <- c()
        }
        if (length(internal) == 1) {
            for (i in 2:ncol(inputData)) {
                if (is.na(inputData[internal, i])) {
                  inputData[internal, i] <- "Not-detected"
                } else {
                  inputData[i] <- as.numeric(inputData[, i])/as.numeric(inputData[internal, i])
                }
            }
        }
    }
    if (rep == 1) {
        inputData <- rbind(replicates, inputData)
    }
    if (save) {
	sheet <- output
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
      write.csv(inputData, file = store, row.names = FALSE)
	message("File saved: ", store, "\n") 
    }
    return(inputData)
}
