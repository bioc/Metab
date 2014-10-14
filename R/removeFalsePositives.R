removeFalsePositives <-
function (inputData, truePercentage = 50, Name_medium_condition = "none", truePercentageMedium = 50, save = TRUE, folder, output = "NoFalse") 
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
    
    truePercentage <- as.numeric(truePercentage)/100
    
	## Begin collecting arguments
	if (missing(inputData)) {
		inputData <- isCSVdlg("Select the CSV file containing the input data", "The input file MUST be in the format of comma-separated value (csv). Please, choose an input file showing the extension .csv.")
		deleteFalse <- read.csv(inputData, colClasses = "character", check.names = FALSE)
		message("Input file loaded...")
	} else {
		if (is.data.frame(inputData)) {
			deleteFalse <- inputData
			message("Data frame loaded...")
		} else {
			if (is.character(inputData)) {
				checkIfCsv <- isCSV(inputData)
				if (checkIfCsv == 1) {
					inputTest <- file.access(inputData, 0)
					if (inputTest == 0) {
						deleteFalse = read.csv(inputData, colClasses = "character", check.names = FALSE)
					} else {
						inputData <- isCSVdlg("Select the CSV file containing the input data", "The input file MUST be in the format of comma-separated value (csv). Please, choose an input file showing the extension .csv.")
						deleteFalse <- read.csv(inputData, colClasses = "character", check.names = FALSE)
						message("Input file loaded...")
					}
				} else {
					inputData <- isCSVdlg("Select the CSV file containing the input data", "The input file MUST be in the format of comma-separated value (csv). Please, choose an input file showing the extension .csv.")
					deleteFalse <- read.csv(inputData, colClasses = "character", check.names = FALSE)
					message("Input file loaded...")
				}
			} else {
				inputData <- isCSVdlg("Select the CSV file containing the input data", "The input file MUST be in the format of comma-separated value (csv). Please, choose an input file showing the extension .csv.")
				deleteFalse <- read.csv(inputData, colClasses = "character", check.names = FALSE)
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
						folder = dlgDir(title = "Select the folder where the output file will be saved.")$res
					}
				} else {
					folder = dlgDir(title = "Select the folder where the output file will be saved.")$res
				}
			} else {
				folder = dlgDir(title = "Select the folder where the output file will be saved.")$res
			}
		}
	}
    if (toupper(deleteFalse[1, 1]) == "REPLICATES") {
        if (Name_medium_condition == "none") {
            replicates <- as.character(deleteFalse[1, ])
            rep <- 1
            reps <- factor(replicates[-1])
        } else {
            medium <- which(Name_medium_condition == deleteFalse[1, ])
            if (length(medium) > 0) {
                medium.samples <- deleteFalse[c(medium)]
                replicates.medium <- as.character(medium.samples[1,])
                for (i in 1:nrow(medium.samples)) {
                  if (sum(!is.na(medium.samples[i, ]))/ncol(medium.samples) < truePercentageMedium){
                    medium.samples[i, ] <- NA
                  }
                }
                deleteFalse[medium] <- medium.samples
            } else {
                stop("The Name_medium_condition was not found in the row \"Replicates\"")
            }
            replicates <- as.character(deleteFalse[1, ])
            rep <- 1
            reps <- factor(replicates[-1])
        }
    } else {
        stop("There is no information about replicates in the data. The first row of the input data should contain the names of the conditions from where each sample belongs to. Try data(exampleMetReport) to see an example.")
    }
    for (z in 1:length(levels(reps))) {
        if (as.character(levels(reps)[z]) == Name_medium_condition) {
        } else {
            column <- which(as.character(levels(reps)[z]) == as.character(deleteFalse[1, ]))
            yMat <- deleteFalse[column]
            for (i in 1:nrow(yMat)) {
                if (sum(!is.na(yMat[i, ]))/ncol(yMat) < truePercentage) {
                  yMat[i, ] <- NA
                }
            }
            deleteFalse[column] <- yMat
        }
    }
    yMat <- deleteFalse[-1, -1]
    missing <- t(apply(yMat, 1, function(y) tapply(as.numeric(y), reps, function(x) sum(!is.na(x)))))
    missing2 <- apply(t(apply(missing, 1, function(x) x == 0)), 1, sum)
    missing2 <- as.data.frame(missing2)
    deleteFalse <- merge(deleteFalse, missing2, by = 0)
    deleteFalse <- subset(deleteFalse, deleteFalse$missing2 < length(levels(reps)))
    deleteFalse$missing2 <- NULL
    deleteFalse$Row.names <- NULL
    if (rep == 1) {
        deleteFalse <- rbind(c(replicates, NA), deleteFalse)
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
      write.csv(deleteFalse, file = store, row.names = FALSE)
	message("File saved: ", store, "\n")
    }
    return(deleteFalse)
}
