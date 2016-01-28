normalizeByBiomass <-
  function (inputData, biomass, save = TRUE, folder, output = "norm_bio") 
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
        folder = dlgDir(title = "Select the folder where the results will be saved.")$res
      } else {
        if (is.character(folder)) {
          isFolder <- file.access(as.character(folder), 0)
          if (isFolder == 0) {
            isFolder <- file.info(folder)
            if (!isFolder$isdir) {
              folder = dlgDir(title = "Select the folder where the results will be saved.")$res
            }
          } else {
            folder = dlgDir(title = "Select the folder where the results will be saved.")$res
          }
        } else {
          folder = dlgDir(title = "Select the folder where the results will be saved.")$res
        }
      }
    }
    if (missing(biomass)){
      biomass <- c()
    } else {
      if (is.data.frame(biomass)) {
        biomass <- biomass
        message("Biomass loaded...")
      } else {
        if (is.character(biomass)) {
          checkIfCsv <- isCSV(biomass)
          if (checkIfCsv == 1) {
            inputTest <- file.access(biomass, 0)
            if (inputTest == 0) {
              biomass <- read.csv(biomass, colClasses = "character", check.names = FALSE)
            } else {
              dlgMessage("Biomass file not accessible.")
              biomass  <- isCSVdlg("Select the CSV file containing the biomass", "Please, choose a Biomass with the extension .csv.")
              biomass  <- read.csv(biomass, colClasses = "character", check.names = FALSE)
              message("Input biomass file loaded...")
            }
          } else {
            dlgMessage("Biomass file not accessible.")
            biomass <- isCSVdlg("Select the CSV file containing the biomass", "Please, choose a Biomass with the extension .csv.")
            biomass <- read.csv(inputData, colClasses = "character", check.names = FALSE)
            message("Input file loaded...")
          }
        } else {
          dlgMessage("Biomass file not accessible.")
          biomass <- isCSVdlg("Select the CSV file containing the biomass", "Please, choose a Biomass with the extension .csv.")
          biomass <- read.csv(biomass, colClasses = "character", check.names = FALSE)
          message("Input file loaded...")
        }
      }
    }
    
    if (toupper(as.character(inputData[1, 1])) == "REPLICATES") {
      replicates <- as.character(inputData[1, ])
      inputData <- inputData[-1, ]
      rep <- 1
    }
    if (length(biomass) == 0){
      for (i in 2:ncol(inputData)) {
        sample.name <- names(inputData)[i]
        biomass <- dlgInput(paste("Biomass for sample ", sample.name, ":", sep = ""))$res
        if(!length(biomass)){
          stop("The process was canceled by the user.")
        } else {
          biomass <- unlist(strsplit(biomass, "returned:", fixed = TRUE))
          biomass <- biomass[length(biomass)]
        }
        inputData[i] <- as.numeric(inputData[, i])/as.numeric(biomass)
      }
    } else {
      for (i in 2:ncol(inputData)) {
        sample.name <- names(inputData[i])
        biomass.sample <- biomass[biomass[1] == sample.name]
        if (length(biomass.sample) >= 1) {
          inputData[i] <- as.numeric(inputData[, i])/as.numeric(biomass.sample[2])
        } else {
          warning(paste("Biomass was not found for", sample.name))
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