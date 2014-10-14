MetReport <-
    function (inputData, singleFile = FALSE, AmdisReport, ionLib, save = TRUE, output = "metab_data", TimeWindow = 2.5, Remove, abundance = "recalculate", folder)
{
################################
##### Check if valid folder ####
################################
    validFolder <- function(pathFolder, errorMsg1, errorMsg2, titleMSG1){
        ## Is character? ###
        if (is.character(pathFolder)){
            isFolder <- file.access(as.character(pathFolder), 0)
            if (isFolder == 0){
                isFolder <- file.info(pathFolder)
                if (isFolder$isdir){
                    pathFolder <- pathFolder
                } else {
                    dlgMessage(errorMsg2)
                    pathFolder <- dlgDir(title = titleMSG1)$res
                }
            } else {
                dlgMessage(errorMsg2)
                pathFolder <- dlgDir(title = titleMSG1)$res
            }
        } else {
### If not caharacter ###
            dlgMessage(errorMsg1)
            pathFolder <- dlgDir(title = titleMSG1)$res
        }
        return(pathFolder)
    }
    
    title1 <- "Select the folder containing the GC-MS data in CDF format (NetCDF or AIA)."
    error1 <- "The dataFolder is not defined as character. A new window will open allowing you to select a valid folder"
    error2 <- "The dataFolder defined is not a valid folder. A new window will open allowing you to select a valid folder"
    
####################################
### Check if a CSV or .msl file ####
####################################
    isCSVORmsldlg <- function(titleMSG, errorMSG){
        t = 0
        while (t == 0){
            checkIfCsv <- dlgOpen(title = titleMSG, multiple = FALSE)$res
            checkIfCsv2 <- basename(checkIfCsv)
            checkIfCsv3 <- unlist(strsplit(checkIfCsv2, "\\."))
            checkIfCsv4 <- checkIfCsv3[length(checkIfCsv3)]
            if (checkIfCsv4 %in% c("CSV", "csv")){
                t = 1
                return(c(checkIfCsv, "csv"))
            } else {
                if (checkIfCsv4 %in% c("MSL", "msl")){
                    t = 1
                    return(c(checkIfCsv, "msl"))
                } else {
                    dlgMessage(errorMSG)
                }
            }
        }
    }
    
##################################################
### Check if a CSV or .msl file  NO DIALOG BOX ###
##################################################
    isCSVORmsl <- function(pathfile, errorMSG){
        t = 0
        checkIfCsv2 <- basename(pathfile)
        checkIfCsv3 <- unlist(strsplit(checkIfCsv2, "\\."))
        checkIfCsv4 <- checkIfCsv3[length(checkIfCsv3)]
        if (checkIfCsv4 %in% c("CSV", "csv")){
            t = "csv"
            return(t)
        } else {
            if (checkIfCsv4 %in% c("MSL", "msl")){
                t = "msl"
                return(t)
            } else {
                t = 0
                return(t)
            }
        }
    }
    
######################################################
############# Function to check if file is CDF #######
######################################################
    isCDFdlg <- function(titleMSG, errorMSG) {
        t = 0
        while (t == 0) {
            checkIfCDF <- dlgOpen(title = titleMSG, multiple = FALSE)$res
            checkIfCDF2 <- basename(checkIfCDF)
            checkIfCDF3 <- unlist(strsplit(checkIfCDF2, "\\."))
            checkIfCDF4 <- checkIfCDF3[length(checkIfCDF3)]
            if (checkIfCDF4 %in% c("CDF", "cdf")) {
                t = 1
                return(checkIfCDF)
            } else {
                dlgMessage(errorMSG)
            }
        }
    }
    
#######################################################
################# Check if CDF - No dialog box ########
#######################################################
    isCDF <- function(pathFile, errorMSG) {
        t = 0
        checkIfCDF <- pathFile
        checkIfCDF2 <- basename(checkIfCDF)
        checkIfCDF3 <- unlist(strsplit(checkIfCDF2, "\\."))
        checkIfCDF4 <- checkIfCDF3[length(checkIfCDF3)]
        if (checkIfCDF4 %in% c("CDF", "cdf")) {
            t = 1
            return(t)
        } else {
                                        #dlgMessage(errorMSG)
            return(t)
        }
    }
    
#######################################################################
####### If abundances = recalculate, we need the ionlib ###############
#######################################################################
    if (abundance %in% c("recalculate", "RECALCULATE", "R", "r")){
        if (missing(ionLib)) {
            ionLib <- isCSVORmsldlg("Select a CSV file containing the ionLib or the .msl file of the AMDIS library in use.", "The selected file is not a CSV nor a msl file")
            if (ionLib[2] == "csv"){
                ionLibUsed <- ionLib[1]
                ionLib = read.csv(ionLib[1], colClass = "character", check.names = FALSE)
                if (ncol(ionLib) > 2){
                    message("The ionLib selected has more than 2 columns. It is expected if you generated the ion library using the function buildLib. We will select only the first and third columns of the ion library selected.")
                    ionLib <- ionLib[c(1,3)]
                    names(ionLib)[2] <- "ref_ion"
                    ionLib[2] <- as.numeric(ionLib[,2])
                }
                message("ionLib file loaded...")
            } else {
                ionLibUsed <- ionLib[1]
                ionLib <- buildLib(ionLib[1], save = FALSE)
                ionLib <- ionLib[c(1,3)]
                names(ionLib)[2] <- "ref_ion"
                message("ionLib built and loaded...")
            }
        } else {
            if (is.data.frame(ionLib)) {
                if (ncol(ionLib) > 2){
                    message("The ionLib has more than 2 columns. It is expected if you generated the ion library using the function buildLib. We will select only the first and third columns of the ion library selected.")
                    ionLibUsed <- "data_frame"
                    ionLib <- ionLib[c(1,3)]
                    names(ionLib)[2] <- "ref_ion"
                    ionLib[2] <- as.numeric(ionLib[,2])
                }
                message("ionLib - Data frame loaded...")
            } else {
                if (is.character(ionLib)) {
                    checkIfCsv <- isCSVORmsl(ionLib)
                    if (checkIfCsv != 0) {
                        inputTest <- file.access(ionLib, 0)
                        if (inputTest == 0) {
                            if (checkIfCsv == "csv") {
                                ionLibUsed <- ionLib
                                ionLib = read.csv(ionLib, colClasses = "character", check.names = FALSE)
                                if (ncol(ionLib) > 2){
                                    message("The ionLib has more than 2 columns. It is expected if you generated the ion library using the function buildLib. We will select only the first and third columns of the ion library selected.")
                                    ionLib <- ionLib[c(1,3)]
                                    names(ionLib)[2] <- "ref_ion"
                                    ionLib[2] <- as.numeric(ionLib[,2])
                                }
                                message("ionLib loaded...")
                            } else {
                                if (checkIfCsv == "msl"){
                                    ionLibUsed <- ionLib
                                    ionLib <- buildLib(ionLib, save = FALSE)
                                    ionLib <- ionLib[c(1,3)]
                                    names(ionLib)[2] <- "ref_ion"
                                    message("ionLib built and loaded...")
                                }
                            }
                        } else {
                            message("The ionLib specified is not accessible. Please, choose a valid CSV file or msl file to be used as ionLib.")
                            ionLib <- isCSVORmsldlg("Select a CSV file containing the ionLib or the .msl file of the AMDIS library in use.", "The selected file is not a CSV nor a msl file")
                            if (ionLib[2] == "csv"){
                                ionLibUsed <- ionLib
                                ionLib = read.csv(ionLib, colClass = "character", check.names = FALSE)
                                if (ncol(ionLib) > 2){
                                    message("The ionLib has more than 2 columns. It is expected if you generated the ion library using the function buildLib. We will select only the first and third columns of the ion library selected.")
                                    ionLib <- ionLib[c(1,3)]
                                    names(ionLib)[2] <- "ref_ion"
                                    ionLib[2] <- as.numeric(ionLib[,2])
                                }
                                message("ionLib file loaded...")
                            } else {
                                if (ionLib[2] == "msl"){
                                    ionLibUsed <- ionLib[1]
                                    ionLib <- buildLib(ionLib[1], save = FALSE)
                                    ionLib <- ionLib[c(1,3)]
                                    names(ionLib)[2] <- "ref_ion"
                                    message("ionLib built and loaded...")
                                }
                            } 
                        }
                    } else {
                        message("The ionLib specified is not a CSV nor msl file. Please, choose a valid CSV file or msl file to be used as ionLib.")
                        ionLib <- isCSVORmsldlg("Select a CSV file containing the ionLib or the .msl file of the AMDIS library in use.", "The selected file is not a CSV nor a msl file")
                        if (ionLib[2] == "csv"){
                            ionLib = read.csv(ionLib, colClass = "character", check.names = FALSE)
                            if (ncol(ionLib) > 2){
                                message("The ionLib has more than 2 columns. It is expected if you generated the ion library using the function buildLib. We will select only the first and third columns of the ion library selected.")
                                ionLib <- ionLib[c(1,3)]
                                names(ionLib)[2] <- "ref_ion"
                                ionLib[2] <- as.numeric(ionLib[,2])
                            }
                            message("ionLib file loaded...")
                        } else {
                            if (ionLib[2] == "msl"){
                                ionLib <- buildLib(ionLib[1], save = FALSE)
                                ionLib <- ionLib[c(1,3)]
                                names(ionLib)[2] <- "ref_ion"
                                message("ionLib built and loaded...")
                            }
                        }
                    }
                } else {
                    message("The ionLib specified is not a character string nor a data frame. Please, choose a valid CSV file or msl file to be used as ionLib.")
                    ionLib <- isCSVORmsldlg("Select a CSV file containing the ionLib or the .msl file of the AMDIS library in use.", "The selected file is not a CSV nor a msl file")
                    if (ionLib[2] == "csv"){
                        ionLib = read.csv(ionLib, colClass = "character", check.names = FALSE)
                        if (ncol(ionLib) > 2){
                            message("The ionLib has more than 2 columns. It is expected if you generated the ion library using the function buildLib. We will select only the first and third columns of the ion library selected.")
                            ionLib <- ionLib[c(1,3)]
                            names(ionLib)[2] <- "ref_ion"
                            ionLib[2] <- as.numeric(ionLib[,2])
                        }
                        message("ionLib file loaded...")
                    } else {
                        if (ionLib[2] == "msl"){
                            ionLib <- buildLib(ionLib[1], save = FALSE)
                            ionLib <- ionLib[c(1,3)]
                            names(ionLib)[2] <- "ref_ion"
                            message("ionLib built and loaded...")
                        }
                    }
                }
            }
        }
        ionLib_noSpace <- ionLib
        ionLib_noSpace[1] <- gsub("[? ]", "", ionLib_noSpace[,1])
    }
    
#############################################################
######### Check folder in case singleFile = TRUE ###########
#############################################################
    if (singleFile) {
###########################################
############# Get CDF File ################
###########################################
        if (missing(inputData)) {
            inputData <- isCDFdlg("Select the CDF file to be analyzed.", "The selected file is not a CDF file.")
            dataUsed <- inputData
            findCDF <- data.frame(findCDFpre = inputData, V2 = "A", stringsAsFactors = FALSE)
            replicates <- as.character(findCDF[,2])
            reps <- factor(replicates)
            expConditions <- nlevels(reps)
        } else {
            verifCDF <- isCDF(inputData, "The specified file is not a CDF file.")
            if (verifCDF == 1) {
                inputTest <- file.access(inputData, 0)
                if (inputTest == 0) {
                    dataUsed <- inputData
                    findCDF <- data.frame(findCDFpre = inputData, V2 = "A", stringsAsFactors = FALSE)
                    replicates <- as.character(findCDF[,2])
                    reps <- factor(replicates)
                    expConditions <- nlevels(reps)
                } else {
                    inputData <- isCDFdlg("Select the CDF file to be analyzed.", "The selected file is not a CDF file.")
                    dataUsed <- inputData
                    findCDF <- data.frame(findCDFpre = inputData, V2 = "A", stringsAsFactors = FALSE)
                    replicates <- as.character(findCDF[,2])
                    reps <- factor(replicates)
                    expConditions <- nlevels(reps)
                }
            } else {
                inputData <- isCDFdlg("Select the CDF file to be analyzed.", "The selected file is not a CDF file.")
                dataUsed <- inputData
                findCDF <- data.frame(findCDFpre = inputData, V2 = "A", stringsAsFactors = FALSE)
                replicates <- as.character(findCDF[,2])
                reps <- factor(replicates)
                expConditions <- nlevels(reps)
            }
        }
###########################################
############## Get folder #################
###########################################
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
	
###########################################
######## If singleFile = FALSE ############
###########################################
    } else {
###########################################
############### Get folder ################
###########################################
        if (missing(inputData)) {
            inputData <- dlgDir(title = title1)$res
            folderUsed <- inputData
        } else {
            inputData <- validFolder(inputData, error1, error2, title1)
            folderUsed <- inputData
        }
###########################################
############## Get conditions #############
###########################################
### Search subfolders for defining experimental conditions #####
        expConditions <- file.info(list.files(inputData, full.names = TRUE))
        expConditions <- expConditions[expConditions$isdir,]
### If no subfolders found ###
        if(nrow(expConditions) == 0){
            message("The inputData specified contains no subfolders defining experimental conditions. We will look for CDF files...")
            findCDF <- list.files(inputData, ".cdf", ignore.case = TRUE, full.names = TRUE)
            findCDFNames <- list.files(inputData, ".cdf", ignore.case = TRUE, full.names = FALSE)
            if (length(findCDF) == 0){
                stop("There is no CDF files in the inputData specified.")
            }
            message("CDF files found...")
            expConditions <- as.numeric(dlgList(c(1:1000), title = "How many experimental conditions will be analyzed?")$res)
            findCDF <- data.frame(findCDF)
            for (i in 1:expConditions) {
                samples <- dlgList(findCDFNames, title = paste("Select the samples belonging to condition", i), multiple = TRUE)$res
                findCDF[which(findCDFNames == samples), 2] <- paste("ExpCond_", i, sep="")
            }
            replicates <- as.character(findCDF[,2])
            reps <- factor(replicates)
            expConditions <- nlevels(reps)
        } else {
            findCDF <- c()
            message("Subfolders representing experimental conditions found...")
            subfolders <- data.frame(basename(row.names(expConditions)))
            names(subfolders)[1] <- "ExperimentalConditions"
            pandoc.table(subfolders)
            for (i in 1:nrow(expConditions)){
                findCDFpre <- list.files(row.names(expConditions[i,]), ".cdf", ignore.case = TRUE, full.names = TRUE)
                if (length(findCDFpre) == 0){
                    message(paste("The subfolder", basename(row.names(expConditions[i,])), "contains no CDF files. It will not be considered in this analysis."))
                } else {
                    findCDFpre <- data.frame(findCDFpre)
                    findCDFpre[2] <- basename(row.names(expConditions[i,]))
                    findCDF <- rbind(findCDF, findCDFpre)
                }
            }
            replicates <- as.character(findCDF[,2])
            reps <- factor(replicates)
            expConditions <- nlevels(reps)
        }
        
###########################################
############## Get folder #################
###########################################
        if (save) {
            if (missing(folder)) {
                folder <- folderUsed
            } else {
                if (is.character(folder)) {
                    isFolder <- file.access(as.character(folder), 0)
                    if (isFolder == 0) {
                        isFolder <- file.info(folder)
                        if (!isFolder$isdir) {
                            folder <- folderUsed
                        }
                    } else {
                        folder <- folderUsed
                    }
                } else {
                    folder <- folderUsed
                }
            }
        }
    }
#######################################
########### Get AMDIS report ##########
#######################################
    if (missing(AmdisReport)) {
        AmdisReport <- dlgOpen(title = "Select the AMDIS report generated in batch mode")$res
        AmdisReportUsed <- AmdisReport
        AmdisReport = read.csv(AmdisReport, sep = "\t", colClass = "character")
    } else {
        if (is.data.frame(AmdisReport)) {
            AmdisReportUsed <- "data_frame"
            AmdisReport <- AmdisReport
            message("AMDIS Report - Data frame loaded...")
        } else {
            if (is.character(AmdisReport)) {
                inputTest <- file.access(AmdisReport, 0)
                if (inputTest == 0) {
                    AmdisReportUsed <- AmdisReport
                    AmdisReport = read.csv(AmdisReport, sep = "\t", colClasses = "character")
                } else {
                    message("The AmdisReport specified is not accessible. Please, select the AMDIS report generated in batch mode.")
                    AmdisReport <- dlgOpen(title = "Select the AMDIS report generated in batch mode")$res
                    AmdisReportUsed <- AmdisReport
                    AmdisReport = read.csv(AmdisReport, sep = "\t", colClass = "character")
                    
                }
            } else {
                message("The AmdisReport specified is not valid. Please, select the AMDIS report generated in batch mode.")
                AmdisReport <- dlgOpen(title = "Select the AMDIS report generated in batch mode")$res
                AmdisReportUsed <- AmdisReport
                AmdisReport = read.csv(AmdisReport, sep = "\t", colClass = "character")
            }
        }
    }
    total_report <- AmdisReport
    if (missing(Remove)){
        RemoveUsed <- "None"
    } else {
        RemoveUsed <- Remove
        for (i in 1:length(Remove)){
            toRemove <- grep(Remove[i], total_report$Name, ignore.case = FALSE, value = TRUE)
            total_report <- subset(total_report, !(total_report$Name %in% toRemove))
        }
    }

#######################################
########## Start the process ##########
#######################################
    Result.df <- 1
    if (abundance %in% c("Base.Peak", "BasePeak", "base.peak", "basepeak", "B", "b")){
  	Keep <- c("Name", "RT", "Scan", "Expec..RT", "Base.Peak")
        AbundancesUsed <- "Base.Peak"
        ionLibUsed <- "none"
    } else {
  	if (abundance %in% c("recalculate", "Recalculate", "RECALCULATE", "r", "R")){
            Keep <- c("Name", "RT", "Scan", "Expec..RT", "Base.Peak")
            AbundancesUsed <- "recalculated"
  	} else {
            if (abundance %in% c("Area", "area", "AREA", "a", "A")){
        	Keep <- c("Name", "RT", "Scan", "Expec..RT", "Area")
                AbundancesUsed <- "Area"
                ionLibUsed <- "none"
            }
        }
    }

    for (c in 1:nrow(findCDF)) {
        surefinal <- c()
        file <- as.character(findCDF[c,1])
        file <- gsub(".cdf", ".CDF", file, fixed = TRUE)
        file <- basename(file)
        name.file <- file
        name.file <- gsub(".CDF", "", name.file, fixed = TRUE)
        file <- gsub(".CDF", ".FIN", file, fixed = TRUE)
        file <- grep(file, total_report$FileName, ignore.case = TRUE)
        if (length(file) > 0) {
            data.df <- total_report[file, ]
            data.df <- data.df[, Keep]
            data.df[2] <- as.numeric(data.df[,2])
            data.df[3] <- as.numeric(data.df[,3])
            data.df[4] <- as.numeric(data.df[,4])
            data.df[5] <- as.numeric(data.df[,5])
            data.df$deff <- data.df$RT - data.df$Expec..RT
            data.df <- subset(data.df, abs(data.df$deff) < TimeWindow)
            while (nrow(data.df) != 0) {
                line <- data.df[1, ]
                sameRT <- subset(data.df, data.df$RT == line$RT)
                data.df <- data.df[-which(row.names(data.df) %in% row.names(sameRT)),]
                if (nrow(sameRT) > 1) {
                    doub <- grep("[?]", sameRT$Name, value = TRUE)
                    if (nrow(sameRT) == length(doub)) {
                        oneq <- grep("^?\\s", sameRT$Name, value = TRUE)
                        twoq <- grep("^??\\s", sameRT$Name, value = TRUE)
                        thrq <- grep("^???\\s", sameRT$Name, value = TRUE)
                        if (length(oneq) == 0) {
                            if (length(twoq) == 0) {
                                findCloser <- sameRT
                                findCloser$deff <- abs(findCloser$deff)
                                sure <- sameRT[which(findCloser$deff == min(findCloser$deff)),]
                                if (nrow(sure) > 1){
                                    sure <- sameRT[which(sure$Base.Peak == max(sure$Base.Peak)),]
                                }
                            } else {
                                if (length(twoq) > 1) {
                                    sameRT <- subset(sameRT, sameRT$Name %in% twoq)
                                    findCloser <- sameRT
                                    findCloser$deff <- abs(findCloser$deff)
                                    sure <- sameRT[which(findCloser$deff == min(findCloser$deff)),]
                                    if (nrow(sure) > 1){
                                        sure <- sameRT[which(sure$Base.Peak == max(sure$Base.Peak)),]
                                    }
                                } else {
                                    sameRT <- subset(sameRT, sameRT$Name %in% twoq)
                                    sure <- sameRT
                                }
                            }
                        } else {
                            if (length(oneq) > 1){
                                sameRT <- subset(sameRT, (sameRT$Name %in% oneq))
                                findCloser <- sameRT
                                findCloser$deff <- abs(findCloser$deff)
                                sure <- sameRT[which(findCloser$deff == min(findCloser$deff)),]
                                if (nrow(sure) > 1){
                                    sure <- sameRT[which(sure$Base.Peak == max(sure$Base.Peak)),]
                                }
                            } else {
                                sameRT <- subset(sameRT, sameRT$Name %in% twoq)
                                sure <- sameRT
                            }
                        }
                    } else {
                        sameRT <- subset(sameRT, !(sameRT$Name %in% doub))
                        if (nrow(sameRT) > 1) {
                            findCloser <- sameRT
                            findCloser$deff <- abs(findCloser$deff)
                            sure <- sameRT[which(findCloser$deff == min(findCloser$deff)),]
                            if (nrow(sure) > 1){
                                preSure <- sameRT[which(sure$Base.Peak == max(sure$Base.Peak)),]
                                sure <- preSure[1,]
                            }
                        } else {
                            sure <- sameRT
                        }
                    }
                } else {
                    sure <- sameRT
                }
                if (length(surefinal) == 0){
                    surefinal <- sure
                } else {
                    surefinal <- rbind(sure, surefinal)
                }
            }
            surefinal <- surefinal[!duplicated(surefinal$RT),]
########################################################################################
########## Here we have different approachs according to the abundance chosen ##########
########################################################################################
            
#######################################
########## For recalculate ############
#######################################
            if (AbundancesUsed == "recalculated"){
                raw_data <- xcmsRaw(filename = as.character(findCDF[c,1]))
                plotChrom(raw_data)
                for (h in 1:nrow(surefinal)) {
                    metabolite <- ionLib[which(ionLib_noSpace[1] == gsub("[? ]", "", surefinal$Name[h])),]
                    if (nrow(metabolite) == 0){
                        message(paste("Metabolite ", gsub("[? ]", "", surefinal$Name[h]), "not found in the ion library."))
                    }
                    surefinal[h,1] <- as.character(metabolite[1,1])
                    scan <- getScan(raw_data, scan = surefinal$Scan[h])
                    scan <- data.frame(scan)
                    scan$mz <- trunc(scan$mz)
                    scan_ion <- (metabolite$ref_ion[1]) - 2
                    ion_set <- c()
                    for (k in 1:3) {
                        abundance <- subset(scan, scan$mz == (scan_ion + k))
                        if(length(abundance$intensity) > 0){
                            maxabundance <- max(abundance$intensity)
                            ion_set <- c(ion_set, maxabundance)
                        }
                    }
                    surefinal$Base.Peak[h] <- max(ion_set)
                }
                names(surefinal)[5] <- name.file
                dupli <- surefinal[duplicated(surefinal[,1]),]	
                if (nrow(dupli) > 0){
                    while(nrow(dupli) > 0){
                        checkDupli <- dupli[1,]
                        sameName <- subset(surefinal, surefinal$Name == checkDupli$Name)
                        sameName$deff <- abs(sameName$deff)
                        choseName <- sameName[which(sameName$deff == min(abs(sameName$deff), na.rm = TRUE)),]
                        if (nrow(choseName) > 1){
                            choseName <- choseName[which(choseName[,5] == max(choseName[,5], na.rm = TRUE)),]
                        }
                        sameName <- sameName[-(which(row.names(sameName) == row.names(choseName))),]
                        surefinal <- surefinal[-which(row.names(surefinal) %in% row.names(sameName)),]
                        dupli <- dupli[-(which(dupli$Name %in% checkDupli$Name)),]
                    }
                }
            } else {
                surefinal[1] <- gsub("? ", "", surefinal[,1], fixed = TRUE)
                surefinal[1] <- gsub("?", "", surefinal[,1], fixed = TRUE)     
                names(surefinal)[5] <- name.file
                dupli <- surefinal[duplicated(surefinal[,1]),]	
                if (nrow(dupli) > 0){
                    while(nrow(dupli) > 0){
                        checkDupli <- dupli[1,]
                        sameName <- subset(surefinal, surefinal$Name == checkDupli$Name)
                        sameName$deff <- abs(sameName$deff)
                        choseName <- sameName[which(sameName$deff == min(abs(sameName$deff), na.rm = TRUE)),]
                        if (nrow(choseName) > 1){
                            choseName <- choseName[which(choseName[,5] == max(choseName[,5], na.rm = TRUE)),]
                        }
                        sameName <- sameName[-(which(row.names(sameName) == row.names(choseName))),]
                        surefinal <- surefinal[-which(row.names(surefinal) %in% row.names(sameName)),]
                        dupli <- dupli[-(which(dupli$Name %in% checkDupli$Name)),]
                    }
                }
            }        
            if (Result.df == 1) {
                final.df <- surefinal[c(1,5)]
                confirmation <- paste("File ", c, " (", name.file,".CDF) ", "done!", sep = "")
                message(confirmation)
                Result.df <- Result.df + 1
            } else {
                surefinal <- surefinal[c(1,5)]
                final.df <- merge(final.df, surefinal, by.x = "Name", by.y = "Name", all = TRUE)
                confirmation <- paste("File ", c, " (", name.file,".CDF) ", "done!", sep = "")
                message(confirmation)
                final.df <- subset(final.df, final.df[1] != "CompError")
            }                
        } else {
            confirmation <- paste("File ", c, " (", name.file,".CDF) ", "not analyzed! Probably there is no metabolites detected for this sample in the AMDIS report", sep = "")
            message(confirmation)
            if (Result.df == 1){
        	final.df <- data.frame(Name = "CompError", error = NA, stringsAsFactors = FALSE)
        	names(final.df)[ncol(final.df)] <- name.file
        	Result.df <- Result.df + 1
            } else {
        	final.df$error <- NA
                names(final.df)[ncol(final.df)] <- name.file
            }
        }
    }
    final.df <- data.frame(final.df, stringsAsFactors = FALSE, check.names = FALSE)
    final.df[1] <- as.character(final.df[,1])
    final.df <- final.df[order(final.df[, 1], decreasing = FALSE),]
    rep.name.final <- c("Replicates", replicates)
    final.df <- rbind(rep.name.final, final.df)
    row.names(final.df) <- 1:nrow(final.df)
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
        write.csv(final.df, file = store, row.names = FALSE)
        message("File saved: ", store, "\n")
##### Generate log file ######
        logFile <- data.frame(Directory = folder, Amdis_report = AmdisReportUsed, Ion_library = ionLibUsed, Output_file = store, Time_window = TimeWindow, Analysis_date = date(), Metabolites_removed = RemoveUsed, Abundances = AbundancesUsed)
        sheet <- "logFile"
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
        write.csv(logFile, file = store, row.names = FALSE)	
        message("Log file saved: ", store, "\n")
    }
    return(final.df)
}
