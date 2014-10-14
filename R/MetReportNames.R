MetReportNames <-
function (data, AmdisReport, base.peak = FALSE, save = TRUE, folder, output = "metab_data", TimeWindow = 2.5, Remove)
{    
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

 ######### Check if a CSV or .msl file ############
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
  ####################################################
  
  ######### Check if a CSV or .msl file  NO DIALOG BOX ############
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
  ####################################################  

  ##### If no AMDIS report specified ####
  if (missing(AmdisReport)) {
    AmdisReport <- dlgOpen(title = "Select the AMDIS report generated in batch mode")$res
    AmdisReportUsed <- AmdisReport
    AmdisReport = read.csv(AmdisReport, sep = "\t", colClass = "character")
  } else {
    if (is.data.frame(AmdisReport)) {
	AmdisReportUsed <- "data_frame"
      AmdisReport <- AmdisReport
      message("AmdisReport - Data frame loaded...")
    } else {
      if (is.character(AmdisReport)) {
        inputTest <- file.access(AmdisReport, 0)
          if (inputTest == 0) {
		AmdisReportUsed <- AmdisReport
            AmdisReport = read.csv(AmdisReport, sep = "\t", colClasses = "character")
          } else {
            dlgMessage("The AmdisReport specified is not accessible. Please, select the AMDIS report generated in batch mode.")
            AmdisReport <- dlgOpen(title = "Select the AMDIS report generated in batch mode")$res
		AmdisReportUsed <- AmdisReport
            AmdisReport = read.csv(AmdisReport, sep = "\t", colClass = "character")
            
          } 
      } else {
        dlgMessage("The AmdisReport specified is not valid. Please, select the AMDIS report generated in batch mode.")
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


	######################################################
	########## Collect Data ##############################
######################################################  
if (missing(data)) {
		filesToAnalyse <- total_report[1]
		filesToAnalyse <- data.frame(filesToAnalyse[!duplicated(filesToAnalyse),], stringsAsFactors = FALSE)
		filesToAnalyse <- data.frame(gsub("\\", "/", filesToAnalyse[,1], fixed = TRUE), stringsAsFactors = FALSE)
		filesToAnalyse <- data.frame(apply(filesToAnalyse, 1, function(x) basename(as.character(x))), stringsAsFactors = FALSE) 
		filesToAnalyse[1] <- gsub(".FIN", "", filesToAnalyse[,1], fixed = TRUE)
		data <- dlgList(filesToAnalyse[,1], multiple = TRUE, title = "Select the files to be analyzed.")$res
		dataUsed <- data
	} else {
		if (is.character(data)) {
			dataUsed <- data
			dataUsed <- gsub(".FIN", "", dataUsed, fixed = TRUE)
		} else {
			message("data must be specified as character.")
			filesToAnalyse <- total_report[1]
			filesToAnalyse <- data.frame(filesToAnalyse[!duplicated(filesToAnalyse),], stringsAsFactors = FALSE)
			filesToAnalyse <- data.frame(gsub("\\", "/", filesToAnalyse[,1], fixed = TRUE), stringsAsFactors = FALSE)
			filesToAnalyse <- data.frame(apply(filesToAnalyse, 1, function(x) basename(as.character(x))), stringsAsFactors = FALSE) 
		    filesToAnalyse[1] <- gsub(".FIN", "", filesToAnalyse[,1], fixed = TRUE)
			data <- dlgList(filesToAnalyse[,1], multiple = TRUE, title = "Select the files to be analyzed.")$res
			dataUsed <- data
		}
	}

	######################################################
	########## Collect Folder ############################
###################################################### 
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

#################################
###### Start analysis ###########
#################################
  Result.df <- 1
  for (i in 1:length(dataUsed)){
  	surefinal <- c()
  	file <- dataUsed[i]
  	name.file <- file
  	file <- paste(file, ".FIN", sep = "")
  	file <- grep(file, total_report$FileName, fixed = TRUE)
  	if (length(file) > 0) {
	  	data.df <- total_report[file, ]
	  	if (base.peak){
        	Keep <- c("Name", "RT", "Scan", "Expec..RT", 
                  "Base.Peak")
            AbundancesUsed <- "Base.Peak"
        } else {
        	Keep <- c("Name", "RT", "Scan", "Expec..RT", 
                  "Area")  
            AbundancesUsed <- "Area"	
        }
        data.df <- data.df[, Keep]
        data.df[2] <- as.numeric(data.df[,2])
        data.df[3] <- as.numeric(data.df[,3])
        data.df[4] <- as.numeric(data.df[,4])
        data.df[5] <- as.numeric(data.df[,5])
        data.df$deff <- data.df$RT - data.df$Expec..RT
        data.df <- subset(data.df, data.df$deff < TimeWindow & data.df$deff > -TimeWindow)
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
	  	
	  	if (Result.df == 1) {
          final.df <- surefinal[c(1,5)]
          confirmation <- paste("File", name.file, "done!", sep = " ")
          message(confirmation)
	      Result.df <- Result.df + 1
        } else {
          surefinal <- surefinal[c(1,5)]
          final.df <- merge(final.df, surefinal, by.x = "Name", by.y = "Name", all = TRUE)
          confirmation <- paste("File", name.file, "done!", sep = " ")
          message(confirmation)
        }
      } else {
        confirmation <- paste("File ", name.file, " not analyzed! Probably there is no metabolites detected for this sample in the AMDIS report", sep = "")
        message(confirmation)
        final.df$error <- NA
        names(final.df)[ncol(final.df)] <- name.file
      }
    }	
    final.df <- final.df[order(final.df[, 1], decreasing = F),]
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
	  	logFile <- data.frame(Directory = data, Amdis_report = AmdisReportUsed, Output_file = store, Time_window = TimeWindow, Analysis_date = date(), Metabolites_removed = RemoveUsed, Abundances = AbundancesUsed)
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
