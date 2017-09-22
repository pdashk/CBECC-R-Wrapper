## CBECC-Res 2016 Auto-Run Script
## Phi Nguyen, Energy Solutions
## Last Edit: September 22, 2017

## NOTES ------------------------------------------------------------------------------------------
# chose to disable the feature that allows keep/removal of output because file size pretty small (7kb), and would prefer to have CBECC analayses completed as soon as possible. Worth giving up disk space for slightly faster completion
# will read files after all CBECC runs are done
# run by sourcing cbeccWrapper.R (this file)
# if CBECC crashes or is closed, then might receive errors before combining

## SETUP ------------------------------------------------------------------------------------------
##load lirbaries, working directory, default variables

library(XML) #required for CBECC inputs (they are in XML format)
path_to_cbecc_exe <- "C:\\Program Files (x86)\\CBECC-Res 2016\\CBECC-Res16.exe" #default CBECC path

# messauge upon sourcing script
cat("Hi, welcome to my wrapper.
Made for CBECC-Res 2016. Have not yet tested on CBECC-Com or CBECC 2019.
This was made by Phi Nguyen, Energy Solutions 2017

Here's what it does:
1) Takes CBECC XML File + config.R file that specifies which parameters to run
2) Creates all combinations of desired parameters using the XML File as base (for parameters not selected)
3) Uses Windows cmd.exe to run CBECC-Res for each parameter combination 
4) Creates a folder called '-results' with all the run .log files (deletes other files)
5) Takes all of these .log files and compiles into on master dataset.
6) Also has indicator to see progress, or time remaining

Here's how to use it:
1) Configure a config.R file, placed in same directory as CBECC XML file.
2) Make sure using checkPath(), testPath(), or setPath() that CBECC-Res 2016 execution file path is correct
3) To start, initiate(config = \"config.R\", keep.output = FALSE)")


## ANCILLARY FUNCTIONS ----------------------------------------------------------------------------
## will be used in the core function

cbeccXML <- function(y) { 
    mainXML <- newXMLNode("SDDXML")
    xmlnsxsi <- newXMLNamespace(mainXML, "http://www.w3.org/2001/XMLSchema-instance", "xsi")
    xmlnsxsd <- newXMLNamespace(mainXML, "http://www.w3.org/2001/XMLSchema", "xsd")
    
    newXMLNode(names(y)[1], attrs = c(file = y$RulesetFilename[[1]]), parent = mainXML) # just for this first one with the funny attribute
    
    for (i in 2:length(y)) {
        nodeList <- list() # to preserve heirarchy
        nodeList[[1]] <- newXMLNode(names(y)[i], parent = mainXML) # OK because these nodes don't have text or attributes
        
        z <- list() # list to track node level
        k <- vector() # vector to track of node levels.
        j <- 1 # start level index
        k[j] <- 1 # start element
        z[[j]] <- y[[i]] # start level
        
        while (k[1] <= length(z[[1]])) { # Cycle through until all elements of all levels are accounted for 
            while (k[j] <= length(z[[j]])) { # Cycle throught current level until all elements of that are accounted for
                if (length(names(z[[j]][[k[j]]])) < 2) {
                    text <- z[[j]][[k[j]]]
                    subNode <- newXMLNode(names(z[[j]])[k[j]], text, parent = nodeList[[j]]) # no attribute, otherwise length(z) > 1. This is because nodes that do not have attributes have text at parent level
                    k[j] <- k[j] + 1
                } else if (".attrs" %in% names(z[[j]][[k[j]]])) {
                    text <- z[[j]][[k[j]]]$text
                    attrs <- z[[j]][[k[j]]]$.attrs
                    subNode <- newXMLNode(names(z[[j]])[k[j]], text, attrs = attrs, parent = nodeList[[j]]) # when there is an attribute
                    k[j] <- k[j] + 1
                } else {
                    nodeList[[j+1]] <- newXMLNode(names(z[[j]])[k[j]], parent = nodeList[[j]]) # parent nodes don't have text or attributes
                    j <- j + 1 # move down a level
                    z[[j]] <- z[[j-1]][[k[j-1]]]  
                    k[j] <- 1 # start over on this new level
                }
            }
            j <- j - 1 # go back a level after all elements considered
            k[j] <- k[j] + 1
        }
    }
    return(mainXML)
} # this takes parsed XML object that already has changed parameters, and puts everything into a new XMl object and saves onto disk

combineLogs <- function(directory = dirName) {
    logs <- list.files(path = dirName, pattern = ".log.csv", full.names = T, include.dirs = F)
    logList <- list()
    logList[[logs[[1]]]] <- read.csv(logs[1], header = F, stringsAsFactors = F)
    for (log in logs[-1]) {
        logList[[log]] <- read.csv(log, header = F, stringsAsFactors = F)[4,]
    }
    cbeccResults <- do.call("rbind", logList)
    cbeccResults <- cbeccResults[,-236] #last NA column
    rownames(cbeccResults) <- c() #remove row names
    return(cbeccResults) 
} # function to take all .log.csv files and combine with master parameter set

resultsHeaderClean <- function(cbeccResults = cbeccResults, headerRows = 3) {
    resultsCleaned <- cbeccResults
    headers <- vector()
    for (i in 1:headerRows) {
        for (j in 2:ncol(resultsCleaned)) {
            if (resultsCleaned[i,j] == "") {
                if (resultsCleaned[i,j-1] == "" ) {
                    resultsCleaned[i,j] <- "" 
                } else {
                    resultsCleaned[i,j] <- resultsCleaned[i,j-1] 
                }
            }
        }
    }
    for (i in 1:ncol(resultsCleaned)) {
        headers[i] <- paste(resultsCleaned[1:headerRows,i], collapse = " ")
    }
    resultsCleaned <- resultsCleaned[(headerRows+1):nrow(resultsCleaned),]
    names(resultsCleaned) <- headers
    return(resultsCleaned)
} # function to give logs proper header

checkPath <- function() {
    return(path_to_cbecc_exe)
} #check path to CBECC execution files

setPath <- function(path = x) {
    path_to_cbecc_exe <<- path
    return(path_to_cbecc_exe)
} #set path to CBECC execution files

testPath <- function() {
    system("cmd.exe", wait = T, input = paste0("\"",path_to_cbecc_exe,"\""))
    print("Should open CBECC. If successful, you still need to close it on your own.")
} #test path to CBECC execution files



## CORE FUNCTION ----------------------------------------------------------------------------------
## input config.R (base file and parameters to run), output master CSV, and all individual .log files
## this iteration creates all .log files first, then assesses (instead of assess within loop) in order to prioritize completing CBECC runs

initiate <- function(config, keep.output = TRUE) {
    source(config) #source config file to obtain path to 
    startTime <- Sys.time() #start time to see how long whole process takes and to estimate remaining time
    master <- expand.grid(newVars, stringsAsFactors = F) #formulate master final table by permutation of desired parameters and eventually adding in read csv results
    fileList <- vector()
    dirName <- paste0("testresults-",base_xml) #create directory name for test results
    dir.create(dirName, showWarnings = FALSE) #create directory to store results
    a <- xmlParse(paste0(base_xml,".xml")) #read base XML file
    progress <- winProgressBar(title = "Running CBECC Automation", label = "Progress", min = 0, max = nrow(master), width = 600) #progress bar
    for (i in 1:nrow(master)) {
        file <- paste0(base_xml,"-",i)
        output_log_file <- paste0(dirName,"/",file,".log.csv")
        xml_path <- paste0(dirName,"/",file,".xml")
        fileList[i] <- file    
        b <- xmlToList(a)
        if (file.exists(output_log_file)) {
            next # move on if output already exists
        } else {
            b$Proj$Name <- base_xml
            b$Proj$ProjFileName <- file
            b$Proj$CreateDate <- as.numeric(startTime) #createdate, moddate, rundate
            b$Proj$ModDate <- as.numeric(startTime)
            b$Proj$RunDate <- as.numeric(startTime)
            b$Proj$Address <- "default"
            b$Proj$City <- "default"
            b$Proj$ZipCode <- 0
            for (param in names(master)) {
                z <- master[i,param]
                if (is.character(z)) {
                    z <- paste0("\"",z,"\"")
                }
                eval(parse(text = paste(param,"<-",z)))
            } # change all params of interest
            
            # account for other areas dependent of floor area
            b$Proj$Zone$CeilingBelowAttic$Area <- b$Proj$Zone$FloorArea
            b$Proj$Zone$SlabFloor$Area <- as.numeric(b$Proj$Zone$FloorArea) / as.numeric(b$Proj$Zone$NumStories)
            b$Proj$Zone$SlabFloor$Perimeter <- sqrt(b$Proj$Zone$SlabFloor$Area)*4
            
            c <- cbeccXML(b) #createXML
            saveXML(c, xml_path) #saveXML for CBECC (later to be deleted)
            system("cmd.exe", wait = T, input = paste(paste0("\"",path_to_cbecc_exe,"\""),"-pa","-nogui",xml_path)) # CBECC execuation via windows cmd line (xml input)
            
            # checks in case CBECC software gets stuck (not common)
            ticker <- 1 #start ticker to move onto next analaysis in case this one gets stuck
            ticker_limit <- 180 #3 min limit to run
            tickerExpire <- 0 #start counter for number of failures
            tickerExpire_limit <- 10 #if more than 10 failures, then terminate loop
            while (!file.exists(output_log_file) && ticker < ticker_limit) { 
                Sys.sleep(1)
                ticker <- ticker + 1
            } # wait for cbecc to finish (by scanning every second for output file) or skip if longer than limit
            if (ticker == ticker_limit) {
                tickerExpire <- tickerExpire + 1
            } # failure counter
            if (tickerExpire > 10) {
                stop("Too many failtures! Terminating script!")
            }  #stop if too many failures
            
            unlink(list.dirs(path = dirName, recursive = F), recursive = T) # remove all directories created by CBECC
            file.remove(grep(list.files(path = dirName, full.names = T, include.dirs = F), pattern='.log.csv', inv = T, value = T)) # removes all files that are not .log.csv
        }    
        
        remainTime <- as.numeric(Sys.time() - startTime, units = "hours")/i*(nrow(master)-i)  #calculate time since start, dvided by iterations to estimate time remaining
        setWinProgressBar(progress, i, label = paste(round(i/nrow(master)*100,0),"% Complete -","Estimated Time Remaining:",round(remainTime,3),"hours.","Failures:",tickerExpire)) #change progress bar
        
        
    } # for-loop to run each row in the master file
    
    master["XML File"] <- fileList
    names(master) <- gsub(".*\\$","",names(master))
    a <- resultsHeaderClean(combineLogs(dirName))
    a[," Project Path/File"] <- sapply(a[," Project Path/File"], FUN = gsub, pattern = ".*\\\\|.xml", replacement = "", USE.NAMES = F)
    # 
    # read the CSVs and input results in master sheet
    master <- merge(master, a, by.x = "XML File", by.y = " Project Path/File") # merge results and parameters
    
    outputName <- paste0(config, "-",Sys.Date(),".csv")
    write.csv(master, outputName) # write the output as csv (no return for this function)
    
    close(progress)
    print(paste("Successfully Completed Running",base_xml, "on", Sys.time()))
    print(paste("Output saved as:", outputName))
    print(paste("Total Elapsed Time:", as.numeric(Sys.time() - startTime, units = "hours"))) #calculate total run time
} # MAIN FUNCTION

