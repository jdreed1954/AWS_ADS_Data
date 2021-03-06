library(tidyverse)

getwd()
print(paste('Working directory:',getwd()))

dataNodes           <- c( "destinationProcessConnection", "networkInterface", "osInfo", "process",
                          "results", "sourceProcessConnection", "systemPerformance")

getAgentIDs         <- function(root = 'data/agentExports') {
  # Define top of data heirarchy, typically: 'data/agentExports' 
  agentsRoot <- root
  
  # Agent IDs
  return (list.files(agentsRoot))
}

getPerf             <- function(root = 'data/agentExports', agentID= "o-19bub6kx628zxp16t") {
  
  # Get performance data for one agentID
  sysPerfDir <- paste(root,"/",agentID,"/","systemPerformance", sep = "")
  
  c_names <- c("accountNumber", "agentId",        "DiskBytesReadPS",    "DiskBytesWrittenPS",
               "DiskReadOpsPS", "DiskWriteOpsPS", "NetworkBytesReadPS", "NetworkBytesWrittenPS",
               "NumLogProcs"  , "NumCores",       "NumCpus",            "NumDisks", 
               "NumNwCrds",     "CpuUsagePct",    "DiskSize.GB",        "DiskFreeSize.GB", 
               "RAM.MB",        "freeRAM.MB",     "timestamp")
  
  c_types <- c("dcddddddiiiiidddddT")
  
  perf_tbl <- tibble()
  perfFiles    <- list.files(sysPerfDir, full.name = TRUE)
  perfFilesNum <- length(perfFiles)

  for (j in 1:perfFilesNum){
    perf_tbl  <- rbind(perf_tbl,read_csv(perfFiles[j], 
                      col_names = c_names, col_types = c_types, skip = 1))
  }
  
  return(perf_tbl)
}

getosInfo           <- function(root = 'data/agentExports', agentID= "o-19bub6kx628zxp16t") {

    # Get OS Info data for one agentID
  dDir <- paste(root,"/",agentID,"/","osInfo", sep = "")
  
  c_names <- c("accountNumber", "agentId",    "osName",     "osVersion",
               "cpuType",       "hostName",   "hypervisor", "timestamp")

  c_types <- c("dccccccT")

  d_tbl <- tibble()
  dFiles    <- list.files(dDir, full.name = TRUE)
  dFilesNum <- length(dFiles)
  
  for (j in 1:dFilesNum){
    d_tbl  <- rbind(d_tbl,read_csv(dFiles[j], col_names = c_names, 
                                   col_types = c_types, skip = 1))
  }
  
  return(d_tbl)
}

getDestProcConn     <- function(root = 'data/agentExports', agentID= "o-19bub6kx628zxp16t") {
  
  # Get Destination Process Connection one agentID
  dDir <- paste(root,"/",agentID,"/","destinationProcessConnection", sep = "")
  
  c_names <- c("accountNumber",  "agentId",           "sourceIp",   "sourcePort",
               "destinationIp",  "destinationPort",   "ipVersion",  "transportProtocol",
               "agentAssProcId", "agentCreationDate", "daemonName", "daemonParam")
  
  c_types <- c("dccccccccTcc")
  
  d_tbl     <- tibble()
  dFiles    <- list.files(dDir, full.name = TRUE)
  dFilesNum <- length(dFiles)
  
  for (j in 1:dFilesNum){
    d_tbl  <- rbind(d_tbl, read_csv(dFiles[j], col_names = c_names, col_types = c_types, skip = 1))
  }
  
  return(d_tbl)
}

getSourceProcConn   <- function(root = 'data/agentExports', agentID= "o-19bub6kx628zxp16t") {
  
  # Get Source Process Connection one agentID
  dDir <- paste(root,"/",agentID,"/","sourceProcessConnection", sep = "")
  
  c_names <- c("accountNumber",  "agentId",           "sourceIp",   "sourcePort",
               "destinationIp",  "destinationPort",   "ipVersion",  "transportProtocol",
               "agentAssProcId", "agentCreationDate", "daemonName", "daemonParam")
  
  c_types <- c("dccccccccTcc")
  
  d_tbl     <- tibble()
  dFiles    <- list.files(dDir, full.name = TRUE)
  dFilesNum <- length(dFiles)
  
  for (j in 1:dFilesNum){
    d_tbl  <- rbind(d_tbl, read_csv(dFiles[j], col_names = c_names, col_types = c_types, skip = 1))
  }
  
  return(d_tbl)
}

getProcess          <- function(root = 'data/agentExports', agentID= "o-19bub6kx628zxp16t") {
  
  # Get Source Process Connection one agentID
  dDir <- paste(root,"/",agentID,"/","process", sep = "")
  
  c_names <- c("accountNumber", "agentId", "agentAssignedProcessId",   "isSystem",
               "name",          "cmdLine", "path",                     "agentProvidedTimeStamp")
               
  c_types <- c("dcclcccT")
  
  d_tbl     <- tibble()
  dFiles    <- list.files(dDir, full.name = TRUE)
  dFilesNum <- length(dFiles)
  
  for (j in 1:dFilesNum){
    d_tbl  <- rbind(d_tbl, read_csv(dFiles[j], col_names = c_names, col_types = c_types, skip = 1))
  }
  
  return(d_tbl)
}

getNetworkInterface <- function(root = 'data/agentExports', agentID= "o-19bub6kx628zxp16t") {
  
  # Get Source Process Connection one agentID
  dDir <- paste(root,"/",agentID,"/","networkInterface", sep = "")
  
  c_names <- c("accountNumber", "agentId",   "name",    "macAddress",
               "family",        "ipAddress", "gateway", "netMask",
               "timestamp")
  
  c_types <- c("dcccccccT")
  
  d_tbl     <- tibble()
  dFiles    <- list.files(dDir, full.name = TRUE)
  dFilesNum <- length(dFiles)
  
  for (j in 1:dFilesNum){
    d_tbl  <- rbind(d_tbl, read_csv(dFiles[j], col_names = c_names, col_types = c_types, skip = 1))
  }
  
  return(d_tbl)
}

getResults          <- function(root = 'data/agentExports', agentID= "o-19bub6kx628zxp16t"){
  # Get results for one agentID
  dDir <- paste(root,"/",agentID,"/","results", sep = "")
  
  d_tbl <- tibble(
    accountNumber = NA,     agentID = NA  ,            requestedStartTime = NA, 
    requestedEndTime = NA,  resultFilesGenerated = NA, totalSizeOfResults = NA, 
    exportStartedAt= NA,    exportStatus = NA,         exportSummaryisTruncated = NA,
    actualStartTime = NA,   actualEndTime = NA,        statusMessage = NA
  )
  
  d_tbl_out <- d_tbl
  
  dFiles    <- list.files(dDir, full.name = TRUE)
  dFilesNum <- length(dFiles)
  
  for (j in 1:dFilesNum){
    d_json  <- fromJSON(file = dFiles[j], simplify = TRUE)
    
    d_tbl$accountNumber            <- d_json$CustomerAccount
    d_tbl$agentID                  <- d_json$RequestedFilters[[1]]$Values
    d_tbl$requestedStartTime       <- d_json$RequestedStartTime
    d_tbl$requestedEndTime         <- d_json$RequestedEndTime
    d_tbl$resultFilesGenerated     <- d_json$ResultFilesGenerated
    d_tbl$totalSizeOfResults       <- d_json$TotalSizeOfResultFiles
    d_tbl$exportStartedAt          <- d_json$ExportStartedAt
    d_tbl$exportStatus             <- d_json$ExportStatus
    d_tbl$exportSummaryisTruncated <- d_json$ExportSummary$isTruncated
    d_tbl$actualStartTime          <- d_json$ExportSummary$ActualStartTime
    d_tbl$actualEndTime            <- d_json$ExportSummary$ActualEndTime
    d_tbl$statusMessage            <- d_json$StatusMessage
  
    d_tbl_out <- rbind(d_tbl_out,d_tbl)
  }
  d_tbl_out <- d_tbl_out[2:nrow(d_tbl_out),]
  return(d_tbl_out)
}
  
lsRoot              <- function(root = 'data/agentExports'){
   # List, recursively, the directories in the root directory 
   list.dirs(root, recursive = TRUE)
}

lsRootFiles         <- function(root = 'data/agentExports'){
  # List, recursively, the files in the root directory 
  list.files(root, recursive = TRUE)
}
  