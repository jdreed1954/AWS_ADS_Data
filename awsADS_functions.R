require(tidyverse)
require(rjson)

print(paste('Working directory:',getwd()))

dataNodes           <-
  c(
    "destinationProcessConnection",
    "networkInterface",
    "osInfo",
    "process",
    "results",
    "sourceProcessConnection",
    "systemPerformance"
  )

getAgentIDs         <- function(root = 'data/agentExports') {
  # Define top of data heirarchy, typically: 'data/agentExports'
  agentsRoot <- root
  
  # Agent IDs
  list.files(agentsRoot)
}

getPerf             <-
  function(root = 'data/agentExports', agentID = "o-19bub6kx628zxp16t") {
    # Get performance data for one agentID
    sysPerfDir <-
      paste(root, "/", agentID, "/", "systemPerformance", sep = "")
    
    c_names <-
      c(
        "accountNumber",
        "agentId",
        "DiskBytesReadPS",
        "DiskBytesWrittenPS",
        "DiskReadOpsPS",
        "DiskWriteOpsPS",
        "NetworkBytesReadPS",
        "NetworkBytesWrittenPS",
        "NumLogProcs"  ,
        "NumCores",
        "NumCpus",
        "NumDisks",
        "NumNwCrds",
        "CpuUsagePct",
        "DiskSize.GB",
        "DiskFreeSize.GB",
        "RAM.MB",
        "freeRAM.MB",
        "timestamp"
      )
    
    c_types <- c("dcddddddiiiiidddddT")
    
    perf_tbl <- tibble()
    perfFiles    <- list.files(sysPerfDir, full.name = TRUE)
    perfFilesNum <- length(perfFiles)
    if (perfFilesNum == 0) {
      return(perf_tbl)
    }
    
    for (j in 1:perfFilesNum) {
      perf_tbl  <- rbind(perf_tbl,
                         read_csv(
                           perfFiles[j],
                           col_names = c_names,
                           col_types = c_types,
                           skip = 1
                         ))
    }
    
    return(perf_tbl)
  }

getosInfo           <-
  function(root = 'data/agentExports', agentID = "o-19bub6kx628zxp16t") {
    # Get OS Info data for one agentID
    dDir <- paste(root, "/", agentID, "/", "osInfo", sep = "")
    
    c_names <-
      c(
        "accountNumber",
        "agentId",
        "osName",
        "osVersion",
        "cpuType",
        "hostName",
        "hypervisor",
        "timestamp"
      )
    
    c_types <- c("dccccccT")
    
    d_tbl <- tibble()
    dFiles    <- list.files(dDir, full.name = TRUE)
    dFilesNum <- length(dFiles) 
    if (dFilesNum == 0) { return (d_tbl) }
    
    for (j in 1:dFilesNum) {
      d_tbl  <- rbind(d_tbl,
                      read_csv(
                        dFiles[j],
                        col_names = c_names,
                        col_types = c_types,
                        skip = 1
                      ))
    }
    d_tbl$hostName <- fixHostnames(d_tbl$hostName)
    return(d_tbl)
  }

getDestProcConn     <-
  function(root = 'data/agentExports', agentID = "o-19bub6kx628zxp16t") {
    # Get Destination Process Connection one agentID
    dDir <-
      paste(root, "/", agentID, "/", "destinationProcessConnection", sep = "")
    
    c_names <-
      c(
        "accountNumber",
        "agentId",
        "sourceIp",
        "sourcePort",
        "destinationIp",
        "destinationPort",
        "ipVersion",
        "transportProtocol",
        "agentAssProcId",
        "agentCreationDate",
        "daemonName",
        "daemonParam"
      )
    
    c_types <- c("dccccccccTcc")
    
    d_tbl     <- tibble()
    dFiles    <- list.files(dDir, full.name = TRUE)
    dFilesNum <- length(dFiles)
    
    for (j in 1:dFilesNum) {
      d_tbl  <-
        rbind(d_tbl,
              read_csv(
                dFiles[j],
                col_names = c_names,
                col_types = c_types,
                skip = 1
              ))
    }
    
    return(d_tbl)
  }

getSourceProcConn   <-
  function(root = 'data/agentExports', agentID = "o-19bub6kx628zxp16t") {
    # Get Source Process Connection one agentID
    dDir <-
      paste(root, "/", agentID, "/", "sourceProcessConnection", sep = "")
    
    c_names <-
      c(
        "accountNumber",
        "agentId",
        "sourceIp",
        "sourcePort",
        "destinationIp",
        "destinationPort",
        "ipVersion",
        "transportProtocol",
        "agentAssProcId",
        "agentCreationDate",
        "daemonName",
        "daemonParam"
      )
    
    c_types <- c("dccccccccTcc")
    
    d_tbl     <- tibble()
    dFiles    <- list.files(dDir, full.name = TRUE)
    dFilesNum <- length(dFiles)
    
    for (j in 1:dFilesNum) {
      d_tbl  <-
        rbind(d_tbl,
              read_csv(
                dFiles[j],
                col_names = c_names,
                col_types = c_types,
                skip = 1
              ))
    }
    
    return(d_tbl)
  }

getProcess          <-
  function(root = 'data/agentExports', agentID = "o-19bub6kx628zxp16t") {
    # Get Processes one agentID
    dDir <- paste(root, "/", agentID, "/", "process", sep = "")
    
    c_names <-
      c(
        "accountNumber",
        "agentId",
        "agentAssignedProcessId",
        "isSystem",
        "name",
        "cmdLine",
        "path",
        "agentProvidedTimeStamp"
      )
    
    c_types <- c("dcclcccT")
    
    d_tbl     <- tibble()
    dFiles    <- list.files(dDir, full.name = TRUE)

    dFilesNum <- length(dFiles)
    if ( dFilesNum == 0 ) { return(d_tbl) }
    for (j in 1:dFilesNum) {
      d_tbl  <-
        rbind(d_tbl,
              read_csv(
                dFiles[j],
                col_names = c_names,
                col_types = c_types,
                skip = 1
              ))
    }
    
    return(d_tbl)
  }

getNetworkInterface <-
  function(root = 'data/agentExports', agentID = "o-19bub6kx628zxp16t") {
    # Get Source Process Connection one agentID
    dDir <- paste(root, "/", agentID, "/", "networkInterface", sep = "")
    
    c_names <-
      c(
        "accountNumber",
        "agentId",
        "name",
        "macAddress",
        "family",
        "ipAddress",
        "gateway",
        "netMask",
        "timestamp"
      )
    
    c_types <- c("dcccccccT")
    
    d_tbl     <- tibble()
    dFiles    <- list.files(dDir, full.name = TRUE)
    dFilesNum <- length(dFiles)
    if(dFilesNum == 0) { return(d_tbl) }
    for (j in 1:dFilesNum) {
      d_tbl  <-
        rbind(d_tbl,
              read_csv(
                dFiles[j],
                col_names = c_names,
                col_types = c_types,
                skip = 1
              ))
    }
    
    return(d_tbl)
  }

getResults          <- function(root = 'data/agentExports', agentID = "o-19bub6kx628zxp16t") {
    # Get results for one agentID
    dDir <- paste(root, "/", agentID, "/", "results", sep = "")
    d_tbl <- tibble(
      accountNumber = NA,
      agentID = NA  ,
      requestedStartTime = NA,
      requestedEndTime = NA,
      resultFilesGenerated = NA,
      totalSizeOfResults = NA,
      exportStartedAt = NA,
      exportStatus = NA,
      exportSummaryisTruncated = NA,
      actualStartTime = NA,
      actualEndTime = NA,
      statusMessage = NA
    )
    
    d_tbl_out <- d_tbl
    
    dFiles    <- list.files(dDir, full.name = TRUE)
    dFilesNum <- length(dFiles)
    
    for (j in 1:dFilesNum) {
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
      if (is.null(d_json$ExportSummary$ActualStartTime)){
        d_tbl$actualStartTime          <- NA
        d_tbl$actualEndTime            <- NA
      } else {
        d_tbl$actualStartTime          <- d_json$ExportSummary$ActualStartTime
        d_tbl$actualEndTime            <- d_json$ExportSummary$ActualEndTime
      }
      d_tbl$statusMessage            <- d_json$StatusMessage
     
      d_tbl_out <- rbind(d_tbl_out, d_tbl)
    }
    d_tbl_out <- d_tbl_out[2:nrow(d_tbl_out), ]
    return(d_tbl_out)
  }


getDestProcConn <- function(root = 'data/agentExports', agentID = "o-19bub6kx628zxp16t") {
  
    # Get Destination Process Connection one agentID
    dDir <- paste(root, "/", agentID, "/", "destinationProcessConnection", sep = "")
    
    c_names <-
      c(
        "accountNumber",
        "agentId",
        "sourceIp",
        "sourcePort",
        "destinationIp",
        "destinationPort",
        "ipVersion",
        "transportProtocol",
        "agentAssignedProcessId",
        "agentCreationDate",	
        "commandPath",
        "commandParam"
      )
    
    c_types <- c("dccccccccTcc")
    
    d_tbl     <- tibble()
    dFiles    <- list.files(dDir, full.name = TRUE)
    dFilesNum <- length(dFiles)
    if  (dFilesNum == 0) { return(d_tbl)}
    for (j in 1:dFilesNum) {
      d_tbl  <-
        rbind(d_tbl,
              read_csv(
                dFiles[j],
                col_names = c_names,
                col_types = c_types,
                skip = 1
              ))
    }
    
    return(d_tbl)
  }

getSrcProcConn <- function(root = 'data/agentExports', agentID = "o-19bub6kx628zxp16t") {
  
  # Get Destination Process Connection one agentID
  dDir <- paste(root, "/", agentID, "/", "sourceProcessConnection", sep = "")
  
  c_names <-
    c(
      "accountNumber",
      "agentId",
      "sourceIp",
      "sourcePort",
      "destinationIp",
      "destinationPort",
      "ipVersion",
      "transportProtocol",
      "agentAssignedProcessId",
      "agentCreationDate",	
      "commandPath",
      "commandParam"
    )
  
  c_types <- c("dccccccccTcc")
  
  d_tbl     <- tibble()
  dFiles    <- list.files(dDir, full.name = TRUE)
  dFilesNum <- length(dFiles)
  if  (dFilesNum == 0) { return(d_tbl)}
  for (j in 1:dFilesNum) {
    d_tbl  <-
      rbind(d_tbl,
            read_csv(
              dFiles[j],
              col_names = c_names,
              col_types = c_types,
              skip = 1
            ))
  }
  
  return(d_tbl)
}




lsRoot              <- function(root = 'data/agentExports') {
  # List, recursively, the directories in the root directory
  list.dirs(root, recursive = TRUE)
}

lsRootFiles         <- function(root = 'data/agentExports') {
  # List, recursively, the files in the root directory
  list.files(root, recursive = TRUE)
}

getStats <- function(df,metric) {
  IDs <- unique(df$agentId)
  stats <- tibble(
    Host = map_chr(IDs, ~ subset(df,agentId == .x)$hostName[1]),
    Min =  map_dbl(IDs, ~ sapply(filter(df,agentId == .x)[,metric], min, na.rm = TRUE)),
    Max =  map_dbl(IDs, ~ sapply(filter(df,agentId == .x)[,metric], max, na.rm = TRUE)),
    Mean = map_dbl(IDs, ~ sapply(filter(df,agentId == .x)[,metric], mean, na.rm = TRUE)),
    Std =  map_dbl(IDs, ~ sapply(filter(df,agentId == .x)[,metric], sd, na.rm = TRUE)),
    Med =  map_dbl(IDs, ~ sapply(filter(df,agentId == .x)[,metric], median, na.rm = TRUE)),
    P95 =  map_dbl(IDs, ~ sapply(filter(df,agentId == .x)[,metric], quantile, .95))
  )
  stats$Host <- fixHostnames(stats$Host)
  return (stats)
  
}


fixHostnames <- function(names) {
  ret <- vector(mode = "character", length = length(names))
  for ( i in 1:length(names)) { 
    ret[i] <- strsplit(names[i], ".", fixed = TRUE)[[1]][1]
  }
  return(unlist(ret))
}


getCPUs <- function(df) {
  
  dt <- getStats(df,"CpuUsagePct")
  IDs <- unique(df$agentId)
  dt$CPU <-  map_dbl(IDs, ~ sapply(filter(df,agentId == .x)[,"NumCpus"], max, na.rm = TRUE))
  dt$CPU.util = dt$P95 * dt$CPU /100
  dt$CPUs.Rec = ceiling(dt$CPU.util)
  
  return (dt)
  
}

getMEMs <- function(df) {
  
  dt <- getStats(df,"freeRAM.MB")
  dt <- dt %>% select(-"P95")
  IDs <- unique(df$agentId)
  dt$RAM.MB   <- map_dbl(IDs, ~ sapply(filter(df,agentId == .x)[,"RAM.MB"], max, na.rm = TRUE))
  dt$RAM.GB  <- ceiling(dt$RAM.MB / 1024)
  dt$RAM.util = (dt$RAM.MB - dt$Min) / dt$RAM.MB * 100
  #dt$RAM.rec  = ceiling(dt$RAM.util)
  
  return (dt)
  
}


getDiskOpsStats <- function(df) {
  
  dtr <- getStats(df,"DiskReadOpsPS")    %>% arrange(Host)
  dtw <- getStats(df,"DiskWriteOpsPS") %>% arrange(Host)
  
  return (list(DiskReadOps = dtr, DiskWriteOps = dtw))
  
}

getDiskRWStats <- function(df) {
  
  dtr <- getStats(df,"DiskBytesReadPS")    %>% arrange(Host)
  dtw <- getStats(df,"DiskBytesWrittenPS") %>% arrange(Host)
  
  return (list(DiskRead = dtr, DiskWrite = dtw))
  
}

getNetStats <- function(df) {
  dtr <- getStats(df,"NetworkBytesReadPS")    %>% arrange(Host)
  dtw <- getStats(df,"NetworkBytesWrittenPS") %>% arrange(Host)
  
  return (list(NetworkRead = dtr, NetworkWrite = dtw))
  
}


genStatsDF <- function(df) {
  
  out <- tibble(
    hostName              = df$"hostName"[1],
    agentId               = df$"agentId"[1],
    DiskBytesReadPS       = getStats(df,"DiskBytesReadPS"),
    DiskBytesWrittenPS    = getStats(df,"DiskBytesWrittenPS"),
    DiskReadOpsPS         = getStats(df,"DiskReadOpsPS"),
    DiskWriteOpsPS        = getStats(df,"DiskWriteOpsPS"),       
    NetworkBytesReadPS    = getStats(df,"NetworkBytesReadPS"),
    NetworkBytesWrittenPS = getStats(df,"NetworkBytesWrittenPS"),
    NumLogProcs           = getStats(df,"NumLogProcs"),
    NumCores              = getStats(df,"NumCores"),             
    NumCpus               = getStats(df,"NumCpus"),
    NumDisks              = getStats(df,"NumDisks"),
    NumNwCrds             = getStats(df,"NumNwCrds"),
    CpuUsagePct           = getStats(df,"CpuUsagePct"),          
    DiskSize.GB           = getStats(df,"DiskSize.GB"),
    DiskFreeSize.GB       = getStats(df,"DiskFreeSize.GB"),
    RAM.MB                = getStats(df,"RAM.MB"),
    freeRAM.MB            = getStats(df,"freeRAM.MB"),
    cpuType               = df$cpuType[1],
    hypervisor            = df$hypervisor[1]
  )
  return(out)
  
}

genCompPerf <-  function(DataRoot, agentIDs) {
  
  prList  <-   map(agentIDs, ~ getPerf(DataRoot,. ))
  osList  <-   map(agentIDs, ~ getosInfo(DataRoot,. ))
  
  # Build comprehensive join of perfAll and osAll
  
  perfAll <- bind_rows(prList) %>% select(1,2,19, 3:18) %>% arrange(agentId,timestamp)
  osAll   <- bind_rows(osList) %>% select(1,2,8,6,3:5,7) %>% arrange(agentId, timestamp)
  
  
  osValues <- osAll %>% 
    group_by(agentId) %>% 
    summarize( hostName =   unique(hostName),  osName  = unique(osName),   
               osVersion =  unique(osVersion), cpuType = unique(cpuType), 
               hypervisor = unique(hypervisor))
  
  # This is the tibble to use for sizing and review - write this to an Exce3l file in tabs: Summary, host(1), host(2), ...
  cInventory <- left_join(perfAll, osValues, by = "agentId") %>% select(1,2,20,3:19,21:24)
  
  return( cInventory )
}

genPctPlot <- function(dat, metric){
  print(dat$metric)
  p <- ggplot(dat, aes(x = timestamp, y = metric)) + geom_line()
  return(p)
}


getHostCollection <- function(DataRoot) {
  
  agentIDs <- getAgentIDs(root = DataRoot)
  collected <-  tibble(
    Number   = seq(1:length(agentIDs)),
    AgentID = agentIDs,
    Hostname = map_chr(agentIDs, 
                       ~as.character(getosInfo(root = DataRoot, agentID = .)[1,"hostName"])),
    IPaddress = map_chr(agentIDs, 
                        ~as.character(getSourceProcConn(root = DataRoot, agentID = .)[1,"sourceIp"]))
  )
  
  return(collected)
}

getHostSummary <- function(root = DataRoot) {
  
  agentIDs <- getAgentIDs(root = DataRoot)
  hs <-  tibble(
    Number     = seq(1:length(agentIDs)),
    Host       = map_chr(agentIDs, 
                  ~as.character(getosInfo(root = DataRoot, agentID = .x)[1,"hostName"])),
    IP         = map_chr(agentIDs,
                  ~as.character(getSourceProcConn(root = DataRoot, agentID = .x)[1,"sourceIp"])),
    First      =  as_datetime(map_dbl(agentIDs, ~min(getPerf(DataRoot, .x)$timestamp))),
    Last       =  as_datetime(map_dbl(agentIDs, ~max(getPerf(DataRoot, .x)$timestamp))), 
    Perf.Days  = difftime(as.POSIXct(Last), as.POSIXct(First), units = "days"),
    OS.Name         = map_chr(agentIDs,
                         ~as.character(getosInfo(root = DataRoot, agentID = .x)[1,"osName"])),
    OS.Version = map_chr(agentIDs,
                         ~as.character(getosInfo(root = DataRoot, agentID = .x)[1,"osVersion"]))
  )
  hs <- arrange(hs, Host)
  hs$Number <- seq(1:length(agentIDs))
  
  return(hs)
}

configRmd <- function(inp = "ADS_Downloaded_Data_Inventory") {
  require(rmarkdown)
  #-------- define the input filename --------#
  infile <- "ADS_Downloaded_Data_Inventory"
  #----- Now just hit the source button! -----#
  
  
  # check that the input file actually exists!
  stopifnot(file.exists(paste(infile,".Rmd", sep = "")))
  
  # create the output filename
  outfile <- paste("ADS_Downloaded_Data_Inventory_","(",as.Date(Sys.time()),").pdf", sep = "")
  #outfile <- paste("ADS_Downloaded_Data_Inventory_.pdf", sep = "")
  
  # compile the document
  rmarkdown::render(input="ADS_Downloaded_Data_Inventory.Rmd", output_file=outfile)
  
 
} 


getCompInventory <- function(DataRoot) {
  
  agentIDs <- getAgentIDs(root = DataRoot)
  prList   <- map(agentIDs, ~ getPerf(DataRoot,. ))
  osList   <- map(agentIDs, ~ getosInfo(DataRoot,. ))
  
  # Build comprehensive join of perfAll and osAll
  
  perfAll <- bind_rows(prList) %>% select(1,2,19, 3:18) %>% arrange(agentId,timestamp)
  osAll   <- bind_rows(osList) %>% select(1,2,8,6,3:5,7) %>% arrange(agentId, timestamp)
  
  
  osValues <- osAll %>% 
    group_by(agentId) %>% 
    summarize( hostName =   unique(hostName),  osName  = tail(osName, n = 1),   
               osVersion =  unique(osVersion), cpuType = unique(cpuType), 
               hypervisor = unique(hypervisor))
  
  # This is the tibble to use for sizing and review - write this to an Exce3l file in tabs: Summary, host(1), host(2), ...
  compInventory <- left_join(perfAll, osValues, by = "agentId") %>% select(1,2,20,3:19,21:24)
  
  return(compInventory)
  
  
}
