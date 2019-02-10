# TREMSEN toolbox  -------------------------------------------------------------------
# Version: 1.0
# Date: Feb 09, 2019
# Latest version available @ https://github.com/NIATS-UFU/TREMSEN-Toolbox.git

# Author: Prof. Adriano de Oliveira Andrade
# Contact email: adriano@ufu.br
# CV LATTES: http://lattes.cnpq.br/1229329519982110
# ORCID ID: http://orcid.org/0000-0002-5689-6606
# Researcher ID: http://www.researcherid.com/rid/D-9721-2012 
# GOOGLE ACADEMIC: https://scholar.google.com.br/citations?user=8nHKQHMAAAAJ&hl=pt-BR


# Adddress: Centre for Innovation and Technology Assessment in Health, 
#           Postgraduate Program in Electrical and Biomedical Engineering, 
#           Faculty of Electrical Engineering, Federal University of Uberlândia, 
#           Uberlândia, Brazil
# Webpage:  http://www.niats.feelt.ufu.br/

# Description: toolbox for processing data collected with the device TREMSEN


#' Set of customized functions used for data analysis

buildDataSet <-function(filename, processData = FALSE){
  
  #http://www.gastonsanchez.com/Handling_and_Processing_Strings_in_R.pdf
  
  f <- basename(filename)
  
  matches <- regmatches(f, gregexpr("[[:digit:]]+", f))
  
  subjectID <- as.numeric(unlist(matches))[1]
  trial<- as.numeric(unlist(matches))[2]
  
  
  matches <- regmatches(f, gregexpr('SC|CC|CC92|CC184', f))
  carga <- matches[[1]]
  carga <- as.numeric(unlist(regmatches(carga, gregexpr("[[:digit:]]+", carga))))[1]
  
  if(is.na(carga)==TRUE) carga <- 0
  
  x = regexpr(pattern = 'CONTROLE|PARKINSON', filename)
  grupo <- substring(filename, x, x + attr(x, "match.length") - 1)
  
  x = gregexpr(pattern = '(REPOUSO|FLEXAO|EXTENSAO|ESTATICO|ESPIRAL|SENOIDAL|FORCA)', filename)
  protocol <- substring(filename, x[[1]], x[[1]] + attr(x[[1]], "match.length") - 1)
  protocol <-  paste(protocol,collapse='-')
  
  if(processData == FALSE){
    
    dataSet <- list ('filename' = filename, 'subjectID' = subjectID, 'grupo' = grupo, 
                     'protocol' = protocol, 'carga' = carga,'trial' = trial) 
  }
  else{
    
    
    show(filename)
    
    #Load TREMSEN dataset
    df <- LoadTREMSENFile(filename)
    
    #Detrend TREMSEN dataset (nonlinear detrending)
    df.nonlineardetrended <- nonLineardetrendTremsenData(df)
    
    #Estimate the power spectrum of a TREMSEN dataset
    pp <- psdTremsenData(df.nonlineardetrended) ## It is a good practice to remove trends prior to use this function
    headStr <- names(pp[[1]])
    
    #Get the number of signal windows
    Nwindows <- length(pp)
    
    #Create the main list that will store all the resultant information
    myls <- vector("list", length = length(headStr))
    names(myls)<-headStr
    
    #Create label for each signal window
    wndLabels <- laply(1:Nwindows, function(x) paste('Window', x,sep=''))
    
    for(i in seq(1,length(headStr))){
      myls[[i]]<- vector("list", length = length(wndLabels))
      names(myls[[i]])<- wndLabels
    }
    
    
    #median of the RMS
    df.featTremsenData <- featExtractFromTremenDataSet(df.nonlineardetrended,w=50,s=10, method = "rms")
    medianRMS <- getStatisticsFromWindowedTremenDataSet(df.featTremsenData, f=median)
    
    for(i in seq(1,length(headStr))){
      for(j in 1:Nwindows){
        
        #myls[[i]][wndLabels[j]]<- pp[[j]]
        
        #Select disctinct frequency bands
        indxf1 <- which(pp[[j]][[i]]$freq < 2.5)
        indxf2 <- which(pp[[j]][[i]]$freq >= 2.5 & pp[[j]][[i]]$freq < 5.0)
        indxf3 <- which(pp[[j]][[i]]$freq >= 5.0 & pp[[j]][[i]]$freq <= 10)
        indxf4 <- which(pp[[j]][[i]]$freq > 10 & pp[[j]][[i]]$freq <=15)
        
        wE1 <- weighted.mean(pp[[j]][[i]]$spec[indxf1],pp[[j]][[i]]$freq[indxf1])
        wE2 <- weighted.mean(pp[[j]][[i]]$spec[indxf2],pp[[j]][[i]]$freq[indxf2])
        wE3 <- weighted.mean(pp[[j]][[i]]$spec[indxf3],pp[[j]][[i]]$freq[indxf3])
        wE4 <- weighted.mean(pp[[j]][[i]]$spec[indxf4],pp[[j]][[i]]$freq[indxf4])
        
        wF1 <- weighted.mean(pp[[j]][[i]]$freq[indxf1],pp[[j]][[i]]$spec[indxf1])
        wF2 <- weighted.mean(pp[[j]][[i]]$freq[indxf2],pp[[j]][[i]]$spec[indxf2])
        wF3 <- weighted.mean(pp[[j]][[i]]$freq[indxf3],pp[[j]][[i]]$spec[indxf3])
        wF4 <- weighted.mean(pp[[j]][[i]]$freq[indxf4],pp[[j]][[i]]$spec[indxf4])
        
        myls[[i]][[wndLabels[j]]]$'wE1'<- wE1
        myls[[i]][[wndLabels[j]]]$'wE2'<- wE2
        myls[[i]][[wndLabels[j]]]$'wE3'<- wE3
        myls[[i]][[wndLabels[j]]]$'wE4'<- wE4
        
        myls[[i]][[wndLabels[j]]]$'wF1'<- wF1
        myls[[i]][[wndLabels[j]]]$'wF2'<- wF2
        myls[[i]][[wndLabels[j]]]$'wF3'<- wF3
        myls[[i]][[wndLabels[j]]]$'wF4'<- wF4
        
        myls[[i]][[wndLabels[j]]]$'medianRMS'<- medianRMS[[j]][[1]][[headStr[i]]]
        
        
      }
    }
    
    
    dataSet <- list ('filename' = filename, 'subjectID' = subjectID, 'grupo' = grupo, 
                     'protocol' = protocol, 'carga' = carga,'trial' = trial, 
                     'ls' = myls) 
    
  }
  
  return(dataSet)
}

getData <- function(sensorId, dataSet){
  
  teste <- list.select(dataSet,
                       filename, 
                       subjectID, 
                       grupo, 
                       protocol, 
                       carga, 
                       trial, 
                       'sensor' = names(dataSet[[1]]$ls[sensorId]),
                       "medianRMS" = ls[[sensorId]]$Window1$medianRMS,
                       "wE1" = ls[[sensorId]]$Window1$wE1,
                       "wE2" = ls[[sensorId]]$Window1$wE2,
                       "wE3" = ls[[sensorId]]$Window1$wE3,
                       "wE4" = ls[[sensorId]]$Window1$wE4,
                       "wF1" = ls[[sensorId]]$Window1$wF1,
                       "wF2" = ls[[sensorId]]$Window1$wF2,
                       "wF3" = ls[[sensorId]]$Window1$wF3,
                       "wF4" = ls[[sensorId]]$Window1$wF4)
  
  X<-as.data.frame(list.rbind(teste))
  
  X$filename <- as.character(X$filename)
  X$subjectID <- as.numeric(X$subjectID)
  X$grupo <- as.character(X$grupo)
  X$protocol <- as.character(X$protocol)
  X$carga <- as.numeric(X$carga)
  X$trial <- as.numeric(X$trial)
  X$sensor <- as.character(X$sensor)
  X$medianRMS <- as.numeric(X$medianRMS)
  X$wE1 <- as.numeric(X$wE1)
  X$wE2 <- as.numeric(X$wE2)
  X$wE3 <- as.numeric(X$wE3)
  X$wE4 <- as.numeric(X$wE4)
  X$wF1 <- as.numeric(X$wF1)
  X$wF2 <- as.numeric(X$wF2)
  X$wF3 <- as.numeric(X$wF3)
  X$wF4 <- as.numeric(X$wF4)
  
  X <- X[order(X$grupo, X$subjectID, X$protocol, X$carga, X$trial,X$sensor ),]
  
}