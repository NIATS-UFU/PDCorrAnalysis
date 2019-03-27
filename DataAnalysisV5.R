# Accompanying code  -------------------------------------------------------------------
# Version: 2.0
# Date: March 26, 2019

# Author: Prof. Adriano de Oliveira Andrade, PhD
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

# Description: Accompanying code of the manuscript 
#        "Task-specific Tremor Quantification in a Clinical Setting for Parkinson's 
#         Disease Using Wearable Inertial Sensors"



# Load libraries ----------------------------------------------------------
source("TREMSENToolbox.r") #tremsen toolbox

# Figure 2 ----------------------------------------------------------------

#'Loading a sample file of a collected data (V15C2RSC.txt)
strFolder <- "./SampleData/"
strFileName <- "V15C2RSC.txt"
df <- LoadTREMSENFile(paste(strFolder,strFileName,sep = ""))

# A) Visualization of the raw data collected 
#    from a subject with the wrist flexed at rest. 
#    Signals from IMU1, IMU2, and the digital pulse are shown. 
#    Triaxial signals from the gyroscope, accelerometer, 
#    and magnetometer are presented for each IMU. Signals 
#    from the accelerometer and magnetometer are subjected 
#    to the influence of gravity and Earth's magnetic field 
#    respectively. 

gg1 <- plotTremsenDataset(list(df),indxs=c(1,2),printplot=FALSE, droplvs = c("X.PULSE.A.","X.PULSE.B."))
gg2 <- plotTremsenDataset(list(df),indxs=c(3,4),printplot=FALSE, droplvs = c("X.PULSE.A.","X.PULSE.B."))
gg3 <- plotTremsenDataset(list(df),indxs=c(5,6),printplot=FALSE, droplvs = c("X.PULSE.A.","X.PULSE.B."))
gout <- grid.arrange(gg1[[1]], gg2[[1]], gg3[[1]], nrow = 1, ncol=3, top = "")
ggsave("./Figures/Figure2a.tiff",plot = gout, device="tiff",width = 38, height = 20, units = "cm",dpi = 300)

# B) Typical signals after pre-processing the raw signals in (A).

df.nonlineardetrended <- nonLineardetrendTremsenData(df)
lsplot <- list(df.nonlineardetrended)
gg1 <- plotTremsenDataset(lsplot,indxs=c(1,2),printplot=FALSE, droplvs = c("X.PULSE.A.","X.PULSE.B."), alphavec = c(1, 0.3))
gg2 <- plotTremsenDataset(lsplot,indxs=c(3,4),printplot=FALSE, droplvs = c("X.PULSE.A.","X.PULSE.B."), alphavec = c(1, 0.3))
gg3 <- plotTremsenDataset(lsplot,indxs=c(5,6),printplot=FALSE, droplvs = c("X.PULSE.A.","X.PULSE.B."), alphavec = c(1, 0.3))
gout <- grid.arrange(gg1[[1]], gg2[[1]], gg3[[1]], nrow = 1, ncol=3, top = "")
ggsave("./Figures/Figure2b.tiff",plot = gout, device="tiff",width = 38, height = 20, units = "cm",dpi = 300)



# Figure 4 ----------------------------------------------------------------

# Figure 4. Example of pre-processed signals (in red) and their respective RMS curves (in black). The medianRMS is estimated as the median of the RMS 
# curve in the region of analysis, i.e., in which the PULSE equals to one.

df.featTremsenData <- featExtractFromTremenDataSet(df.nonlineardetrended,w=50,s=10, method = "rms")
gg1 <- plotTremsenDataset(list(df.featTremsenData,df.nonlineardetrended ),indxs=c(1,2),printplot=FALSE, droplvs = c("X.PULSE.A.","X.PULSE.B."), alphavec = c(1, 0.3))
gg2 <- plotTremsenDataset(list(df.featTremsenData,df.nonlineardetrended ),indxs=c(3,4),printplot=FALSE, droplvs = c("X.PULSE.A.","X.PULSE.B."), alphavec = c(1, 0.3))
gg3 <- plotTremsenDataset(list(df.featTremsenData,df.nonlineardetrended ),indxs=c(5,6),printplot=FALSE, droplvs = c("X.PULSE.A.","X.PULSE.B."), alphavec = c(1, 0.3))
gout <- grid.arrange(gg1[[1]], gg2[[1]], gg3[[1]], nrow = 1, ncol=3, top = "")
ggsave("./Figures/Figure4.tiff",plot = gout, device="tiff",width = 38, height = 20, units = "cm",dpi = 300)


# Figure 5 ----------------------------------------------------------------
# Figure 5. Example of PSD estimated from the signals shown in Figure 2B. The energy of wE1, wE2, wE3, and wE4 are calculated from the first four frequency bands. The energy is in 
# log_10 scale for better visualization.

pp <- psdTremsenData(df.nonlineardetrended) ## It is a good practice to remove trends prior to use this function
g1<-plotPSTremsenDataSet(pp[[1]],printplot = TRUE)

tiff("./Figures/Figure5.tiff", width = 10, height = 30, units = "cm", res = 300) #saving in tiff
print(g1)
dev.off() #fechar o arquivo

pdf("./Figures/Figure5.pdf", width = 5, height = 10) #saving in pdf
print(g1)
dev.off() #fechar o arquivo

getPeakFreq <- function(pp)
{
  
  indx <- which.max(log10(pp$spec))
  val <- pp$freq[indx]
  return(val)
}


# RQ1: Sensor positioning -------------------------------------------
# This analysis aims to verify which sensor positioning is more correlated with the UPDRS scores for the evaluation
# of rest and postural tremor

# Load data set (two data sets are available: DataSet_fb1.xlsx and DataSet_fb2.xlsx).
#  The features wE1, wE2, wE3 and wE4 were estimated using the frequency bands below, for each data set.
#  fb1 <- list(b1=c(0, 2.5), b2 = c(2.5, 5.0), b3= c(5.0, 10.0), b4 = c(10, 15)) - DataSet_fb1.xlsx
#  fb2 <- list(b1=c(0, 4), b2 = c(4, 7), b3= c(7, 14), b4 = c(15, 21)) - DataSet_fb2.xlsx


# Load data set
Excelfilename <- "./DataSet/DataSet_fb1.xlsx"
sheetName <- "Sheet 1"
D <- read.xlsx(Excelfilename,sheetName)

# Get sensor list
sensors <- levels(factor(D$sensor))

# Subject list
# These are the subjects who executed both rest and postural protocols in the study 
# (data from the other subjects are not included in the analysis)

ss_list <- c(3, 5, 9, 10, 11, 15, 16, 18, 19, 21, 22)

# Feature list
feat_list <- c("medianRMS", "wE1","wE2",  "wE3", "wE4")

# Function for estimating the correlation coefficient between features and UPDRS scores
cor_estimate <- function (x, y) {
  cr <- cor.test(x, y, method = "spearman", continuity = TRUE, exact=FALSE)
  c("r" = cr$estimate, "p.value" = cr$p.value)
}


# Estimate correlation for the rest protocol
ds_RT0 <- subset(D, subjectID %in% ss_list &  protocol == "REPOUSO" & carga == 0,
                 select=c(sensor, UPDRS.T.20, medianRMS, wE1, wE2, wE3, wE4))

ds_RT0.split <- split(ds_RT0, ds_RT0$sensor)

ds_RT0.corr <- lapply(ds_RT0.split,
       df_corr <- function(df){
         mapply(cor_estimate,df[c("medianRMS","wE1", "wE2", "wE3", "wE4")], df["UPDRS.T.20"])
       })

ds_RT0.res <- melt(ds_RT0.corr)

ds_RT0.res$protocol <- "REPOUSO"


# Estimate correlation for the postural protocol
ds_RP0 <- subset(D, subjectID %in% ss_list &  protocol == "ESTATICO" & carga == 0,
                 select=c(sensor, UPDRS.P.21, medianRMS, wE1, wE2, wE3, wE4))

ds_RP0.split <- split(ds_RP0, ds_RP0$sensor)

ds_RP0.corr <- lapply(ds_RP0.split,
                      df_corr <- function(df){
                        mapply(cor_estimate,df[c("medianRMS","wE1", "wE2", "wE3", "wE4")], df["UPDRS.P.21"])
                      })

ds_RP0.res <- melt(ds_RP0.corr)

ds_RP0.res$protocol <- "ESTATICO"

# binding results
dfres <- rbind(ds_RP0.res, ds_RT0.res)
dfres<-dcast(dfres, protocol + L1  ~ Var1 + Var2 ) 
colnames(dfres)[2] <- "sensor"

# resultant data frame
dfres$sensor <- substring(gsub("\\.", "", dfres$sensor), 2)

# Figure 6. --------------------------------------
# Figure 6. Radar plot, showing the correlation coefficients (r) between sensor features and UPDRS scores (UPDRS_P_21 for postural protocol and UPDRS_T_20 for rest protocol). Strong and very strong correlations (0.6 < r < 1) are in the region between the dotted circles. The yellow (rest) and grey (postural) lines connecting 
# the correlation coefficient values was used to enhance visualization of results.
# function for plot correlation coefficients
plotdfres <- function(dfres, corrCoef, featLabel, SaveFig=FALSE, figname="fig", plotFig = TRUE)
{
  
  g <- ggplot(data=dfres,  aes(x=dfres$sensor , y= corrCoef , 
                               group=dfres$protocol, colour=dfres$protocol)) +
    ylim(0,1)+
    geom_hline(aes(yintercept=0.99), lwd=1, lty=2) + 
    geom_hline(aes(yintercept=0.6), lwd=1, lty=2) + 
    geom_point(size=5) + 
    geom_line() + 
    
    annotate("text", x = 0, 
             y = seq(0, 1, 0.20), 
             label = seq(0, 1, 0.20), size = 7) +
    
    coord_polar() + theme_classic(base_size = 30) + 
    
    # remove original y-axis text / ticks & black border
    theme(axis.text.x = element_text(colour="black", size=25), 
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.line = element_blank(),
          legend.title = element_blank(),
          legend.direction = 'horizontal',
          #legend.position = 'bottom',
          legend.position = c(0.1, 0.99),
          panel.border = element_blank(),
          panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                          colour = "grey95")) +
    
    scale_color_manual(labels = c("Postural", "Rest"),
                       values = c("#999999", "#E69F00")) +
    xlab("") +
    ylab(paste("r ", "(", featLabel, ")", sep = ""))
  
  if(plotFig==TRUE)
    (
      print(g)
      
    )
  
  if (SaveFig == TRUE)(
    ggsave(
      paste(figname, ".tiff", sep = ""),
      plot = g,
      width = 10,
      height = 10,
      dpi = 300,
      device = "tiff")
  )
  
  return(g)
  
}

figname <- paste("./Figures/","Fig", 1, sep = "")
g1 <- plotdfres(dfres, dfres$r.rho_medianRMS, "medianRMS", SaveFig=TRUE, figname = figname)

figname <- paste("./Figures/","Fig", 2, sep = "")
g2 <- plotdfres(dfres, dfres$r.rho_wE1, "wE1", SaveFig=TRUE, figname = figname)

figname <- paste("./Figures/","Fig", 3, sep = "")
g3 <- plotdfres(dfres, dfres$r.rho_wE2, "wE2", SaveFig=TRUE, figname = figname)

figname <- paste("./Figures/","Fig", 4, sep = "")
g4 <- plotdfres(dfres, dfres$r.rho_wE3, "wE3", SaveFig=TRUE, figname = figname)

figname <- paste("./Figures/","Fig", 5, sep = "")
plotdfres(dfres, dfres$r.rho_wE4, "wE4", SaveFig=TRUE, figname = figname)


# Table 4 ------------------------------------------------------------------
# Table 4. Mean correlation coefficient of strong and very strong correlations (>0.6) for each feature, 
# sensor and task. The overall mean is estimated for each sensor and task.
rmean <- function(x, estimateMean = TRUE)
{
  a1 <- mean(x[which(x>= 0.6)], na.rm = TRUE)
  a2 <- sd(x[which(x>= 0.6)], na.rm = TRUE)
  
  if(estimateMean == TRUE){
    return(a1) # return the mean
  }
  else
  {
    return(a2) # return the standard deviation
  }
}


sensorStats <- function(df1, st)
{
  print(st)
  
  vv.mean <- sapply(split(df1, df1$protocol), function(x) apply(x[,3:7], 2, 
                                                                rmean, estimateMean=TRUE))
  vv.sd <- sapply(split(df1, df1$protocol), function(x) apply(x[,3:7], 2, 
                                                              rmean, estimateMean=FALSE))
  print("mean")
  print(vv.mean)
  
  print( apply(vv.mean,2,mean) )
  
  print("sd")
  print(vv.sd)
  
}

library(tidyverse)
g <- str_detect(dfres$sensor, "2")

df.splitbysensor <- split(dfres, g)

sensorStats(df.splitbysensor[[1]], "sensor 1 - corcoef >= 0.6 (mean - sd)")
sensorStats(df.splitbysensor[[2]], "sensor 2 - corcoef >= 0.6 (mean - sd)")

# Table 5 - RQ2: Estimate the best sensor components ---------------------------------------------
# Table 5. Sensor ranking according to the largest correlation coefficient of each task and feature.
ss <- split(dfres, dfres$protocol)
df_Post <- ss[[1]]
df_Rest <- ss[[2]]


df1 <- df_Post[which.max(df_Post$r.rho_medianRMS), c(1, 2, 3, 8 ) ]
df2 <- df_Rest[which.max(df_Rest$r.rho_medianRMS), c(1, 2, 3, 8 ) ]

df3 <-df_Post[which.max(df_Post$r.rho_wE1), c(1, 2, 4, 9 ) ]
df4 <- df_Rest[which.max(df_Rest$r.rho_wE1), c(1, 2, 4, 9 ) ]

df5 <- df_Post[which.max(df_Post$r.rho_wE2), c(1, 2, 5, 10 ) ]
df6 <- df_Rest[which.max(df_Rest$r.rho_wE2), c(1, 2, 5, 10 ) ]

df7 <- df_Post[which.max(df_Post$r.rho_wE3), c(1, 2, 6, 11 ) ]
df8 <- df_Rest[which.max(df_Rest$r.rho_wE3), c(1, 2, 6, 11 ) ]

df9 <- df_Post[which.max(df_Post$r.rho_wE4), c(1, 2, 7, 12 ) ]
df10 <- df_Rest[which.max(df_Rest$r.rho_wE4), c(1, 2, 7, 12 ) ]

ll <- list(df1, df2, df3, df4, df5, df6, df7, df8, df9, df10)
ll <- melt(ll)
ll[order( ll$protocol, ll$value, decreasing = TRUE),]

# Table 6 - RQ3: Severity ranking ---------------------------------------------------
# Table 6. Objective assessment of the rest tremor severity. The objective score is contrasted with the UPDRS_T_20 
# score for each participant.
# Rest protocol
dfRT <- subset(D, subjectID %in% ss_list &  protocol == "REPOUSO" & carga == 0 & sensor=="X.A1.X.",
                 select=c(subjectID, UPDRS.T.20, medianRMS, wE1, wE2, wE3, wE4))

rr <- aggregate(dfRT, by= list(dfRT$subjectID), FUN=mean)

rr[, 4:8] <- scores(rr[, 4:8])

rr[order(rr$wE3, decreasing = TRUE), c("subjectID", "UPDRS.T.20", "wE3")]
cor.test(rr$wE3, rr$UPDRS.T.20, method = "spearman", continuity = TRUE, exact=FALSE)

# Postural protocol
# Table 7. Objective assessment of the postural tremor severity. The objective score is contrasted with the 
# UPDRS_P_21 score for each participant.
dfPT <- subset(D, subjectID %in% ss_list &  protocol == "ESTATICO" & carga == 0 & sensor=="X.G1.X.",
               select=c(subjectID, UPDRS.P.21, medianRMS, wE1, wE2, wE3, wE4))

rr <- aggregate(dfPT, by= list(dfPT$subjectID), FUN=mean)

rr[, 4:8] <- scores(rr[, 4:8])

rr[order(rr$wE4, decreasing = TRUE), c("subjectID", "UPDRS.P.21", "wE4")]

cor.test(rr$wE4, rr$UPDRS.P.21, method = "spearman", continuity = TRUE, exact=FALSE)


# Figure 8 - RQ4: Discrimination analysis -------------------------------------------------
# Sensitivity of the methods/sensors to change as function of the experimental protocols (rest, postural)
# Figure 8. Scatter plots showing data visualizations obtained for Subject 3. Joint visualization of pair of scores estimated from distinct
# sensor features are presented. 

# Load data
#Excelfilename <- "./DataSet/DataSet_fb1.xlsx" # fb1 = list(b1=c(0, 2.5), b2 = c(2.5, 5.0), b3= c(5.0, 10.0), b4 = c(10, 15))
sheetName <- "Sheet 1"
D <- read.xlsx(Excelfilename,sheetName)
D <- D[order(D$subjectID, D$carga, D$sensor, D$trial ),] # sort data frame

# Data scaling (see the R function scale for more details)
medianRMS_list <- tapply(D$medianRMS, list(D$sensor, D$carga, D$subjectID), scale) 
medianRMS_scaled <- unlist(medianRMS_list)

wE1_list <- tapply(D$wE1, list(D$sensor, D$carga, D$subjectID), scale) 
wE1_scaled <- unlist(wE1_list)

wE2_list <- tapply(D$wE2, list(D$sensor, D$carga, D$subjectID), scale) 
wE2_scaled <- unlist(wE2_list)

wE3_list <- tapply(D$wE3, list(D$sensor, D$carga, D$subjectID), scale) 
wE3_scaled <- unlist(wE3_list)

wE4_list <- tapply(D$wE4, list(D$sensor, D$carga, D$subjectID), scale) 
wE4_scaled <- unlist(wE4_list)


Dscaled <- D
Dscaled[,"medianRMS"] <- medianRMS_scaled
Dscaled[,"wE1"] <- wE1_scaled
Dscaled[,"wE2"] <- wE2_scaled
Dscaled[,"wE3"] <- wE3_scaled
Dscaled[,"wE4"] <- wE4_scaled

Dscaled <- Dscaled[order(Dscaled$subjectID, 
                         Dscaled$carga, 
                         Dscaled$sensor,
                         Dscaled$protocol,
                         Dscaled$trial ),] # sort data frame

# Sensor list
sensors <- list("X.A1.X.", "X.A1.Y.", "X.A1.Z.", "X.A2.X.", "X.A2.Y.", "X.A2.Z.", "X.G1.X.",
                "X.G1.Y.", "X.G1.Z.", "X.G2.X.", "X.G2.Y.", "X.G2.Z.", "X.M1.X.", "X.M1.Y.",
                "X.M1.Z.", "X.M2.X.", "X.M2.Y.", "X.M2.Z.")

ss_list <- c(3, 5, 9, 10, 11, 15, 16, 18, 19, 21, 22) # These are the subjects who executed both rest and postural protocols



ss <- ss_list[1] # Select the subject ID here for data visualization


# Select data of subject ss
X <- subset(Dscaled,subjectID%in%ss & carga == 0 & sensor %in% sensors,
            select=c(subjectID, protocol, sensor, trial, medianRMS, wE1, wE2, wE3, wE4))

# function for scatter plotting
scatterPlot <- function(df,v1,v2, ptitle)
{
  
  textconf <- element_text(size = 10)
  psize <- 1.5
  
  g <- ggplot(df, aes(x= df[,v1], y= df[,v2], color=protocol)) + geom_point(shape=19, size = psize)
  
  g <- g + ggtitle(as.character(ss))
  g <- g + xlim(-2, 2)
  g <- g + ylim(-2, 2)
  
  
  g <- g +  theme_light(base_size = 12)
  g <- g + theme(legend.position = "right",
                 legend.key.size = unit(2, 'lines'))
  g <- g + ggtitle(ptitle)
  g <- g + theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = c(0.01, 0.99),
    legend.justification = c("left", "top"),
    legend.title = element_blank(),
    legend.direction = "vertical",
    legend.text = textconf,
    axis.text.x = textconf,
    axis.text.y = textconf,
    axis.title.x = textconf,
    axis.title.y = textconf,
    legend.key = element_rect(size = 5),
    legend.key.size = unit(1, 'lines')
  )
  g <-
    g +  scale_color_manual(labels = c("Postural", "Rest"),
                            values = c("#999999", "#E69F00"))
  g <- g +  xlab(paste("score ", "(", v1, ")", sep = ""))
  g <- g +  ylab(paste("score ", "(", v2, ")", sep = ""))
  return(g)
  
}


g12 <- scatterPlot(X, "medianRMS", "wE1", paste("Subject", ss))
g13 <- scatterPlot(X, "medianRMS", "wE2", paste("Subject", ss))
g14 <- scatterPlot(X, "medianRMS", "wE3", paste("Subject", ss))
g15 <- scatterPlot(X, "medianRMS", "wE4", paste("Subject", ss))

g23 <- scatterPlot(X, "wE1", "wE2", paste("Subject", ss))
g24 <- scatterPlot(X, "wE1", "wE3", paste("Subject", ss))
g25 <- scatterPlot(X, "wE1", "wE4", paste("Subject", ss))

g34 <- scatterPlot(X, "wE2", "wE3", paste("Subject", ss))
g35 <- scatterPlot(X, "wE2", "wE4", paste("Subject", ss))

g45 <- scatterPlot(X, "wE3", "wE4", paste("Subject", ss))


gg <- grid.arrange(g12, g13, g14, g15,
                   g23, g24, g25, g34, 
                   g35, g45, ncol=4)


SaveFig <- TRUE

figname <- paste("./Figures/","Subject", ss, sep = "")

if (SaveFig == TRUE)(
  ggsave(
    paste(figname, ".tiff", sep = ""),
    plot = gg,
    width = 15,
    height = 10,
    dpi = 300,
    device = "tiff")
)

# Comparing Means from Paired Samples Using R -----------------------------

# http://sphweb.bumc.bu.edu/otlt/MPH-Modules/QuantCore/PH717_ComparingMeans/PH717_ComparingMeans4.html


x.split <- split(X, X$protocol)


# From the output, the p-value > 0.05 implying that
# the distribution of the data are not significantly different from normal distribution. 
# In other words, we can assume the normality.


p.diffAnalysis <- function(v1, v2)
{
  vd <- v2 - v1
  shap <- shapiro.test(vd)
  
  if(shap$p.value < 0.05)
  {
    # normality cannot be assumed
    p <- wilcox.test(v1, v2, paired = TRUE) #http://www.sthda.com/english/wiki/paired-samples-wilcoxon-test-in-r
    
  } else
  {
    # normality can be assumed
    p <- t.test(v1,v2,paired=TRUE, alternative = "two.sided") # http://www.sthda.com/english/wiki/paired-samples-t-test-in-r
  }
  
  return (c(mean(vd), p$p.value)) # mean of the differences and the p-value, respectively
}

p.diffAnalysis(v1 = x.split[[1]]$medianRMS, v2 = x.split[[2]]$medianRMS)
p.diffAnalysis(v1 = x.split[[1]]$wE1, v2 = x.split[[2]]$wE1)
p.diffAnalysis(v1 = x.split[[1]]$wE2, v2 = x.split[[2]]$wE2)
p.diffAnalysis(v1 = x.split[[1]]$wE3, v2 = x.split[[2]]$wE3)
p.diffAnalysis(v1 = x.split[[1]]$wE4, v2 = x.split[[2]]$wE4)