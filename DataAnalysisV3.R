# Accompanying code  -------------------------------------------------------------------
# Version: 1.0
# Date: Feb 03, 2019

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

# Description: Accompanying code of the manuscript "Discriminating Rest and Postural Tremors
#              in a Clinical Setting for Parkinson's Disease Using Wearable Inertial Sensors"

# R packages --------------------------------------------------------------
if (!require(mvnormtest)) install.packages(c("mvnormtest"))
if (!require(readxl)) install.packages(c("readxl"))
if (!require(xlsx)) install.packages(c("xlsx"))
if (!require(lm)) install.packages(c("lm"))
if (!require(ggplot2)) install.packages('ggplot2', dep = TRUE)
if (!require(colorspace)) install.packages("colorspace")
if (!require(leaps)) install.packages("leaps")
if (!require(beanplot)) install.packages("beanplot")
if (!require(moments)) install.packages("moments")
if (!require(fBasics)) install.packages("fBasics")
if (!require(lawstat)) install.packages("lawstat")
if (!require(plotly)) install.packages("plotly")
if (!require(robust)) install.packages("robust")
if (!require(tsne)) install.packages("tsne")
if (!require(reshape2)) install.packages("reshape2")
if (!require(pracma)) install.packages("pracma")
if (!require(seewave)) install.packages("seewave") ## http://rug.mnhn.fr/seewave/
if (!require(psd)) install.packages(c("psd"))
if (!require(rlist)) install.packages("rlist")
if (!require(ggpubr)) install.packages("ggpubr") ##http://www.sthda.com/english/articles/24-ggpubr-publication-ready-plots/81-ggplot2-easy-way-to-mix-multiple-graphs-on-the-same-page/
if (!require(outliers))install.packages("outliers") ## https://cran.r-project.org/web/packages/outliers/outliers.pdf
if (!require(EMD)) install.packages("EMD") #https://cran.r-project.org/web/packages/EMD/EMD.pdf
if (!require(openxlsx)) install.packages("openxlsx")
if (!require(scales)) install.packages("scales")
if (!require(mclust)) install.packages("mclust")
if (!require(psych)) install.packages("psych")
if (!require(sigclust)) install.packages("sigclust")
if (!require(Rmixmod)) install.packages("Rmixmod")

# Visualiza??o de packages instalados
#search()



# Load libraries ----------------------------------------------------------
library(readxl)
library(mvnormtest)
library(ggplot2)
library(Hmisc)
library(leaps)
library(beanplot)
library(moments)
library(fBasics)
library(lawstat)
library(plotly)
library(robust)
library(mclust) #citation("mclust")
library(plyr)
library(tsne)
library(boot)
library(reshape2)
library(pracma)
library(seewave)
library(psd)
library(rlist)
library(ggpubr)
library(gridExtra)
library(grid)
library(outliers)
library (EMD)
library(openxlsx)
library(ggrepel)
library(psych)
library(sigclust)
library(Rmixmod)

source("TremsenToolbox.r") #tremsen toolbox
#search() #Verifica todas as bibliotecas carregadas



# Figure 2 ----------------------------------------------------------------

#'Loading a sample file of a collected data (V15C2RSC.txt)
strFolder <- "./SampleData/"
strFileName <- "V15C2RSC.txt"
df <- LoadTREMSENFile(paste(strFolder,strFileName,sep = ""))

# A) Visualization of the raw data collected from a subject with the wrist at rest. 
# Signals from IMU1, IMU2 and the digital pulse are shown. Triaxial signals from the gyroscope,
# accelerometer, and magnetometer are presented for each IMU. Signals from the accelerometer
# and magnetometer are subjected to the influence of gravity and the Earth's magnetic field 
# respectively.

gg1 <- plotTremsenDataset(list(df),indxs=c(1,2),printplot=FALSE, droplvs = c("X.PULSE.A.","X.PULSE.B."))
gg2 <- plotTremsenDataset(list(df),indxs=c(3,4),printplot=FALSE, droplvs = c("X.PULSE.A.","X.PULSE.B."))
gg3 <- plotTremsenDataset(list(df),indxs=c(5,6),printplot=FALSE, droplvs = c("X.PULSE.A.","X.PULSE.B."))
gout <- grid.arrange(gg1[[1]], gg2[[1]], gg3[[1]], nrow = 1, ncol=3, top = "")
ggsave("./Figures/Figure2a.tiff",plot = gout, device="tiff",width = 38, height = 20, units = "cm",dpi = 300)

# B) Typical signals resulting from the application of the pre-processing 
# method to the raw signals shown in Figure 2A

df.nonlineardetrended <- nonLineardetrendTremsenData(df)
lsplot <- list(df.nonlineardetrended)
gg1 <- plotTremsenDataset(lsplot,indxs=c(1,2),printplot=FALSE, droplvs = c("X.PULSE.A.","X.PULSE.B."), alphavec = c(1, 0.3))
gg2 <- plotTremsenDataset(lsplot,indxs=c(3,4),printplot=FALSE, droplvs = c("X.PULSE.A.","X.PULSE.B."), alphavec = c(1, 0.3))
gg3 <- plotTremsenDataset(lsplot,indxs=c(5,6),printplot=FALSE, droplvs = c("X.PULSE.A.","X.PULSE.B."), alphavec = c(1, 0.3))
gout <- grid.arrange(gg1[[1]], gg2[[1]], gg3[[1]], nrow = 1, ncol=3, top = "")
ggsave("./Figures/Figure2b.tiff",plot = gout, device="tiff",width = 38, height = 20, units = "cm",dpi = 300)


# Figure 4 ----------------------------------------------------------------

# Figure 4. Example of pre-processed signals (in red) and their respective RMS curve (in black). The medianRMS is estimated as the median of the RMS 
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
g1<-plotPSTremsenDataSet(pp[[1]],printplot = FALSE)

tiff("./Figures/Figure5.tiff", width = 10, height = 30, units = "cm", res = 300) #saving in tiff
print(g1)
dev.off() #fechar o arquivo

pdf("./Figures/Figure5.pdf", width = 5, height = 10) #saving in pdf
print(g1)
dev.off() #fechar o arquivo

# Figure 6 ----------------------------------------------------------------

# Figure 6. Box plot, showing the median, minimum, maximum, first quartile, third quartile, 
# and outliers of the correlation coefficients (r) between extracted features v1={MedianRMS, wE1, wE2, wE3, wE4} and experimental variables
# v2={Load, UPDRS_M_20_25, UPDRS_P_21, UPDRS_T_20}, for each protocol (Postural, Rest).

Excelfilename <- "./Correlations/CorrelationCoefs.xlsx"
sheetName <- "CorrCoefs"
D <- read.xlsx(Excelfilename,sheetName)

fsize <- 22
ys <- c(-0.2,0.9)

X <- D[which(D$VARIAVEL2=='medianRMS'),]
g1 <- ggplot(X, aes(x=VARIAVEL1, y=r12, fill=PROTOCOLO)) 
g1 <- g1 +  geom_boxplot(alpha = 0.50)
g1 <- g1 +  theme_light(base_size=fsize)  + theme(legend.position="none")
g1 <- g1 +  ylim(ys) + xlab("") + ylab("r") + ggtitle('medianRMS')  + theme(axis.text.x = element_text(angle = 90, hjust = 1))
g1 <- g1 +  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
g1 <- g1 + scale_fill_manual(values=c("#999999", "#E69F00"), 
                             name="Protocol",
                             labels=c("Postural", "Rest"))

X <- D[which(D$VARIAVEL2=='wE1'),]
g2 <- ggplot(X, aes(x=VARIAVEL1, y=r12, fill=PROTOCOLO)) 
g2 <- g2 +  geom_boxplot(alpha = 0.50)
g2 <- g2 +  theme_light(base_size=fsize)  + theme(legend.position="none")
g2 <- g2 +  ylim(ys) + xlab("") + ylab("") + ggtitle('wE1') + theme(axis.text.x = element_text(angle = 90, hjust = 1))
g2 <- g2 +  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
g2 <- g2 + scale_fill_manual(values=c("#999999", "#E69F00"), 
                             name="Protocol",
                             labels=c("Postural", "Rest"))

X <- D[which(D$VARIAVEL2=='wE2'),]
g3 <- ggplot(X, aes(x=VARIAVEL1, y=r12, fill=PROTOCOLO)) 
g3 <- g3 +  geom_boxplot(alpha = 0.50)
g3 <- g3 +  theme_light(base_size=fsize)  + theme(legend.position="none")
g3 <- g3 +  ylim(ys) + xlab("") + ylab("") + ggtitle('wE2') + theme(axis.text.x = element_text(angle = 90, hjust = 1))
g3 <- g3 +  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
g3 <- g3 + scale_fill_manual(values=c("#999999", "#E69F00"), 
                             name="Protocol",
                             labels=c("Postural", "Rest"))

X <- D[which(D$VARIAVEL2=='wE3'),]
g4 <- ggplot(X, aes(x=VARIAVEL1, y=r12, fill=PROTOCOLO)) 
g4 <- g4 +  geom_boxplot(alpha = 0.50) 
g4 <- g4 +  theme_light(base_size=fsize) + theme(legend.position="none")
g4 <- g4 +  ylim(ys) + xlab("") + ylab("") + ggtitle('wE3') + theme(axis.text.x = element_text(angle = 90, hjust = 1))
g4 <- g4 +  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
g4 <- g4 + scale_fill_manual(values=c("#999999", "#E69F00"), 
                             name="Protocol",
                             labels=c("Postural", "Rest"))

X <- D[which(D$VARIAVEL2=='wE4'),]
g5 <- ggplot(X, aes(x=VARIAVEL1, y=r12, fill=PROTOCOLO)) 
g5 <- g5 +  geom_boxplot(alpha = 0.50)
g5 <- g5 +  theme_light(base_size=fsize)
g5 <- g5 +  ylim(ys) + xlab("") + ylab("") + ggtitle('wE4') + theme(axis.text.x = element_text(angle = 90, hjust = 1))
g5 <- g5 +  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
g5 <- g5 + scale_fill_manual(values=c("#999999", "#E69F00"), 
                             name="Protocol",
                             labels=c("Postural", "Rest"))

p <- ggarrange(g1, g2,g3, g4, g5, ncol=5, nrow=1, common.legend = TRUE, legend="bottom")
print(p)
ggsave(paste('./Figures/Figure6',".tiff",sep = ""),width = 18, height = 9, dpi = 300, device = "tiff")



# Table 4 -----------------------------------------------------------------

# Table 4. Mean significant (p<0.05) correlation coefficient and its standard deviation for each
# classification range between all features (v1) and experimental variables (v2).  The ones with 
# no occurrence in the correlation classification are labelled with NA (Not Available).

Excelfilename <- "./Correlations/CorrelationCoefs.xlsx"
sheetName <- "CorrCoefs"
D <- read.xlsx(Excelfilename,sheetName)


#"very strong" correlation
DATA <- D[which(D$pVALUE <= 0.05 & D$r12>=0.8),]
des <- describeBy(x=DATA[,c(2,7,8,9)], group=list(DATA$PROTOCOLO, DATA$VARIAVEL1,DATA$VARIAVEL2))
print(des, digits=5)

aggdata <-aggregate(x=DATA[,c(2,3,7,8,9)], by=list(DATA$PROTOCOLO, DATA$VARIAVEL1,DATA$VARIAVEL2, DATA$SENSOR), FUN=mean)
print(aggdata)
write.xlsx(aggdata, './Correlations/rVeryStrong.xlsx')

#"strong" correlation
cat("\014") #clear screen
DATA <- D[which(D$pVALUE <= 0.05 & (D$r12>=0.6 & D$r12<0.8)),]
des <- describeBy(x=DATA[,c(2,7,8,9)], group=list(DATA$PROTOCOLO,DATA$VARIAVEL1,DATA$VARIAVEL2))
print(des, digits=5)
aggdata <-aggregate(x=DATA[,c(2,3,7,8,9)], by=list(DATA$PROTOCOLO, DATA$VARIAVEL1,DATA$VARIAVEL2, DATA$SENSOR), FUN=mean)
print(aggdata)
write.xlsx(aggdata, './Correlations/rStrong.xlsx')

#"moderate" correlation
cat("\014") #clear screen
DATA <- D[which(D$pVALUE <= 0.05 & (D$r12>=0.4 & D$r12<0.6)),]
des <- describeBy(x=DATA[,c(2,7,8,9)], group=list(DATA$PROTOCOLO,DATA$VARIAVEL1,DATA$VARIAVEL2))
print(des, digits=5)
aggdata <-aggregate(x=DATA[,c(2,3,7,8,9)], by=list(DATA$PROTOCOLO, DATA$VARIAVEL1,DATA$VARIAVEL2, DATA$SENSOR), FUN=mean)
print(aggdata)
write.xlsx(aggdata, './Correlations/rModerate.xlsx')

#"weak" correlation
cat("\014") #clear screen
DATA <- D[which(D$pVALUE <= 0.05 & (D$r12>=0.2 & D$r12<0.4)),]
des <- describeBy(x=DATA[,c(2,7,8,9)], group=list(DATA$PROTOCOLO,DATA$VARIAVEL1,DATA$VARIAVEL2))
print(des, digits=5)
aggdata <-aggregate(x=DATA[,c(2,3,7,8,9)], by=list(DATA$PROTOCOLO, DATA$VARIAVEL1,DATA$VARIAVEL2, DATA$SENSOR), FUN=mean)
write.xlsx(aggdata, './Correlations/rWeak.xlsx')


#"very weak" correlation
cat("\014") #clear screen
DATA <- D[which(D$pVALUE <= 0.05 & (D$r12>=0.0 & D$r12<0.2)),]
des <- describeBy(x=DATA[,c(2, 7,8,9)], group=list(DATA$PROTOCOLO,DATA$VARIAVEL1,DATA$VARIAVEL2))
print(des, digits=5)
aggdata <-aggregate(x=DATA[,c(2,3,7,8,9)], by=list(DATA$PROTOCOLO, DATA$VARIAVEL1,DATA$VARIAVEL2, DATA$SENSOR), FUN=mean)
write.xlsx(aggdata, './Correlations/rVeryWeak.xlsx')

# Figure 7 ----------------------------------------------------------------

# Figure 7. Correlation coefficient between wE3 and an experimental variable 
# (e.g., Load, UPDRS_M_20_25, UPDRS_P_21, UPDRS_T_20) against the correlation 
# coefficient between wE4 and the same experimental variable, for both protocols
# (Postural and Rest). The relationship is presented for each available sensor. 
# Sensor labels in rectangles indicate a not significant correlation (p > 0.05).


#' Definition of function to plot scatter plots
plotScatterPlot <- function (feat1, feat2, clinicalScale, figname,xl, yl)
{
  Excelfilename <- "./Correlations/CorrelationCoefs.xlsx"
  
  sheetName <- "CorrCoefs"
  D <- read.xlsx(Excelfilename,sheetName)
  
  # Avaliacao das correlacoes -----------------------------------------------
  
  
  Z1 <- D[which(D$VARIAVEL1 == clinicalScale & D$VARIAVEL2 == feat1),9]
  Z2 <- D[which(D$VARIAVEL1 == clinicalScale & D$VARIAVEL2 == feat2),9]
  Protocol <- D[which(D$VARIAVEL1 == clinicalScale & D$VARIAVEL2 == feat2),2]
  SENSOR <- D[which(D$VARIAVEL1 == clinicalScale & D$VARIAVEL2 == feat2),3]
  pVALUE <- D[which(D$VARIAVEL1 == clinicalScale & D$VARIAVEL2 == feat2),10]
  
  Z <- data.frame(Z1, Z2, Protocol,SENSOR,pVALUE)
  
  
  psize <- 3
  psize2 <- 6
  
  g <- ggplot()
  g <- g +  geom_point(data = Z[which(pVALUE <= 0.05),], size = psize, aes(x=Z1, y=Z2, color = Protocol))
  g <- g +  geom_text_repel(data = Z[which(pVALUE <= 0.05),], size = psize2, 
                            show.legend=F, aes(x=Z1, y=Z2, label=SENSOR, 
                                               color = Protocol),
                            box.padding = 0.5)
  g <- g +  theme_light(base_size=25)  + theme(legend.position="right",legend.key.size = unit(2, 'lines'))
  g <- g +  geom_point(data = Z[which(pVALUE > 0.05),], size = psize,show.legend=F,  aes(x=Z1, y=Z2,color = Protocol))
  g <- g +  geom_point(data = Z[which(pVALUE > 0.05),], size = 1,show.legend=F, colour = "black", size = 1, aes(x=Z1, y=Z2) )
  g <- g +  geom_label_repel(data = Z[which(pVALUE > 0.05),], size = psize2, show.legend=F, aes(x=Z1, y=Z2, label=SENSOR, color = Protocol))
  g <- g +  scale_color_manual(labels = c("Postural", "Rest"), values = c("#999999", "#E69F00"))
  g <- g +  xlab(paste("r ", "(", feat1, ")", sep = "")) 
  g <- g +  ylab(paste("r ", "(", feat2, ")", sep = ""))  
  g <- g + ggtitle(clinicalScale)
  
  textconf <- element_text(size = 30)
  
  g <- g + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                 legend.position = c(0.1, 0.95),
                 legend.justification = c("left", "top"),
                 legend.title = element_blank(),
                 legend.direction = "horizontal",
                 legend.text = textconf,
                 axis.text.x = textconf,
                 axis.text.y = textconf,
                 axis.title.x = textconf,
                 axis.title.y = textconf
                 )
  g <- g + xlim(xl)
  g <- g + ylim(yl)
  
  print(g)
  
  # save figure
  ggsave(paste(figname,".tiff",sep = ""),width = 10, height = 10, dpi = 300, device = "tiff")
}

#' Scatter plots
plotScatterPlot (feat1 = 'wE3', feat2 = 'wE4', clinicalScale = 'Load', figname = "./Figures/Figure7a", xl=c(-0.3, 0.3), yl= c(-0.3, 0.3))
plotScatterPlot (feat1 = 'wE3', feat2 = 'wE4', clinicalScale = 'UPDRS_M_20_25', figname = "./Figures/Figure7b", xl=c(0, 1), yl= c(0, 1))
plotScatterPlot (feat1 = 'wE3', feat2 = 'wE4', clinicalScale = 'UPDRS_P_21', figname = "./Figures/Figure7c", xl=c(0, 1), yl= c(0, 1)) 
plotScatterPlot (feat1 = 'wE3', feat2 = 'wE4', clinicalScale = 'UPDRS_T_20', figname = "./Figures/Figure7d", xl=c(0, 1), yl= c(0, 1)) 
 

# Figure 8 and Table 5 ----------------------------------------------------

estimateEllipse <- function(covariance, meanC)
{
  v <- eigen(covariance)
  eigenvec <- v$vectors
  eigenval <- v$values
  
  #The eigenvalues are always returned in decreasing order,
  #and each column of vectors corresponds to the elements in values
  # To obtain the orientation of the ellipse, we simply calculate the angle of the largest eigenvector towards the x-axis:
  
  # Get the largest eigenvalue
  largest_eigenval <-  max(max(eigenval))
  
  
  # Get the index of the largest eigenvector
  largest_eigenvec_ind_c <- which(eigenval == largest_eigenval)
  largest_eigenvec <- eigenvec[, largest_eigenvec_ind_c]
  
  # Get the smallest eigenvector and eigenvalue
  smallest_eigenval <- min(eigenval)
  smallest_eigenvec <-
    eigenvec[, which(eigenval == smallest_eigenval)]
  
  # Calculate the angle between the x-axis and the largest eigenvector
  angle <-  atan2(largest_eigenvec[2], largest_eigenvec[1])
  
  # This angle is between -pi and pi.
  # Let's shift it such that the angle is between 0 and 2pi
  if (angle < 0)
  {
    angle <- angle + 2 * pi
  }
  
  # Get the coordinates of the data mean
  avg <- meanC
  
  
  # Get the 95% confidence interval error ellipse
  #chisquare_val <- 2.4477
  chisquare_val <- 1
  theta_grid <- linspace(0, 2 * pi, n = 100)
  phi <- angle
  X0 <- avg[1]
  Y0 <- avg[2]
  a <- chisquare_val * sqrt(largest_eigenval)
  b <- chisquare_val * sqrt(smallest_eigenval)
  
  # the ellipse in x and y coordinates
  ellipse_x_r  <- a * cos(theta_grid)
  ellipse_y_r  <- b * sin(theta_grid)
  
  #Define a rotation matrix
  R <- matrix(c(cos(phi), sin(phi),-sin(phi), cos(phi)), nrow = 2, byrow = TRUE)
  #let's rotate the ellipse to some angle phi
  m <- matrix(data = NA,
              nrow = 2,
              ncol = length(ellipse_x_r))
  m[1,] <- ellipse_x_r
  m[2,] <- ellipse_y_r
  
  
  r_ellipse <- t(m) %*% R
  
  r_ellipse[,1] <-  r_ellipse[,1] + X0
  r_ellipse[,2]  <- r_ellipse [ ,2] + Y0
  
  
  return(list ("r_ellipse" = r_ellipse, "largest_eigenval" = largest_eigenval, 
               "largest_eigenvec" = largest_eigenvec, 
               "smallest_eigenval" = smallest_eigenval, 
               "smallest_eigenvec" = smallest_eigenvec,
               "X0" = X0, "Y0" = Y0,
               "Cov" = covariance,
               "avg" = avg,
               "R" = R))
  
}

estimateGaussianModel <- function (feat1, feat2, clinicalScale)
{
  Excelfilename <- "./Correlations/CorrelationCoefs.xlsx"
  
  sheetName <- "CorrCoefs"
  D <- read.xlsx(Excelfilename,sheetName)
  
  # Statiscal analysis of clusters -----------------------------------------------
  # pval < 0.05 implies the existence of two clusters
  
  Z1 <- D[which(D$VARIAVEL1 == clinicalScale & D$VARIAVEL2 == feat1),9]
  Z2 <- D[which(D$VARIAVEL1 == clinicalScale & D$VARIAVEL2 == feat2),9]
  Protocol <- D[which(D$VARIAVEL1 == clinicalScale & D$VARIAVEL2 == feat2),2]
  SENSOR <- D[which(D$VARIAVEL1 == clinicalScale & D$VARIAVEL2 == feat2),3]
  pVALUE <- D[which(D$VARIAVEL1 == clinicalScale & D$VARIAVEL2 == feat2),10]
  
  Z <- data.frame(wE3 = Z1, wE4 = Z2, Protocol,SENSOR,pVALUE)
  
  dat <- data.frame(Z[1:2])
  
  xem1<-mixmodCluster(dat,nbCluster = 2, knownLabels = Z$Protocol)
  # summary(xem1)
  # windows()
  # plot(xem1, col = c("#999999", "#E69F00"))
  # 
  
  # Calculate the eigenvectors and eigenvalues
  covariance <- xem1@bestResult@parameters@variance[[1]]
  meanC <- xem1@bestResult@parameters@mean[1, ]
  e1 <- estimateEllipse(covariance = covariance, meanC = meanC)
  
  covariance <- xem1@bestResult@parameters@variance[[2]]
  meanC <- xem1@bestResult@parameters@mean[2, ]
  e2 <- estimateEllipse(covariance = covariance, meanC = meanC)
  
  
  # returning relevant information
  df <- cbind.data.frame(Z, xem1@bestResult@proba, xem1@knownLabels, clinicalScale)
  names(df)[6] <- "Prob Label 1"
  names(df)[7] <- "Prob Label 2"
  names(df)[8] <- "Protocol Label"
  
  return(list( "xm" = xem1, "e1" = e1, "e2" = e2, "data" = df))
  
}


plotModel <- function(gaussianModel,
                      clinicalScale = 'UPDRS_T_20',
                      xl = c(0, 1),
                      yl = c(0, 1),
                      SaveFig = FALSE,
                      figname = "fig",
                      plotEigenc = FALSE,
                      feat1,
                      feat2)
{
  m1 <- gaussianModel
  plotData <- data.frame(m1$xm@data)
  
  
  textconf <- element_text(size = 30)
  psize <- 3
  
  g <- ggplot()
  g <-
    g + geom_point(data = plotData,
                   size = psize,
                   aes(
                     x = plotData[, 1],
                     y = plotData[, 2],
                     color = factor(m1$xm@knownLabels)
                   ))
  g <- g +  theme_light(base_size = 25)
  g <- g + theme(legend.position = "right",
                 legend.key.size = unit(2, 'lines'))
  g <- g + ggtitle(clinicalScale)
  g <- g + theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = c(0.1, 0.95),
    legend.justification = c("left", "top"),
    legend.title = element_blank(),
    legend.direction = "horizontal",
    legend.text = textconf,
    axis.text.x = textconf,
    axis.text.y = textconf,
    axis.title.x = textconf,
    axis.title.y = textconf
  )
  g <-
    g +  scale_color_manual(labels = c("Postural", "Rest"),
                            values = c("#999999", "#E69F00"))
  g <- g +  xlab(paste("r ", "(", feat1, ")", sep = ""))
  g <- g +  ylab(paste("r ", "(", feat2, ")", sep = ""))
  
  plotDataEl1 <- as.data.frame(m1$e1$r_ellipse)
  g <- g + geom_path(data = plotDataEl1,
                     aes(x = plotDataEl1[, 1], y = plotDataEl1[, 2]))
  
  plotDataEl2 <- as.data.frame(m1$e2$r_ellipse)
  g <- g + geom_path(data = plotDataEl2,
                     aes(x = plotDataEl2[, 1], y = plotDataEl2[, 2]))
  
  
  dfPlotMean <- as.data.frame(m1$xm@bestResult@parameters@mean)
  g <-  g + geom_point(data = dfPlotMean,
                       size = psize,
                       aes(x = V1, y = V2),
                       shape = 15)
  
  
  g <- g + xlim(xl)
  g <- g + ylim(yl)
  
  
  if (plotEigenc == TRUE)
  {
    df <- data.frame(
      x1 = m1$e1$X0,
      x2 = m1$e1$largest_eigenvec[1] * sqrt(m1$e1$largest_eigenval),
      y1 = m1$e1$Y0,
      y2 = m1$e1$largest_eigenvec[2] * sqrt(m1$e1$largest_eigenval)
    )
    
    g <-
      g + geom_segment(
        aes(
          x = x1,
          y = y1,
          xend = x1 + x2,
          yend = y1 + y2
        ),
        data = df,
        arrow = arrow(length = unit(0.5, "cm"))
      )
    
    
    
    
    df <- data.frame(
      x1 = m1$e1$X0,
      x2 = m1$e1$smallest_eigenvec[1] * sqrt(m1$e1$smallest_eigenval),
      y1 = m1$e1$Y0,
      y2 = m1$e1$smallest_eigenvec[2] * sqrt(m1$e1$smallest_eigenval)
    )
    
    g <-
      g + geom_segment(
        aes(
          x = x1,
          y = y1,
          xend = x1 + x2,
          yend = y1 + y2
        ),
        data = df,
        arrow = arrow(length = unit(0.5, "cm"))
      )
    
    
    
    ##
    
    df <- data.frame(
      x1 = m1$e2$X0,
      x2 = m1$e2$largest_eigenvec[1] * sqrt(m1$e2$largest_eigenval),
      y1 = m1$e2$Y0,
      y2 = m1$e2$largest_eigenvec[2] * sqrt(m1$e2$largest_eigenval)
    )
    
    g <-
      g + geom_segment(
        aes(
          x = x1,
          y = y1,
          xend = x1 + x2,
          yend = y1 + y2
        ),
        data = df,
        arrow = arrow(length = unit(0.5, "cm"))
      )
    
    
    
    
    df <- data.frame(
      x1 = m1$e2$X0,
      x2 = m1$e2$smallest_eigenvec[1] * sqrt(m1$e2$smallest_eigenval),
      y1 = m1$e2$Y0,
      y2 = m1$e2$smallest_eigenvec[2] * sqrt(m1$e2$smallest_eigenval)
    )
    
    g <-
      g + geom_segment(
        aes(
          x = x1,
          y = y1,
          xend = x1 + x2,
          yend = y1 + y2
        ),
        data = df,
        arrow = arrow(length = unit(0.5, "cm"))
      )
    
  }
  
  print(g)
  # save figure
  if (SaveFig == TRUE)
    ggsave(
      paste(figname, ".tiff", sep = ""),
      width = 10,
      height = 10,
      dpi = 300,
      device = "tiff"
    )
  
}


m1 <- estimateGaussianModel (feat1 = 'wE3', feat2 = 'wE4', clinicalScale = 'Load')
summary(m1$xm) # Table 5
plotModel(m1, clinicalScale = 'Load', xl = c(-0.3, 0.3),yl = c(-0.3, 0.3), 
          SaveFig = TRUE,
          figname = "./Figures/Figure8a",
          plotEigenc = TRUE, feat1 = 'wE3', feat2 = 'wE4')

m2 <- estimateGaussianModel (feat1 = 'wE3', feat2 = 'wE4', clinicalScale = 'UPDRS_M_20_25')
summary(m2$xm) # Table 5
plotModel(m2, clinicalScale = 'UPDRS_M_20_25', xl = c(0.0, 1.0),yl = c(0.0, 1.0),
          SaveFig = TRUE,figname = "./Figures/Figure8b", 
          plotEigenc = TRUE, feat1 = 'wE3', feat2 = 'wE4')

m3 <- estimateGaussianModel (feat1 = 'wE3', feat2 = 'wE4', clinicalScale = 'UPDRS_P_21')
summary(m3$xm) # Table 5
plotModel(m3, clinicalScale = 'UPDRS_P_21', xl = c(0.0, 1.0),yl = c(0.0, 1.0), 
          SaveFig = TRUE,figname = "./Figures/Figure8c", 
          plotEigenc = TRUE, feat1 = 'wE3', feat2 = 'wE4')

m4 <- estimateGaussianModel (feat1 = 'wE3', feat2 = 'wE4', clinicalScale = 'UPDRS_T_20')
summary(m4$xm) # Table 5
plotModel(m4, clinicalScale = 'UPDRS_T_20', xl = c(0.0, 1.0),yl = c(0.0, 1.0), 
          SaveFig = TRUE,figname = "./Figures/Figure8d",
          plotEigenc = TRUE, feat1 = 'wE3', feat2 = 'wE4')



dfRes <- rbind.data.frame(m1$data, m2$data, m3$data, m4$data)
print(dfRes)
write.xlsx(dfRes, './Correlations/ClustRes.xlsx')



# Table 6 -----------------------------------------------------------------

Excelfilename <- "./Correlations/ClustRes.xlsx"
D <- read.xlsx(Excelfilename)


# Estimates for the rest protocol
aa <- D  %>%
  filter(Protocol.Label == 2) %>% #Rest
  select(SENSOR, Prob.Label.1, Prob.Label.2)


meanRest <- aggregate(aa, by=list(aa$SENSOR), 
          FUN=mean, na.rm=TRUE)

Pa1a2 <- meanRest$Prob.Label.1
names(Pa1a2) <- levels(factor(D$SENSOR))

Pa2a2 <- meanRest$Prob.Label.2
names(Pa2a2) <- levels(factor(D$SENSOR))


stdRest <-aggregate(aa, by=list(aa$SENSOR), 
          FUN=sd)
std12 <- stdRest$Prob.Label.1
names(std12) <- levels(factor(D$SENSOR))

std22 <- stdRest$Prob.Label.2
names(std22) <- levels(factor(D$SENSOR))


# Estimates for the postural protocol
aa <- D  %>%
  filter(Protocol.Label == 1) %>% #Postural
  select(SENSOR, Prob.Label.1, Prob.Label.2)


meanPostural <- aggregate(aa, by=list(aa$SENSOR), 
                      FUN=mean, na.rm=TRUE)

Pa1a1 <- meanPostural$Prob.Label.1
names(Pa1a1) <- levels(factor(D$SENSOR))

Pa2a1 <- meanPostural$Prob.Label.2
names(Pa2a1) <- levels(factor(D$SENSOR))

stdPostural <-aggregate(aa, by=list(aa$SENSOR), 
                    FUN=sd)
std11 <- stdPostural$Prob.Label.1
names(std11) <- levels(factor(D$SENSOR))

std21 <- stdPostural$Prob.Label.2
names(std21) <- levels(factor(D$SENSOR))

df <- data.frame(Pa1a1, std11, Pa2a1, std21, Pa1a2, std12, Pa2a2, std22)
print(df)


# Table 7 -----------------------------------------------------------------
# Quality index for each sensor signal
QI <- (df$Pa1a1 + df$Pa2a2)/2 - (df$Pa1a2 + df$Pa2a1)/2 - sqrt( (df$std11^2 + df$std12^2 + df$std21^2 + df$std22^2)/4 )
names(QI) <- levels(factor(D$SENSOR))
QI <- sort(QI, decreasing = TRUE) 
print(QI)
