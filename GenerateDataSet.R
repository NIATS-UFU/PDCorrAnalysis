# GenerateDataSet  -------------------------------------------------------------------
# Version: 2.0
# Date: March 07, 2019
# Latest version available @ 

# Author: Prof. Adriano de Oliveira Andrade
# Contact email: adriano@ufu.br
# CV LATTES: http://lattes.cnpq.br/1229329519982110
# ORCID ID: http://orcid.org/0000-0002-5689-6606
# Researcher ID: http://www.researcherid.com/rid/D-9721-2012 
# GOOGLE ACADEMIC: https://scholar.google.com.br/citations?user=8nHKQHMAAAAJ&hl=pt-BR


# Adddress: Centre for Innovation and Technology Assessment in Health, 
#           Postgraduate Program in Electrical and Biomedical Engineering, 
#           Faculty of Electrical Engineering, Federal University of Uberl�ndia, 
#           Uberl�ndia, Brazil
# Webpage:  http://www.niats.feelt.ufu.br/

# Description: This script is responsible for extracting features for the whole data set. 
#              The file DataSet.xlsx is generated as result
# 
# Remarks: the raw data (time-series) and the file 'QUADRO updrs PACIENTES.xlsx' cannot be shared 
#          due to restrictions of the Ethics Committee. Please contact us if there is interest
#          on how the information should be organized in the files.

# Required set of libraries -----------------------------------------------
source("FeatExtract.R")

# Build data set ------------------------------------------------------

path.Control <-"D:/Adriano/OneDrive/Papers/2017/AnaPaulaPaixao/DADOS/BD2/PARKINSON"
sub.folders1 <- list.dirs(path=path.Control, full.names = TRUE, recursive = TRUE)

filtro1 <- "^V(.*RSC|.*RCC92|.*RCC184|.*ESC|.*ECC92|.*ECC184).txt$" #filtro para selecao de dados referentes ao protocolo REPOUSO
file.name.v <- list.files(sub.folders1, pattern=filtro1, full.names = TRUE)

fb1 = list(b1=c(0, 2.5), b2 = c(2.5, 5.0), b3= c(5.0, 10.0), b4 = c(10, 15))
#fb2 <- list(b1=c(0, 4), b2 = c(4, 7), b3= c(7, 14), b4 = c(15, 21))
dataSet <- lapply(file.name.v,buildDataSet, processData=TRUE, fb=fb1, logScaleEnergy = FALSE)

save(dataSet,file = paste(path.Control,'/dataSet',sep=''),compress="xz",compression_level=9)

X <- melt(dataSet)

# Load data set -----------------------------------------------------------

load(paste(path.Control,'/dataSet',sep=''))


# Data analysis -----------------------------------------------------------
# Extraindo informações do DataSet na forma matricial -------------------------------------------------------------
sensor <- names(dataSet[[1]]$ls)
names(sensor) <- sensor
X<- lapply(sensor, getData, dataSet)
X<-as.data.frame(list.rbind(X))
X <- X[order(X$grupo, X$subjectID, X$protocol, X$carga,X$sensor, X$trial ),]
rownames(X) <- c()


Excelfilename <- "D:\\Adriano\\OneDrive\\Papers\\2017\\AnaPaulaPaixao\\DADOS\\BD2\\QUADRO updrs PACIENTES.xlsx"
datExcel <- read.xlsx(Excelfilename)


X$Age <- NA
X$Gender <- NA
X$Side <- NA
X$DiagnosticTime <- NA
X$UPDRS.M.20.25 <- NA
X$UPDRS.T.20 <- NA
X$UPDRS.P.21 <- NA

for(i in 1:nrow(datExcel)){
  X[which(X$subjectID==i),17:23] <- datExcel[i,3:9]
}

X <- X[c(1,2,17:23,3:12)] #reordering data frame

sensors <- c("X.G1.RES.", "X.G1.X.", "X.G1.Y.", "X.G1.Z.",  
             "X.G2.RES.", "X.G2.X.", "X.G2.Y.", "X.G2.Z.",
             "X.A1.RES.", "X.A1.X.", "X.A1.Y.", "X.A1.Z.",  
             "X.A2.RES.", "X.A2.X.",  "X.A2.Y.","X.A2.Z.",
             "X.M1.RES.", "X.M1.X.", "X.M1.Y." ,"X.M1.Z.",
             "X.M2.RES.", "X.M2.X.", "X.M2.Y.",  "X.M2.Z.")

X <- X[which( (X$sensor %in% sensors) == TRUE),]

rownames(X) <- c()
X <- X[order(X$grupo, X$subjectID, X$protocol, X$carga,X$sensor, X$trial ),]


#o uso da função write.xls requer a instalação da ferramenta RTools (https://cran.r-project.org/bin/windows/Rtools/)
write.xlsx(X, file = "./DataSet/DataSet.xlsx", 
           colNames = TRUE, borders = "columns")