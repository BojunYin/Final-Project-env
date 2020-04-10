#install packages about this project
#install.packages("StatDA")
library(StatDA)
#available data in this package
data(moss)
data(ohorizon)
data(bhorizon)
data(chorizon)
data(topsoil)
data(timetrend)
# Read data into R 
CHorANADUP <- read.csv("CHorANADUP.csv")
CHorFieldDUP <- read.csv("CHorFieldDUP.csv")
CHorSTANDARD <- read.csv("CHorSTANDARD.csv")
# make background map:
data.boundary <- read.csv("kola-background-boundary.csv")
data.coast <- read.csv("kola-background-coast.csv")
data.borders <- read.csv("kola-background-borders.csv")
data.lakes <- read.csv("kola-background-lakes.csv")
kola.background <- list(boundary=data.boundary,coast=data.coast,borders=data.borders,lakes=data.lakes)