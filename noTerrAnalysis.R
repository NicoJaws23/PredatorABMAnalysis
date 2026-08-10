#No Territory No Memory Code
library(tidyverse)
library(ggplot2)
library(lme4)
library(sf)
library(igraph)
library(pbapply)
library(tictoc)
library(progress)

##########################################
#Step 1: Load in data from all variations#
##########################################

#No Terr Mem
path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\NoTerritoryMemory\\preyCoords\\p1"
ntm1 <- fileRead(path, numPred = 1, type = "coords")
ntm1 <- piv(ntm1, 1)


path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\NoTerritoryMemory\\preyCoords\\p2"
ntm2 <- fileRead(path, numPred = 2, type = "coords")
ntm2 <- piv(ntm2, 2)

path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\NoTerritoryMemory\\preyCoords\\p3"
ntm3 <- fileRead(path, numPred = 3, type = "coords")
ntm3 <- piv(ntm3, 3)

path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\NoTerritoryMemory\\preyCoords\\p4"
ntm4 <- fileRead(path, numPred = 4, type = "coords")
ntm4 <- piv(ntm4, 4)

#No Terr Mem Pred Mem
path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\NoTerritoryMemoryPredMemory\\preyCoords\\p1"
ntmpm1 <- fileRead(path, numPred = 1, type = "coords")
ntmpm1 <- piv(ntmpm1, 1)

path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\NoTerritoryMemoryPredMemory\\preyCoords\\p2"
ntmpm2 <- fileRead(path, numPred = 2, type = "coords")
ntmpm2 <- piv(ntmpm2, 2)

path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\NoTerritoryMemoryPredMemory\\preyCoords\\p3"
ntmpm3 <- fileRead(path, numPred = 3, type = "coords")
ntmpm3 <- piv(ntmpm3, 3)

path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\NoTerritoryMemoryPredMemory\\preyCoords\\p4"
ntmpm4 <- fileRead(path, numPred = 4, type = "coords")
ntmpm4 <- piv(ntmpm4, 4)

#No Terr No Mem
path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\NoTerritoryNoMemory\\preyCoords\\p1"
ntnm1 <- fileRead(path, numPred = 1, type = "coords")
ntnm1 <- piv(ntnm1, 1)

path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\NoTerritoryNoMemory\\preyCoords\\p2"
ntnm2 <- fileRead(path, numPred = 2, type = "coords")
ntnm2 <- piv(ntnm2, 2)

path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\NoTerritoryNoMemory\\preyCoords\\p3"
ntnm3 <- fileRead(path, numPred = 3, type = "coords")
ntnm3 <- piv(ntnm3, 3)

path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\NoTerritoryNoMemory\\preyCoords\\p4"
ntnm4 <- fileRead(path, numPred = 4, type = "coords")
ntnm4 <- piv(ntnm4, 4)

#No Terr No Mem Pred Mem
path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\NoTerritoryNoMemoryPredMemory\\preyCoords\\p1"
ntnmpm1 <- fileRead(path, numPred = 1, type = "coords")
ntnmpm1 <- piv(ntnmpm1, 1)

path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\NoTerritoryNoMemoryPredMemory\\preyCoords\\p2"
ntnmpm2 <- fileRead(path, numPred = 2, type = "coords")
ntnmpm2 <- piv(ntnmpm2, 2)

path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\NoTerritoryNoMemoryPredMemory\\preyCoords\\p3"
ntnmpm3 <- fileRead(path, numPred = 3, type = "coords")
ntnmpm3 <- piv(ntnmpm3, 3)

path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\NoTerritoryNoMemoryPredMemory\\preyCoords\\p4"
ntnmpm4 <- fileRead(path, numPred = 4, type = "coords")
ntnmpm4 <- piv(ntnmpm4, 4)

#No Terr Shared Mem
path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\NoTerritorySharedMemory\\preyCoords\\p1"
ntsm1 <- fileRead(path, numPred = 1, type = "coords")
ntsm1 <- piv(ntsm1, 1)

path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\NoTerritorySharedMemory\\preyCoords\\p2"
ntsm2 <- fileRead(path, numPred = 2, type = "coords")
ntsm2 <- piv(ntsm2, 2)

path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\NoTerritorySharedMemory\\preyCoords\\p3"
ntsm3 <- fileRead(path, numPred = 3, type = "coords")
ntsm3 <- piv(ntsm3, 3)

path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\NoTerritorySharedMemory\\preyCoords\\p4"
ntsm4 <- fileRead(path, numPred = 4, type = "coords")
ntsm4 <- piv(ntsm4, 4)

#No Terr Shared Mem Pred Mem
path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\NoTerritorySharedMemoryPredMemory\\preyCoords\\p1"
ntsmpm1 <- fileRead(path, numPred = 1, type = "coords")
ntsmpm1 <- piv(ntsmpm1, 1)

path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\NoTerritorySharedMemoryPredMemory\\preyCoords\\p2"
ntsmpm2 <- fileRead(path, numPred = 2, type = "coords")
ntsmpm2 <- piv(ntsmpm2, 2)

path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\NoTerritorySharedMemoryPredMemory\\preyCoords\\p3"
ntsmpm3 <- fileRead(path, numPred = 3, type = "coords")
ntsmpm3 <- piv(ntsmpm3, 3)

path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\NoTerritorySharedMemoryPredMemory\\preyCoords\\p4"
ntsmpm4 <- fileRead(path, numPred = 4, type = "coords")
ntsmpm4 <- piv(ntsmpm4, 4)

NTM_all <- bind_rows(ntm1, ntm2, ntm3, ntm4)
write_csv(NTM_all, "C://Users//Jawor//Desktop//R_repos//PredatorABMAnalysis//csvFiles//NTM_all.csv")
NTNM_all <- bind_rows(ntnm1, ntnm2, ntnm3, ntnm4)
write_csv(NTNM_all, "C://Users//Jawor//Desktop//R_repos//PredatorABMAnalysis//csvFiles//NTNM_all.csv")
NTSM_all <- bind_rows(ntsm1, ntsm2, ntsm3, ntsm4)
write_csv(NTSM_all, "C://Users//Jawor//Desktop//R_repos//PredatorABMAnalysis//csvFiles//NTSM_all.csv")


####################################
#Step 2: Pairwise Distance Analysis#
####################################

#No Terr Mem
NTMd1 <- pairDist(ntm1, 1)
write_csv(NTMd1, "C://Users//Jawor//Desktop//R_repos//PredatorABMAnalysis//csvFiles//coords_dist//NTM_Dist_p1.csv")
NTMd2 <- pairDist(ntm2, 2)
write_csv(NTMd2, "C://Users//Jawor//Desktop//R_repos//PredatorABMAnalysis//csvFiles//coords_dist//NTM_Dist_p2.csv")
NTMd3 <- pairDist(ntm3, 3)
write_csv(NTMd3, "C://Users//Jawor//Desktop//R_repos//PredatorABMAnalysis//csvFiles//coords_dist//NTM_Dist_p3.csv")
NTMd4 <- pairDist(ntm4, 4)
write_csv(NTMd4, "C://Users//Jawor//Desktop//R_repos//PredatorABMAnalysis//csvFiles//coords_dist//NTM_Dist_p4.csv")
NTM_allDist <- bind_rows(NTMd1, NTMd2, NTMd3, NTMd4)
write_csv(NTM_allDist, "C://Users//Jawor//Desktop//R_repos//PredatorABMAnalysis//csvFiles//coords_dist//NTM_allDist.csv")
rm(NTMd1, NTMd2, NTMd3, NTMd4, NTM_allDist)

#No Terr Mem Pred Mem
#NTMPMd1 <- pairDist(ntmpm1, 1)
#NTMPMd2 <- pairDist(ntmpm2, 2)
#NTMPMd3 <- pairDist(ntmpm3, 3)
#NTMPMd4 <- pairDist(ntmpm4, 4)
#NTMPM_allDist <- bind_rows(NTMPMd1, NTMPMd2, NTMPMd3, NTMPMd4)
#write_csv(NTMPM_allDist, "C://Users//Jawor//Desktop//R_repos//PredatorABMAnalysis//csvFiles//NTMPM_allDist.csv")

#No Terr No Mem
NTNMd1 <- pairDist(ntnm1, 1)
write_csv(NTNMd1, "C://Users//Jawor//Desktop//R_repos//PredatorABMAnalysis//csvFiles//coords_dist//NTNM_Dist_p1.csv")
NTNMd2 <- pairDist(ntnm2, 2)
write_csv(NTNMd2, "C://Users//Jawor//Desktop//R_repos//PredatorABMAnalysis//csvFiles//coords_dist//NTNM_Dist_p2.csv")
NTNMd3 <- pairDist(ntnm3, 3)
write_csv(NTNMd3, "C://Users//Jawor//Desktop//R_repos//PredatorABMAnalysis//csvFiles//coords_dist//NTNM_Dist_p3.csv")
NTNMd4 <- pairDist(ntnm4, 4)
write_csv(NTNMd4, "C://Users//Jawor//Desktop//R_repos//PredatorABMAnalysis//csvFiles//coords_dist//NTNM_Dist_p4.csv")
NTNM_allDist <- bind_rows(NTNMd1, NTNMd2, NTNMd3, NTNMd4)
write_csv(NTNM_allDist, "C://Users//Jawor//Desktop//R_repos//PredatorABMAnalysis//csvFiles//coords_dist//NTNM_allDist.csv")
rm(NTNMd1, NTNMd2, NTNMd3, NTNMd4, NTNM_allDist)

#No Terr No Mem Pred Mem
#NTNMPMd1 <- pairDist(ntnmpm1, 1)
#NTNMPMd2 <- pairDist(ntnmpm2, 2)
#NTNMPMd3 <- pairDist(ntnmpm3, 3)
#NTNMPMd4 <- pairDist(ntnmpm4, 4)
#NTNMPM_allDist <- bind_rows(NTNMPMd1, NTNMPMd2, NTNMPMd3, NTNMPMd4)
#write_csv(NTNMPM_allDist, "C://Users//Jawor//Desktop//R_repos//PredatorABMAnalysis//csvFiles//NTNMPM_allDist.csv")

#No Terr Shared Mem
NTSMd1 <- pairDist(ntsm1, 1)
write_csv(NTSMd1, "C://Users//Jawor//Desktop//R_repos//PredatorABMAnalysis//csvFiles//coords_dist//NTSM_Dist_p1.csv")
NTSMd2 <- pairDist(ntsm2, 2)
write_csv(NTSMd2, "C://Users//Jawor//Desktop//R_repos//PredatorABMAnalysis//csvFiles//coords_dist//NTSM_Dist_p2.csv")
NTSMd3 <- pairDist(ntsm3, 3)
write_csv(NTSMd3, "C://Users//Jawor//Desktop//R_repos//PredatorABMAnalysis//csvFiles//coords_dist//NTSM_Dist_p3.csv")
NTSMd4 <- pairDist(ntsm4, 4)
write_csv(NTSMd4, "C://Users//Jawor//Desktop//R_repos//PredatorABMAnalysis//csvFiles//coords_dist//NTSM_Dist_p4.csv")
NTSM_allDist <- bind_rows(NTSMd1, NTSMd2, NTSMd3, NTSMd4)
write_csv(NTSM_allDist, "C://Users//Jawor//Desktop//R_repos//PredatorABMAnalysis//csvFiles//coords_dist//NTSM_allDist.csv")
rm(NTSMd1, NTSMd2, NTSMd3, NTSMd4, NTSM_allDist)

#No Terr Shared Mem Pred Mem
#NTSMPMd1 <- pairDist(ntsmpm1, 1)
#NTSMPMd2 <- pairDist(ntsmpm2, 2)
#NTSMPMd3 <- pairDist(ntsmpm3, 3)
#NTSMPMd4 <- pairDist(ntsmpm4, 4)
#NTSMPM_allDist <- bind_rows(NTSMPMd1, NTSMPMd2, NTSMPMd3, NTSMPMd4)
#write_csv(NTSMPM_allDist, "C://Users//Jawor//Desktop//R_repos//PredatorABMAnalysis//csvFiles//NTSMPM_allDist.csv")

#Pred Terr Mem
PDTMd1 <- pairDist(pdtm1, 1)
write_csv(PDTMd1, "C://Users//Jawor//Desktop//R_repos//PredatorABMAnalysis//csvFiles//coords_dist//PDTM_Dist_p1.csv")
PDTMd2 <- pairDist(pdtm2, 2)
write_csv(PDTMd2, "C://Users//Jawor//Desktop//R_repos//PredatorABMAnalysis//csvFiles//coords_dist//PDTM_Dist_p2.csv")
PDTMd3 <- pairDist(pdtm3, 3)
write_csv(PDTMd3, "C://Users//Jawor//Desktop//R_repos//PredatorABMAnalysis//csvFiles//coords_dist//PDTM_Dist_p3.csv")
PDTMd4 <- pairDist(pdtm4, 4)
write_csv(PDTMd4, "C://Users//Jawor//Desktop//R_repos//PredatorABMAnalysis//csvFiles//coords_dist//PDTM_Dist_p4.csv")
PDTM_allDist <- bind_rows(PDTMd1, PDTMd2, PDTMd3, PDTMd4)
write_csv(PDTM_allDist, "C://Users//Jawor//Desktop//R_repos//PredatorABMAnalysis//csvFiles//coords_dist//PDTM_allDist.csv")
rm(PDTMd1, PDTMd2, PDTMd3, PDTMd4, PDTM_allDist)
#Pred Terr Mem Pred Mem
#PDTMPMd1 <- pairDist(pdtmpm1, 1)
#PDTMPMd2 <- pairDist(pdtmpm2, 2)
#PDTMPMd3 <- pairDist(pdtmpm3, 3)
#PDTMPMd4 <- pairDist(pdtmpm4, 4)
#PDTMPM_allDist <- bind_rows(PDTMPMd1, PDTMPMd2, PDTMPMd3, PDTMPMd4)
#write_csv(PDTMPM_allDist, "C://Users//Jawor//Desktop//R_repos//PredatorABMAnalysis//csvFiles//PDTMPM_allDist.csv")

#Pred Terr No Mem
PDTNMd1 <- pairDist(pdtnm1, 1)
write_csv(PDTNMd1, "C://Users//Jawor//Desktop//R_repos//PredatorABMAnalysis//csvFiles//coords_dist//PDTNM_Dist_p1.csv")
PDTNMd2 <- pairDist(pdtnm2, 2)
write_csv(PDTNMd2, "C://Users//Jawor//Desktop//R_repos//PredatorABMAnalysis//csvFiles//coords_dist//PDTNM_Dist_p2.csv")
PDTNMd3 <- pairDist(pdtnm3, 3)
write_csv(PDTNMd3, "C://Users//Jawor//Desktop//R_repos//PredatorABMAnalysis//csvFiles//coords_dist//PDTNM_Dist_p3.csv")
PDTNMd4 <- pairDist(pdtnm4, 4)
write_csv(PDTNMd4, "C://Users//Jawor//Desktop//R_repos//PredatorABMAnalysis//csvFiles//coords_dist//PDTNM_Dist_p4.csv")
PDTNM_allDist <- bind_rows(PDTNMd1, PDTNMd2, PDTNMd3, PDTNMd4)
write_csv(PDTNM_allDist, "C://Users//Jawor//Desktop//R_repos//PredatorABMAnalysis//csvFiles//coords_dist//PDTNM_allDist.csv")
rm(PDTNMd1, PDTNMd2, PDTNMd3, PDTNMd4, PDTNM_allDist)
#Pred Terr No Mem Pred Mem
#PDTNMPMd1 <- pairDist(pdtnmpm1, 1)
#PDTNMPMd2 <- pairDist(pdtnmpm2, 2)
#PDTNMPMd3 <- pairDist(pdtnmpm3, 3)
#PDTNMPMd4 <- pairDist(pdtnmpm4, 4)
#PDTNMPM_allDist <- bind_rows(PDTNMPMd1, PDTNMPMd2, PDTNMPMd3, PDTNMPMd4)
#write_csv(PDTNMPM_allDist, "C://Users//Jawor//Desktop//R_repos//PredatorABMAnalysis//csvFiles//PDTNMPM_allDist.csv")

#Pred Terr Shared Mem Pred Mem
PDTSMd1 <- pairDist(pdtsm1, 1)
write_csv(PDTSMd1, "C://Users//Jawor//Desktop//R_repos//PredatorABMAnalysis//csvFiles//coords_dist//PDTSM_Dist_p1.csv")
PDTSMd2 <- pairDist(pdtsm2, 2)
write_csv(PDTSMd2, "C://Users//Jawor//Desktop//R_repos//PredatorABMAnalysis//csvFiles//coords_dist//PDTSM_Dist_p2.csv")
PDTSMd3 <- pairDist(pdtsm3, 3)
write_csv(PDTSMd3, "C://Users//Jawor//Desktop//R_repos//PredatorABMAnalysis//csvFiles//coords_dist//PDTSM_Dist_p3.csv")
PDTSMd4 <- pairDist(pdtsm4, 4)
write_csv(PDTSMd4, "C://Users//Jawor//Desktop//R_repos//PredatorABMAnalysis//csvFiles//coords_dist//PDTSM_Dist_p4.csv")
PDTSM_allDist <- bind_rows(PDTSMd1, PDTSMd2, PDTSMd3, PDTSMd4)
write_csv(PDTSM_allDist, "C://Users//Jawor//Desktop//R_repos//PredatorABMAnalysis//csvFiles//coords_dist//PDTSM_allDist.csv")
rm(PDTSMd1, PDTSMd2, PDTSMd3, PDTSMd4, PDTSM_allDist)
#Pred Terr Shared Mem Pred Mem
#PDTSMPMd1 <- pairDist(pdtsmpm1, 1)
#PDTSMPMd2 <- pairDist(pdtsmpm2, 2)
#PDTSMPMd3 <- pairDist(pdtsmpm3, 3)
#PDTSMPMd4 <- pairDist(pdtsmpm4, 4)
#PDTSMPM_allDist <- bind_rows(PDTSMPMd1, PDTSMPMd2, PDTSMPMd3, PDTSMPMd4)
#write_csv(PDTSMPM_allDist, "C://Users//Jawor//Desktop//R_repos//PredatorABMAnalysis//csvFiles//PDTSMPM_allDist.csv")

#Prey Terr Mem
PYTMd1 <- pairDist(pytm1, 1)
write_csv(PYTMd1, "C://Users//Jawor//Desktop//R_repos//PredatorABMAnalysis//csvFiles//coords_dist//PYTM_Dist_p1.csv")
PYTMd2 <- pairDist(pytm2, 2)
write_csv(PYTMd2, "C://Users//Jawor//Desktop//R_repos//PredatorABMAnalysis//csvFiles//coords_dist//PYTM_Dist_p2.csv")
PYTMd3 <- pairDist(pytm3, 3)
write_csv(PYTMd3, "C://Users//Jawor//Desktop//R_repos//PredatorABMAnalysis//csvFiles//coords_dist//PYTM_Dist_p3.csv")
PYTMd4 <- pairDist(pytm4, 4)
write_csv(PYTMd4, "C://Users//Jawor//Desktop//R_repos//PredatorABMAnalysis//csvFiles//coords_dist//PYTM_Dist_p4.csv")
PYTM_allDist <- bind_rows(PYTMd1, PYTMd2, PYTMd3, PYTMd4)
write_csv(PYTM_allDist, "C://Users//Jawor//Desktop//R_repos//PredatorABMAnalysis//csvFiles//coords_dist//PYTM_allDist.csv")
rm(PYTMd1, PYTMd2, PYTMd3, PYTMd4, PYTM_allDist)
#Prey Terr Mem Pred Mem
#PYTMPMd1 <- pairDist(pytmpm1, 1)
#PYTMPMd2 <- pairDist(pytmpm2, 2)
#PYTMPMd3 <- pairDist(pytmpm3, 3)
#PYTMPMd4 <- pairDist(pytmpm4, 4)
#PYTMPM_allDist <- bind_rows(PYTMPMd1, PYTMPMd2, PYTMPMd3, PYTMPMd4)
#write_csv(PYTMPM_allDist, "C://Users//Jawor//Desktop//R_repos//PredatorABMAnalysis//csvFiles//PYTMPM_allDist.csv")

#Prey Terr No Mem
PYTNMd1 <- pairDist(pytnm1, 1)
write_csv(PYTNMd1, "C://Users//Jawor//Desktop//R_repos//PredatorABMAnalysis//csvFiles//coords_dist//PYTNM_Dist_p1.csv")
PYTNMd2 <- pairDist(pytnm2, 2)
write_csv(PYTNMd2, "C://Users//Jawor//Desktop//R_repos//PredatorABMAnalysis//csvFiles//coords_dist//PYTNM_Dist_p2.csv")
PYTNMd3 <- pairDist(pytnm3, 3)
write_csv(PYTNMd3, "C://Users//Jawor//Desktop//R_repos//PredatorABMAnalysis//csvFiles//coords_dist//PYTNM_Dist_p3.csv")
PYTNMd4 <- pairDist(pytnm4, 4)
write_csv(PYTNMd4, "C://Users//Jawor//Desktop//R_repos//PredatorABMAnalysis//csvFiles//coords_dist//PYTNM_Dist_p4.csv")
PYTNM_allDist <- bind_rows(PYTNMd1, PYTNMd2, PYTNMd3, PYTNMd4)
write_csv(PYTNM_allDist, "C://Users//Jawor//Desktop//R_repos//PredatorABMAnalysis//csvFiles//coords_dist//PYTNM_allDist.csv")
rm(PYTNMd1, PYTNMd2, PYTNMd3, PYTNMd4, PYTNM_allDist)

#Prey Terr No Mem Pred Mem
#PYTNMPMd1 <- pairDist(pytnmpm1, 1)
#PYTNMPMd2 <- pairDist(pytnmpm2, 2)
#PYTNMPMd3 <- pairDist(pytnmpm3, 3)
#PYTNMPMd4 <- pairDist(pytnmpm4, 4)
#PYTNMPM_allDist <- bind_rows(PYTNMPMd1, PYTNMPMd2, PYTNMPMd3, PYTNMPMd4)
#write_csv(PYTNMPM_allDist, "C://Users//Jawor//Desktop//R_repos//PredatorABMAnalysis//csvFiles//PYTNMPM_allDist.csv")

#Prey Terr Shared Mem
PYTSMd1 <- pairDist(pytsm1, 1)
write_csv(PYTSMd1, "C://Users//Jawor//Desktop//R_repos//PredatorABMAnalysis//csvFiles//coords_dist//PYTSM_Dist_p1.csv")
PYTSMd2 <- pairDist(pytsm2, 2)
write_csv(PYTSMd2, "C://Users//Jawor//Desktop//R_repos//PredatorABMAnalysis//csvFiles//coords_dist//PYTSM_Dist_p2.csv")
PYTSMd3 <- pairDist(pytsm3, 3)
write_csv(PYTSMd3, "C://Users//Jawor//Desktop//R_repos//PredatorABMAnalysis//csvFiles//coords_dist//PYTSM_Dist_p3.csv")
PYTSMd4 <- pairDist(pytsm4, 4)
write_csv(PYTSMd4, "C://Users//Jawor//Desktop//R_repos//PredatorABMAnalysis//csvFiles//coords_dist//PYTSM_Dist_p4.csv")
PYTSM_allDist <- bind_rows(PYTSMd1, PYTSMd2, PYTSMd3, PYTSMd4)
write_csv(PYTSM_allDist, "C://Users//Jawor//Desktop//R_repos//PredatorABMAnalysis//csvFiles//coords_dist//PYTSM_allDist.csv")
rm(PYTSM_allDist, PYTSMd1, PYTSMd2, PYTSMd3, PYTSMd4)
#Prey Terr Shared Mem Pred Mem
#PYTSMPMd1 <- pairDist(pytsmpm1, 1)
#PYTSMPMd2 <- pairDist(pytsmpm2, 2)
#PYTSMPMd3 <- pairDist(pytsmpm3, 3)
#PYTSMPMd4 <- pairDist(pytsmpm4, 4)
#PYTSMPM_allDist <- bind_rows(PYTSMPMd1, PYTSMPMd2, PYTSMPMd3, PYTSMPMd4)
#write_csv(PYTSMPM_allDist, "C://Users//Jawor//Desktop//R_repos//PredatorABMAnalysis//csvFiles//PYTSMPM_allDist.csv")

########################################################
#Step 3: Components Analysis: Number of Components######
#Size of Components, Distance Between Component Members#
########################################################

NTM1_comps <- analyze_networks_tidy(Distdf = NTM_Dist_p1, threshold = 6, pred = 1)
NTM2_comps <- analyze_networks_tidy(Distdf = NTM_Dist_p2, threshold = 6, pred = 2)
NTM3_comps <- analyze_networks_tidy(Distdf = NTM_Dist_p3, threshold = 6, pred = 3)
NTM4_comps <- analyze_networks_tidy(Distdf = NTM_Dist_p4, threshold = 6, pred = 4)

NTM_compSummary <- bind_rows(NTM1_comps$summary, NTM2_comps$summary, NTM3_comps$summary, NTM4_comps$summary)
write.csv(NTM_compSummary, "C:\\Users\\Jawor\\Desktop\\R_repos\\PredatorABMAnalysis\\csvFiles\\compAnalysis\\NTM_compSummary.csv")
NTM_compSizes <- bind_rows(NTM1_comps$comp_sizes, NTM2_comps$comp_sizes, NTM3_comps$comp_sizes, NTM4_comps$comp_sizes)
write.csv(NTM_compSizes, "C:\\Users\\Jawor\\Desktop\\R_repos\\PredatorABMAnalysis\\csvFiles\\compAnalysis\\NTM_compSizes.csv")
NTM_compDists <- bind_rows(NTM1_comps$within_comp_dist, NTM2_comps$within_comp_dist, NTM3_comps$within_comp_dist, NTM4_comps$within_comp_dist)
write.csv(NTM_compDists, "C:\\Users\\Jawor\\Desktop\\R_repos\\PredatorABMAnalysis\\csvFiles\\compAnalysis\\NTM_compDists.csv")
rm(NTM1_comps, NTM2_comps, NTM3_comps, NTM4_comps)

################
NTNM1_comps <- analyze_networks_tidy(Distdf = NTNM_Dist_p1, threshold = 6, pred = 1)
NTNM2_comps <- analyze_networks_tidy(Distdf = NTNM_Dist_p2, threshold = 6, pred = 2)
NTNM3_comps <- analyze_networks_tidy(Distdf = NTNM_Dist_p3, threshold = 6, pred = 3)
NTNM4_comps <- analyze_networks_tidy(Distdf = NTNM_Dist_p4, threshold = 6, pred = 4)

NTNM_compSummary <- bind_rows(NTNM1_comps$summary, NTNM2_comps$summary, NTNM3_comps$summary, NTNM4_comps$summary)
write.csv(NTNM_compSummary, "C:\\Users\\Jawor\\Desktop\\R_repos\\PredatorABMAnalysis\\csvFiles\\compAnalysis\\NTNM_compSummary.csv")
NTNM_compSizes <- bind_rows(NTNM1_comps$comp_sizes, NTNM2_comps$comp_sizes, NTNM3_comps$comp_sizes, NTNM4_comps$comp_sizes)
write.csv(NTNM_compSizes, "C:\\Users\\Jawor\\Desktop\\R_repos\\PredatorABMAnalysis\\csvFiles\\compAnalysis\\NTNM_compSizes.csv")
NTNM_compDists <- bind_rows(NTNM1_comps$within_comp_dist, NTNM2_comps$within_comp_dist, NTNM3_comps$within_comp_dist, NTNM4_comps$within_comp_dist)
write.csv(NTNM_compDists, "C:\\Users\\Jawor\\Desktop\\R_repos\\PredatorABMAnalysis\\csvFiles\\compAnalysis\\NTNM_compDists.csv")
rm(NTNM1_comps, NTNM2_comps, NTNM3_comps, NTNM4_comps)

################
NTSM1_comps <- analyze_networks_tidy(Distdf = NTSM_Dist_p1, threshold = 6, pred = 1)
NTSM2_comps <- analyze_networks_tidy(Distdf = NTSM_Dist_p2, threshold = 6, pred = 2)
NTSM3_comps <- analyze_networks_tidy(Distdf = NTSM_Dist_p3, threshold = 6, pred = 3)
NTSM4_comps <- analyze_networks_tidy(Distdf = NTSM_Dist_p4, threshold = 6, pred = 4)

NTSM_compSummary <- bind_rows(NTSM1_comps$summary, NTSM2_comps$summary, NTSM3_comps$summary, NTSM4_comps$summary)
write.csv(NTSM_compSummary, "C:\\Users\\Jawor\\Desktop\\R_repos\\PredatorABMAnalysis\\csvFiles\\compAnalysis\\NTSM_compSummary.csv")
NTSM_compSizes <- bind_rows(NTSM1_comps$comp_sizes, NTSM2_comps$comp_sizes, NTSM3_comps$comp_sizes, NTSM4_comps$comp_sizes)
write.csv(NTNM_compSizes, "C:\\Users\\Jawor\\Desktop\\R_repos\\PredatorABMAnalysis\\csvFiles\\compAnalysis\\NTSM_compSizes.csv")
NTSM_compDists <- bind_rows(NTSM1_comps$within_comp_dist, NTSM2_comps$within_comp_dist, NTSM3_comps$within_comp_dist, NTSM4_comps$within_comp_dist)
write.csv(NTNM_compDists, "C:\\Users\\Jawor\\Desktop\\R_repos\\PredatorABMAnalysis\\csvFiles\\compAnalysis\\NTSM_compDists.csv")
rm(NTSM1_comps, NTSM2_comps, NTSM3_comps, NTSM4_comps)

#pred terr
PDTM1_comps <- analyze_networks_tidy(Distdf = PDTM_Dist_p1, threshold = 6, pred = 1)
PDTM2_comps <- analyze_networks_tidy(Distdf = PDTM_Dist_p2, threshold = 6, pred = 2)
PDTM3_comps <- analyze_networks_tidy(Distdf = PDTM_Dist_p3, threshold = 6, pred = 3)
PDTM4_comps <- analyze_networks_tidy(Distdf = PDTM_Dist_p4, threshold = 6, pred = 4)

PDTM_compSummary <- bind_rows(PDTM1_comps$summary, PDTM2_comps$summary, PDTM3_comps$summary, PDTM4_comps$summary)
write.csv(PDTM_compSummary, "C:\\Users\\Jawor\\Desktop\\R_repos\\PredatorABMAnalysis\\csvFiles\\compAnalysis\\PDTM_compSummary.csv")
PDTM_compSizes <- bind_rows(PDTM1_comps$comp_sizes, PDTM2_comps$comp_sizes, PDTM3_comps$comp_sizes, PDTM4_comps$comp_sizes)
write.csv(PDTM_compSizes, "C:\\Users\\Jawor\\Desktop\\R_repos\\PredatorABMAnalysis\\csvFiles\\compAnalysis\\PDTM_compSizes.csv")
PDTM_compDists <- bind_rows(PDTM1_comps$within_comp_dist, PDTM2_comps$within_comp_dist, PDTM3_comps$within_comp_dist, PDTM4_comps$within_comp_dist)
write.csv(PDTM_compDists, "C:\\Users\\Jawor\\Desktop\\R_repos\\PredatorABMAnalysis\\csvFiles\\compAnalysis\\PDTM_compDists.csv")
rm(PDTM1_comps, PDTM2_comps, PDTM3_comps, PDTM4_comps)
#####################
PDTNM1_comps <- analyze_networks_tidy(Distdf = PDTNM_Dist_p1, threshold = 6, pred = 1)
PDTNM2_comps <- analyze_networks_tidy(Distdf = PDTNM_Dist_p2, threshold = 6, pred = 2)
PDTNM3_comps <- analyze_networks_tidy(Distdf = PDTNM_Dist_p3, threshold = 6, pred = 3)
PDTNM4_comps <- analyze_networks_tidy(Distdf = PDTNM_Dist_p4, threshold = 6, pred = 4)

PDTNM_compSummary <- bind_rows(PDTNM1_comps$summary, PDTNM2_comps$summary, PDTNM3_comps$summary, PDTNM4_comps$summary)
write.csv(PDTNM_compSummary, "C:\\Users\\Jawor\\Desktop\\R_repos\\PredatorABMAnalysis\\csvFiles\\compAnalysis\\PDTNM_compSummary.csv")
PDTNM_compSizes <- bind_rows(PDTNM1_comps$comp_sizes, PDTNM2_comps$comp_sizes, PDTNM3_comps$comp_sizes, PDTNM4_comps$comp_sizes)
write.csv(PDTNM_compSizes, "C:\\Users\\Jawor\\Desktop\\R_repos\\PredatorABMAnalysis\\csvFiles\\compAnalysis\\PDTNM_compSizes.csv")
PDTNM_compDists <- bind_rows(PDTNM1_comps$within_comp_dist, PDTNM2_comps$within_comp_dist, PDTNM3_comps$within_comp_dist, PDTNM4_comps$within_comp_dist)
write.csv(PDTNM_compDists, "C:\\Users\\Jawor\\Desktop\\R_repos\\PredatorABMAnalysis\\csvFiles\\compAnalysis\\PDTNM_compDists.csv")
rm(PDTNM1_comps, PDTNM2_comps, PDTNM3_comps, PDTNM4_comps)
####################
PDTSM1_comps <- analyze_networks_tidy(Distdf = PDTSM_Dist_p1, threshold = 6, pred = 1)
PDTSM2_comps <- analyze_networks_tidy(Distdf = PDTSM_Dist_p2, threshold = 6, pred = 2)
PDTSM3_comps <- analyze_networks_tidy(Distdf = PDTSM_Dist_p3, threshold = 6, pred = 3)
PDTSM4_comps <- analyze_networks_tidy(Distdf = PDTSM_Dist_p4, threshold = 6, pred = 4)

PDTSM_compSummary <- bind_rows(PDTSM1_comps$summary, PDTSM2_comps$summary, PDTSM3_comps$summary, PDTSM4_comps$summary)
write.csv(PDTSM_compSummary, "C:\\Users\\Jawor\\Desktop\\R_repos\\PredatorABMAnalysis\\csvFiles\\compAnalysis\\PDTSM_compSummary.csv")
PDTSM_compSizes <- bind_rows(PDTSM1_comps$comp_sizes, PDTSM2_comps$comp_sizes, PDTSM3_comps$comp_sizes, PDTSM4_comps$comp_sizes)
write.csv(PDTSM_compSizes, "C:\\Users\\Jawor\\Desktop\\R_repos\\PredatorABMAnalysis\\csvFiles\\compAnalysis\\PDTSM_compSizes.csv")
PDTSM_compDists <- bind_rows(PDTSM1_comps$within_comp_dist, PDTSM2_comps$within_comp_dist, PDTSM3_comps$within_comp_dist, PDTSM4_comps$within_comp_dist)
write.csv(PDTSM_compDists, "C:\\Users\\Jawor\\Desktop\\R_repos\\PredatorABMAnalysis\\csvFiles\\compAnalysis\\PDTSM_compDists.csv")
rm(PDTSM1_comps, PDTSM2_comps, PDTSM3_comps, PDTSM4_comps)

#prey terr
PYTM1_comps <- analyze_networks_tidy(Distdf = PYTM_Dist_p1, threshold = 6, pred = 1)
PYTM2_comps <- analyze_networks_tidy(Distdf = PYTM_Dist_p2, threshold = 6, pred = 2)
PYTM3_comps <- analyze_networks_tidy(Distdf = PYTM_Dist_p3, threshold = 6, pred = 3)
PYTM4_comps <- analyze_networks_tidy(Distdf = PYTM_Dist_p4, threshold = 6, pred = 4)

PYTM_compSummary <- bind_rows(PYTM1_comps$summary, PYTM2_comps$summary, PYTM3_comps$summary, PYTM4_comps$summary)
write.csv(PYTM_compSummary, "C:\\Users\\Jawor\\Desktop\\R_repos\\PredatorABMAnalysis\\csvFiles\\compAnalysis\\PYTM_compSummary.csv")
PYTM_compSizes <- bind_rows(PYTM1_comps$comp_sizes, PYTM2_comps$comp_sizes, PYTM3_comps$comp_sizes, PYTM4_comps$comp_sizes)
write.csv(PYTM_compSizes, "C:\\Users\\Jawor\\Desktop\\R_repos\\PredatorABMAnalysis\\csvFiles\\compAnalysis\\PYTM_compSizes.csv")
PYTM_compDists <- bind_rows(PYTM1_comps$within_comp_dist, PYTM2_comps$within_comp_dist, PYTM3_comps$within_comp_dist, PYTM4_comps$within_comp_dist)
write.csv(PYTM_compDists, "C:\\Users\\Jawor\\Desktop\\R_repos\\PredatorABMAnalysis\\csvFiles\\compAnalysis\\PYTM_compDists.csv")
rm(PYTM1_comps, PYTM2_comps, PYTM3_comps, PYTM4_comps)

###################
PYTNM1_comps <- analyze_networks_tidy(Distdf = PYTNM_Dist_p1, threshold = 6, pred = 1)
PYTNM2_comps <- analyze_networks_tidy(Distdf = PYTNM_Dist_p2, threshold = 6, pred = 2)
PYTNM3_comps <- analyze_networks_tidy(Distdf = PYTNM_Dist_p3, threshold = 6, pred = 3)
PYTNM4_comps <- analyze_networks_tidy(Distdf = PYTNM_Dist_p4, threshold = 6, pred = 4)

PYTNM_compSummary <- bind_rows(PYTNM1_comps$summary, PYTNM2_comps$summary, PYTNM3_comps$summary, PYTNM4_comps$summary)
write.csv(PYTNM_compSummary, "C:\\Users\\Jawor\\Desktop\\R_repos\\PredatorABMAnalysis\\csvFiles\\compAnalysis\\PYTNM_compSummary.csv")
PYTNM_compSizes <- bind_rows(PYTNM1_comps$comp_sizes, PYTNM2_comps$comp_sizes, PYTNM3_comps$comp_sizes, PYTNM4_comps$comp_sizes)
write.csv(PYTNM_compSizes, "C:\\Users\\Jawor\\Desktop\\R_repos\\PredatorABMAnalysis\\csvFiles\\compAnalysis\\PYTNM_compSizes.csv")
PYTNM_compDists <- bind_rows(PYTNM1_comps$within_comp_dist, PYTNM2_comps$within_comp_dist, PYTNM3_comps$within_comp_dist, PYTNM4_comps$within_comp_dist)
write.csv(PYTNM_compDists, "C:\\Users\\Jawor\\Desktop\\R_repos\\PredatorABMAnalysis\\csvFiles\\compAnalysis\\PYTNM_compDists.csv")
rm(PYTNM1_comps, PYTNM2_comps, PYTNM3_comps, PYTNM4_comps)

######################
PYTSM1_comps <- analyze_networks_tidy(Distdf = PYTSM_Dist_p1, threshold = 6, pred = 1)
PYTSM2_comps <- analyze_networks_tidy(Distdf = PYTSM_Dist_p2, threshold = 6, pred = 2)
PYTSM3_comps <- analyze_networks_tidy(Distdf = PYTSM_Dist_p3, threshold = 6, pred = 3)
PYTSM4_comps <- analyze_networks_tidy(Distdf = PYTSM_Dist_p4, threshold = 6, pred = 4)

PYTSM_compSummary <- bind_rows(PYTSM1_comps$summary, PYTSM2_comps$summary, PYTSM3_comps$summary, PYTSM4_comps$summary)
write.csv(PYTSM_compSummary, "C:\\Users\\Jawor\\Desktop\\R_repos\\PredatorABMAnalysis\\csvFiles\\compAnalysis\\PYTSM_compSummary.csv")
PYTSM_compSizes <- bind_rows(PYTSM1_comps$comp_sizes, PYTSM2_comps$comp_sizes, PYTSM3_comps$comp_sizes, PYTSM4_comps$comp_sizes)
write.csv(PYTSM_compSizes, "C:\\Users\\Jawor\\Desktop\\R_repos\\PredatorABMAnalysis\\csvFiles\\compAnalysis\\PYTSM_compSizes.csv")
PYTSM_compDists <- bind_rows(PYTSM1_comps$within_comp_dist, PYTSM2_comps$within_comp_dist, PYTSM3_comps$within_comp_dist, PYTSM4_comps$within_comp_dist)
write.csv(PYTSM_compDists, "C:\\Users\\Jawor\\Desktop\\R_repos\\PredatorABMAnalysis\\csvFiles\\compAnalysis\\PYTSM_compDists.csv")
rm(PYTSM1_comps, PYTSM2_comps, PYTSM3_comps, PYTSM4_comps)

#Start plotting stuff blud

#NTM
NTM_compDist3 <- NTM_compDists |>
  filter(tick >= 3000)
ggplot(NTM_compDist3, mapping = aes(x = as.factor(numPred), y = mean_within_dist)) +
  geom_boxplot()

ggplot(NTM_compDist3, aes(x = as.factor(numPred), y = mean_within_dist, color = factor(numPred))) +
  geom_boxplot() +
  ylim(0, NA) +
  labs(
    color = "Predators",
    x = "Number of Predators",
    y = "Mean inter-prey distance",
    title = "No Territory With Memory, Effect of predator number on within group prey density"
  ) +
  theme_minimal()

NTM_compSizes3 <- NTM_compSizes |>
  filter(tick >= 3000)
ggplot(NTM_compSizes3, aes(x = as.factor(numPred), y = compSize, color = factor(numPred))) +
  geom_boxplot() +
  ylim(0, NA) +
  labs(
    color = "Predators",
    x = "Number of Predators",
    y = "Group Size",
    title = "No Territory With Memory, Effect of predator number on group size"
  ) +
  theme_minimal()

NTM_compSummary3 <- NTM_compSummary |>
  filter(tick >= 3000)
ggplot(NTM_compSummary3, aes(x = as.factor(numPred), y = num_components, color = factor(numPred))) +
  geom_boxplot() +
  ylim(0, NA) +
  labs(
    color = "Predators",
    x = "Number of Predators",
    y = "Number of Groups",
    title = "No Territory With Memory, Effect of predator number on Number of Groups"
  ) +
  theme_minimal()

NTM_compDists <- NTM_compDists |>
  filter(tick <= 3000)
ggplot(NTM_compDists, aes(x = tick, y = mean_within_dist, color = factor(numPred))) +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 10), se = FALSE, linewidth = 1.2) +
  theme_minimal() +
  labs(color = "Number of Predators",
       x = "Tick",
       y = "Inter-Individual Distance",
       title = "No Territory With Memory, Inter-Individual Distance From 0-3000 Ticks")
NTM_compSizes <- NTM_compSizes |>
  filter(tick <= 3000)
ggplot(NTM_compSizes, aes(x = tick, y = compSize, color = factor(numPred))) +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 10), se = FALSE, linewidth = 1.2) +
  theme_minimal() +
  labs(color = "Number of Predators",
       x = "Tick",
       y = "Group Size",
       title = "No Territory With Memory, Group Size From 0-3000 Ticks")
NTM_compSummary <- NTM_compSummary |>
  filter(tick <= 3000)
ggplot(NTM_compSummary, aes(x = tick, y = num_components, color = factor(numPred))) +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 10), se = FALSE, linewidth = 1.2) +
  theme_minimal() +
  labs(color = "Number of Predators",
       x = "Tick",
       y = "Number of Groups",
       title = "No Territory With Memory, Number of Groups From 0-3000 Ticks")

#NTNM
NTNM_compDist3 <- NTNM_compDists |>
  filter(tick >= 3000)
ggplot(NTNM_compDist3, aes(x = as.factor(numPred), y = mean_within_dist, color = factor(numPred))) +
  geom_boxplot() +
  ylim(0, NA) +
  labs(
    color = "Predators",
    x = "Number of Predators",
    y = "Mean inter-prey distance",
    title = "No Territory With No Memory, Effect of predator number on within group prey density"
  ) +
  theme_minimal()

NTNM_compSizes3 <- NTNM_compSizes |>
  filter(tick >= 3000)
ggplot(NTNM_compSizes3, aes(x = as.factor(numPred), y = compSize, color = factor(numPred))) +
  geom_boxplot() +
  ylim(0, NA) +
  labs(
    color = "Predators",
    x = "Number of Predators",
    y = "Group Size",
    title = "No Territory With No Memory, Effect of predator number on group size"
  ) +
  theme_minimal()

NTNM_compSummary3 <- NTNM_compSummary |>
  filter(tick >= 3000)
ggplot(NTNM_compSummary3, aes(x = as.factor(numPred), y = num_components, color = factor(numPred))) +
  geom_boxplot() +
  ylim(0, NA) +
  labs(
    color = "Predators",
    x = "Number of Predators",
    y = "Number of Groups",
    title = "No Territory With No Memory, Effect of predator number on Number of Groups"
  ) +
  theme_minimal()

NTNM_compDists <- NTNM_compDists |>
  filter(tick <= 3000)
ggplot(NTNM_compDists, aes(x = tick, y = mean_within_dist, color = factor(numPred))) +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 10), se = FALSE, linewidth = 1.2) +
  theme_minimal() +
  labs(color = "Number of Predators",
       x = "Tick",
       y = "Inter-Individual Distance",
       title = "No Territory With No Memory, Inter-Individual Distance From 0-3000 Ticks")
NTNM_compSizes <- NTNM_compSizes |>
  filter(tick <= 3000)
ggplot(NTNM_compSizes, aes(x = tick, y = compSize, color = factor(numPred))) +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 10), se = FALSE, linewidth = 1.2) +
  theme_minimal() +
  labs(color = "Number of Predators",
       x = "Tick",
       y = "Group Size",
       title = "No Territory With No Memory, Group Size From 0-3000 Ticks")
NTNM_compSummary <- NTNM_compSummary |>
  filter(tick <= 3000)
ggplot(NTNM_compSummary, aes(x = tick, y = num_components, color = factor(numPred))) +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 10), se = FALSE, linewidth = 1.2) +
  theme_minimal() +
  labs(color = "Number of Predators",
       x = "Tick",
       y = "Number of Groups",
       title = "No Territory With No Memory, Number of Groups From 0-3000 Ticks")
#NTSM
NTSM_compDist3 <- NTSM_compDists |>
  filter(tick >= 3000)
ggplot(NTSM_compDist3, aes(x = as.factor(numPred), y = mean_within_dist, color = factor(numPred))) +
  geom_boxplot() +
  ylim(0, NA) +
  labs(
    color = "Predators",
    x = "Number of Predators",
    y = "Mean inter-prey distance",
    title = "No Territory With Shared Memory, Effect of predator number on within group prey density"
  ) +
  theme_minimal()

NTSM_compSizes3 <- NTSM_compSizes |>
  filter(tick >= 3000)
ggplot(NTSM_compSizes3, aes(x = as.factor(numPred), y = compSize, color = factor(numPred))) +
  geom_boxplot() +
  ylim(0, NA) +
  labs(
    color = "Predators",
    x = "Number of Predators",
    y = "Group Size",
    title = "No Territory With Shared Memory, Effect of predator number on group size"
  ) +
  theme_minimal()

NTSM_compSummary3 <- NTSM_compSummary |>
  filter(tick >= 3000)
ggplot(NTSM_compSummary3, aes(x = as.factor(numPred), y = num_components, color = factor(numPred))) +
  geom_boxplot() +
  ylim(0, NA) +
  labs(
    color = "Predators",
    x = "Number of Predators",
    y = "Number of Groups",
    title = "No Territory With Shared Memory, Effect of predator number on Number of Groups"
  ) +
  theme_minimal()

NTSM_compDists <- NTSM_compDists |>
  filter(tick <= 3000)
ggplot(NTSM_compDists, aes(x = tick, y = mean_within_dist, color = factor(numPred))) +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 10), se = FALSE, linewidth = 1.2) +
  theme_minimal() +
  labs(color = "Number of Predators",
       x = "Tick",
       y = "Inter-Individual Distance",
       title = "No Territory With Shared Memory, Inter-Individual Distance From 0-3000 Ticks")
NTSM_compSizes <- NTSM_compSizes |>
  filter(tick <= 3000)
ggplot(NTSM_compSizes, aes(x = tick, y = compSize, color = factor(numPred))) +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 10), se = FALSE, linewidth = 1.2) +
  theme_minimal() +
  labs(color = "Number of Predators",
       x = "Tick",
       y = "Group Size",
       title = "No Territory With Shared Memory, Group Size From 0-3000 Ticks")
NTSM_compSummary <- NTSM_compSummary |>
  filter(tick <= 3000)
ggplot(NTSM_compSummary, aes(x = tick, y = num_components, color = factor(numPred))) +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 10), se = FALSE, linewidth = 1.2) +
  theme_minimal() +
  labs(color = "Number of Predators",
       x = "Tick",
       y = "Number of Groups",
       title = "No Territory With Shared Memory, Number of Groups From 0-3000 Ticks")
############################
#Step 4: Space Use Analysis#
############################

#No Terr Mem
path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\NoTerritoryMemory\\patchCounts\\p1"
NTM_patch1 <- fileRead(path, numPred = 1, type = "patchCount")
NTM_patch1Maps <- heatMap(df = NTM_patch1, numPred = 1, titleText = "No Territory with Memory Prey Density, 1 Predator")
NTM_patch1Maps$preyDes
NTM_patch1Maps$grid

path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\NoTerritoryMemory\\patchCounts\\p2"
NTM_patch2 <- fileRead(path, numPred = 2, type = "patchCount")
NTM_patch2Maps <- heatMap(df = NTM_patch2, numPred = 2, titleText = "No Territory with Memory Prey Density, 2 Predators")
NTM_patch2Maps$preyDes

path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\NoTerritoryMemory\\patchCounts\\p3"
NTM_patch3 <- fileRead(path, numPred = 3, type = "patchCount")
NTM_patch3Maps <- heatMap(df = NTM_patch3, numPred = 3, titleText = "No Territory with Memory Prey Density, 3 Predators")
NTM_patch3Maps$preyDes

path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\NoTerritoryMemory\\patchCounts\\p4"
NTM_patch4 <- fileRead(path, numPred = 4, type = "patchCount")
NTM_patch4Maps <- heatMap(df = NTM_patch4, numPred = 4, titleText = "No Territory with Memory Prey Density, 4 Predators")
NTM_patch4Maps$preyDes

#No Terr Mem Pred Mem

#No Terr No Mem
path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\NoTerritoryNoMemory\\patchCounts\\p1"
NTNM_patch1 <- fileRead(path, numPred = 1, type = "patchCount")
NTNM_patch1Maps <- heatMap(df = NTNM_patch1, numPred = 1, titleText = "No Territory No Memory,  Prey Density, 1 Predator")
NTNM_patch1Maps$preyDes

path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\NoTerritoryNoMemory\\patchCounts\\p2"
NTNM_patch2 <- fileRead(path, numPred = 2, type = "patchCount")
NTNM_patch2Maps <- heatMap(df = NTNM_patch2, numPred = 2, titleText = "No Territory No Memory,  Prey Density, 2 Predators")
NTNM_patch2Maps$preyDes

path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\NoTerritoryNoMemory\\patchCounts\\p3"
NTNM_patch3 <- fileRead(path, numPred = 3, type = "patchCount")
NTNM_patch3Maps <- heatMap(df = NTNM_patch3, numPred = 3, titleText = "No Territory No Memory,  Prey Density, 3 Predators")
NTNM_patch3Maps$preyDes

path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\NoTerritoryNoMemory\\patchCounts\\p4"
NTNM_patch4 <- fileRead(path, numPred = 4, type = "patchCount")
NTNM_patch4Maps <- heatMap(df = NTNM_patch4, numPred = 4, titleText = "No Territory No Memory,  Prey Density, 4 Predators")
NTNM_patch4Maps$preyDes

#No Terr No Mem Pred Mem

#No Terr Shared Mem
path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\NoTerritorySharedMemory\\patchCounts\\p1"
NTSM_patch1 <- fileRead(path, numPred = 1, type = "patchCount")
NTSM_patch1Maps <- heatMap(df = NTSM_patch1, numPred = 1, titleText = "No Territory with Shared Memory Prey Density, 1 Predator")
NTSM_patch1Maps$preyDes

path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\NoTerritorySharedMemory\\patchCounts\\p2"
NTSM_patch2 <- fileRead(path, numPred = 2, type = "patchCount")
NTSM_patch2Maps <- heatMap(df = NTSM_patch2, numPred = 2, titleText = "No Territory with Shared Memory Prey Density, 2 Predators")
NTSM_patch2Maps$preyDes

path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\NoTerritorySharedMemory\\patchCounts\\p3"
NTSM_patch3 <- fileRead(path, numPred = 3, type = "patchCount")
NTSM_patch3Maps <- heatMap(df = NTSM_patch3, numPred = 3, titleText = "No Territory with Shared Memory Prey Density, 3 Predators")
NTSM_patch3Maps$preyDes

path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\NoTerritorySharedMemory\\patchCounts\\p4"
NTSM_patch4 <- fileRead(path, numPred = 4, type = "patchCount")
NTSM_patch4Maps <- heatMap(df = NTSM_patch4, numPred = 4, titleText = "No Territory with Shared Memory Prey Density, 4 Predators")
NTSM_patch4Maps$preyDes

#No Terr Shared Mem Pred Mem

##########################################################################
#Step 5: Pred Distance in Relation to Prey#
##########################################################################
#NTM
path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\NoTerritoryMemory\\predCoords\\p1"
ntmpc1 <- fileRead(path, numPred = 1, type = "coords")
ntm1_PDPYDist <- predPreyDist(ntm1, ntmpc1, 1)

path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\NoTerritoryMemory\\predCoords\\p2"
ntmpc2 <- fileRead(path, numPred = 2, type = "coords")
ntm2_PDPYDist <- predPreyDist(ntm2, ntmpc2, 2)

path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\NoTerritoryMemory\\predCoords\\p3"
ntmpc3 <- fileRead(path, numPred = 3, type = "coords")
ntm3_PDPYDist <- predPreyDist(ntm3, ntmpc3, 3)

path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\NoTerritoryMemory\\predCoords\\p4"
ntmpc4 <- fileRead(path, numPred = 4, type = "coords")
ntm4_PDPYDist <- predPreyDist(ntm4, ntmpc4, 4)

#NTNM
path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\NoTerritoryNoMemory\\predCoords\\p1"
ntnmpc1 <- fileRead(path, numPred = 1, type = "coords")
ntnm1_PDPYDist <- predPreyDist(ntnm1, ntnmpc1, 1)

path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\NoTerritoryNoMemory\\predCoords\\p2"
ntnmpc2 <- fileRead(path, numPred = 2, type = "coords")
ntnm2_PDPYDist <- predPreyDist(ntnm2, ntnmpc2, 2)

path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\NoTerritoryNoMemory\\predCoords\\p3"
ntnmpc3 <- fileRead(path, numPred = 3, type = "coords")
ntnm3_PDPYDist <- predPreyDist(ntnm3, ntnmpc3, 3)

path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\NoTerritoryNoMemory\\predCoords\\p4"
ntnmpc4 <- fileRead(path, numPred = 4, type = "coords")
ntnm4_PDPYDist <- predPreyDist(ntnm4, ntnmpc4, 4)

#NTSM
path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\NoTerritorySharedMemory\\predCoords\\p1"
ntsmpc1 <- fileRead(path, numPred = 1, type = "coords")
ntsm1_PDPYDist <- predPreyDist(ntsm1, ntsmpc1, 1)
closestPrey <- ntsm1_PDPYDist |>
  group_by(behaviorSpaceRun, tick, predator_id) |>
  slice_min(dist, n = 1, with_ties = FALSE) |>
  ungroup() |>
  rename(closest_prey_id = prey_id,
         min_distance = dist)

path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\NoTerritorySharedMemory\\predCoords\\p2"
ntsmpc2 <- fileRead(path, numPred = 2, type = "coords")
ntsm2_PDPYDist <- predPreyDist(ntsm2, ntsmpc2, 2)

path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\NoTerritorySharedMemory\\predCoords\\p3"
ntsmpc3 <- fileRead(path, numPred = 3, type = "coords")
ntsm3_PDPYDist <- predPreyDist(ntsm3, ntsmpc3, 3)

path <- "C:\\Users\\Jawor\\Desktop\\ABM_ConferenceCourse\\ExperimentOutputs\\NoTerritorySharedMemory\\predCoords\\p4"
ntsmpc4 <- fileRead(path, numPred = 4, type = "coords")
ntsm4_PDPYDist <- predPreyDist(ntsm4, ntsmpc4, 4)

