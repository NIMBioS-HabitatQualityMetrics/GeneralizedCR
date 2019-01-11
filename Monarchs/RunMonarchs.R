#########################
# This code sets up and runs the monarch migration network metrics
# Monarch data can be found in metric_inputs_monarch.xlsx
# CR.R does the full CR calculation and averaging.


## USERS MUST SUPPLY INPUT FILES (metric_inputs_name.xlsx) for each class
## The format of the input files is important
## One tab (sheet) for each season
## Node Attributes at the top to include: Population, Survival Rate, Reproduction or Transition rates, and a list of allowed class transitions
## Then Path Survival rates in matrix form
## Finally Path Transition rates in matrix form

# Clear Workspace
rm(list=ls())

#########################
### USER DEFINED DATA ###
#########################

seasons <- 7 # Number of seasons or steps in one annual cycle. 
# This must match number of spreadsheets in input files

num_nodes <- 4 # Number of nodes in the network
# This must match the number of initial conditions given in input files

NODENAMES <- c("Mexico", "South", "Central", "North")
# Used to name row values in CR outputs, should be ordered to match node order in the .xlsx file

NETNAME <- c("monarch") # Give a distinct name for each class as used in input files
# Input files should be in the same directory as RunSpecies.R and named: metric_inputs_NETNAME[[]].xlsx
# Order is important when looking at the code: here we would index [[1]] = class 1 and [[2]] = class 2

PRINT_RESULTS <- TRUE # If true final CR results will print to the screen




#############################################################
### Users should not need to interact with the code below ###
#############################################################


# Set the working directory
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)
source("../CR.R")


if(PRINT_RESULTS == TRUE){
  ## Print data to screen
  print("C-metric for all classes, seasons, and nodes:")
  print(round(CR,4))
  print("-------------------------------------------------------------------")
  
  print("Class averaged C-metric for all nodes and seasons:")
  print(round(CRt,4))
  print("-------------------------------------------------------------------")
  
  print("Season averaged C-metric for all nodes:")
  print(round(CRs,4))
  print("-------------------------------------------------------------------")
  
  print("Network Growth Rate for all seasons:")
  print(round(LAMBDAt,4))
  print("-------------------------------------------------------------------")
  
  print("Cpath-metric for all pathways, classes, and seasons:")
  print(round(CRpath,4))
  print("-------------------------------------------------------------------")
  
  print("Class averaged Cpath-metric for all pathways and seasons")
  print(round(CRpatht,4))
  print("-------------------------------------------------------------------")
  
  print("Season averaged Cpath-metric for all pathways:")
  print(round(CRpaths,4))
  print("-------------------------------------------------------------------")
  
}