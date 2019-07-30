########## NODE IMPORTANCE METRIC CALCULATION CODE ############
# This code calculates:
# CR - the C-metric for each class and each node at each season step
# CRt - the class population weighted C-metric for each node at each season step
# CRs - the season population weighted C-metric for each node across the annual cycle
# LAMBDAt - the network growth rate for each season
#
# Uses the generalized formulation from: Quantifying source and sink habitats and pathways in spatially structured populations: a generalized modelling approach 
#              - Christine Sample, Joanna A. Bieri, Benjamin Allen, Yulia Dementieva, Alyssa Carson, Connor Higgins, Sadie Piatt, Shirley Qiu, Summern Stafford, 
#                Brady J. Mattsson, Darius Semmens, Wayne E. Thogmartin, Jay E. Diffendorfer




library(XLConnect)

### Users should not need to interact with the code below ###

AMATRIX <- list()
AMATRIX_CR <- list()
f_update <- list()
p_update <- list()
s_update <- list()
rates <- c("population","survival","reproduction","transition")

### Find the number of classes
NUMNET <- length(NETNAME)

# Set the input file names
input_file_names <- matrix(0,1,NUMNET)
for (i in 1:NUMNET){
  input_file_names[i]<-c(paste("metric_inputs_",NETNAME[i],".xlsx", sep="")) 
}

#####################################
### CHECK VALIDITY OF INPUT FILES ###
#####################################

for (i in 1:NUMNET){
  CHECK_WORKBOOK <- file.exists(input_file_names[i])
  if (CHECK_WORKBOOK==F){stop(paste("The Workbook,", input_file_names[i],"could not be found. \n *** Please check that the end of the input file names match NETNAME"))}
}
rm(i)
for (i in 1:NUMNET){
  SPREADSHEETS <- getSheets(loadWorkbook(input_file_names[i]))
  NUMSHEETS <- length(SPREADSHEETS)
  if (NUMSHEETS != seasons){stop(paste("\n The Workbook,", input_file_names[i],"has an incorrect number of sheets. \n *** It should contain one sheet for initial conditions and one sheet for each season."))}
}

###################################
### IMPORT NODE CHARACTERISTICS ###
###################################

NODE_ATTRIBUTES <- list()
PATH_ATTRIBUTES <- list()
TRANS_ATTRIBUTES <- list()

## Node data belongs on the top starting in cell C3
## Path data should be contained in nXn ranges below the node data with 3 rows separating
## Survival probabilities should start in cell C(n+6)
## Transition rates should start in cell C(2n+9) where n=number of nodes


for (i in 1:NUMNET) {
  NODE_ATTRIBUTES[[NETNAME[i]]]<- list()
  PATH_ATTRIBUTES[[NETNAME[i]]]<- list()
  TRANS_ATTRIBUTES[[NETNAME[i]]] <- list()
  for (k in 1:seasons){
    # READ IN THE DATA
    SR <- 2 # row in the spreadsheet where we start reading node data
    SC <- 3 # column in the spreadsheet where we start reading node data
  
    ###____seasonal node characteristics ____###
    NODE_ATTRIBUTES[[NETNAME[i]]][[k]] <- list()
    for (m in 1:3) {
      NODE_ATTRIBUTES[[NETNAME[i]]][[k]][[rates[[m]]]]<- matrix(0,nrow = num_nodes,ncol = 1)
      NODE_ATTRIBUTES[[NETNAME[i]]][[k]][[rates[[m]]]]<- readWorksheetFromFile(input_file_names[i], sheet=k, startRow=SR, endRow=SR+num_nodes, startCol=SC, endCol=SC)
      SC <- SC + 1
      # Check that there are no empty cells in node characteristics
      if(any(is.na(NODE_ATTRIBUTES[[NETNAME[i]]][[k]][[m]]))){
        stop(paste('\n Check the input file! \n *** There is at least one empty node characteristic in season', k))}
    }
    
    TRANS_ATTRIBUTES[[NETNAME[i]]][[k]]<- matrix(0,nrow=NUMNET,ncol=1)
    TRANS_ATTRIBUTES[[NETNAME[i]]][[k]]<- readWorksheetFromFile(input_file_names[i], sheet=k, startRow=SR, endRow=SR+NUMNET, startCol=7, endCol=7)
    # Check that the transition atributes are in the correct order
    temp <- matrix(0,nrow=NUMNET,ncol=1)
    temp <- readWorksheetFromFile(input_file_names[i], sheet=k, startRow=SR, endRow=SR+NUMNET, startCol=6, endCol=6)
    if(any(temp != NETNAME)){
      stop(paste('\n Check the input file! \n *** The order of classes for allowed transitions must match the network order in season', k, 'for class', NETNAME[i],'\n *** check the variables temp and NETNAME.'))
    }
    
    SC <- 3
    ###____seasonal path characteristics ____###
    PATH_ATTRIBUTES[[NETNAME[i]]][[k]]<- list()
    SR <- num_nodes + 5
    for (n in 1:2){
      PATH_ATTRIBUTES[[NETNAME[i]]][[k]][[n]] <- readWorksheetFromFile(input_file_names[i],sheet=k,startRow=SR,endRow=SR+num_nodes,startCol=SC,endCol= SC + num_nodes)
      SR <- (2 * num_nodes) + 8
    }
  } 
}

#####################
### CREATE N LIST ###
#####################
N <- matrix(0, nrow = NUMNET*num_nodes, ncol = seasons)
for (SN in 1:seasons) {
  for (ND in 1:num_nodes) {
    for (CL in 1:NUMNET) {
      N[CL+NUMNET*(ND-1),SN]<- NODE_ATTRIBUTES[[NETNAME[[CL]]]][[SN]]$population[[1]][ND]
    }
  }
}

#########################
### CREATE A MATRICES ###
#########################

for (SN in 1:seasons) {
## MODEL FUNCTIONS ##
  f_update[[SN]]<- list()
  for (i in 1:num_nodes){
    node <- i
    f_temp <- matrix(0,nrow = NUMNET,ncol = NUMNET)
    for (j in 1:NUMNET) {
      for (k in 1:NUMNET) {
        if (j==k){
          # Choose from survival rates: NODE_ATRIBUTES[[NETNAME]][[season]][[2=survival]][[1]][[node=i]] 
          f_temp[j,k]<- NODE_ATTRIBUTES[[NETNAME[[j]]]][[SN]][[2]][[1]][[i]]
        }
        else{
          # Choose from transition/reproduction rates and then test if transitions are allowed (0=no, 1=yes)
          f_temp[j,k]<- NODE_ATTRIBUTES[[NETNAME[[j]]]][[SN]][[3]][[1]][[i]]*TRANS_ATTRIBUTES[[NETNAME[[j]]]][[SN]][[1]][[k]]
        }
     }
    }
    f_update[[SN]][[i]] <- f_temp
  }
  rm(i)
  p_update[[SN]] <- list()
  s_update[[SN]] <- list()
  for (i in 1:NUMNET){
    type <- i
    p_update[[SN]][[i]] <- PATH_ATTRIBUTES[[i]][[SN]][[1]]   
    s_update[[SN]][[i]] <- PATH_ATTRIBUTES[[i]][[SN]][[2]]
  }
rm(i)


## MATRICES NEEDED FOR UPDATE ##
## F Block Diagonal ##
FBLOCK <- matrix(0,nrow = NUMNET*num_nodes, ncol = NUMNET*num_nodes)
for (i in 1:num_nodes){
  E <- matrix(0,nrow = num_nodes, ncol = num_nodes)
  E[i,i] <- 1
  FBLOCK <- FBLOCK + kronecker(E, t(matrix(f_update[[SN]][[i]],nrow=NUMNET,ncol=NUMNET)))
}
rm(i,E)
## Q Block Diagonal ##
QBLOCK <- matrix(0,nrow = NUMNET*num_nodes, ncol = NUMNET*num_nodes)
for (i in 1:NUMNET){
  E <- matrix(0,nrow = NUMNET,ncol = NUMNET)
  E[i,i] <- 1
  QBLOCK <- QBLOCK + kronecker(t(matrix(unlist(s_update[[SN]][[i]]),nrow = num_nodes,ncol = num_nodes) * matrix(unlist(p_update[[SN]][[i]]),nrow = num_nodes,ncol = num_nodes)),E)
}
rm(i,E)

AMATRIX <- QBLOCK %*% FBLOCK
AMATRIX_CR[[SN]] <- AMATRIX
AMATRIX_CR[[SN+seasons]] <- AMATRIX
}

################################
### CALCULATE CR NODE VALUES ###
################################

POPnode <- matrix(0,nrow = num_nodes, ncol = seasons)
CR <- matrix(0,nrow = nrow(AMATRIX), ncol = seasons) #Cr values for each class/node/season
CRweight <- matrix(0,nrow = nrow(AMATRIX), ncol = seasons) #Cr*Nr values for each class/node/season 
ONES <- matrix(1,nrow = nrow(AMATRIX), ncol = 1)
CRt <- matrix(0,nrow = num_nodes, ncol = seasons) #Population weighted CR values for each node/season
CRs <- matrix(0,nrow = num_nodes, ncol = 1) #Population weighted CR values for each node/season
#Choose each season as a focal season
for (SN in 1:seasons){
  CRtemp <- diag(nrow(AMATRIX))
  taustart <- SN  
  tauend <- SN+seasons-1
  for (i in taustart:tauend){
    CRtemp <- CRtemp %*% t(matrix(AMATRIX_CR[[i]], nrow = nrow(AMATRIX)))
  }
  CR[,SN] <- CRtemp %*% ONES
  CRweight[,SN] <- CR[,SN] * N[,SN]
  
  for (j in 1:num_nodes){
    POPnode[j,SN] <- sum(N[((j-1)*NUMNET+1):(j*NUMNET),SN])
    if(POPnode[j,SN]>0){
      CRt[j,SN] <- sum(CRweight[((j-1)*NUMNET+1):(j*NUMNET),SN])/POPnode[j,SN] # class based population weighted average within nodes
    }
  }
}

## Seasonal Population Weighted CR values
for(i in 1:num_nodes){
  if(seasons==1){
    if(POPnode[[i]]!=0){
      CRs[[i]] <- CRt[[i]]}
    }
  else{
    if(rowSums(POPnode)[[i]]!=0){
      CRs[[i]] <- (rowSums(CRt*POPnode)[[i]])/rowSums(POPnode)[[i]]}
  }
}

## Name columns and rows
SEASONNAMES <- c()
for (i in 1:seasons){
  SEASONNAMES <- c(SEASONNAMES, paste("season", i))
}
NAMESofROW <- c()
num_class <- length(NETNAME)
for (i in 1:num_nodes){
  for (j in 1:num_class){
    NAMESofROW <- c(NAMESofROW, paste(NODENAMES[i], NETNAME[j]))
  }
}
NAMESofROW <- gsub("_","",NAMESofROW)

rownames(CR) <- NAMESofROW
colnames(CR) <- SEASONNAMES

rownames(CRt) <- NODENAMES
colnames(CRt) <- colnames(CR)

rownames(CRs) <- NODENAMES
colnames(CRs) <- c("C-metric")


## Population Proportion and Network Growth Rate
LAMBDAt <- matrix(0, 1, seasons)
WR <- matrix(0, NUMNET*num_nodes, seasons) #Contains population porportion for each class at each node.
WRt <- matrix(0,num_nodes,seasons) #Contains the population proportiona for each node, summing the classes
WRs <- matrix(0,num_nodes,1) #Contains the network wide average WR values - divide by number of seasons
for (SN in 1:seasons){
  TIME <- SN
  WR[,SN] <- N[,TIME]/sum(N[,TIME])
  TIME <- TIME + 1
}
colnames(WR) <- colnames(CR)
rownames(WR) <- rownames(CR)

for (i in 1:num_nodes){
  if(NUMNET==1){
    WRt[i,] <- WR[i,]
  }
  else{  
    if (seasons==1) {
      WRt[i,] <- sum(WR[((i-1)*NUMNET+1):(i*NUMNET),])}
    else{
      WRt[i,] <- colSums(WR[((i-1)*NUMNET+1):(i*NUMNET),])}
  }
}

WRs <- rowSums(WRt)/3

for (SN in 1:seasons){
  LAMBDAt[SN] <- t(WR[,SN])%*%CR[,SN]
}
rownames(LAMBDAt)<-c("network growth rate")
colnames(LAMBDAt)<-colnames(CR)


###################################
### CALCULATE CR PATHWAY VALUES ###
###################################

POPnode <- matrix(0,nrow = num_nodes, ncol = seasons)
CRpath <- matrix(0,nrow = num_nodes*num_nodes*NUMNET, ncol = seasons) #Cr pathway values for each class/node/season
CRpathweight <- matrix(0,nrow = num_nodes*num_nodes*NUMNET, ncol = seasons)
pathPOP <- matrix(0,nrow = num_nodes*num_nodes*NUMNET, ncol = seasons)
CRpatht <- matrix(0,nrow = num_nodes*num_nodes, ncol = seasons)
pathPOPtotal <- matrix(0,nrow = num_nodes*num_nodes, ncol = seasons)
CRpathweighttotal <- matrix(0,nrow = num_nodes*num_nodes, ncol = seasons)
CRpaths <- matrix(0,nrow = num_nodes*num_nodes, ncol = 1)

ONES <- matrix(1,nrow = nrow(AMATRIX), ncol = 1)

# Choose each season as a focal season
for (SN in 1:seasons){
  # Choose each possible path
  count <- 0
  for (r in 1:num_nodes){
    for (d in 1:num_nodes){
      # Consider each class
      for (CLS in 1:NUMNET){
        count <- count +1
        PRD <- PATH_ATTRIBUTES[[CLS]][[SN]][[1]][r,d] # CLS=class, SN=season, 1=transition prob, [r,d]= from node r to d
        # only claculate for paths that can be used during the focal season
        if(PRD!=0){
          Hc <- matrix(0,nrow=NUMNET,ncol=NUMNET)
          Hc[,CLS] <- matrix(1,nrow=NUMNET, ncol=1) # c X c matrix with ones in cloumn x=CLS and zeros otherwise
          Enrd <- matrix(0,nrow=num_nodes,ncol=num_nodes)
          Enrd[r,d] <- 1 # n X n matrix with a one in location r,d
          # Calculate annual projection matrix
          CRtemp <- diag(nrow(AMATRIX)) # Initialize with identity
          taustart <- SN + 1
          tauend <- SN+seasons-1
          for (i in taustart:tauend){
            CRtemp <- CRtemp %*% t(matrix(AMATRIX_CR[[i]], nrow = nrow(AMATRIX)))
          }
          CRpath[count,SN] <- (t(ONES)/PRD)%*%(t(matrix(AMATRIX_CR[[SN]], nrow=nrow(AMATRIX)))*(kronecker(Enrd,Hc)))%*%CRtemp%*%ONES
        }
        pathPOP[count,SN] <- PRD*N[CLS+NUMNET*(r-1),SN]
        CRpathweight[count,SN] <- pathPOP[count,SN]*CRpath[count,SN]
      }
    }
  }
}

PATHNAMES <- c()
for (i in 1:num_nodes){
  for (j in 1:num_nodes){
    for (CLS in 1:NUMNET){
        PATHNAMES <- c(PATHNAMES,paste("$C_{",i,j,",t}$",NETNAME[CLS],sep=""))
    }
  }
}
rownames(CRpath) <- PATHNAMES
colnames(CRpath) <- colnames(CR)

# Class population weighted average
for (k in 1:nrow(CRpatht)){
  for (SN in 1:seasons){
    pathPOPtotal[k,SN] <- sum(pathPOP[seq(1+(k-1)*NUMNET,k*NUMNET),SN])
    CRpathweighttotal[k,SN] <- sum(CRpathweight[seq(1+(k-1)*NUMNET,k*NUMNET),SN])
    if(pathPOPtotal[k,SN]!=0){
      CRpatht[k,SN] <- CRpathweighttotal[k,SN]/pathPOPtotal[k,SN]
    }
  }
}

PATHNAMES <- c()
for (i in 1:num_nodes){
  for (j in 1:num_nodes){
    PATHNAMES <- c(PATHNAMES,paste("$C_{",i,j,",t}$",sep=""))
  }
}
rownames(CRpatht) <- PATHNAMES
colnames(CRpatht) <- colnames(CR)

# Seasonal population weighted average
seasonPOPtotal <- matrix(rowSums(pathPOPtotal),nrow=nrow(pathPOPtotal),ncol=1)
sumCRpathweighttotal <- matrix(rowSums(CRpathweighttotal),nrow=nrow(CRpatht),ncol=1)
for (k in 1:length(seasonPOPtotal)){
  if(seasonPOPtotal[k]!=0){
    CRpaths[k] <- sumCRpathweighttotal[k]/seasonPOPtotal[k]
  }
}

rownames(CRpaths) <- rownames(CRpatht)
colnames(CRpaths) <- c("C-metric pathway")


