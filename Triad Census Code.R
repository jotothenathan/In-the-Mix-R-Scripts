################################################################################
#                                                                              #
#     Jonathan D. Ware                                                         #
#     Thesis Project Colored Triad Census Code                                 #
#     Updated: 11/13/2022                                                      #
#                                                                              #
################################################################################


# 0. Setup working environment #################################################

setwd("C:/MSRP")
library(statnet)    
library(sna)
library(igraph) 
load("C:/MSRP/Enviornment 11.13.22.RData")

# 1.  Data Setup ###############################################################

# Recode raceeth -- keeping this for reference

# School Aqua
newrace1 <- raceeth
newrace1[newrace1==5] <- 2 #Panethnic Asian
newrace1[newrace1==6] <- 2
newrace1[newrace1==7] <- 8 #Panethnic Other
table(newrace1)

# School Magma

newrace2 <- raceeth2
newrace2[newrace2==5] <- 2 #Panethnic Asian
newrace2[newrace2==6] <- 2
newrace2[newrace2==7] <- 8 #Panethnic Other
table(newrace2)

# (NOT USED)--Recode numeric variables into nominal--(NOT USED)

newrace[newrace==1] <- 'Black'
newrace[newrace==2] <- 'Asian'
newrace[newrace==3] <- 'White'
newrace[newrace==4] <- 'Latinx'
newrace[newrace==8] <- 'Other'
newrace[newrace==9] <- 'Mixed'
table(newrace)

# Need to coerce into an integer for CTC to run

newrace1 <- as.integer(newrace1);newrace2 <- as.integer(newrace2)

#removing na from network

net1[is.na(net1)] <- 0
net2[is.na(net2)] <- 0
net3[is.na(net3)] <- 0
net4[is.na(net4)] <- 0
net5[is.na(net5)] <- 0
net6[is.na(net6)] <- 0


# 2.  Colored Triad Census #####################################################

setwd("C:/MSRP/ColoredTriadCensus-master")
source("colored.triad.census.R")

ColTriCen <- colored.triad.census(net1,col = newrace, directed = F)

#Heatmap of iso classes against coloring

iso.class<-sapply(strsplit(names(ColTriCen),"-"),function(x) x[[1]])
col.class<-sapply(strsplit(names(ColTriCen),"-"),function(x) x[[2]])
outmat<-matrix(0,nrow=length(unique(iso.class)),ncol=length(unique(col.class)),
               dimnames = list(unique(iso.class),unique(col.class)))

for(i in 1:length(col.2)){
  outmat[iso.class[i],col.class[i]]<-col.2[i]
}

heatmap(outmat,Rowv=NA,Colv=NA)

# 3. Process to pull out pertinent data points ################################


BrokTab <- ColTriCen[names(ColTriCen) %in% c(
                                    "T201-612","T201-613","T201-614","T201-615",
                                    "T201-623","T201-624","T201-625","T201-634", 
                                    "T201-635","T201-645"
                                            )]

TriTab <- ColTriCen[names(ColTriCen) %in% c(
                                    "T300-126","T300-136","T300-146","T300-156",
                                    "T300-236","T300-246","T300-256","T300-346", 
                                    "T300-356","T300-456")]

# Run permutations on network ties for comparisons with the observed network

# Process for one run
permnet1 <- rmperm(net1)


PermColTriCermn <- colored.triad.census(permnet1,col = newrace, directed = F)

PermBrokTab <- PermColTriCen[names(PermColTriCen) %in% c(
                                    "T201-612","T201-613","T201-614","T201-615",
                                    "T201-623","T201-624","T201-625","T201-634", 
                                    "T201-635","T201-645"
)]
 
PermTriTab <- PermColTriCen[names(PermColTriCen) %in% c(
                                    "T300-126","T300-136","T300-146","T300-156",
                                    "T300-236","T300-246","T300-256","T300-346", 
                                    "T300-356","T300-456")]

# Loop to run 1000 permutations of the network

nperm <- 1000
mat1 <- matrix(ncol = 2, nrow = nperm) # first column for 201 counts, second for 
mat2 <- matrix(ncol = 2, nrow = nperm) # 300 counts; rows=number of permutations
mat3 <- matrix(ncol = 2, nrow = nperm)
mat4 <- matrix(ncol = 2, nrow = nperm)
mat5 <- matrix(ncol = 2, nrow = nperm)
mat6 <- matrix(ncol = 2, nrow = nperm)

# Run permutations

# School Aqua Wave 1

for(i in 1:nperm) {
  
  permnet <- rmperm(net1)
  
  PermColTriCen <- colored.triad.census(permnet,col = newrace, directed = F)
  
  PermBrokTab <- PermColTriCen[names(PermColTriCen) %in% c(
    
    "T201-612","T201-613","T201-614","T201-615",
    
    "T201-623","T201-624","T201-625","T201-634",
    
    "T201-635","T201-645"
    
  )]
  
  PermTriTab <- PermColTriCen[names(PermColTriCen) %in% c(
    
    "T300-126","T300-136","T300-146","T300-156",
    
    "T300-236","T300-246","T300-256","T300-346",
    
    "T300-356","T300-456")]
  
  mat1[i,1] <- sum(PermBrokTab)  # edited to only store the frequency across all
                                 # brokerage triads, and put in column 1

  mat1[i,2] <- sum(PermTriTab)   # edited to only store the frequency across all
                                 # closed triads and put in column 2
}

#save
save.image(file = "Environment 11.13.21")

# School Aqua Wave 2

for(i in 1:nperm) {
  
  permnet <- rmperm(net2)
  
  PermColTriCen <- colored.triad.census(permnet,col = newrace, directed = F)
  
  PermBrokTab <- PermColTriCen[names(PermColTriCen) %in% c(
    
    "T201-612","T201-613","T201-614","T201-615",
    
    "T201-623","T201-624","T201-625","T201-634",
    
    "T201-635","T201-645"
    
  )]
  
  PermTriTab <- PermColTriCen[names(PermColTriCen) %in% c(
    
    "T300-126","T300-136","T300-146","T300-156",
    
    "T300-236","T300-246","T300-256","T300-346",
    
    "T300-356","T300-456")]
  
  mat2[i,1] <- sum(PermBrokTab)  
  
  mat2[i,2] <- sum(PermTriTab)  
  
}

#save
save.image(file = "MSDP Environment 11.13.21")

# School Aqua Wave 3

for(i in 1:nperm) {
  
  permnet <- rmperm(net3)
  
  PermColTriCen <- colored.triad.census(permnet,col = newrace, directed = F)
  
  PermBrokTab <- PermColTriCen[names(PermColTriCen) %in% c(
    
    "T201-612","T201-613","T201-614","T201-615",
    
    "T201-623","T201-624","T201-625","T201-634",
    
    "T201-635","T201-645"
    
  )]
  
  PermTriTab <- PermColTriCen[names(PermColTriCen) %in% c(
    
    "T300-126","T300-136","T300-146","T300-156",
    
    "T300-236","T300-246","T300-256","T300-346",
    
    "T300-356","T300-456")]
  
  mat3[i,1] <- sum(PermBrokTab)  
  
  mat3[i,2] <- sum(PermTriTab)  
  
}

#save
save.image(file = "MSDP Environment 11.13.21")

# School Magma Wave 1

for(i in 1:nperm) {
  
  permnet <- rmperm(net4)
  
  PermColTriCen <- colored.triad.census(permnet,col = newrace2, directed = F)
  
  PermBrokTab <- PermColTriCen[names(PermColTriCen) %in% c(
    
    "T201-612","T201-613","T201-614","T201-615",
    
    "T201-623","T201-624","T201-625","T201-634",
    
    "T201-635","T201-645"
    
  )]
  
  PermTriTab <- PermColTriCen[names(PermColTriCen) %in% c(
    
    "T300-126","T300-136","T300-146","T300-156",
    
    "T300-236","T300-246","T300-256","T300-346",
    
    "T300-356","T300-456")]
  
  mat4[i,1] <- sum(PermBrokTab)  
  
  mat4[i,2] <- sum(PermTriTab)  
  
}

#save
save.image(file = "MSDP Environment 11.13.22")

# School Magma Wave 2

for(i in 1:nperm) {
  
  permnet <- rmperm(net5)
  
  PermColTriCen <- colored.triad.census(permnet,col = newrace2, directed = F)
  
  PermBrokTab <- PermColTriCen[names(PermColTriCen) %in% c(
    
    "T201-612","T201-613","T201-614","T201-615",
    
    "T201-623","T201-624","T201-625","T201-634",

    "T201-635","T201-645"
    
  )]
  
  PermTriTab <- PermColTriCen[names(PermColTriCen) %in% c(
    
    "T300-126","T300-136","T300-146","T300-156",
    
    "T300-236","T300-246","T300-256","T300-346",
    
    "T300-356","T300-456")]
  
  mat5[i,1] <- sum(PermBrokTab)  
  
  mat5[i,2] <- sum(PermTriTab)  
  
}

#save
save.image(file = "Environment 11.13.22")

# School Magma Wave 3

for(i in 1:nperm) {
  
  permnet <- rmperm(net6)
  
  PermColTriCen <- colored.triad.census(permnet,col = newrace2, directed = F)
  
  PermBrokTab <- PermColTriCen[names(PermColTriCen) %in% c(
    
    "T201-612","T201-613","T201-614","T201-615",
    
    "T201-623","T201-624","T201-625","T201-634",
    
    "T201-635","T201-645"
    
  )]
  
  PermTriTab <- PermColTriCen[names(PermColTriCen) %in% c(
    
    "T300-126","T300-136","T300-146","T300-156",
    
    "T300-236","T300-246","T300-256","T300-346",
    
    "T300-356","T300-456")]
  
  mat6[i,1] <- sum(PermBrokTab)  
  
  mat6[i,2] <- sum(PermTriTab)  
  
}

#save
save.image(file = "Environment 11.13.22")

# 4. Plotting Distributions and adding observed data

#Wave 1
BrokObs <- sum(BrokTab)
TriObs <- sum(TriTab)

hist(mat1[,1],breaks=31)
abline( v=BrokObs, col="red", lwd=3, lty=1)

hist(mat1[,2],breaks=21)
abline( v=TriObs, col="red", lwd=3, lty=1)

#Wave 2
hist(mat2[,1],breaks=26)
abline( v=BrokObs, col="red", lwd=3, lty=1)

hist(mat2[,2],breaks=11)
abline( v=TriObs, col="red", lwd=3, lty=1)
