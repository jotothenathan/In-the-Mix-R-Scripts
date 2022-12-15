################################################################################
#                                                                              #
#     Jonathan D. Ware                                                         #
#     Thesis Project Data Management and Setup Code                            #
#     Updated: 12/04/2022                                                      #
#                                                                              #
################################################################################


# 0. Setup working environment #################################################

setwd("C:/MSRP")
library(statnet) 
library(network)
library(sna) 
library(RSiena)
library(lattice)
library(igraph)
load("C:/MSRP/MSDP Enviornment.RData")

# 1. Setup data ################################################################

# 1.1.1 Dealing with School Aqua (102) first
net1 <- sociomatrix.list.102[[1]]
net2 <- sociomatrix.list.102[[2]]
net3 <- sociomatrix.list.102[[3]]

lapply(sociomatrix.list.102, function(x) table(x, useNA='always'))  

# 1.1.2 Next, school Magma (105) 

net4 <- sociomatrix.list.105[[1]]
net5 <- sociomatrix.list.105[[2]]
net6 <- sociomatrix.list.105[[3]]

lapply(sociomatrix.list.105, function(x) table(x, useNA='always'))  

# 1.2 Siena objects treat structural zeros as 10s--need to recode them as NAs
#     for some descriptives

net1w10 <- net1
net2w10 <- net2
net3w10 <- net3
net4w10 <- net4
net5w10 <- net5
net6w10 <- net6

net1[net1==10] <- NA
net2[net2==10] <- NA
net3[net3==10] <- NA
net4[net4==10] <- NA
net5[net5==10] <- NA
net6[net6==10] <- NA

# 1.3 Need to add attribute data to affiliation network 

# First, transform into network object
ergnet_1 <- as.network(net1)
ergnet_2 <- as.network(net2)
ergnet_3 <- as.network(net3)
ergnet_4 <- as.network(net4)
ergnet_5 <- as.network(net5)
ergnet_6 <- as.network(net6)

# Add gender 
# First, School Aqua

gender <- c(aquaatr[,10])
set.vertex.attribute(ergnet_1, 'gender', gender)
set.vertex.attribute(ergnet_2, 'gender', gender)
set.vertex.attribute(ergnet_3, 'gender', gender)

#Check
ergnet_1 %v% 'gender' #looks good

# Next, School Magma
magatr <- attribute[2]
magatr <- as.data.frame(magatr)
gender2 <-magatr[,10]

#Check
ergnet_1 %v% 'gender' #nice

# Now, add Raceeth--Aggregate Race/Ethnicity
# First, School Aqua (102)

raceeth2 <- magatr[,15]
set.vertex.attribute(ergnet_4, 'raceeth', raceeth2)
set.vertex.attribute(ergnet_5, 'raceeth', raceeth2)
set.vertex.attribute(ergnet_6, 'raceeth', raceeth2)
#Check
ergnet_4 %v% 'raceeth' #looks good

# Next, School Magma (105)
raceeth2 <- c(magmaatr[,15])
set.vertex.attribute(ergnet_1, 'raceeth', raceeth2)
set.vertex.attribute(ergnet_2, 'raceeth', raceeth2)
set.vertex.attribute(ergnet_3, 'raceeth', raceeth2)
#Check
ergnet_4 %v% 'raceeth'

# 1.4 Create array 

# First, Aqua
aquaarray <- array(c(net1w10,net2w10,net3w10),dim=c(dim(net1w10),3))
dim(aquaarray);class(aquaarray) #looks good

# Next, Magma
magmaarray <- array(c(net4w10,net5w10,net6w10),dim=c(dim(net4w10),3))
dim(magmaarray);class(magmaarray)

# 1.5 Create matrix for shared-classes and shared-activities
# Here, 1, 2, 3, refer to Aqua and 4,5,6, refer to Magma--focusing on Aqua

class1 <- share.classes.102[[1]]
class2 <- share.classes.102[[2]]
class3 <- share.classes.102[[3]]
class4 <- share.classes.105[[1]]
class5 <- share.classes.105[[2]]
class6 <- share.classes.105[[3]]

class1 <- as.matrix(class1)
class2 <- as.matrix(class2)
class3 <- as.matrix(class3)
class4 <- as.matrix(class4)
class5 <- as.matrix(class5)
class6 <- as.matrix(class6)

activity1 <- share.activities.102[[1]]
activity2 <- share.activities.102[[2]]
activity3 <- share.activities.102[[3]]
activity4 <- share.activities.105[[1]]
activity5 <- share.activities.105[[2]]
activity6 <- share.activities.105[[3]]

activity1 <- as.matrix(activity1)
activity2 <- as.matrix(activity2)
activity3 <- as.matrix(activity3)
activity4 <- as.matrix(activity4)
activity5 <- as.matrix(activity5)
activity6 <- as.matrix(activity6)

# Create an array for shared classes

aquaclassarray <- array(c(class2,class3),dim=c(352,352,2))
dim(aquaclassarray);class(aquaclassarray)

magmaclassarray <- array(c(class5,class6),dim=c(390,390,2))
dim(magmaclassarray);class(magmaclassarray)

# create an array for shared ECAs

aquaactivityarray <- array(c(activity2,activity3),dim=c(352,352,2))
dim(aquaactivityarray);class(aquaactivityarray)

magmaactivityarray <- array(c(activity5,activity6),dim=c(390,390,2))
dim(magmaactivityarray);class(magmaactivityarray)

# create an object for GPA. Siena only needs first two waves of data

gpa_w1<- as.matrix(c(aquaatr[,35]))
gpa_w2<- as.matrix(c(aquaatr[,36]))
aquagpa <- as.matrix(cbind(gpa_w1,gpa_w2))

maggpa_w1 <- as.matrix(c(magatr[,34]))
maggpa_w2 <- as.matrix(c(magatr[,35]))
magmagpa <- as.matrix(cbind(maggpa_w1,maggpa_w2))

# create objects for proportion same-race in class as control for availability;
# this terms also needs to be lagged to capture effect (similar with class + 
# and activity data)

propsamerace_w2 <- as.matrix(c(aquaatr[,21]))
propsamerace_w3 <- as.matrix(c(aquaatr[,22]))
bindproprace <- as.matrix(cbind(propsamerace_w2,propsamerace_w3))

magsamerace_w2 <- as.matrix(c(aquaatr[,21]))
magsamerace_w3 <- as.matrix(c(aquaatr[,22]))
magproprace <- as.matrix(cbind(magsamerace_w2,magsamerace_w3))


# 1.6 Specify SIENA objects

#First, School Aqua

friendship <- sienaDependent(aquaarray)
class <- varDyadCovar(aquaclassarray)
activity <- varDyadCovar(aquaactivityarray)
gen <- coCovar(c(aquaatr[,10]))
aggre <- coCovar(c(aquaatr[,15]))
ses <- coCovar(c(aquaatr[,18]))
gpa <- varCovar(bindgpa)
proprace <- varCovar(bindproprace)
aquadata <- sienaDataCreate(friendship,gen,aggre,class,activity,gpa,ses,proprace)
aquadata #Looks good

# Write descriptive summary file; way of checking coding
print01Report(aquadata, modelname='aqua.model.v1')

#______________________________________________________________________________#
#______________________________________________________________________________#

# Follow similar process for school magma (105)

net4 <- sociomatrix.list.105[[1]]
net5 <- sociomatrix.list.105[[2]]
net6 <- sociomatrix.list.105[[3]]

lapply(sociomatrix.list.105, function(x) table(x, useNA='always'))  

# Siena objects treat structural zeros as 10s--need to recode them as NAs
# for some desprictives

net4w10 <- net4
net5w10 <- net5
net6w10 <- net6

net4[net4==10] <- NA
net5[net5==10] <- NA
net6[net6==10] <- NA

# Need to add attribute data to affiliation network 
# First, transform into network object

ergnet_4 <- as.network(net4)
ergnet_5 <- as.network(net5)
ergnet_6 <- as.network(net6)

# Add gender 
magatr <- attribute[2]
magatr <- as.data.frame(magatr)
gender2 <-magatr[,10]
set.vertex.attribute(ergnet_4, 'gender', gender2)
set.vertex.attribute(ergnet_5, 'gender', gender2)
set.vertex.attribute(ergnet_6, 'gender', gender2)

#Check
ergnet_4 %v% 'gender' #nice

# Now, add Raceeth--Aggregate Race/Ethnicity
raceeth2 <- c(magatr[,15])
set.vertex.attribute(ergnet_4, 'raceeth', raceeth2)
set.vertex.attribute(ergnet_5, 'raceeth', raceeth2)
set.vertex.attribute(ergnet_6, 'raceeth', raceeth2)

#Check
ergnet_4 %v% 'raceeth'

# Create array 
magmaarray <- array(c(net4w10,net5w10,net6w10),dim=c(dim(net4w10),3))
dim(magmaarray);class(magmaarray)

# Create matrix for shared-classes and shared-activities
# Here, 1, 2, 3, refer to Aqua and 4,5,6, refer to Magma--focusing on Magma
  
class4 <- share.classes.105[[1]]
class5 <- share.classes.105[[2]]
class6 <- share.classes.105[[3]]

class4 <- as.matrix(class4)
class5 <- as.matrix(class5)
class6 <- as.matrix(class6)

activity4 <- share.activities.105[[1]]
activity5 <- share.activities.105[[2]]
activity6 <- share.activities.105[[3]]

activity4 <- as.matrix(activity4)
activity5 <- as.matrix(activity5)
activity6 <- as.matrix(activity6)
    
# Create Siena data objects
friendshipmag <- sienaDependent(magmaarray)
classmag <- varDyadCovar(magmaclassarray)
activitymag <- varDyadCovar(magmaactivityarray)
genmag <- coCovar(c(gender2))
aggremag <- coCovar(c(raceeth2))
magses <- coCovar(c(magatr[,18]))
magprace <- varCovar(magproprace)
maggpa <- varCovar(magmagpa)
magdata <- sienaDataCreate(friendshipmag,activitymag,genmag,aggremag,classmag,magses,maggpa)
magdata  

# Write descriptive summary file; way of checking coding
print01Report(magdata, modelname='magma.model.v1')
