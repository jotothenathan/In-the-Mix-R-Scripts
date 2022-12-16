################################################################################
#                                                                              #
#     Jonathan D. Ware                                                         #
#     Thesis Project Model Estimation and Analysis                             #
#     Updated: 12/04/2022                                                      #
#                                                                              #
################################################################################


# 0. Setup working environment #################################################
setwd("C:/MSRP")
library(statnet)    
library(sna) 
library(RSiena)
library(lattice)
library(igraph)
load("C:/MSRP/MSDP Enviornment.RData")

# 1. Inspect Data ##############################################################

# Basic structure of data

dim(net1);dim(net2);dim(net3);dim(net4);dim(net5);dim(net6)
head(net1);head(net2);head(net3)head(net4);head(net5);head(net6)

class(aquafriendshipdata)

apply(net1,FUN=table,MARGIN=2,useNA='always')
cor(data.frame(net1))

# Provides stability of ties across network samples

# Aqua
(tab1to2 <- table(net1,net2) )
(tab2to3 <- table(net2,net3) )

#Magma
(mag1to2 <- table(net4,net5) )
(mag2to3 <- table(net5,net6) )


#Measure stability using the Jaccard index
#Jaccard should be minimum of .2 for RSiena
mag1to2[2,2] / (sum(mag1to2)-mag1to2[1,1])   # jaccard=0.235

mag2to3[2,2] / (sum(mag2to3)-mag2to3[1,1])   # jaccard=0.237

# 2. Network-level Descriptives ################################################

# 2.1 Density ##################################################################

#Aqua
gden(net1);gden(net2);gden(net3)

#Magma
gden(net4);gden(net5);gden(net6)

# 2.2 Dyad and Triad Census ####################################################

#Aqua
dyad.census(net1);dyad.census(net2);dyad.census(net3) 
triad.census(net1,mode="graph");triad.census(net2,mode="graph");triad.census(net3,mode="graph")

G <- as.undirected(graph.adjacency(net1, weighted = T))

#Magma
dyad.census(net4);dyad.census(net5);dyad.census(net6) 
triad.census(net4,mode="graph");triad.census(net5,mode="graph");triad.census(net6,mode="graph")

# 2.3 degree distributions #####################################################

opar <- par()
par(mfrow=c(1,3))

#Aqua
table(indegree_1);plot(table(indegree_1), main = "Aqua Degree Distribution W1",xlab="Degree",ylab="Frequency")     
abline( v=mean(indegree_1), col="red", lwd=3, lty=1)
table(indegree_2);plot(table(indegree_2), main = "Aqua Degree Distribution W2",xlab="Degree",ylab="Frequency")     
abline( v=mean(indegree_2), col="red", lwd=3, lty=1)
table(indegree_3);plot(table(indegree_3), main = "Aqua Degree Distribution W3",xlab="Degree",ylab="Frequency")     
abline( v=mean(indegree_3), col="red", lwd=3, lty=1)

#Magma

table(indegree_4);plot(table(indegree_4), main = "Magma Degree Distribution W1",xlab="Degree",ylab="Frequency")     
abline( v=mean(indegree_4), col="red", lwd=3, lty=1)
table(indegree_5);plot(table(indegree_5), main = "Magma Degree Distribution W2",xlab="Degree",ylab="Frequency")     
abline( v=mean(indegree_5), col="red", lwd=3, lty=1)
table(indegree_6);plot(table(indegree_6), main = "Magma Degree Distribution W3",xlab="Degree",ylab="Frequency")     
abline( v=mean(indegree_6), col="red", lwd=3, lty=1)

# Mean outdegree by race/ethnicity

# Aqua
aggregate(degree(ergnet_1, cmode='outdegree'),list(ergnet_1 %v% "raceeth"),mean)

aggregate(degree(ergnet_2, cmode='outdegree'),list(ergnet_1 %v% "raceeth"),mean)

aggregate(degree(ergnet_3, cmode='outdegree'),list(ergnet_1 %v% "raceeth"),mean)

# Magma
aggregate(degree(ergnet_4, cmode='outdegree'),list(ergnet_4 %v% "raceeth"),mean)

aggregate(degree(ergnet_5, cmode='outdegree'),list(ergnet_4 %v% "raceeth"),mean)

aggregate(degree(ergnet_6, cmode='outdegree'),list(ergnet_4 %v% "raceeth"),mean)

# 2.4 Examine homophily ########################################################

# Get mixing matrix

#Aqua
# Wave 1
mixtabg1 <- mixingmatrix(ergnet_1, "gender") 
mixtabr1 <- mixingmatrix(ergnet_1, "raceeth")
# Wave 2
mixtabg2 <- mixingmatrix(ergnet_2, "gender") 
mixtabr2 <- mixingmatrix(ergnet_2, "raceeth")
# Wave 3
mixtabg3 <- mixingmatrix(ergnet_3, "gender") 
mixtabr3 <- mixingmatrix(ergnet_3, "raceeth")

mixtabg1;mixtabg2;mixtabg3
mixtabr1;mixtabr2;mixtabr3

#Magma

# Wave 1
mixtabg4 <- mixingmatrix(ergnet_4, "gender") 
mixtabr4 <- mixingmatrix(ergnet_4, "raceeth")
# Wave 2
mixtabg5 <- mixingmatrix(ergnet_5, "gender") 
mixtabr5 <- mixingmatrix(ergnet_5, "raceeth")
# Wave 3
mixtabg6 <- mixingmatrix(ergnet_6, "gender") 
mixtabr6 <- mixingmatrix(ergnet_6, "raceeth")

mixtabg4;mixtabg5;mixtabg6
mixtabr4;mixtabr5;mixtabr6

# Test correlation between group and degree

# School Aqua; covar gender

cor.test(degree(ergnet_1, cmode='indegree'), ergnet_1 %v% "gender") #significant
cor.test(degree(ergnet_1, cmode='outdegree'), ergnet_1 %v% "gender") #NS 

cor.test(degree(ergnet_2, cmode='indegree'), ergnet_1 %v% "gender") #significant
cor.test(degree(ergnet_2, cmode='outdegree'), ergnet_1 %v% "gender")#significant 

cor.test(degree(ergnet_3, cmode='indegree'), ergnet_1 %v% "gender") #significant
cor.test(degree(ergnet_3, cmode='outdegree'), ergnet_1 %v% "gender")#significant

# School Aqua; covar raceeth
cor.test(degree(ergnet_1, cmode='indegree'), ergnet_1 %v% "raceeth") #NS
cor.test(degree(ergnet_1, cmode='outdegree'), ergnet_1 %v% "raceeth") #Really NS

cor.test(degree(ergnet_2, cmode='indegree'), ergnet_1 %v% "raceeth") #NS
cor.test(degree(ergnet_2, cmode='outdegree'), ergnet_1 %v% "raceeth") #Really NS

cor.test(degree(ergnet_3, cmode='indegree'), ergnet_1 %v% "raceeth") #Sig at .1
cor.test(degree(ergnet_3, cmode='outdegree'), ergnet_1 %v% "raceeth") #NS

# School Magma; covar gender
cor.test(degree(ergnet_4, cmode='indegree'), ergnet_4 %v% "gender") #significant
cor.test(degree(ergnet_4, cmode='outdegree'), ergnet_4 %v% "gender") #NS 

cor.test(degree(ergnet_5, cmode='indegree'), ergnet_4 %v% "gender") #significant
cor.test(degree(ergnet_5, cmode='outdegree'), ergnet_4 %v% "gender")#significant 

cor.test(degree(ergnet_6, cmode='indegree'), ergnet_4 %v% "gender") #significant
cor.test(degree(ergnet_6, cmode='outdegree'), ergnet_4 %v% "gender")#significant

# School Magma; covar raceeth
cor.test(degree(ergnet_4, cmode='indegree'), ergnet_4 %v% "raceeth") #NS
cor.test(degree(ergnet_4, cmode='outdegree'), ergnet_4 %v% "raceeth") #Really NS

cor.test(degree(ergnet_5, cmode='indegree'), ergnet_4 %v% "raceeth") #NS
cor.test(degree(ergnet_5, cmode='outdegree'), ergnet_4 %v% "raceeth") #Really NS

cor.test(degree(ergnet_6, cmode='indegree'), ergnet_4 %v% "raceeth") #Sig at .1
cor.test(degree(ergnet_6, cmode='outdegree'), ergnet_4 %v% "raceeth") #NS

# Can use ERGM to test homophily on Race/Eth and Gender

# First, let's look at gender across waves
ergmod1 <- ergm(ergnet_1 ~ edges + nodematch("gender"))
summary(ergmod1)
exp(ergmod1$coef[2]) #Probability of tie 6.75 times more likely when same gender
ergmod2 <- ergm(ergnet_2 ~ edges + nodematch("gender"))
summary(ergmod2)
exp(ergmod2$coef[2]) # Probability 7.71 times more likely
ergmod3 <- ergm(ergnet_3 ~ edges + nodematch("gender"))
summary(ergmod3)
exp(ergmod3$coef[2]) # Probability 5.44 times more likely

# Next, let's look at Race/Ethnicity

ergmod4 <- ergm(ergnet_1 ~ edges + nodematch("raceeth"))
summary(ergmod4)
exp(ergmod4$coef[2]) # Probability of tie 2.15 times more likely if same 
                     # race/ethnicity
ergmod5 <- ergm(ergnet_2 ~ edges + nodematch("raceeth"))
summary(ergmod5)
exp(ergmod5$coef[2]) # Probability 2.41 times more likely
ergmod6 <- ergm(ergnet_3 ~ edges + nodematch("raceeth"))
summary(ergmod6)
exp(ergmod6$coef[2]) # Probability 3.26 times more likely

# 2.5 geodesic distance ########################################################
dist <- geodist(net1, inf.replace = Inf, count.paths = TRUE)
# calculate the geodesic distance (shortest path length) matrix
dist$gd
dist$counts
table(dist$gd)
table(dist$counts)

# 3. Setting up Siena Model ####################################################

# 3.1 Create model specification object
aquaeff <- getEffects(aquadata)
aquaeff # Have rateT1. rateT2, density, and reciprocity

# Check possible effects that can be included
effectsDocumentation(aquaeff)

# 3.2 Add structural effects
aquaeff <- includeEffects(aquaeff, transTrip)

# Test whether interaction between reciprocity/transitivity should be included
aquaeff <- setEffect(aquaeff, transRecTrip,
                   include=T, fix=F, test=F) 

# 3.3 Include homophily effects for the constant covariates gender, ses and raceeth
aquaeff <- includeEffects(aquaeff, sameX, interaction1 = "gen" )
aquaeff <- includeEffects(aquaeff, sameX, interaction1 = "aggre" )
aquaeff <- includeEffects(aquaeff, simX, interaction1 = "ses" )

# 3.4 Interested in adding popularity effects; test scores used to assess inclusion

aquaeff <- includeEffects(aquaeff, inPopSqrt, name = "friendship")
aquaeff <- includeEffects(aquaeff, outPop, name = "friendship", include = F)

# Test for inclusion of inAct and outAct; also test inActSrqt and outActSqrt

aquaeff <- setEffect(aquaeff, inAct,
                     include=T, fix=T, test=T) 
aquaeff <- setEffect(aquaeff, inActSqrt,
                     include=T, fix=T, test=T) 
aquaeff <- setEffect(aquaeff, outAct,
                     include=F, fix=F, test=F) 
aquaeff <- setEffect(aquaeff, outActSqrt,
                     include=T, fix=F, test=F) 

# indegree activity (product of incoming and outgoing ties) were NS in score test
# Remove unneeded terms and specify test to F 
aquaeff <- setEffect(aquaeff, inAct,
                     include=F, test=F) 
aquaeff <- setEffect(aquaeff, inActSqrt,
                     include=F, fix=F, test=F) 
aquaeff <- setEffect(aquaeff, outAct,
                     include=F, fix=F, test=F) 
aquaeff <- setEffect(aquaeff, outActSqrt,
                     include=T, fix=F, test=F) 

# 3.5 To help account for propinquity, control for shared classes and activities

aquaeff <- includeEffects(aquaeff, X,
                            interaction1="class", name="friendship", include = T
                          , fixed=F,test = F)
aquaeff <- includeEffects(aquaeff, X,
                          interaction1="activity", name="friendship", include =T
                          ,fixed=F,test = F)

# 3.6 Include terms for and gpa

aquaeff <- includeEffects(aquaeff, simX, interaction1 = "gpa", include=T 
                          ,fix=F,test=F)
aquaeff <- includeEffects(aquaeff, simX, interaction1 = "ses", include=T 
                          ,fix=F,test=F)

# 3.7 Include effect for proportion of same-race classmates. In this context, 
# want to lag this term to capture impact of this effect

aquaeff <- includeEffects(aquaeff, simX, interaction1 = "proprace", include =T,
                          fix = F, test = F)
aquaeff <- includeEffects(aquaeff, cycle3, include=T)

# 3.8 Attempted to include terms to account issues in GoF for triad census; transTrip2 
# prevented model convergence so was dropped

aquaeff <- includeEffects(aquaeff, cycle3, include=F)

aquaeff <- includeEffects(aquaeff,transTrip2, include = F, fix=F, test = F)

aquaeff <- includeEffects(aquaeff, nbrDist2, include = T)

# 3.9 Include interaction effect for transTrip and sameX to test H2

effectsDocumentation(aquaeff.v4)

aquaeff.v4 <- includeEffects(aquaeff.v4, sameXTransTrip, include=T, 
                             interaction1 = c("aggre"))

# 3.10 Include between effect to assess brokerage in the network

aquaeff.v5 <- includeEffects(aquaeff.v4, between, include=F)

#Check effects
aquaeff

aquaeff.v4 <- aquaeff

# 3.11 Define algorithm settings

myalgor <- sienaAlgorithmCreate(projname = "aqua.model.v5", n3 = 5000)

# 3.12 Preen model of ns effects (ses similarity, proprace similarity, 3-cycle,
#  outdegree popularity)

aquaeff.v4 <- includeEffects(aquaeff.v4, simX, interaction1 = "ses", include =F,
                          fix = F, test = F)
aquaeff.v4 <- includeEffects(aquaeff.v4, simX, interaction1 = "proprace", 
                             include =F, fix = F, test = F)
aquaeff.v4 <- includeEffects(aquaeff.v4, cycle3, include =F,
                             fix = F, test = F)
aquaeff.v4 <- includeEffects(aquaeff.v4, outPop, include =F,
                             fix = F, test = F)

# 3.13 Run same model specification with Magma Data

magmaeff <- getEffects(magdata)
magmaeff

effectsDocumentation(magmaeff)

magmaeff <- includeEffects(magmaeff, transTrip)
magmaeff <- setEffect(magmaeff, transRecTrip,
                     include=T, fix=F, test=F) 
magmaeff <- includeEffects(magmaeff, sameX, interaction1 = "genmag" )
magmaeff <- includeEffects(magmaeff, sameX, interaction1 = "aggremag" )
magmaeff <- includeEffects(magmaeff, simX, interaction1 = "magses" )
magmaeff <- includeEffects(magmaeff, inPopSqrt, name = "friendshipmag")
magmaeff <- setEffect(magmaeff, outActSqrt,
                     include=T, fix=F, test=F) 
magmaeff <- includeEffects(magmaeff, X,
                          interaction1="classmag", name="friendshipmag",
                          include = T, fixed=F,test = F)
magmaeff <- includeEffects(magmaeff, X,
                          interaction1="activitymag", name="friendshipmag",
                          include =T,fixed=F,test = F)
magmaeff <- includeEffects(magmaeff, simX, interaction1 = "maggpa", include=T 
                          ,fix=F,test=F)
magmaeff <- includeEffects(magmaeff, simX, interaction1 = "magses", include=T 
                          ,fix=F,test=F)
magmaeff <- includeEffects(magmaeff, nbrDist2, include = T)

magmaeff <- includeEffects(magmaeff, sameXTransTrip, include=T, 
                             interaction1 = c("aggremag"))

#Check effects
magmaeff

# 3.11 Define algorithm settings

AquaAlgor <- sienaAlgorithmCreate(projname = "aqua.model.v1", n3 = 5000)

MagmaAlgor <- sienaAlgorithmCreate(projname = "magma.model.v1", n3 = 5000)

# 3.12 Preen model of ns effects (ses similarity, proprace similarity, 3-cycle,
#  outdegree popularity)

aquaeff.v1 <- includeEffects(aquaeff.v1, simX, interaction1 = "ses", include =F,
                             fix = F, test = F)
aquaeff.v1 <- includeEffects(aquaeff.v1, simX, interaction1 = "proprace", include =F,
                             fix = F, test = F)
aquaeff.v1 <- includeEffects(aquaeff.v1, cycle3, include =F,
                             fix = F, test = F)
aquaeff.v1 <- includeEffects(aquaeff.v1, outPop, include =F,
                             fix = F, test = F)

# 4. Estimate models ###########################################################
(aquaresults <- siena07(AquaAlgor, data = aquadata, effects = aquaeff.v1, returnDeps=T ))
outTable(aquaresults)
summary(aquaresults)

(magmaresults <- siena07(MagmaAlgor, data = magdata, effects = magmaeff.v1, returnDeps=T  ))
outTable(magmaresults)
summary(magmaresults)

# Convergence t-ratios are below |0.4| so rerun with previous model estimates
(aquaresults <- siena07(AquaAlgor, data = aquadata, effects = aquaeff.v1,
                    prevAns = aquaresults, returnDeps=T))
outTable(aquaresults)
summary(aquaresults) # t-ratios looks much better

# Need to specify simulated networks for GoF tests. Use returndeps=T
(aquaresults <- siena07(AquaAlgor, data = aquadata, effects = aquaeff.v1,
               prevAns = aquaresults, returnDeps=T))

# 5. Assessing GoF #############################################################

# 5.1 First, let's assess indegree distribution
( gof.id <- sienaGOF(aquaresults, verbose=TRUE, varName="friendship", 
                     IndegreeDistribution, join=T, cumulative=F) )
plot(gof.id, main="School Aqua Indegree", xlab="Degree")

# 5.2 Next, let's turn to outdegree
( gof.od <- sienaGOF(aquaresults, verbose=TRUE, varName="friendship", 
                     OutdegreeDistribution, join=T, cumulative=F) )
plot(gof.od,main="School Aqua Outdegree", xlab="Degree")

( gof.id <- sienaGOF(myRes, verbose=TRUE, varName="friendship", IndegreeDistribution,
                     join=T, cumulative=F) )

plot(gof.id)  # looks much better

# outdegree
( gof.od <- sienaGOF(aquaresults, verbose=TRUE, varName="friendship", OutdegreeDistribution,
                     join=T, cumulative=F) )
plot(gof.od)  # good

# More GOF functions (OPTIONAL)

# geodesic distances
( gof.gd <- sienaGOF(aquaresults, verbose=TRUE, varName="friendship", GeodesicDistribution,
                     join=T, cumulative=F) )
plot(gof.gd)

# triad census
(gof.tc <- sienaGOF(aquaresults, verbose=TRUE, varName="friendship", TriadCensus, join=T) )
plot(gof.tc, scale=TRUE, center=TRUE, main="School Aqua Triad Census", xlab="Triads")

# 6. Checking for time heterogeneity ###########################################

# test whether estimates differ from wave 1 to 2 vs. wave 2 to 3
tt <- sienaTimeTest(aquaresults)
summary(tt)
plot(tt)

# 7. Adding interaction effect to test H3: X-Group tie more likely with mutual #

effectsDocumentation(aquaeff.v1)

aquaeff.v2 <- includeEffects(aquaeff.v1,sameXTransTrip,interaction1 = "aggre", include=F)

# also want to add an effect for dense triads in the network to see if underlying
# process of transitivity is present regardless of nodal attribute

aquaeff.v2 <- includeEffects(aquaeff.v1, denseTriads)
