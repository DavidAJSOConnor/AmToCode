########################################
# Habitat-centric model of mitochondrial introgression between two species of animal
#	Created: 9 August 2014
#	Created by: David O'Connor
#	Contact: david.oconnor@mail.mcgill.ca
#	Version 1,12
# Version date: 27 April 2018
# This null model looks at two species, that start out in thir respecitve ranges and then expand their range
# There is an effect of habitat quality on mortality
# This is the trsecondary null model
# There is migration in this model
# One species is choosy in its mate choice
# Density has an effect on migration probability
# This is simply testing for two ranges meeting with no difference between either species
######################################

#library(plot3D)

# Setting base birth, death, carrying capacity, starting number of individuals and time of simulation

Off<- 5 # of offspring
d <- 0.25 # Probability of death
m <- 0.10 #probability of migration
Cap <-50 # Carrying capacity
Dif<- 0.5 # This is the genetic difference between two potential mates
Choose<- 0.3 # this determines the base choosiness of individuals, the lower this number the more choosy
DifCalc<- 0.66 # This helps to calculate the acceptable difference an individual can tolerate to have with its mate
SexCap<-Cap*5 # WHAT ARE YOU YOU STUPID SEXCAP CODE? OHHHH You set up the superficial limt on the size of the array of toads

N0 <-10 # Starting number of individuals either male and female total number of individuals will be 2x this 
t <- 500 # Time of simulation
l <- 7 # number of locations must use an odd number
Rand<-1 #This is just a holder for a random number
Males<- c(1:l)
Females<- c(1:l) 
Migrated<-FALSE
Repetitions <- 100
RecordmtDNA <-matrix(data = NA, nrow= Repetitions, ncol = l)
RecordnDNA <-matrix(data = NA, nrow= Repetitions, ncol = l)
RecordPop <-matrix(data = NA, nrow= Repetitions, ncol = l)
RecordDisc <-matrix(data = NA, nrow= Repetitions, ncol = l)
BreedingHappened<-FALSE


# This data array will save all the data that will be exported and recorded.
MEGACOUNT<-(Repetitions*t)
# MEGADATA<-c(1:48)
# Pacman <-2+(4*l)
MEGADATA<- matrix(data = 0,nrow=1 ,ncol=(2+(4*l)))
MEGACOUNT<-1

# This creates the initial csv file of megadata
write.table(MEGADATA, "megadata.csv", row.names=F, na="NA", append=F, quote = FALSE, sep=";", col.names=F)


# This is to run the entire simulation as many times as required the total number of runs that must be done
for (MEGA in 1:Repetitions){

# These are the arrays that will contain all the data we need, they must be 'reset' before each run
Loc<- array(0,dim=c(7,l,t)) # So dimensions are the variable, then the habitat and then the time point
Male <- array(0,dim=c(l,SexCap,5)) # Toads are arranged by location, then the toad’s number in the location and then the variable in question
Female <- array(0,dim=c(l,SexCap,5))

# Seeding the location list
for(i in 1:l){
  Loc[1,i,1:t]<- (-1+(2*((i-1)/(l-1)))) # Habitat is at position 1 in loc
  Loc[2,i,1:t]<- 0 # Population is at position 2 in loc
  Loc[3,i,1:t] <- 1:t # Time is at position 3 in loc
  Loc[4,i,1:t]<- 0.000 # Seed nDNA is at position 4 in loc
  Loc[5,i,1:t]<-0 # Seed mtDNA is at position 5 in loc
  Loc[6,i,1:t]<-0 # Dischord is recorded at position 6 in loc
  Loc[7,i,1:t] <- 0 # Proportion of the population at position 7 in loc
  
  #not sure what to do with these guys.
  # loc[[i]]$male<-vector(mode="integer", SexCap)
  # loc[[i]]$female<-vector(mode="integer", SexCap)
  
  
  #this sets all of the individual toads as DEAD before we seed in the starting population, there should be one for the Males and one for the females
    for(j in 1:SexCap){
      Male[i,j,1] <- 0 # Alive or dead status is in end position 1
      Male[i,j,2] <- 0 # Age is stored in end position 2
      Male[i,j,3] <- 0 # nDNA is stored in the end position 3
      Male[i,j,4] <- 0 # mtDNA is stored in the end position 4
      Male[i,j,5] <- 0 # Breeding status is in end position 5
      }  
    for(j in 1:SexCap){
      Female[i,j,1] <- 0 # Alive or dead status is in end position 1
      Female[i,j,2] <- 0 # Age is stored in end position 2
      Female[i,j,3] <- 0 # nDNA is stored in the end position 3
      Female[i,j,4] <- 0 # mtDNA is stored in the end position 4
      Female[i,j,5] <- 0 # Breeding status is in end position 5
      } 
}

# This seeds the starting populations, one at either end of the gradient, there should be one for the males and one for the females
# These seed populations start out as pure populations of their specific species

#This seeds at one end
for(j in 1:N0){
  Male[1,j,1] <- 1
  Male[1,j,2] <- 0
  Male[1,j,3] <-Loc[1,1,1] # 1 is one species, -1 is the second species 0 is a perfect hybrid
  Male[1,j,4] <-Male[1,j,3] # Since this is the seed population, this must match the nDNA of the individual
  Male[1,j,5] <- 0 # This is to determine if an individual has bred that year or not, individuals can only breed once a year.
}  
for(j in 1:N0){
  Female[1,j,1] <- 1
  Female[1,j,2] <- 0
  Female[1,j,3] <-Loc[1,1,1] # 1 is one species, -1 is the second species 0 is a perfect hybrid
  Female[1,j,4] <-Female[1,j,3] # Since this is the seed population, this must match the nDNA of the individual
  Female[1,j,5] <- 0 # This is to determine if an individual has bred that year or not, individuals can only breed once a year.
} 

#this seeds the opposite end
for(j in 1:N0){
  Male[l,j,1] <- 1
  Male[l,j,2] <- 0
  Male[l,j,3] <- Loc[1,l,1] # 1 is one species, -1 is the second species 0 is a perfect hybrid
  Male[l,j,4] <- Male[l,j,3] # Since this is the seed population, this must match the nDNA of the individual
  Male[l,j,5] <- 0 # This is to determine if an individual has bred that year or not, individuals can only breed once a year.
}  
for(j in 1:N0){
  Female[l,j,1] <- 1
  Female[l,j,2] <- 0
  Female[l,j,3] <-Loc[1,l,1] # 1 is one species, -1 is the second species 0 is a perfect hybrid
  Female[l,j,4] <-Female[l,j,3] # Since this is the seed population, this must match the nDNA of the individual
  Female[l,j,5] <- 0 # This is to determine if an individual has bred that year or not, individuals can only breed once a year.
} 


#Setting a few initial variables

for (i in 1:l){
  Males[i]<-1
  Females[i]<-1
  for (j in 1:Cap){
    if(Male[i,j,1]==1){Males[i]<-Males[i]+1}
    if(Female[i,j,1]==1){Females[i]<-Females[i]+1}
  }
}

FirstAlive<-1
Migrate<-0
Count<-0
Repolimit<-N0
Order<- 1
nDNAFloat<-0
mtDNAFloat<-0
OrderFloat<- 1
SexFloat<- 1

#This is where it gets into performing an actual simulation


#This will take us through the number of time iterations as requried in the initial parameters
for(k in 1:t){

  #This will take us through each location as determined by the number of locations in the initial parameters
  for(q in 1:l){
    
    if(MEGA %% 2 < 1){
      if(q==1){i=4}
      else if(q==2){i=5}
      else if(q==3){i=3}
      else if(q==4){i=6}
      else if(q==5){i=2}
      else if(q==6){i=7}
      else {i=1}
    }else{if(q==1){i=4}
      else if(q==2){i=3}
      else if(q==3){i=5}
      else if(q==4){i=2}
      else if(q==5){i=6}
      else if(q==6){i=1}
      else {i=7}
      }
    
    
    #Erasing a few float variables
    nDNAFloat<-0
    mtDNAFloat<-0
    
    # This calculates how close the population is to its carrying capacity
    Loc[7,i,k]<-(Males[i]+Females[i])/Cap
    
    # We only care about breeding the living toads, and breeding is entirely random, we assume each toad can only breed once
    # so we take the number of breeding age males just run through with them.
   
     if (Males[i]<Females[i]){Repolimit<-Males[i]}else{Repolimit<-Females[i]}
    if(Repolimit>=SexCap){Repolimit<-SexCap}
    
  
    # But, we are going to need to know if we have hit the maximum number of females
    
    
    # Here is the actual breeding, we go through the Males[i], and breed them against their random partner
    # that was chosen. Offpsring are randomly placed as either male or female, and there could be migration.
    # Migration is chosen depending on the migration parameter.
   
      FemaleOrder<-sample(seq(from =1, to =Females[i], by =1), size=Females[i], replace=FALSE)
      for(j in 1:Repolimit){
        Loc[2,i,k]<-Males[i]+Females[i]
        
        # BreedingHappened<-FALSE
        # while(BreedingHappened==FALSE){
          for(FemaleNumber in 1:Repolimit){
              OrderFloat<-FemaleOrder[FemaleNumber]
              if (Male[i,j,2]>=2 & Male[i,j,1]==1 & Female[i,OrderFloat,2]>=2 & Female[i,OrderFloat,1]==1 & Loc[2,i,k]<Cap & Female[i,j,5]==0){    
                   # BreedingHappened<-TRUE
 
                
                                 # This is the choosy part, wherein we are seeing if the male will be willing to mate with the female hat has arrived to him
                  # Comment the lines between this line 
     #             Dif<-(((Male[i,j,3]+1)*DifCalc)^2+Choose)
                  #possible alt code for choosiness
                  # if(Male[i,j,3]>=Female[i,OrderFloat,3]-Dif){
     #             if(Male[i,j,3]>=Female[i,OrderFloat,3]-Dif & Male[i,j,3]<=Female[i,OrderFloat,3]+Dif){
                  # and this line if you want to do the null without choosiness
                  
                  #Creating the offspring - the random three offspring way
                  for (h in 1:Off) {
                      # Deciding if the offspring migrate or not
                      Rand<-runif(1)
                      Migrate<-i
                      # If they do migrate is decided upon the migration random being less than the base chance of migration and the population size effect
                      if (Rand<m){
                        Rand<-runif(1)
                            Migrated<-TRUE
                            # Deciding the movemnt direction of dispersal
                            if(Rand>0.5){Migrate<-(i+1)}else{Migrate<-(i-1)}
                           if(Migrate<=1){Migrate<-1}
                            if(Migrate>=(l)){Migrate<-l}
                            }
                      # Deciding if they are male of female
                      Rand<-runif(1)
                      # offspring creation
                      if (Rand<0.50){
                        Males[Migrate]<-Males[Migrate]+1
                        if(Males[Migrate]<=SexCap){
                        Male[Migrate,Males[Migrate],1] <- 1 # Alive
                        Male[Migrate,Males[Migrate],2] <- 0 # Age
                        Male[Migrate,Males[Migrate],3] <- ((Male[i,j,3]+Female[i,OrderFloat,3])/2) # nDNA is comibination of the parents
                        Male[Migrate,Males[Migrate],4] <- Female[i,OrderFloat,4]  #must have the mtDNA of the mother
                        Male[Migrate,Males[Migrate],5] <- 0 # Bred status (since newborn can't breed)
                        }
                        }else{
                        Females[Migrate]<-Females[Migrate]+1
                        if(Females[Migrate]<=SexCap){
                        Female[Migrate,Females[Migrate],1] <- 1 # Alive
                        Female[Migrate,Females[Migrate],2] <- 0 # Age
                        Female[Migrate,Females[Migrate],3] <- ((Male[i,j,3]+Female[i,OrderFloat,3])/2) # nDNA is comibination of the parents
                        Female[Migrate,Females[Migrate],4] <- Female[i,OrderFloat,4]  #must have the mtDNA of the mother
                        Female[Migrate,Females[Migrate],5] <- 0 # Bred status (since newborn can't breed)
                        }
                        }
                      # End of offspring creation
                      }
                
                # # Creating offspring the force dispersal way
                # for (h in 1:4) {
                #     if(h==1 & i<l){Migrate<-(i+1)}
                #     else if(h==2 & i>1){Migrate<-(i-1)}
                #     else{Migrate<-i}
                #           
                #     # Deciding if they are male of female
                #     Rand<-runif(1)
                #     # Male creation
                #     if (Rand<0.5){
                #       Males[Migrate]<-Males[Migrate]+1
                #       SexFloat<-Males[Migrate]
                #       if(SexFloat>SexCap){SexFloat<-SexCap}
                #       Male[Migrate,SexFloat,1] <- 1 # Alive
                #       Male[Migrate,SexFloat,2] <- 0 # Age
                #       Male[Migrate,SexFloat,3] <- ((Male[i,j,3]+Female[i,OrderFloat,3])/2) # nDNA is comibination of the parents
                #       Male[Migrate,SexFloat,4] <- Female[i,OrderFloat,4]  #must have the mtDNA of the mother
                #       Male[Migrate,SexFloat,5] <- 0 # Bred status (since newborn can't breed)
                #       }
                #     # Female creation
                #     if (Rand>=0.5){
                #       Females[Migrate]<-Females[Migrate]+1
                #       SexFloat<-Females[Migrate]
                #       if(SexFloat>SexCap){SexFloat<-SexCap}
                #       Female[Migrate,SexFloat,1] <- 1 # Alive
                #       Female[Migrate,SexFloat,2] <- 0 # Age
                #       Female[Migrate,SexFloat,3] <- ((Male[i,j,3]+Female[i,OrderFloat,3])/2) # nDNA is comibination of the parents
                #       Female[Migrate,SexFloat,4] <- Female[i,OrderFloat,4]  #must have the mtDNA of the mother
                #       Female[Migrate,SexFloat,5] <- 0 # Bred status (since newborn can't breed)
                #       }
                #     # End of offspring creation
                #    }

                
                
                
                
                  Female[i,OrderFloat,5] <- 1 # Changes breeeding status to 'bred'
                  FemaleNumber <- Repolimit   # Sets the counter to the limit to end the breeding
                  # BreedingHappened<-TRUE      # Sets the trigger to the limit to end the breeding
                  
                  # need to comment this line as well if you aren't being choosy
              #    }
                  
                  # End of the checking compatability if code
                  }

                      # End of the loop going through females code
              }
          # End of the While loop code
          # }
        # End of the actual breeding look
        }
        
 
   # This will determine which toads survive to the end of this year. We only care about living toads
   # Therefore dead toads are simply replaced in the order
  for(j in 1:Males[i]){
    if(runif(1) <= (d+abs((Male[i,j,3]-Loc[1,i,k])/l)+(Loc[7,i,k]/4)) & j<Males[i]){
      for (h in j:Males[i]-1){
      Male[i,j,] <- Male[i,j+1,]
      }
      Males[i]<-Males[i]-1
    }
    Male[i,j,2] <- Male[i,j,2]+1 # Ages the individual by one year
    Male[i,j,5] <- 0 # Resets breeding status to unbred
  }
  for(j in 1:Females[i]){
    if(runif(1) <= (d+abs((Female[i,j,3]-Loc[1,i,k])/l)+(Loc[7,i,k]/4)) & j<Females[i]){
      for (h in j:Females[i]-1){
        Female[i,j,] <- Female[i,j+1,]
      }
      Females[i]<-Females[i]-1
    }
    Female[i,j,2] <- Female[i,j,2]+1 # Ages the individual by one year
    Female[i,j,5] <- 0 # Resets breeding status to unbred
  }
  
      
# Getting the values for mtDNA and nDNA in that location at that time.


for (j in 1:Males[i]){
  nDNAFloat<-nDNAFloat+Male[i,j,3]
  mtDNAFloat<-mtDNAFloat+Male[i,j,4]  
}
for (j in 1:Females[i]){
  nDNAFloat<-nDNAFloat+Female[i,j,3]
  mtDNAFloat<-mtDNAFloat+Female[i,j,4]  
}

#Inserting the nuclear and mt DNA values into the array
Loc[4,i,k] <- (nDNAFloat/Loc[2,i,k])   # Saves the nDNA value by taking to total and dividing by the pop'n
Loc[5,i,k] <- (mtDNAFloat/Loc[2,i,k])  # Saves the mtDNA value by taking to total and dividing by the pop'n
Loc[6,i,k] <- Loc[4,i,k]-Loc[5,i,k] # Calculates dischordance by subtracting mtDNA from nDNA

# This would seem to be where I need to put in the code for the MEGADATA recording

#recording which repetition we're at
  MEGADATA[1]<- MEGA
#recording the generation we're at  
  
# recording the location
  
#number of males at that time and location
  MEGADATA[(i*4)-1]<- Males[i]
# number of females at that time and location
  MEGADATA[(i*4)]<- Females[i]
# DNA at that time and location
  MEGADATA[(i*4)+1]<- Loc[4,i,k]
#mtDNA at that time and location
  MEGADATA[(i*4)+2]<- Loc[5,i,k]

  #this ends the loop for location in each round
  }
  
  #this is the status update when all locations for a particular repetition are complete
  MEGADATA[1]<-MEGA
  MEGADATA[2]<-k
    
  # MEGADATAMATRIX<-as.matrix(t(MEGADATA))
  # write.table(MEGADATAMATRIX, "MEGADATA.csv", row.names=F, na="NA", append=T, quote = FALSE, sep=";", col.names=F)

  write.table(MEGADATA, "MEGADATA.csv", row.names=F, na="NA", append=T, quote = FALSE, sep=";", col.names=F)
  
  print(paste("Finished", "[", round(k/t*100),"%] of ",MEGA,"/",Repetitions," runs")) 
  
  }

for (i in 1:l){
  RecordmtDNA[MEGA,i] <- Loc[5,i,t]
  RecordnDNA[MEGA,i] <- Loc[4,i,t]
  RecordPop[MEGA,i] <- Loc[7,i,t]
  RecordDisc[MEGA,i] <- Loc[6,i,t]
}
print(paste("Finished", MEGA,"/",Repetitions," runs")) 
}

# This will plot the mtDNA and the nDNA as well as the Proportional population and the dichordance between n and mt
for(i in 1:l){
  plot(x=Loc[3,i,],y=Loc[4,i,],ylim=c(-2,2),col="blue",main=i) # Plotting nDNA
  points(x=Loc[3,i,],y=Loc[5,i,],col="red") # Adding mtDNA
  points(x=Loc[3,i,],y=Loc[7,i,],col="green") # Adding ProPop
  points(x=Loc[3,i,],y=Loc[6,i,],col="black") # Adding dischord
 # points3D(x=loc[[i]]$time,y=loc[[i]]$Dichord,z=loc[[i]]$Habitat)
}

# This is recording everything in external csv files
write.csv(RecordmtDNA, "mtDNA.csv")
write.csv(RecordnDNA, "nDNA.csv")
write.csv(RecordPop, "ProPop.csv")
write.csv(RecordDisc, "Disc.csv")
# write.csv(MEGADATA, "MegaData.csv")


# # These are lines for graphing the different outputs to check for model function
# # Read in the alternate hypothesis discordance
# Disc<-read.csv(file=file.choose())
AltMtDNA<-read.csv(file=file.choose())
AltNDNA<-read.csv(file=file.choose())
Disc<-abs(AltNDNA-AltMtDNA)

# Graph it in a 2x4 panel figure
op=par(mfrow=c(2,4))
hist(Disc$V1, xlab = "Location 1", main ="")
hist(Disc$V2, xlab = "Location 2", main ="")
hist(Disc$V3, xlab = "Location 3", main ="")
hist(Disc$V4, xlab = "Location 4", main ="")
hist(Disc$V5, xlab = "Location 5", main ="")
hist(Disc$V6, xlab = "Location 6", main ="")
hist(Disc$V7, xlab = "Location 7", main ="")
par(op)

op=par(mfrow=c(2,4))
hist(AltNDNA$V1)
hist(AltNDNA$V2)
hist(AltNDNA$V3)
hist(AltNDNA$V4)
hist(AltNDNA$V5)
hist(AltNDNA$V6)
hist(AltNDNA$V7)
par(op)

# Read in the null model hypothesis
# nullDisc<-read.csv(file=file.choose())
nullMtDNA<-read.csv(file=file.choose())
nullNDNA<-read.csv(file=file.choose())
nullDisc<-abs(nullNDNA-nullMtDNA)

# Graph it in a 2x4 panel figure
op=par(mfrow=c(2,4))
hist(nullDisc$V1, xlab = "Location 1", main ="")
hist(nullDisc$V2, xlab = "Location 2", main ="")
hist(nullDisc$V3, xlab = "Location 3", main ="")
hist(nullDisc$V4, xlab = "Location 4", main ="")
hist(nullDisc$V5, xlab = "Location 5", main ="")
hist(nullDisc$V6, xlab = "Location 6", main ="")
hist(nullDisc$V7, xlab = "Location 7", main ="")
par(op)

op=par(mfrow=c(2,4))
hist(nullNDNA$V1)
hist(nullNDNA$V2)
hist(nullNDNA$V3)
hist(nullNDNA$V4)
hist(nullNDNA$V5)
hist(nullNDNA$V6)
hist(nullNDNA$V7)
par(op)


drops<-c("X")
nullDisc<-nullDisc[ , !(names(nullDisc) %in% drops)]
Disc<-Disc[ , !(names(Disc) %in% drops)]
nullNDNA<-nullNDNA[ , !(names(nullNDNA) %in% drops)]
AltNDNA<-AltNDNA[ , !(names(AltNDNA) %in% drops)]


boxplot(nullDisc, main="Null model discordance", xlab="Location", ylab="Discordance")
boxplot(Disc, main="Alternate model discordance", xlab="Location", ylab="Discordance")
boxplot(nullNDNA, main="Null model nuclear DNA", xlab="Location", ylab="Nuclear DNA")
boxplot(AltNDNA, main="Alternate model nuclear DNA", xlab="Location", ylab="Nuclear DNA")


# t-tests between
t.test(Disc$V1,nullDisc$V1)
t.test(Disc$V2,nullDisc$V2)
t.test(Disc$V3,nullDisc$V3)
t.test(Disc$V4,nullDisc$V4)
t.test(Disc$V5,nullDisc$V5)
t.test(Disc$V6,nullDisc$V6)
t.test(Disc$V7,nullDisc$V7)

Arranged<-read.csv(file=file.choose(),sep=";",dec=",")
fit<-aov(Disc~Location+Model,data=Arranged)
summary(fit)
