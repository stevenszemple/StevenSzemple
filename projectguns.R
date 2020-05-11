#############################
## This times the Charm ##
#############################

projectguns <- read.csv("shootingsrevised.csv")
projectguns

#############################

dim(projectguns)
head(projectguns)
summary(projectguns)
range(projectguns$year)

#############################

table(projectguns$TypeofGunGeneral)

#############################

levels(projectguns$TypeofGunGeneral)

#############################

table(Fatalities = projectguns$TotalNumberofFatalities, Guns = projectguns$TypeofGunGeneral)

#############################

tapply(projectguns$TotalNumberofFatalities, projectguns$TypeofGunGeneral, mean)

tapply(projectguns$TotalNumberofFatalities, projectguns$TypeofGunGeneral, sum)

tapply(projectguns$TotalNumberofFatalities > 3, projectguns$TypeofGunGeneral, mean)

tapply(projectguns$TotalNumberofFatalities > 3, projectguns$TypeofGunGeneral, sum)

#############################

mentalillness <- subset(projectguns, PossibleMotiveGeneral == "Mental illness")
mentalillness

#############################

table(mentalillness$TypeofGunGeneral)
tapply(mentalillness$TotalNumberofFatalities, mentalillness$TypeofGunGeneral, sum)

#############################

MassShootingMentalIllness <- subset(mentalillness, TotalNumberofFatalities > 2)
MassShootingMentalIllness

#############################

MassShootingByYearMentalIllness <- table(MassShootingMentalIllness$year)
MassShootingByYearMentalIllness

#############################

barplot(MassShootingByYearMentalIllness)

graph1 <- tapply(projectguns$TotalNumberofFatalities > 3, projectguns$TypeofGunGeneral, mean)

graph2 <- tapply(projectguns$TotalNumberofFatalities > 3, projectguns$TypeofGunGeneral, sum)

barplot(graph1)
barplot(graph2)

#############################

barplot(graph2,
        ylim =  c(0, 100),# y-axis dimensions
        names= c("Handguns", "MultipleGuns", "Rifle", "Shotgun", "Unknown"),
        col = c("blue", "yellow", "red3", "black", "green"),# color of bars
        border = NA,# removes bar borders
        main = "Mass Shootings by Guntype",# plot title
        cex.main = .8,# size of plot title
        ylab = "Number of Shootings",# yaxis label
        xlab = "Type of Gun",
        cex.lab = .8,# size of yaxis label,
        cex.names = .8,# size of xaxis label,
        las = 1)# controls angle of axis labels
abline(h=0, lty=1, col = "Black")# adds horizontal line at 0 with dashes

table(projectguns$MassShooting)

table(projectguns$MassShooting, projectguns$TypeofGunGeneral)
mg <- subset(projectguns, TypeofGunGeneral == "Multiple guns")
mg
mg.rifle <-subset(mg, NumberofRifles > 0)
mg.rifle.mass <- subset(mg.rifle, MassShooting == "Y" )
mg.rifle.mass
mg.rifle.mass.mental <- subset(mg.rifle.mass, PossibleMotiveGeneral == "Mental illness")
mg.rifle.mass.mental
sum(mg.rifle.mass.mental$TotalNumberofFatalities)
totalfatalities.mg.rifle.mass.mental <- sum(mg.rifle.mass.mental$TotalNumberofFatalities)
totalfatalities.mg.rifle.mass.mental/7

# avg number of deaths when multipleguns + rifle is used in a mass shooting mental illness

#############################


hg <- subset(projectguns, TypeofGunGeneral == "Handgun")
hg.mass <- subset(hg, MassShooting == "Y")
hg.mass.mental <- subset(hg.mass, PossibleMotiveGeneral == "Mental illness")
hg.mass.mental
totalfatalities.hg.mass.mental <- sum(hg.mass.mental$TotalNumberofFatalities)
totalfatalities.hg.mass.mental/8

# avg number of deaths when only a handgun is used + MS + MI

###############################

rifle <- subset(projectguns, TypeofGunGeneral == "Rifle")
rifle.mass <- subset(rifle, MassShooting == "Y")
rifle.mass.mental <- subset(rifle.mass, PossibleMotiveGeneral == "Mental illness")
totalfatalities.rifle.mass.metal <- sum(rifle.mass.mental$TotalNumberofFatalities)
totalfatalities.rifle.mass.metal/3

# avg number of deaths when only a rifle is used in a mass shooting + MI

###############################

tfmgr <- totalfatalities.mg.rifle.mass.mental/7
tfr <- totalfatalities.rifle.mass.metal/3
tfhg <- totalfatalities.hg.mass.mental/8
graph3 <- cbind(tfmgr, tfr, tfhg)

barplot(graph3,
        ylim =  c(0, 10),# y-axis dimensions
        names= c("MG + Rifle", "Rifle", "Handguns"),
        col = c("Red", "yellow2", "Blue3"),# color of bars
        border = NA,# removes bar borders
        main = "Mass Shooting Motivated by Mental Illness",# plot title
        cex.main = .8,# size of plot title
        ylab = "Avg. Number of Deaths per Shooting",# yaxis label
        xlab = "Type of Gun",
        cex.lab = .8,# size of yaxis label,
        cex.names = .8,# size of xaxis label,
        las = 1)# controls angle of axis labels
abline(h=0, lty=1, col = "Black")# adds horizontal line at 0 with dashes)

#############################
## Causal Relationship ##
#############################

projectguns2 <- read.csv("shootings.csv")

NJ <- subset(projectguns2, State == "New Jersey")
Texas <- subset(projectguns2, State == "Texas")

nj.mass <- subset(NJ, TotalNumberofFatalities > 2)
texas.mass <- subset(Texas, TotalNumberofFatalities > 2)

## percentage of shootings in texas being mass shooting

16/19

## percentage of shootings in NJ being a mass shooting

2/4

#############################

massafterobama <- subset(projectguns2, (TotalNumberofFatalities > 2) & (year > 2008))
massbeforeobama <- subset(projectguns2, (TotalNumberofFatalities > 2) & (year < 2009))

beforeobama <- subset(projectguns2, year < 2009)
afterobama <- subset(projectguns2, year > 2008)

## percentage of shootings being mass shootings Obama

71/115 #before

109/192 #after

##############################

diffBeforeAfter <- mean(beforeobama$TotalNumberofFatalities) - mean(afterobama$TotalNumberofFatalities)
diffBeforeAfter ## on avergaae, 1.5 less people died during obama presidency

#############################

range(projectguns$TotalNumberofFatalities)
levels(projectguns$TypeofGunGeneral)






