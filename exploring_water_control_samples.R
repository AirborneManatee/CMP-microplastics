#exploring water control samples

watercontrols_bymp <- read.delim("C:/Users/Gray Ryan.000/Desktop/Geist Lab/Data/watercontrols_bymp.txt")
watercontrols_bysamp <- read.delim("C:/Users/Gray Ryan.000/Desktop/Geist Lab/Data/watercontrols_bysamp.txt")

#rename group names because this is bullshit
watercontrols_bysamp$ctrl.set <- paste("g", watercontrols_bysamp$ctrl.set, sep="")

library(ggplot2)

#############################################################################################

#testing purposes

#define some variables

file.by.sample <- watercontrols_bysamp
file.by.mp <- watercontrols_bymp
of.what <- "red"
of.what.header <- watercontrols_bymp$color
group.name <- "g1"
group.header <- watercontrols_bysamp$ctrl.set
sample.name.header.mp <- watercontrols_bymp$ctrl.id
sample.name.header.sample <- watercontrols_bysamp$ctrl.id


vector <- create.vector.of.mp.values.for.group(file.by.sample, 
                                               file.by.mp, 
                                               of.what, 
                                               of.what.header,
                                               group.name, 
                                               group.header,
                                               sample.name.header.mp,
                                               sample.name.header.sample)

vector

#############################################################################################

#more testing

file.by.sample <- watercontrols_bysamp
file.by.mp <- watercontrols_bymp
color.vector <- c("red", "yel", "blk")
color.header <- watercontrols_bymp$color
group.vector <- c("g1", "g2", "g3")
group.header <- watercontrols_bysamp$ctrl.set
sample.name.header.mp <- watercontrols_bymp$ctrl.id
sample.name.header.sample <- watercontrols_bysamp$ctrl.id

color.df <- build.df.of.color.aves(file.by.sample, 
                                   file.by.mp, 
                                   color.vector, 
                                   color.header,
                                   group.vector, 
                                   group.header,
                                   sample.name.header.mp,
                                   sample.name.header.sample)

color.df

#############################################################################################

#plotting occurance of each color in each control group

file.by.sample <- watercontrols_bysamp
file.by.mp <- watercontrols_bymp
color.vector <- get.mp.color.vector()
color.header <- watercontrols_bymp$color
group.vector <- c("g1", "g2", "g3")
group.header <- watercontrols_bysamp$ctrl.set
sample.name.header.mp <- watercontrols_bymp$ctrl.id
sample.name.header.sample <- watercontrols_bysamp$ctrl.id

color.df <- build.df.of.color.aves.for.ggplot(file.by.sample, 
                                              file.by.mp,
                                              color.vector,
                                              color.header,
                                              group.vector, 
                                              group.header,
                                              sample.name.header.mp,
                                              sample.name.header.sample)
color.df

p<- ggplot(color.df, aes(x=group, y=average, fill=color)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=average-sd, ymax=average+sd), width=.2,
                position=position_dodge(.9)) 
print(p)

#######################################################################################################

group1.samp <- watercontrols_bysamp[watercontrols_bysamp$ctrl.set=="g1",]
group1.samp[is.na(group1.samp)] = 0
group1.samp

group1.mp <- watercontrols_bymp[watercontrols_bymp$ctrl.set=="1",]
group1.mp[is.na(group1.mp)] = 0
group1.mp
group1.mp[101:153,]

group2.samp <- watercontrols_bysamp[watercontrols_bysamp$ctrl.set=="g2",]
group2.samp[is.na(group2.samp)] = 0
group2.samp

group2.mp <- watercontrols_bymp[watercontrols_bymp$ctrl.set=="2",]
group2.mp[is.na(group2.mp)] = 0
group2.mp

group3.samp <- watercontrols_bysamp[watercontrols_bysamp$ctrl.set=="g3",]
group3.samp[is.na(group3.samp)] = 0
group3.samp

group3.mp <- watercontrols_bymp[watercontrols_bymp$ctrl.set=="3",]
group3.mp[is.na(group3.mp)] = 0
group3.mp
