#exploring water control samples

watercontrols_bymp <- read.delim("C:/Users/Gray Ryan.000/Desktop/Geist Lab/Data/watercontrols_bymp.txt")
watercontrols_bysamp <- read.delim("C:/Users/Gray Ryan.000/Desktop/Geist Lab/Data/watercontrols_bysamp.txt")

#rename group names because this is bullshit
watercontrols_bysamp$ctrl.set <- paste("g", watercontrols_bysamp$ctrl.set, sep="")

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

ggplot(data) +
  geom_bar( aes(x=name, y=value), stat="identity", fill="skyblue", alpha=0.7) +
  geom_errorbar( aes(x=name, ymin=value-sd, ymax=value+sd), width=0.4, colour="orange", alpha=0.9, size=1.3)


