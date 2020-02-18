#############################################################################################
#############################################################################################
#############################################################################################
###                                                                                       ###
###                  MASTER SCRIPT OF ALL FUNCTIONS MADE FOR ANALYSIS                     ###
###                                                                                       ###
#############################################################################################
#############################################################################################
#############################################################################################

get.mp.type.vector <- function (){
  #this function returns a vector of every type of microplastic
  c(fib, flm, prt)
}

#############################################################################################

get.mp.color.vector <- function (){
  #this function returns a vector of every color of microplastic
  c(red, orn, yel, grn, blu, pur, pnk, clr, blk, wht, gry, brn, tan)
}

#############################################################################################

string.to.function <- function (vector.of.strings){
  final.string<-""
  i<-1
  while (i<=length(vector.of.strings)){
    final.string<-paste(final.string, vector.of.strings[i], sep="")
    i<-1+i
  }
  print(final.string)
  eval(parse(text=final.string))
}

#############################################################################################

create.vector.of.mp.values.for.group<- function(file.by.sample, 
                                                file.by.mp,
                                                of.what,
                                                of.what.header,
                                                group.name, 
                                                group.header,
                                                sample.name.header) {
  #RETURNS A VECTOR OF HOW MANY OF of.what TYPE OF MICROPLASTICS OCCURED IN THE DEFINED group
    #per sample (each value in the vector is a separate sample)
  #file.by.sample: tidy data text file in which each row is a sample
  #of.what: the type or color of microplastic you which to recieve a vector of
  #of.what.header: the column name of whichever column contains the color/type values
  #group.name: which group of samples you would like a vector of
  #group.header: the column name of whichever column contains the group values
  #sample.name.header: the header of the column containing sample names (whould be the same in
    #text files)
  
  #create empty vector with correct number of observations
    trimmed.file.by.sample <- file.by.sample[file.by.sample$group.header==group.name]
    print(trimmed.file.by.sample)
    num.of.samples.in.group <- length(trimmed.file.by.sample$group.header)
    return.vector <- rep(0, num.of.samples.in.group)
  #create vector of sample names within specified group
    sample.names.in.group<-trimmed.file.by.sample$sample.name.header
  location.in.vector<-1
  for (sample in sample.names.in.group)
  {
    trimmed.mp.by.sample.data <- file.by.mp[file.by.mp$sample.name.header==sample]
    trimmed.mp.by.sample.and.of.what.data <- 
      trimmed.mp.by.sample.data[trimmed.mp.by.sample.data$of.what.header==of.what]
    return.vector[location.in.vector]<-
      length(trimmed.mp.by.sample.and.of.what.data$of.what.header)
    location.in.vector<-location.in.vector+1
  }
}

#############################################################################################








