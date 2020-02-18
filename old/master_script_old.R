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
                                                sample.name.header.mp,
                                                sample.name.header.sample) {
  
  #this function accepts 8 arguments and returns a vector of the quantity of mp of a defined 
    #type or color (of.what), with each value of the vector corresponding to one sample with-
    #in a defined group of samples (group.name)
  
  #file.by.sample: the name of the text file containing tidy data with sample as unit of mea-
    #surement
  #file.by.mp: the name of the text file containing tidy data with microplastic as unit of m-
    #easurement
  #of.what: a string indicating what the value is of the type/color of mp you are interested 
    #in analyzing (for example: "grn", "prt")
  #of.what.header: the column name where the of.what value can be found, including the file 
    #name (for example: data$color)
  #group.name: a string of the particular group of samples you want to analyze (for example: 
    #"group 1")
  #group.header:the column name where the group.name is located, including the file name (for
    #example: data$group)
  #sample.name.header.mp: column where sample names are stored in mp data, including file na-
    #me (for example: data$sample.id)
  #sample.name.header.mp: column where sample names are stored in sample data, including fil-
    #e name (for example: data$sample.id)
  
  #save some temp columns
    file.by.sample$temp.sample.name <- sample.name.header.sample
    file.by.sample$temp.group.name <- group.header
    file.by.mp$temp.sample.name <- sample.name.header.mp
  
  #create vector of zeros of a length equal to the number of samples in the defined group
    trimmed.sample.data.to.group <- file.by.sample
    trimmed.sample.data.to.group <- trimmed.sample.data.to.group[trimmed.sample.data.to.group$temp.group.name == group.name]
    trimmed.sample.data.to.group
    
}

#############################################################################################








