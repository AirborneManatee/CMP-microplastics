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
  #takes a vector of strings and runs them all pasted together as code
  final.string<-""
  i<-1
  while (i<=length(vector.of.strings)){
    final.string<-paste(final.string, vector.of.strings[i], sep="")
    i<-1+i
  }
  print(final.string)
  final.string
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
    file.by.mp$temp.of.what.col <- of.what.header
  
  #create vector of zeros of a length equal to the number of samples in the defined group
    trimmed.sample.data.to.group <- file.by.sample
    trimmed.sample.data.to.group <- 
      trimmed.sample.data.to.group[trimmed.sample.data.to.group$temp.group.name == group.name,]
    num.samples <- length(trimmed.sample.data.to.group$temp.group.name)
    final.vector <- rep(0, num.samples)
  
  #go throught each sample name within defined group and count how many occurences of of.what
    sample.names.in.group <- trimmed.sample.data.to.group$temp.sample.name
    sample.names.in.group
    place.in.vector <-1
    for (sample in sample.names.in.group) {
      trimmed.mp.data.to.sample <- file.by.mp
      trimmed.mp.data.to.sample <- 
        trimmed.mp.data.to.sample[trimmed.mp.data.to.sample$temp.sample.name == sample,]
      trimmed.mp.data.to.sample.to.of.what <- 
        trimmed.mp.data.to.sample[trimmed.mp.data.to.sample$temp.of.what.col == of.what,]
      final.vector[place.in.vector] <- 
        length(trimmed.mp.data.to.sample.to.of.what$temp.of.what.col)
      place.in.vector <- place.in.vector + 1
    }
    
    final.vector
}

#############################################################################################

sep.vect <- function(vector){
  #take a vector and print it out in code form (for example: a vector of zeros of length 3 is
    #c(0,0,0))
  final.str <- "c("
  place <- 1
  while (place <= length(vector)){
    if (place <= 1){
      final.str <- paste(final.str, vector[place], sep="")
    } else {
      final.str <- paste(final.str, vector[place], sep=",")
    }
    place <- place+1
  }
  final.str <- paste(final.str, ")", sep="")
  final.str
}

#############################################################################################

build.df.of.color.aves <- function(file.by.sample, 
                                   file.by.mp,
                                   color.vector,
                                   color.header,
                                   group.vector, 
                                   group.header,
                                   sample.name.header.mp,
                                   sample.name.header.sample) {

  #file.by.sample: the name of the text file containing tidy data with sample as unit of mea-
    #surement
  #file.by.mp: the name of the text file containing tidy data with microplastic as unit of m-
    #easurement
  #color.vector: a vector of which colors of mp you want included in data frame 
  #color.header: the column name where the color value can be found, including the file name
    #(for example: data$color)
  #group.vector: a vactor of which groups you want to include in the dataframe
  #group.header:the column name where the group.name is located, including the file name (for
    #example: data$group)
  #sample.name.header.mp: column where sample names are stored in mp data, including file na-
    #me (for example: data$sample.id)
  #sample.name.header.mp: column where sample names are stored in sample data, including fil-
    #e name (for example: data$sample.id)
  
  #build empty df
    final.df <- data.frame("group" = group.vector)
    place.in.color.vector <- 1
    while (place.in.color.vector <= length(color.vector)){
      temp.zero.vect <- rep(0, length(group.vector))
      temp.zero.vect <- sep.vect (temp.zero.vect)
      temp.col.name <- color.vector[place.in.color.vector]
      temp.text.to.run <- string.to.function(c("new.final.df.col <- data.frame (", temp.col.name, " = ", temp.zero.vect, ")"))
      eval(parse(text=temp.text.to.run))
      final.df <- cbind(final.df, new.final.df.col)
      place.in.color.vector <- place.in.color.vector + 1
    }
    final.df
}
 
