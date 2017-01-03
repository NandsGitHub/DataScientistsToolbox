pollutantmean <- function(directory,pollutant,id=1:332){
  pmean <- vector(mode="numeric", length = length(id))
  prows <- vector(mode="numeric", length = length(id))
  
  dir_good <- data.frame()
  for(i in 1:length(id))
  {
    ##Setting path for reading csv file
    path <- fileName(directory,id[i])
    
    ##Reading csv file
    dir <- read.csv(path)
    
    ##Reading the pollutant data and finding not NA values 
    dir_good <- dir[complete.cases(dir[,pollutant]),]
    m1 <- dir_good[[pollutant]]
    prows[i] <- nrow(dir_good)
    
    ##Finding the mean of the pollutant in one file
    pmean[i] <- mean(m1)
    
  }## end for
  
  ## Finding the weighted mean of the pollutant across all the files
  pollmean <- sum((prows*pmean))/sum(prows)
  pollmean
  
}## end function

##Function to return the correct file path
fileName <- function(dir_name,id_no){
  fname <- id_no
  if (id_no < 10)
  {
    fname <- paste("00",id_no,sep="")
  }
  else if(id_no < 100){
    fname <- paste("0",id_no,sep="")
  }
  path_new <- paste(dir_name,"/",fname,".csv",sep="")
  path_new
}
