complete <- function(directory,id=1:332){
  compFrame <- data.frame(row.names = c("id","nobs"))
  nobs <- vector(mode="numeric", length = length(id))
  for(i in 1:length(id))
  {
    ##Setting path for reading csv file
    path <- fileName(directory,id[i])
    
    ##Reading csv file
    dir <- read.csv(path)
    
    ##Reading the pollutant data and finding not NA values 
    m1 <- dir[complete.cases(dir),]
    nobs[i] <- nrow(m1)
    
    
  }## end for
  compFrame <- cbind(id,nobs)
  compFrame <- as.data.frame(compFrame)
  compFrame
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