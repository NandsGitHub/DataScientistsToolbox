corr <- function(directory,threshold=0)
{
  listOfFiles <- list.files(directory)
  id <- list()
  
  ##Getting the id from filenames
  for(i in 1:length(listOfFiles))
  {
    fileName <- listOfFiles[i]
    subFile <- substr(fileName,start=1,stop=3)
    id[i] <- as.numeric(subFile)
  }## end for
  
  ##Getting list files with their number of complete rows
  compFiles <- complete(directory,id)
  
  k <- 1
  threshFiles <- list()
  
  ##Getting list of files which have nobs > threshold
  for(j in 1:nrow(compFiles)){
    row <- compFiles[j,]
    if(row[["nobs"]] > threshold){
      threshFiles[k] <- row[["id"]]
      k <- k+1
    }
  }##end for
  
  ##For each threshold file, finding correlation between sulfate and nitrate columns
  corrData <- matrix()
  if(length(threshFiles) > 0){
    for(n in 1:length(threshFiles)){
        fName <- fileName(directory,threshFiles[n]) 
        file <- read.csv(fName)
        good <- complete.cases(file)
        goodFile <- file[good,]
        corrData[n] <- cor(goodFile[["sulfate"]],goodFile[["nitrate"]])
    }##end for
  }##end if
  
  corrData ## Return correlated data
}


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