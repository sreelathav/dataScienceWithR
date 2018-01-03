getmonitor <- function(id,directory,summarize){
 if (is.numeric(id)){
 file1 <- paste("/",directory,"/",sprintf("%03d",id),".CSV",sep="")}
 if (is.character(id)){
 file1 <- paste("/",directory,"/",sprintf("%03d",as.numeric(id)),".CSV",sep="")}
 data1 <- read.csv(file1,header = TRUE)
 if(summarize){
 summary(data1)}
 return(data1)
}