complete <- function( directory, id = 1:332 ) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return a data frame of the form:
        ## id nobs
        ## 1 117
        ## 2 1041
        ## ...
        ## where 'id' is the monitor ID number and 'nobs' is the 
        ## number of complete cases
        
        result <- data.frame()
        
        for( i in 1:length(id) ) {
                currentfile <- read.csv( 
                        paste( directory,"/", 
                               if( id[i] < 10 ) {
                                       paste( "00",id[i],sep = "" )
                               }
                               else if( id[i] < 100 ) {
                                       paste( "0",id[i],sep = "")
                               }
                               else {
                                       id[i]
                               }
                               ,".csv",sep = ""
                        )
                )
                currentrow <- c( currentfile[1,"ID"],
                                 sum( complete.cases(currentfile) )
                                 )
                result <- rbind( result, currentrow )
        }
        names(result) <- c("id","nobs")
        result
}