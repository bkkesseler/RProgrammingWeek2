corr <- function( directory, threshold = 0 ) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'threshold' is a numeric vector of length 1 indicating the 
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0
        
        ## Return a numeric vector of correlations
        ## NOTE: Do not round the result!
        
        source("complete.R")
        completes <- complete( directory )
        useful <- completes[ completes[,2] >= threshold, 1 ]
        
        if(length(useful) != 0) {
                for( i in 1:length(useful) ) {
                        currentfile <- read.csv( 
                                paste( directory,"/",
                                       if( useful[i] < 10 ) {
                                               paste( "00",useful[i],sep = "" )
                                       }
                                       else if( useful[i] < 100 ) {
                                               paste( "0",useful[i],sep = "")
                                       }
                                       else {
                                               useful[i]
                                       }
                                       ,".csv",sep = ""
                                )
                        )
                        if( sum(complete.cases(currentfile) )==0 ) {next()}
                        else {
                           if(i==1) {
                                result <- cor(currentfile[,"nitrate"],
                                              currentfile[,"sulfate"],
                                              use="pairwise.complete.obs")
                                }
                                else { result <- c(result, cor(currentfile[,"nitrate"],
                                                        currentfile[,"sulfate"],
                                                        use="pairwise.complete.obs")) 
                                }
                        }
                }
                
        }
        else {result <- numeric()}
        result
}