pollutantmean <- function(directory, pollutant, id = 1:332) {

        sysstdwd <- getwd()
        if (!is.null (directory))
                setwd(directory)
        tempwd <- getwd()

        filenames <- list.files(tempwd, full.names = TRUE, 
                                pattern = "*.csv")        
        
        datafiles <- lapply(filenames, read.csv)
        sysstdwd <- setwd(sysstdwd)
        pollutantdata <- do.call(rbind, datafiles)
        
        if (pollutant == "sulfate") {
                sulfsub <- subset(pollutantdata, ID %in% id,
                        select = c(sulfate))
                pmsulf <- colMeans(sulfsub, na.rm = TRUE)
                return(pmsulf)
        } else if (pollutant == "nitrate") {
                nitrsub <- subset(pollutantdata, ID %in% id,
                        select = c(nitrate))
                pmnitr <- colMeans(nitrsub, na.rm = TRUE)
                return(pmnitr)
        } else {
                print("Pollutant not included in data.")
        }
}