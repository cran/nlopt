get_minlplib_problem <- function(name){
    #' @title Gets problem from MINLPLib
    #' @description Function that gets the corresponding problem from MINLPLib library
    #' @param name name of the problem
    #' @return TRUE if there is no error getting the problem and FALSE otherwise
    #' @examples
    #' get_minlplib_problem(name = "alkyl")
    #'
    url <- paste("https://www.minlplib.org/nl/",name,".nl", sep="")
    tryCatch(
        expr = {
            utils::download.file(url, destfile = file.path(tempdir(), "problem.nl"), mode = "wb")
            return(TRUE)
        },
        error = function(e) {
            warning(paste("There was an error downloading the file from https://www.minlplib.org/nl/", name, ".nl. Check the name of the file.", sep=""))
            return(FALSE)
        }
    )
}
