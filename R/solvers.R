check_solver <- function(solver){
    #' @title Checks if solver exists
    #' @description Function that checks if the solver exists and it can be run without any issue
    #' @param solver name of the solver (it has to be in the PATH)
    #' @return TRUE if the solver exists and FALSE otherwise
    #' @examples
    #' check_solver(solver = "ipopt")
    #'
    status = suppressWarnings(system(paste(solver, "-=", sep=" "), ignore.stdout = TRUE, ignore.stderr = TRUE))
    if (status == 0){
        return(TRUE)
    } else {
        warning(paste("Solver", solver, "is not available. Try adding the executable to the PATH", sep=" "))
        return(FALSE)
    }
}

get_available_options <- function(solver){
    #' @title Gets the available options for a solver
    #' @description Function that gets a vector with all the available options for a solver
    #' @param solver name of the solver (it has to be in the PATH)
    #' @return vector with all the available options for a solver
    #' @examples
    #' get_available_options(solver = "ipopt")
    #'
    res <- c()
    if (check_solver(solver)){
        con <- pipe(paste(solver, "-=", sep=" "))
        opts <- readLines(con)
        close(con)
        for (opt in opts){
            if (!startsWith(opt," ") & !startsWith(opt,"\t") & length(opt) > 0){
                if (!is.na(strsplit(opt, " ")[[1]][1])){
                    res = c(res,strsplit(opt, " ")[[1]][1])
                }
            }
        }
    }
    return(res)
}

print_help <- function(solver){
    #' @title Prints the help of the solver
    #' @description Function that prints the help of the solver
    #' @param solver name of the solver (it has to be in the PATH)
    #' @return No return value
    #' @examples
    #' print_help(solver = "ipopt")
    #'
    if (check_solver(solver)){
        if (solver == "raposa"){
            x = system(paste(solver, "-help", sep=" "))
        } else {
            x = system(paste(solver, "-?", sep=" "))
        }
    }
}

print_available_options <- function(solver){
    #' @title Prints the available options of the solver
    #' @description Function that prints the available options of the solver
    #' @param solver name of the solver (it has to be in the PATH)
    #' @return No return value
    #' @examples
    #' print_available_options(solver = "ipopt")
    #'
    if (check_solver(solver)){
        x = system(paste(solver, "-=", sep=" "))
    }
}

print_solver_version <- function(solver){
    #' @title Prints the version of the solver
    #' @description Function that prints the version of the solver
    #' @param solver name of the solver (it has to be in the PATH)
    #' @return No return value
    #' @examples
    #' print_solver_version(solver = "ipopt")
    #'
    if (check_solver(solver)){
        if (solver == "raposa"){
            x = system(paste(solver, "-V", sep=" "))
        } else {
            x = system(paste(solver, "-v", sep=" "))
        }
    }
}
