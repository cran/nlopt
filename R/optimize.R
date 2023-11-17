optimize <- function(solver, file, options){
    #' @title Optimizes the problem
    #' @description Function that calls the corresponding solver with custom options to solve the problem given by the .nl file
    #' @param solver name of the solver (it has to be in the PATH)
    #' @param file .nl file with the optimization problem. If the name of the file starts with MINLPLib:: then the problem will be downloaded from MINLPLib library
    #' @param options list with the options for the solver
    #' @return list with a string of the output given by the solver (output), the optimal value of the problem (objective), the status returned by the solver (status), the optimal primal solution (primal_solution), and the optimal dual solution (dual_solution)
    #' @examples
    #' optimize(solver = "ipopt", file = "MINLPLib::alkyl", options=list(max_cpu_time=300, outlev=3))
    #'
    res = list()
    nl_file = file.path(tempdir(), "problem.nl")
    if (startsWith(file, "MINLPLib::")){
        check = get_minlplib_problem(strsplit(file, "::")[[1]][2])
        if(!check){
            return(res)
        }
    } else {
        if (!file.exists(file)){
            warning(paste("File", file, "not found", sep=" "))
            return(res)
        }
        file.copy(from = file, to = nl_file)
    }
    if (check_solver(solver)){
        available_options = get_available_options(solver)
        command = paste(solver, nl_file, sep=" ")
        for (x in names(options)) {
            if (x %in% available_options){
                if (solver == "raposa"){
                    command = paste(command, paste("-", x, "=", options[[x]],sep=""), sep=" ")
                } else {
                    command = paste(command, paste(x, options[[x]],sep="="), sep=" ")
                }
            } else {
                warning(paste("Option", x, "is not a valid option", sep=" "))
            }
        }
        command = paste(command, "wantsol=1", sep=" ")
        system(command)
        file.remove(nl_file)
        solfile = file.path(tempdir(), "problem.sol")
        if (file.exists(solfile)) {
            res = read_sol_file(solfile)
            file.remove(solfile)
        } else {
            warning("Error reading .sol file")
        }
        return(res)
    }
}
