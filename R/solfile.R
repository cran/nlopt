read_sol_file <- function(solfile){
    #' @title Reads the .sol file
    #' @description Function that reads the corresponding .sol file
    #' @param solfile .sol file with the solution
    #' @return list with a string of the output given by the solver (output), the optimal value of the problem (objective), the status returned by the solver (status), the optimal primal solution (primal_solution), and the optimal dual solution (dual_solution)
    #' @examples
    #' read_sol_file(solfile = "example.sol")
    #'
    example = FALSE
    if (!file.exists(solfile)){
        if (solfile == "example.sol"){
            file_lines = example_sol_file()
            solfile = file.path(tempdir(), "example.sol")
            writeLines(file_lines, solfile)
            example = TRUE
        } else {
            warning(paste("File", solfile, "not found", sep=" "))
            return(list(output=NULL, objective=NULL, status=NULL, primal_solution=NULL, dual_solution=NULL))
        }
    }
    con <- file(solfile, "r")
    status = NULL
    primal_solution = c()
    dual_solution = c()
    options = FALSE
    dual = FALSE
    dual_count = -1
    n_dual = -1
    primal = FALSE
    primal_count = -1
    n_primal = -1
    nopt = -1
    nopt_counter = 0
    obj_val = NULL
    output_bool = TRUE
    output = c()
    while (TRUE) {
        line <- readLines(con, n = 1)
        if (length(line) == 0) {
            break
        }
        if (startsWith(line, "objno")){
            status = as.integer(strsplit(line, " ")[[1]][3])
        }
        if (primal){
            if (primal_count >= 0){
                primal_solution = c(primal_solution, as.double(line))
                primal_count = primal_count + 1
                if (primal_count == n_primal){
                    primal = FALSE
                    n_primal = -1
                }
            }
        }
        if (dual){
            if (dual_count >= 0){
                dual_solution = c(dual_solution, as.double(line))
                dual_count = dual_count + 1
                if (dual_count == n_dual){
                    dual = FALSE
                    n_dual = -1
                    if (n_primal == 0){
                        primal = FALSE
                        n_primal = -1
                    } else {
                        primal_count = 0
                        primal = TRUE
                    }
                }
            } else {
                line <- readLines(con, n = 1)
                n_dual = as.integer(line)
                line <- readLines(con, n = 1)
                line <- readLines(con, n = 1)
                n_primal = as.integer(line)
                if (n_dual == 0){
                    primal = TRUE
                    primal_count = 0
                    dual = FALSE
                    n_dual = -1
                } else {
                    dual_count = 0
                }
            }
        }
        if (options){
            if (nopt == -1){
                nopt = as.integer(line)
            } else {
                nopt_counter = nopt_counter + 1
                if (nopt_counter == nopt){
                    options = FALSE
                    nopt = -1
                    dual = TRUE
                }
            }
        }
        if (startsWith(line, "Options")){
            options = TRUE
            output_bool = FALSE
        }
        if (output_bool & line != " " & line != ""){
            output = c(output, line)
        }
        if (output_bool & grepl("Objective", line)) {
            obj_val = as.double(str_extract(line, "(?i)(?<=Objective\\s)-?\\d+\\.\\d+"))
        } else if (output_bool & grepl("objective", line)) {
            obj_val = as.double(str_extract(line, "(?i)(?<=objective\\s)-?\\d+\\.\\d+"))
        }
    }
    close(con)
    if (example){
        file.remove(solfile)
    }
    return(list(output=output, objective=obj_val, status=status, primal_solution=primal_solution, dual_solution=dual_solution))
}

example_sol_file <- function(){
    #' @title Returns an example.sol file
    #' @description Function that returns an example.sol file
    #' @return An example.sol file
    #' @examples
    #' example_sol_file()
    #'
    file_lines = c(
        "Ipopt 3.12.13: Optimal Solution Found",
        "objective -1.7650016448383354",
        "",
        "Options",
        "3",
        "0",
        "1",
        "0",
        "7",
        "7",
        "14",
        "14",
        "12.709612764748437",
        "0.03500001623670092",
        "-0.6797558044817047",
        "-23.092094501040588",
        "0.3129895562801825",
        "-7.0185537552809905",
        "-5.133149912865754",
        "3.0358220469343755",
        "0.95",
        "1.7037028800994893",
        "0.543083999692675",
        "0.9013193967660605",
        "10.475474992395208",
        "1.561635455874417",
        "1.5353535803661131",
        "0.99",
        "0.99",
        "1.11111111111111",
        "0.99",
        "1.5847096897793138",
        "2",
        "objno 0 0",
        "suffix 4 14 13 0 0",
        "ipopt_zU_out",
        "0 -1.2758026797994728e-09",
        "1 -17.32016688390056",
        "2 -8.457400475809988e-09",
        "3 -3.814648398925517e-09",
        "4 -8.737274978271836e-08",
        "5 -1.6437286264281646e-09",
        "6 -1.0276984729823613e-09",
        "7 -2.960436000630468e-08",
        "8 -1.2466543689173837e-07",
        "9 -1.246654300172923e-07",
        "10 -0.4887756002756028",
        "11 -1.246654307513047e-07",
        "12 -1.6394831054055265e-07",
        "13 -0.8824999460681403",
        "suffix 4 14 13 0 0",
        "ipopt_zL_out",
        "0 8.254448096040982e-10",
        "1 5.0118051291383244e-08",
        "2 1.4708571398300825e-09",
        "3 4.614209947729903e-09",
        "4 4.882955335375897e-08",
        "5 3.352166433374034e-10",
        "6 6.929363357007531e-09",
        "7 2.9359086323286075e-08",
        "8 2.0636177824426754",
        "9 21.93749012823394",
        "10 1.1870068594735836e-08",
        "11 10.77596176182813",
        "12 1.581301373565268e-09",
        "13 1.2529517628245138e-09"
    )
    return (file_lines)
}
