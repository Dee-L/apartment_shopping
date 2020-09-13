tryCatch(
    expr = {
    1 + 1
    stop("custom error")
    }
    , error = function(e) {
        message("You got this error: ")
        message(e)
        john <<- 2 + 2
        4 + 4
    }
)
