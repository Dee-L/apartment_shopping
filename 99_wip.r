test_function <- function(object_to_test) {
    if (length(object_to_test) == 0) {
        class_to_cast <- class(object_to_test)
        object_to_test <- NA
        object_to_test %<>%
            as(., class_to_cast)
        object_to_test
    }
}



test_function(as.numeric())