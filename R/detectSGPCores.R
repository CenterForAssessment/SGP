`detectSGPCores` <-
function(logical=TRUE) {
    return(min(detectCores(logical=logical), 100L))
} ### END detectSGPCores Function
