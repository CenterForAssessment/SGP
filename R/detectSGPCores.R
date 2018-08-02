`detectSGPCores` <-
function(logical=TRUE) {
    return(min(detectCores(logical=logical), 50L))
} ### END detectSGPCores Function
