`detectSGPCores` <- 
function(logical=TRUE) {
    return(min(detectCores(logical=logical), 100))
} ### END detectSGPCores Function
