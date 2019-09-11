`ddcast` <-
function(tmp.dt, ...) {
        if (dim(tmp.dt)[1L]==0L) {
                return(data.table(NULL))
        } else {
                dcast(tmp.dt, ...)
        }
} ### END ddcast Function
