timetakenSGP <-
function(started.at) {

        format = function(secs) {
            secs.integer = as.integer(secs)
            sprintf("%02d:%02d:%02d:%.3f", secs.integer%/%86400L, (secs.integer%/%3600L)%%24L, (secs.integer%/%60L)%%60L, secs%%60L)
        }
        tt = proc.time() - started.at
        format(tt[3L])
} ### END timetakenSGP function
