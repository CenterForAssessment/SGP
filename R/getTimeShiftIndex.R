getTimeShiftIndex <-
function(my.data,
                my.matrix,
                time.shift=365,
                time.buffer=30) {
    (-20:20)[which.max(sapply(-20:20, function(x) my.matrix@Version[['Matrix_Information']][['SGPt']][['MAX_TIME']]+time.buffer+time.shift*x - as.numeric(max(my.data[['TIME']]))) > 0)]
} ### END getTimeShiftIndex
