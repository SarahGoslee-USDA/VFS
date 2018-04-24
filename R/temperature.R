temperature <-
function(ndays, thiswth, A, B, C) {

    if(!missing(thiswth)) {
        A <- thiswth$A
        B <- thiswth$B
        C <- thiswth$C
    }

    # simulate daily temperature
    sapply(seq_len(ndays), function(i){A+B*sin(2*pi/365*(i+365*3/4-C))})
}

