temperature <-
function(ndays, thiswth) {

    A <- thiswth$temperature$A
    B <- thiswth$temperature$B
    C <- thiswth$temperature$C

    # simulate daily temperature
    sapply(seq_len(ndays), function(i){A+B*sin(2*pi/365*(i+365*3/4-C))})
}

