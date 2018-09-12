# Daily MUSLE sediment loss

MUSLE <- function(Q, qp, A, C = 0.085, P = 0.40, K, LS) {

# Q: runoff volume - m3/day
# qp: runoff peak discharge m3/s
# A: field area (ha)
# returns S: Sediment yield - t/day


    11.8 * (Q * qp * A) ^ 0.56 * (C * P * K * LS)

}

