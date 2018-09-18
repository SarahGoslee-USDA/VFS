# Daily MUSLE sediment loss

MUSLE <- function(Q, qp, A, K, LS, C = 0.085, P = 0.40, a = 11.8, b = 0.56) {

# Q: runoff volume - m3/day
# qp: runoff peak discharge m3/s
# A: field area (ha)
# returns S: Sediment yield - t/day


    a * (Q * qp * A) ^ b * (C * P * K * LS)

}

