context("Generate climate data.")
library(VFS)

# import weather data 1980-2009
weather <- read.dly(system.file("extdata", "USC00368449.dly", package = "VFS"))
weather.param  <- wth.param(weather)

# works with param list and with individual variables

# set seed, because of stochastic component
set.seed(1234)
rain1 <- rainfall(365, weather.param)

set.seed(1234)
rain2 <- rainfall(365, depth=weather.param$depth, lambda=weather.param$lambda)

temp1 <- temperature(365, weather.param)
temp2 <- temperature(365, A=weather.param$A, B=weather.param$B, C=weather.param$C)

test_that("param list and separate variables give the same result", {
    expect_equal(rain1, rain2)
    expect_equal(temp1, temp2)
})


