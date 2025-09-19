## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(terra)
library(enerscape)

## ----sirente, fig.width=6, fig.height=3.65------------------------------------
data("sirente")
dem <- rast(
  sirente, 
  crs = "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs",
  extent = ext(879340, 885280, 4672880, 4676810)
)
plot(dem)

## ----crs----------------------------------------------------------------------
crs(dem, proj = TRUE)

## ----enerscape----------------------------------------------------------------
en <- enerscape(dem, 10, neigh = 4, unit = "kcal")

## ----visualize, fig.width=6, fig.height=3.65----------------------------------
en_log <- log(en)
plot(en_log)
terra::contour(dem, add = TRUE, nlevels = 5)

## ----zoom, fig.width=6, fig.height=3.65---------------------------------------
en_log <- crop(en_log, ext(881306, 882853, 4674928, 4675984))
plot(en_log)
terra::contour(dem, add = TRUE, nlevels = 20)

## ----circuitscape, eval=FALSE-------------------------------------------------
# # two random points
# p <- spatSample(en, 2, xy = TRUE)[, c("x", "y")]
# circuitscape_skeleton(en, path = tempdir(), points = p)

## ----compute, eval=FALSE------------------------------------------------------
# julia> using Circuitscape
# julia> compute("circuitscape.ini")
# 
# [ Info: 2022-12-16 10:05:18 : Precision used: Double
# [ Info: 2022-12-16 10:05:18 : Reading maps
# [ Info: 2022-12-16 10:05:18 : Resistance/Conductance map has 231472 nodes
# [ Info: 2022-12-16 10:05:21 : Solver used: AMG accelerated by CG
# [ Info: 2022-12-16 10:05:22 : Graph has 231472 nodes, 2 focal points and 1 connected components
# [ Info: 2022-12-16 10:05:22 : Total number of pair solves = 1
# [ Info: 2022-12-16 10:05:24 : Time taken to construct preconditioner = 2.510170134 seconds
# [ Info: 2022-12-16 10:05:24 : Time taken to construct local nodemap = 0.021110616 seconds
# [ Info: 2022-12-16 10:05:27 : Solving pair 1 of 1
# [ Info: 2022-12-16 10:05:27 : Time taken to solve linear system = 0.512068804 seconds
# [ Info: 2022-12-16 10:05:28 : Time taken to calculate current maps = 0.82549585 seconds
# [ Info: 2022-12-16 10:05:29 : Time taken to complete job = 11.5802219
# 3×3 Matrix{Float64}:
#  0.0  1.0       2.0
#  1.0  0.0       0.559379
#  2.0  0.559379  0.0

## ----omniscape, eval=FALSE----------------------------------------------------
# omniscape_skeleton(en, path = tempdir(), radius = 10)

## ----run, eval=FALSE----------------------------------------------------------
# julia> using Oircuitscape
# julia> run_omniscape("circuitscape.ini")
# 
# [ Info: Starting up Omniscape with 1 workers and double precision
# [ Info: Using Circuitscape with the cg+amg solver...
# [ Info: Solving moving window targets...
#  days,Progress: 100%|                                | Time: 0:08:08
# [ Info: Time taken to complete job: 490.2733 seconds
# [ Info: Outputs written to /tmp/RtmpXZb2Hv//tmp/RtmpXZb2Hv/omniscape
# (Union{Missing, Float64}[missing missing … missing missing; missing 3.4477693535486376 … 4.462051188132685 missing; … ; missing 10.365746449782868 … 4.186837622531591 missing; missing missing … missing missing], Union{Missing, Float64}[missing missing … missing missing; missing 0.652059842432581 … 0.743080786816455 missing; … ; missing 0.7753872880414857 … 0.7333620170587554 missing; missing missing … missing missing])

## ----human speed costs, echo=FALSE, fig.width=6, fig.height=3.65--------------
d <- data.frame(
  v = seq(.1, 5.1, length.out = 41),
  slope = seq(-25, 25, length.out = 41)
)
d <- expand.grid(d)
d$cost <- NA
for (i in seq_len(nrow(d))) {
  d$cost[i] <- energyHuman(
    mass = 78,
    v = d$v[i],
    slope = d$slope[i] / 180 * pi,
    distance = 1,
    res = 1,
    kcal = FALSE
  )
}
oldpar <- par(no.readonly = TRUE)
par(mfrow = c(1, 2))
with(
  d[d$v == 1.1, ],
  plot(
    slope, cost,
    main = "v = 1.1",
    type = "b", pch = 21, bg = "dodgerblue", cex = 1,
    xlab = expression("Slope "* (rad)),
    ylab = expression("Energy cost "* (kcal))
  )
)
with(
  d[d$slope == 0, ],
  plot(
    v, cost, 
    main = "Slope = 0",
    type = "b", pch = 21, bg = "dodgerblue", cex = 1,
    xlab = expression("Speed "* (m/s)),
    ylab = expression("Energy cost "* (kcal))
  )
)
abline(v = 1.39, lty = 2)
text(x = 1.39, y = .2, "v = 1.39", srt = 90, adj = c(0, -0.5))
par(oldpar)

## ----humanscape, fig.width=6, fig.height=3.65---------------------------------
en <- lapply(
  seq(0.5, 2.5, by = .5),
  \(v) humanscape(dem, 10, v = v, neigh = 4, unit = "kcal")
)
en <- rast(en)
names(en) <- paste0("v = ", seq(0.5, 2.5, by = .5))
en_log <- log(en)
panel(en_log, nc = 3)

