#
# CACHEMATRIX.PLOT.R
#
# Plots matrix size vs
#
rm(list = ls())
source("cachematrix.R")
library(ggplot2)

min.matrix.size = 1
max.matrix.size = 50
time.iterations <- 5000

makeMatrix <- function(matrix.size) {
    # make a square matrix of positive random numbers 
    matrix(runif(matrix.size ^ 2, min = 1, max = 50),
           nrow = matrix.size,
           ncol = matrix.size)
}

matrix.sizes <- min.matrix.size:max.matrix.size

# this will time how long it takes to solve the matrix, given a solver function.
# We do multiple iterations because the stopwatch will give us very small
# numbers otherwise, and rouning errors will make the ratios useless.
timeSolve <- function(x, solveFn) {
    i <- 1
    uncachedTime <- system.time({
        while (i <  time.iterations) {
            solveFn(x)
            i <- i + 1
        }
    })
    
    averageInMicroseconds <- (uncachedTime["elapsed"] * 1000000) / time.iterations
}

result = data.frame(size = matrix.sizes)
result$matrix = sapply(matrix.sizes, makeMatrix)
result$cachematrix = Map(makeCacheMatrix, result$matrix)
result$uncachedTime = sapply(result$matrix, timeSolve, solveFn = solve)
result$cachedTime = sapply(result$cachematrix, timeSolve, solveFn = cacheSolve)
result$ratio = result$uncachedTime / result$cachedTime

pic <- ggplot() +
    labs(title="cached vs uncached performance", 
         x="size of square matrix solved", 
         y="average time to solve in microseconds") + 
    geom_line(data = result,
              aes(
                  x = result$size,
                  y = result$uncachedTime,
                  color = "uncached"
              )) +
    geom_line(data = result,
              aes(
                  x = result$size,
                  y = result$cachedTime,
                  color = "cached"
              ))

print(pic)
