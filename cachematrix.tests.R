#
# AUTOMATED TESTS
#
# The code below test that the functions are written correctly.
#
rm(list=ls())

source("cachematrix.R")

library(testthat)

local({
    
    # Three matrix variables to help write tests; the 3x3 identity matrix, a
    # matrix we want to invert (A), and its inverse (A_1). A and A_1 were taken
    # from http://www.purplemath.com/modules/mtrxinvr2.htm
    A   <- matrix(c(  1,   2,   3,
                      0,   1,   4,
                      5,   6,   0), nrow = 3, ncol = 3)

    A_1 <- matrix(c(-24,  18,   5,
                     20, -15,  -4,
                     -5,   4,   1), nrow = 3, ncol = 3)

    I   <- matrix(c(  1,   0,   0,
                      0,   1,   0,
                      0,   0,   1), nrow = 3, ncol = 3)
    
    test_that("makeCacheMatrix creates a new cache with the right content", {
        cacheMatrix1 <- makeCacheMatrix(A)
        expect_identical(A, cacheMatrix1$get())
    })
    
    test_that("makeCacheMatrix$set overwrites the existing matrix", {
        cacheMatrix <- makeCacheMatrix(A)
        cacheMatrix$set(A_1)
        expect_equal(A_1, cacheMatrix$get())
    })
    
    test_that("cached inversion gives the same as normal inversion", {
        # now let's use R's built-n solver; this just proves the test data is OK
        expect_equal(A_1, solve(A)) # A_1 is the inverse of A
        expect_equal(I, A %*% A_1)  # proves that the inverse is correct.
        
        # let's run our own caching version of solve;
        cacheMatrix <- makeCacheMatrix(A) 
        expect_equal(A_1, cacheSolve(cacheMatrix))
        expect_equal(I, cacheMatrix$get() %*% cacheSolve(cacheMatrix))
    })
    
    test_that("prove that the cache is actually faster!", {
        

    })
     
    test_that("makeCacheMatrix only accepts a mtrix", {
        expect_error(makeCacheMatrix("not a matrix"))
        expect_error({
            m <- makeCacheMatrix(matrix(1,1,1))
            m$set("not a matrix")
        })
    })
})
