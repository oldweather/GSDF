source('bogus.R') # Load the test fields

context("IsFull") 

test_that("Distingish between global and regional fields", {

   expect_identical(TRUE,IsFull(circle))
   expect_identical(FALSE,IsFull(uk))

})

context("PadLongitude") 

test_that("Expand Longitude for periodic boundary conditions", {

  pad.yes<-PadLongitude(circle)
  diagnostics<-c(length(pad.yes$dimensions[[2]]$values),
                 pad.yes$dimensions[[2]]$values[1],
                 pad.yes$dimensions[[2]]$values[74])

  expect_equal(c(74,-2.5,362.5),diagnostics)

})
