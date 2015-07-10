source('bogus.R') # Load the test fields

context("FindDimension") 

test_that("Dimensions identifiable from names", {

   expect_equal(1,FindDimension(checkers,'lat'))
   expect_equal(2,FindDimension(checkers,'lon'))
   expect_identical(NULL,FindDimension(checkers,'time'))

})

context("RollDimensions") 

test_that("Coordinates roll out", {

   expect_equal(c(1,2,1,2),RollDimensions(has.leap,1,2))
   expect_equal(c(3,4,3,4,3,4),RollDimensions(has.leap,'lon','time'))

})

context("ReduceDimension") 

test_that("Collapse dimensions", {

   cd<-ReduceDimension(circle,'lon',min)
   expect_equal(1,length(cd$dimensions))
   expect_equal(rep(0,36),cd$data)
   cd<-ReduceDimension(has.leap,'lon',function(x) {return(x[1])})
   expect_equal(2,length(cd$dimensions))
   expect_equal(array(data=c(1,2,5,6,9,10),dim=c(2,3)),cd$data)

})

context("Concatenate fields") 

test_that("Merge Fields", {

   md<-ConcatenateFields(has.leap,has.leap,'lat')
   expect_equal(4,length(md$dimensions[[1]]$values))
   expect_equal(c(1,2,1,2,3,4,3,4,5,6,5,6,7,8,7,8,9,10,9,10,11,12,11,12),
                as.vector(md$data))

})
