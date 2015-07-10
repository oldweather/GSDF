source('bogus.R') # Load the test fields

context("NoLeap") 

no.leap<-Unleap(has.leap)
test_that("Remove data for Feb 29th", {

   expect_equal(2,length(no.leap$dimensions[[3]]$values))
   expect_equal(has.leap$dimensions[[3]]$values[3],
                    no.leap$dimensions[[3]]$values[2])

})

context("AddLeap") 

test_that("Add data for Feb 29th", {

   re.leap<-AddLeap(no.leap)

   expect_equal(3,length(re.leap$dimensions[[3]]$values))
   expect_equal(has.leap$data[,,1],re.leap$data[,,2])

})
