context("Format")

test_that("Calendars supported", {

   expect_identical('gregorian',GSDF.time('1969-12-03:06:01','gregorian')$calendar)
   expect_identical('gregorian',GSDF.time('1969-12-03:06:01','standard')$calendar)
   expect_identical('gregorian',GSDF.time('1969-12-03:06:01','proleptic_gregorian')$calendar)
   expect_identical('360_day',GSDF.time('1969-12-03:06:01','360_day')$calendar)
   expect_identical('365_day',GSDF.time('1969-12-03:06:01','365_day')$calendar)
   expect_identical('365_day',GSDF.time('1969-12-03:06:01','noleap')$calendar)
   expect_error(GSDF.time('1969-12-03:06:01','julian'),
                'Unsupported calendar julian')

})

test_that("Time format supported", {

   expect_identical('1969-12-03:06:01',GSDF.time('1969-12-03:06:01','gregorian')$date)
   expect_identical('1969-12-03:06:01',GSDF.time('1969-12-03:06:01:00','gregorian')$date)
   expect_error(GSDF.time('12 March 1969','gregorian'),
                '12 March 1969 is not in format YYYY-MM-DD:HH:MM')
   expect_error(GSDF.time('1969-12-33:06:01','gregorian'),
                '1969-12-33:06:01 is not a valid gregorian date')
   expect_identical('1969-12-31:06:01',GSDF.time('1969-12-31:06:01:00','gregorian')$date)
   expect_error(GSDF.time('1969-12-31:06:01','360_day'),
                '1969-12-31:06:01 is not a valid 360_day date')
   expect_error(GSDF.time('1969-02-29:06:01','gregorian'),
                '1969-02-29:06:01 is not a valid gregorian date')
   expect_identical('1968-02-29:06:01',GSDF.time('1968-02-29:06:01:00','gregorian')$date)
   expect_error(GSDF.time('1968-02-29:06:01','365_day'),
                '1968-02-29:06:01 is not a valid 365_day date')
})

context("Increments: gregorian")

test_that("Gregorian increments work", {

   expect_identical(GSDF.time('1969-12-03:06:02','gregorian'),
                    GSDF.time.increment(GSDF.time('1969-12-03:06:01','gregorian'),
                                        1,'minutes'))
   expect_identical(GSDF.time('1969-12-03:06:00','gregorian'),
                    GSDF.time.increment(GSDF.time('1969-12-03:06:01','gregorian'),
                                        -1,'minutes'))
   expect_identical(GSDF.time('1969-12-03:07:41','gregorian'),
                    GSDF.time.increment(GSDF.time('1969-12-03:06:01','gregorian'),
                                        100,'minutes'))
   expect_identical(GSDF.time('1969-12-03:07:01','gregorian'),
                    GSDF.time.increment(GSDF.time('1969-12-03:06:01','gregorian'),
                                        1,'hours'))
   expect_identical(GSDF.time('1969-12-03:05:01','gregorian'),
                    GSDF.time.increment(GSDF.time('1969-12-03:06:01','gregorian'),
                                        -1,'hours'))
   expect_identical(GSDF.time('1969-12-07:10:01','gregorian'),
                    GSDF.time.increment(GSDF.time('1969-12-03:06:01','gregorian'),
                                        100,'hours'))
   expect_identical(GSDF.time('1969-12-04:06:01','gregorian'),
                    GSDF.time.increment(GSDF.time('1969-12-03:06:01','gregorian'),
                                        1,'days'))
   expect_identical(GSDF.time('1969-12-02:06:01','gregorian'),
                    GSDF.time.increment(GSDF.time('1969-12-03:06:01','gregorian'),
                                        -1,'days'))
   expect_identical(GSDF.time('1970-03-13:06:01','gregorian'),
                    GSDF.time.increment(GSDF.time('1969-12-03:06:01','gregorian'),
                                        100,'days'))
   expect_identical(GSDF.time('1970-01-03:06:01','gregorian'),
                    GSDF.time.increment(GSDF.time('1969-12-03:06:01','gregorian'),
                                        1,'months'))
   expect_identical(GSDF.time('1969-11-03:06:01','gregorian'),
                    GSDF.time.increment(GSDF.time('1969-12-03:06:01','gregorian'),
                                        -1,'months'))
   expect_identical(GSDF.time('1978-04-03:06:01','gregorian'),
                    GSDF.time.increment(GSDF.time('1969-12-03:06:01','gregorian'),
                                        100,'months'))
   expect_identical(GSDF.time('1970-12-03:06:01','gregorian'),
                    GSDF.time.increment(GSDF.time('1969-12-03:06:01','gregorian'),
                                        1,'years'))
   expect_identical(GSDF.time('1968-12-03:06:01','gregorian'),
                    GSDF.time.increment(GSDF.time('1969-12-03:06:01','gregorian'),
                                        -1,'years'))
   expect_identical(GSDF.time('2069-12-03:06:01','gregorian'),
                    GSDF.time.increment(GSDF.time('1969-12-03:06:01','gregorian'),
                                        100,'years'))

})
context("Increments: 360_day")

test_that("360_day increments work", {

   expect_identical(GSDF.time('1969-12-03:06:02','360_day'),
                    GSDF.time.increment(GSDF.time('1969-12-03:06:01','360_day'),
                                        1,'minutes'))
   expect_identical(GSDF.time('1969-12-03:06:00','360_day'),
                    GSDF.time.increment(GSDF.time('1969-12-03:06:01','360_day'),
                                        -1,'minutes'))
   expect_identical(GSDF.time('1969-12-03:07:41','360_day'),
                    GSDF.time.increment(GSDF.time('1969-12-03:06:01','360_day'),
                                        100,'minutes'))
   expect_identical(GSDF.time('1969-12-03:07:01','360_day'),
                    GSDF.time.increment(GSDF.time('1969-12-03:06:01','360_day'),
                                        1,'hours'))
   expect_identical(GSDF.time('1969-12-03:05:01','360_day'),
                    GSDF.time.increment(GSDF.time('1969-12-03:06:01','360_day'),
                                        -1,'hours'))
   expect_identical(GSDF.time('1969-12-07:10:01','360_day'),
                    GSDF.time.increment(GSDF.time('1969-12-03:06:01','360_day'),
                                        100,'hours'))
   expect_identical(GSDF.time('1969-12-04:06:01','360_day'),
                    GSDF.time.increment(GSDF.time('1969-12-03:06:01','360_day'),
                                        1,'days'))
   expect_identical(GSDF.time('1969-12-02:06:01','360_day'),
                    GSDF.time.increment(GSDF.time('1969-12-03:06:01','360_day'),
                                        -1,'days'))
   expect_identical(GSDF.time('1970-03-13:06:01','360_day'),
                    GSDF.time.increment(GSDF.time('1969-12-03:06:01','360_day'),
                                        100,'days'))
   expect_identical(GSDF.time('1970-01-03:06:01','360_day'),
                    GSDF.time.increment(GSDF.time('1969-12-03:06:01','360_day'),
                                        1,'months'))
   expect_identical(GSDF.time('1969-11-03:06:01','360_day'),
                    GSDF.time.increment(GSDF.time('1969-12-03:06:01','360_day'),
                                        -1,'months'))
   expect_identical(GSDF.time('1978-04-03:06:01','360_day'),
                    GSDF.time.increment(GSDF.time('1969-12-03:06:01','360_day'),
                                        100,'months'))
   expect_identical(GSDF.time('1970-12-03:06:01','360_day'),
                    GSDF.time.increment(GSDF.time('1969-12-03:06:01','360_day'),
                                        1,'years'))
   expect_identical(GSDF.time('1968-12-03:06:01','360_day'),
                    GSDF.time.increment(GSDF.time('1969-12-03:06:01','360_day'),
                                        -1,'years'))
   expect_identical(GSDF.time('2069-12-03:06:01','360_day'),
                    GSDF.time.increment(GSDF.time('1969-12-03:06:01','360_day'),
                                        100,'years'))

})

context("Increments: 365_day")

test_that("365_day increments work", {

   expect_identical(GSDF.time('1969-12-03:06:02','365_day'),
                    GSDF.time.increment(GSDF.time('1969-12-03:06:01','365_day'),
                                        1,'minutes'))
   expect_identical(GSDF.time('1969-12-03:06:00','365_day'),
                    GSDF.time.increment(GSDF.time('1969-12-03:06:01','365_day'),
                                        -1,'minutes'))
   expect_identical(GSDF.time('1969-12-03:07:41','365_day'),
                    GSDF.time.increment(GSDF.time('1969-12-03:06:01','365_day'),
                                        100,'minutes'))
   expect_identical(GSDF.time('1969-12-03:07:01','365_day'),
                    GSDF.time.increment(GSDF.time('1969-12-03:06:01','365_day'),
                                        1,'hours'))
   expect_identical(GSDF.time('1969-12-03:05:01','365_day'),
                    GSDF.time.increment(GSDF.time('1969-12-03:06:01','365_day'),
                                        -1,'hours'))
   expect_identical(GSDF.time('1969-12-07:10:01','365_day'),
                    GSDF.time.increment(GSDF.time('1969-12-03:06:01','365_day'),
                                        100,'hours'))
   expect_identical(GSDF.time('1969-12-04:06:01','365_day'),
                    GSDF.time.increment(GSDF.time('1969-12-03:06:01','365_day'),
                                        1,'days'))
   expect_identical(GSDF.time('1969-12-02:06:01','365_day'),
                    GSDF.time.increment(GSDF.time('1969-12-03:06:01','365_day'),
                                        -1,'days'))
   expect_identical(GSDF.time('1970-03-13:06:01','365_day'),
                    GSDF.time.increment(GSDF.time('1969-12-03:06:01','365_day'),
                                        100,'days'))
   expect_identical(GSDF.time('1970-01-03:06:01','365_day'),
                    GSDF.time.increment(GSDF.time('1969-12-03:06:01','365_day'),
                                        1,'months'))
   expect_identical(GSDF.time('1969-11-03:06:01','365_day'),
                    GSDF.time.increment(GSDF.time('1969-12-03:06:01','365_day'),
                                        -1,'months'))
   expect_identical(GSDF.time('1978-04-03:06:01','365_day'),
                    GSDF.time.increment(GSDF.time('1969-12-03:06:01','365_day'),
                                        100,'months'))
   expect_identical(GSDF.time('1970-12-03:06:01','365_day'),
                    GSDF.time.increment(GSDF.time('1969-12-03:06:01','365_day'),
                                        1,'years'))
   expect_identical(GSDF.time('1968-12-03:06:01','365_day'),
                    GSDF.time.increment(GSDF.time('1969-12-03:06:01','365_day'),
                                        -1,'years'))
   expect_identical(GSDF.time('2069-12-03:06:01','365_day'),
                    GSDF.time.increment(GSDF.time('1969-12-03:06:01','365_day'),
                                        100,'years'))

})

context("Base and Offset")

test_that("NetCDF base and increments are converted correctly", {

   expect_identical(GSDF.time(c("0000-01-01:00:00",
                                "0000-01-01:01:00"),'gregorian'),
              GSDF.time.from.base.and.offset(
                                c(0,1),
                               "0000-01-01:00",'hours','gregorian'))
    expect_identical(GSDF.time(c("1988-12-30:18:00",
                                 "1988-12-30:21:00",
                                 "1989-01-01:00:00"),'360_day'),
              GSDF.time.from.base.and.offset(
                                c(3719.75, 3719.875,3720),
                               "1978-09-01",'days','360_day'))

})
 
