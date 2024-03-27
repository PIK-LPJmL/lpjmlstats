test_that("years subset option should produce ouputs with the configured
           years",
          {

            # configure a subset of years
            set_lpjmlstats_settings(year_subset = 1:5)

            baseline_dir <- testthat::test_path("../testdata/path1")
            under_test_dir <-
              testthat::test_path("../testdata/path2")

            settings <- list(soiln = list(GlobSumTimeseries))

            out <-
              benchmark(baseline_dir,
                        under_test_dir,
                        settings,
                        pdf_report = FALSE)

            # check that the years subset is as expected
            time_ser <-
              out$GlobSumTimeseries$var_grp_list$soiln$under_test$pth2$data

            expect_equal(length(time_ser), 5)
          })
