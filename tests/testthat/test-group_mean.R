test_that("grouped mean works", {
  correct <- tibble::tibble(group=as.factor(c("ctrl", "trt1", "trt2")),
                    n = as.integer(10),
                    mean = c(5.03, 4.66, 5.53))

  output <- group_mean(PlantGrowth, group, weight) %>%
    dplyr::mutate(mean= round(mean, 2))

  expect_identical(output, correct)
})
