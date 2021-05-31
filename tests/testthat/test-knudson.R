library(tibble)
library(dplyr)

context("knudson testing")

# First we can test some basics:
test_that("error throwing for pp_knudson()", {
    expect_error(pp_knudson())
    expect_error(pp_knudson(sex = 1, age = 12, height = 13)) # missing spiro
    expect_error(pp_knudson(sex = 1, age = 12, fev = 1)) # missing height
    expect_error(pp_knudson(sex = 1, height = 1, fev = 1)) # missing age
    expect_error(pp_knudson(age = 6.5, height = 14, fvc = 2)) # missing sex
    # We don't allow multiple spiro variables by design:
    expect_error(pp_knudson(fev = 12, fvc = 27, age = 12, sex = 1, height = 120))
})

test_that("error throwing for predicted_knudson()", {
    expect_error(predicted_knudson(sex = 1, age = 12, height = 130,
                                   measure = "incomprehensible_gibberish"))
    expect_error(predicted_knudson(sex = 1, age = 12, height = 13)) # missing measure
    expect_error(predicted_knudson(sex = 1, age = 12, measure = "fev")) # missing height
    expect_error(predicted_knudson(sex = 1, height = 1, measure = "fvc")) # missing age
    expect_error(predicted_knudson(age = 6.5, height = 14, measure = "fvc")) # missing sex
    # We don't allow multiple spiro variables by design:
    expect_error(predited_knudson(measure = c("fev", "fvc"), age = c(12,12),
                            sex = c(1,1), height = c(120,120)))
})

test_that("pp_knudson() returns appropriate NA values + warnings", {
    # expect_equivalent doesn't care about att
    expect_true(is.na(suppressWarnings(
        pp_knudson(sex = NA, age = 12, height = 130, fev = 2))))
    expect_true(is.na(suppressWarnings(
        pp_knudson(sex = 1, age = NA, height = 130, fev = 2))))
    expect_true(is.na(suppressWarnings(
        pp_knudson(sex = 1, age = 12, height = NA, fev = 2))))
    expect_true(is.na(suppressWarnings(
        pp_knudson(sex = 1, age = 12, height = 130, fev = NA))))
    # when sex is not 1 or 2, or age is less than 6, we also want NA:
    expect_true(is.na(suppressWarnings(
        pp_knudson(sex = 3, age = 12, height = 130, fev = 2))))
    expect_true(is.na(suppressWarnings(
        pp_knudson(sex = 1, age = 5, height = 130, fev = 2))))
    expect_warning(pp_knudson(sex = 1, age = 5, height = 251, fev = 2))
})

test_that("predicted_knudson() returns appropriate NA values + warnings", {
    # expect_equivalent doesn't care about att
    expect_true(is.na(predicted_knudson(sex = NA, age = 12, height = 130, measure = "fev")))
    expect_true(is.na(predicted_knudson(sex = 1, age = NA, height = 130, measure = "fvc")))
    expect_true(is.na(predicted_knudson(sex = 1, age = 12, height = NA, measure = "fef2575")))
})

# We hand-calculate two test cases for each age/sex stratum from Table 1 of the
# Knudson publication.
# Ages were arbritrarily selected and the heights were selected from the approximate
# middle of the CDC tables so we're not testing wacky values.
base_for_tests <- tibble::tribble(
    ~sex, ~age, ~ height,
    1, 8.3, 130,
    1, 11.0, 140,
    1, 13.7, 155,
    1, 17.1, 175,
    1, 29.2, 180,
    1, 45.2, 170,
    2, 7.1, 115,
    2, 10.9, 137,
    2, 11.2, 135,
    2, 19.6, 165,
    2, 20.3, 160,
    2, 65.2, 155)


# We're using fev = 1 for all since that's just a ratio, the testing can be a bit
# relaxed once we've checked the predicted values.

fev_test_cases <- base_for_tests %>%
    dplyr::mutate(fev = 1, measure = "fev") %>%
    # we're not going to be careful about missing data and such here, we
    # only need to handle the test cases.
    dplyr::mutate(manual_pred = dplyr::case_when(
        sex == 1 & age < 12 ~ -2.8142 + 0.0348*height,
        sex == 1 & age < 25 ~ -6.1181 + 0.0519*height + 0.0636*age,
        sex == 1 & age < Inf ~ -6.5147 + 0.0665*height - 0.0292*age,
        sex == 2 & age < 11 ~ -2.7578 + 0.0336*height,
        sex == 2 & age < 20 ~ -3.7622 + 0.0351*height + 0.0694*age,
        sex == 2 & age < Inf ~ -1.4050 + 0.0309*height + -0.0201*age,
        T ~ -Inf
    )) %>%
    dplyr::mutate(manual_pp = fev/manual_pred*100) %>%
    # use the package to compute these values:
    dplyr::mutate(
        package_pred = predicted_knudson(sex = sex, age = age,
                                         height = height, measure = measure),
        package_pp = pp_knudson(sex = sex, age = age,
                                height = height, fev = fev)) %>%
    dplyr::mutate(pred_residual = package_pred-manual_pred,
                  pp_residual = package_pp-manual_pp)

test_that("FEV tests for pp_knudson() and predicted_knudson()", {
    # They should be literally identical, but we want to allow for
    # a tiny bit of flexibility just for consistency with other spiro methods.
    expect_lt(max(abs(pull(fev_test_cases, pred_residual))), 0.01)
    expect_lt(max(abs(pull(fev_test_cases, pp_residual))), 0.01)
})





