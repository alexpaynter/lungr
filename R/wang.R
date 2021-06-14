#' @title Wang (1993) spirometry equations.
#'
#' @description Based on the Harvard Six Cities Study, derived for black and
#'   white children aged 6 to 18.
#'
#' @details Use \code{predicted_wang} to get the predicted spirometry based on race, sex and height of the child.  If a spirometry measurement is known, \code{pp_wang} can be used to calculate the percentage of predicted value instead.
#'
#' Regressions were derived for four measures: FVC, FEV1, FEV1/FVC and FEF25-75.
#' Reference: \href{https://doi.org/10.1002/ppul.1950150204}{https://doi.org/10.1002/ppul.1950150204}
#' @param sex Sex at birth.  "m" = male, "f" = female.
#' @param race Race.  "white", "black" or "other".
#' @param age Age at spirometry observation (years).
#' @param height Height in meters.
#' @param fev Forced expiratory volume in one second (L)
#' @param fvc Forced vital capacity (L).
#' @param fev_fvc fev/fvc ratio.
#' @param fef2575 Forced expiratory flow during the middle half (25-75\%) of
#'   FVC (L/s).
#' @param adult If F, the default, return NA for those 19 and older.  If T, continue to apply the 18 year old equations for adults.
#' @param ... Disregarded.  Input allowed so that methods which require
#'   additional parameters (e.g. race) can be fit with consistent syntax.
#' @name wang
NULL


#' @rdname wang
#' @export
pp_wang <- function(sex, race, age, height,
                    fev = NULL, fvc = NULL, fev_fvc = NULL,
                    fef2575 = NULL,
                    adult = F, ...) {
    measures <- list(fev = fev, fvc = fvc, fev_fvc = fev_fvc, fef2575 = fef2575)
    measures <- measures[!vapply(measures, is.null, logical(1))]

    # we only allow one spirometry value at a time:
    if (length(measures) != 1) {
        stop("This package is only designed to handle one spirometric
             variable (fev, fvc, etc.) at once.")
    }
    n <- length(measures[[1]])
    if (length(sex) != n || length(age) != n || length(height) != n) {
        stop("Input vectors (sex, age, height, fev, etc.)
             must all have the same length.")
    }

    if (any(height > 2.5 | height < 0.5, na.rm = T)) {
        warning("Height values above 250cm and or under 0.5cm were detected.
                This usually indicates the wrong unit (correct: meters), or a
                data error which needs to be resolved.")
    }

    user_df <- tibble::tibble(measure = names(measures),
                              raw_value = measures[[1]]) %>%
        dplyr::mutate(sex = sex, race = race, age = age, height = height) %>%
        dplyr::mutate(pred = predicted_wang(sex = .data$sex,
                                            race = .data$race,
                                            age = .data$age,
                                            height = .data$height,
                                            measure = .data$measure,
                                            adult = adult)) %>%
        dplyr::mutate(pct_pred = .data$raw_value/.data$pred*100)

    return(dplyr::pull(user_df, pct_pred))

}


#' @rdname wang
#' @export
predicted_wang <- function(sex, age, race, height, measure, adult = F) {
    stopifnot(is.character(measure))
    if (!all(measure %in% c("fev", "fvc", "fev_fvc", "fef2575"))) {
        # better to pulls these from the data.
        stop("measure is only supported for fev, fvc, fev_fvc, fef2575.")
    }
    # measure can either be length one, or the same as sex, age and height.
    n <- length(sex)
    if (length(measure) == n) {
        if (any(measure != measure[1])) {
            stop("measure of length > 1 is only supported if all are equal.
            For example, you must have (fev,fev,fev,...) not (fev,fvc,fef2575,...)")
        }
    } else if (length(measure) != 1) {
        stop("measure must be length 1, or the same length as age, sex height")
    }

    if (length(age) != n || length(height) != n) {
        stop("Input vectors (sex, age and height)
             must all have the same length.")
    }

    if (any(!(sex %in% c("m", "f") | is.na(sex)), na.rm = T)) {
        stop("invalid input for sex (must be 'm', 'f' or NA)")
    }

    if (adult) {
        age <- ifelse(age >= 19, 18, age)
    }

    user_df <- tibble::tibble(measure = measure, sex = sex, race = race,
                              age = age, height = height)

    # we choose which equation to use based on the age range (lb for lower bound)
    # and sex of each observation:
    user_df %<>% dplyr::mutate(age_lb = floor(age))

    user_df %<>% dplyr::left_join(., wang_coefs,
                                  by = c("measure", "sex", "race", "age_lb"))
    user_df %<>% dplyr::mutate(pred = exp(alpha)*height^beta)

    return(dplyr::pull(user_df, pred))

}
