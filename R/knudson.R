#' @title Percentage of predicted based on Knudson (1983) spirometric equations.
#'
#' @description Calculates the percentage of the predicted spirometry variable
#'   (fev, fvc, etc.) using Knudson's work for the predicted values.
#'
#' @details RJ Knudson, MD Lebowitz, CJ Holberg and B Burrows published
#'   reference spirometry equations which account for sex, age and height, but
#'   not race.
#'
#'   The "percentage of predicted" is defined by (actual)/(predicted)*100.
#'
#'   In Knudson et. al. Table 1, they state the regression coefficients.  There
#'   are two fits offered, with one stratifying the female fits with breaks at
#'   11 and 20, and the other using breaks at 11, 20 and 70.  We have used the
#'   simpler model without a break at 70, owing to the standards in application
#'   areas such as cystic fibrosis research, and Knudson's own comments that the
#'   model with a break at 70 might be overfitting.  For a reference on this
#'   being a standard in cystic fibrosis please see Table 1 in Stanojevic et.
#'   al. (2014), "The impact of switching..." which can be found at
#'   \href{http://dx.doi.org/10.1016/j.jcf.2013.11.006}{http://dx.doi.org/10.1016/j.jcf.2013.11.006}
#'
#'
#' @param sex Sex at birth. 1 = male, 2 = female.
#' @param age Age at the spirometry observation time in years.
#' @param height Height in centimeters.
#' @param fev Forced expiratory volume in one second (L)
#' @param fvc Forced vital capacity (L).
#' @param vmax50 Maximal flow after exhalation of 50\% of FVC.
#' @param vmax75 Maximal flow after exhalation of 75\% of FVC.
#' @param fef2575 Forced expiratory flow during the middle half (25-75\%) of
#'   FVC.
#' @param ... Disregarded.  Input allowed so that methods which require
#'   additional parameters (e.g. race) can be fit with consistent syntax.
#' @export
pp_knudson <- function(sex, age, height,
                       fev = NULL, fvc = NULL, vmax50 = NULL, vmax75 = NULL,
                       fef2575 = NULL, ...) {
    measures <- list(fev = fev, fvc = fvc, vmax50 = vmax50,
                     vmax75 = vmax75, fef2575 = fef2575)
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

    if (any(height > 250 | height < 100, na.rm = T)) {
        warning("Height values above 250cm and or under 100cm were detected.
                This usually indicates the wrong unit (cm are correct) or a
                data error which needs to be resolved.")
    }

    user_df <- tibble::tibble(measure = names(measures),
                              raw_value = measures[[1]]) %>%
        dplyr::mutate(sex = sex, age = age, height = height) %>%
        dplyr::mutate(pred = predicted_knudson(sex = .data$sex,
                                               age = .data$age,
                                               height = .data$height,
                                               measure = .data$measure)) %>%
        dplyr::mutate(pct_pred = .data$raw_value/.data$pred*100)

    return(dplyr::pull(user_df, pct_pred))
}

#' @title Predicted spirometry based on Knudson (1983)
#'
#' @description Calculates the predicted spirometry variable (fev, fvc, etc.)
#'   using Knudson's work.
#'
#' @details RJ Knudson, MD Lebowitz, CJ Holberg and B Burrows published
#'   reference spirometry equations which account for sex, age and height, but
#'   not race.  DOI link: \href{https://doi.org/10.1164/arrd.1983.127.6.725}{https://doi.org/10.1164/arrd.1983.127.6.725} or PubMedID 6859656.
#'
#' @param measure One of 'fev', 'fvc', 'fef2575', 'vmax50', 'vmax75'.  See
#'   \code{?pp_knudson()} for definitions of these.
#' @inheritParams pp_knudson
#' @export
predicted_knudson <- function(sex, age, height, measure) {
    stopifnot(is.character(measure))
    if (!all(measure %in% c("fev", "fvc", "fef2575", "vmax50", "vmax75"))) {
        # better to pulls these from the data.
        stop("measure is only supported for fev, fvc, fef2575, vmax50 and vmax75")
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

    user_df <- tibble::tibble(measure = measure, sex = sex, age = age, height = height)

    # we choose which equation to use based on the age range (lb for lower bound)
    # and sex of each observation:
    user_df %<>%
        dplyr::mutate(age_lb = dplyr::case_when(
            sex == 1 & age >= 6 & age < 12 ~ 6,
            sex == 1 & age >= 12 & age < 25 ~ 12,
            sex == 1 & age >= 25 ~ 25,
            # different cut points for female participants.
            sex == 2 & age >= 6 & age < 11 ~ 6,
            sex == 2 & age >= 11 & age < 20 ~ 11,
            sex == 2 & age >= 20 ~ 20,
            # making this NA will cause an NA return (for <6yo).
            T ~ NA_real_))

    # knudson_coefs is saved in sysdata.rda. It's table 1 from the paper.
    user_df %<>% dplyr::left_join(., knudson_coefs,
                                  by = c("measure", "sex", "age_lb"))
    user_df %<>% dplyr::mutate(
        pred = coef_const + coef_height*height + coef_age*age +
            coef_age_sq*(age^2))

    return(dplyr::pull(user_df, pred))

}




