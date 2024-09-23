#' @title loess_dx
#'
#' @description Take a grouped tibble with x & y variables, calculates LOESS regression and approximates 1st and 2nd derivatives by aproximating the slope at every point of the curve.
#'
#' @param data data frame containing at least 2 columns of type double. It can be grouped using dplyr::group_by to process within each group.
#' @param y_col Name of the y variable column
#' @param time_col Name of the x variable column, defaults to "Time"
#' @param prediction_size Size of the prediction used for approximating the derivatives. Should be bigger than the number of data points in the actual dataset
#' @param span Window size used for LOESS
#' 
#' @return A tibble with x & y coordinates for (absolute) maxima and minima. If the input is grouped by multiple variables using `dplyr::group_by`, the inflection poinst will be then calculated for each group individually, and the grouping variables will be included in the resulting tibble.
#'
#' @export
#'
#' @importFrom magrittr %>%


loess_dx<- function(data, #Must be grouped by desired factors already
                    y_col, time_col = "Time", #Name of y & x columns to use for regression
                    prediction_size = 100, #Size of predictions for smoothing. Ideally bigger than group n
                    span = 0.6){ #window used for LOESS

    data$Time <- data[[time_col]]
    data$y_col <- data[[y_col]]


    time_pred = (c(0:prediction_size)/prediction_size * (max(data$Time) - min(data$Time))) + min(data$Time) #Time range to run predictions
    dx = diff(time_pred[1:2]) #Step size for derivative estimation


    data %>%
        tidyr::nest() %>%
        dplyr::mutate(loess_raw = purrr::map(data, ~ loess(y_col ~ Time, data = ., span = span))) %>% #fit loess to raw data
        dplyr::mutate(pred_first = purrr::map2(loess_raw, data.frame(Time =time_pred), predict)) %>%
        dplyr::mutate(first_dx = purrr::map(as.vector(pred_first), diff)) %>%
        tidyr::unnest(first_dx) %>%
        dplyr::mutate(Time = time_pred[-1])%>%
        dplyr::mutate(first_dx = first_dx / dx) %>%
        tidyr::drop_na() -> first #calculate 1st derivative

first %>%
    tidyr::nest() %>%
    dplyr::mutate(loess_first = purrr::map(data, ~ loess(first_dx ~ Time, data = ., span = span))) %>%
    dplyr::mutate(pred_second = purrr::map2(loess_first, data.frame(Time = time_pred), predict)) %>%
    dplyr::mutate(second_dx = purrr::map(as.vector(pred_second), diff)) %>%
    tidyr::unnest(second_dx) %>%
    dplyr::mutate(Time = time_pred[-1]) %>%
    dplyr::mutate(second_dx = second_dx /dx) %>%
    tidyr::drop_na() -> second #calculate 2nd derivative

    param_r <- summarise(data,
                         end_point = tail(y_col, n = 1),
                     max_0 = max(y_col),
                     max_0.t = Time[which.max(y_col)],
                     min_0 = min(y_col),
                     min_0.t = Time[which.min(y_col)])

param_1  <- dplyr::summarise(first,
                     max_1 = max(first_dx),
                     max_1.t = Time[which.max(first_dx)],
                     min_1 = min(first_dx),
                     min_1.t = Time[which.min(first_dx)])

param_2  <- dplyr::summarise(second,
                     max_2 = max(second_dx),
                     max_2.t = Time[which.max(second_dx)],
                     min_2 = min(second_dx),
                     min_2.t = Time[which.min(second_dx)])

dplyr::left_join(param_r, param_1) %>%
    dplyr::left_join(param_2) 

}
