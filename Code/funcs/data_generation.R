generate_data <- function(from = '2019/03/09', to = '2019/08/08', starting_reading = 27505, increment = 30, generate_incomplete = TRUE){
    date <- seq.Date(as.Date(from), as.Date(to), 'day')
    date_length <- length(date)
    measurement_seq <- seq(0, date_length - 1)
    measurement <- measurement_seq * increment
    measurement <- measurement + starting_reading
    flucs <- as.integer(rnorm(date_length, 0, 5))
    measurement <- measurement + flucs
    measurement[1] <- starting_reading
    interpolated <- as.logical(rep(NA, date_length))
    if (generate_incomplete) {
        # measurement[unique(as.integer(runif(as.integer(date_length/10), 1, date_length)))] <- NA
        measurement[sample(1:date_length, as.integer(date_length/10))] <- NA
    }
    readings <- data.frame(date, measurement, interpolated)
    return(readings)
}