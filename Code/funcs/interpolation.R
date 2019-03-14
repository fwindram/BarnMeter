# Interpolation functions for calculating electricity usage

library(rlist)

interpolate <- function(df) {
    # Take a df of readings and return an equivalent with nearest-neighbour interpolation performed on the measurements where possible.
    
    if (all(is.na(df$measurement))) {
        warning('Error in tailing NA removal: No data in measurement column!')
        return(df)
    }
    working_df <- df
    
    # Remove ending dates by removing iteratively until we hit a good value.
    while (TRUE) {
        if (is.na(working_df$measurement[nrow(working_df)])) {
            working_df <- working_df[-c(nrow(working_df)),]
        } else {
            break
        }
    }
    
    # Mark values to be interpolated as NA 
    working_df$interpolated[which(is.na(working_df$measurement))] <- TRUE
    
    # Interpolate missing values
    # NA_runs <- NA_runfinder(working_df$measurement)
    working_df$measurement <- interpolation_actor(working_df$measurement, NA_runfinder(working_df$measurement))
    
    # Add back ending NAs when exporting
    if (nrow(working_df) < nrow(df)) {
        rowdiff <- nrow(df) - nrow(working_df)
        # Generate enough continuous dates, and make into df with NAs in other columns
        end_dates <- seq.Date(max(working_df$date) + 1, by = 'day', length.out = rowdiff)
        end_rows <- data.frame(end_dates, rep(NA, rowdiff), rep(NA, rowdiff))
        names(end_rows) <- c('date', 'measurement', 'interpolated')
        # Bind to working_df
        working_df <- rbind(working_df, end_rows)
    }
    return(working_df)
}



NA_runfinder <- function(v){
    # Find runs of NAs in *a vector* and returns a list of vectors defining these runs by index in the following form:
    # c(c(start1, end1), c(start2, end2), ...)
    # If a run is of length one, end == start
    
    # Note: this function assumes that v does NOT end in an NA (this should have been stripped prior to usage)
    
    working_start <- NA
    working_end <- NA
    working_list <- vector('list', ceiling(length(v)/2))   # preallocate list to max possible size (length of v / 2)
    working_list_counter <- 1
    in_run <- FALSE
    
    for (i in 1:length(v)) {
        if (is.na(v[i])) {
            # If value is NA
            if (in_run) {
                # If in run, just ignore for now
                next
            } else {
                # If not in run, index is the start of a new run
                working_start <- i
                in_run <- TRUE
            }
        } else {
            # If value is not NA
            if (in_run) {
                # If in run, set the previous index to be the endpoint, add the new vector to the working_list and reset working variables
                working_end <- i - 1
                working_list[[working_list_counter]] <- c(working_start, working_end)
                working_start <- NA
                working_end <- NA
                working_list_counter <- working_list_counter + 1
                in_run <- FALSE
            } else {
                # If not in run, just ignore
                next
            }
        }
    }
    
    # Remove empty vars from list
    working_list <- list.clean(working_list)
    return(working_list)
}

interpolation_actor <- function(v, index_v, base_start = NA){
    # Take a vector of measurements and a vector of NA run indices
    # Interpolate runs of these values from the direct nearest neighbours
    
    for (x in index_v) {
        run_l <- x[2] - x[1] + 1
        start_value <- 0
        end_value <- 0
        if (x[1] == 1) {
            # If run starts at index 1, use base start as starting value, or assume no change at start if base_start is not provided
            if (is.na(base_start)) {
                start_value <- v[x[2] + 1]
                end_value <- v[x[2] + 1]
            } else {
                start_value <- base_start
                end_value <- v[x[2] + 1]
            }
        } else {
            # Normal interpolation setup
            start_value <- v[x[1] - 1]  # Get value before start of run
            end_value <- v[x[2] + 1]    # Get value after end of run
        }
        
        # Interpolate values in run 
        run <- interpolation_root(start_value, end_value, run_l)
        
        # Replace NA run with new values
        v <- replace(v, x[1]:x[2], run)
    }
    
    return(v)
}

interpolation_root <- function(start, end, l){
    # Take a start and end value along with a length, then generate a vector of ints between these two value (non-inclusive)
    
    difference <- end - start
    stepval <- difference / (l + 1)
    run_replacement <- rep(start, l)
    additive_steps <- seq(1, l)
    additive_steps <- additive_steps * stepval
    run_replacement <- run_replacement + additive_steps
    run_replacement <- as.integer(run_replacement)
    
    return(run_replacement)
}