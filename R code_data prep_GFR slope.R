### Preparing the data

library(haven)
library(labelled)
library(readxl)
library(reshape2)

data <- haven::read_sav("TBD..") 

clinical_data <- data[, c(grep("^(date_measured_gfr|measured_gfr)", colnames(data), value = TRUE),
                               "stdnr", "sex", "dateofbirth", "intnr", "ethnicity")]   

clinical_dataM <- melt(clinical_data, id.vars = c("stdnr", "sex", "dateofbirth", "intnr", "ethnicity"))  
clinical_dataM$nums <- gsub(".*gfr_|.*year", "", clinical_dataM$variable)
clinical_dataM$nums <- gsub("baseline", "0", clinical_dataM$nums)
clinical_dataM$nums <- gsub("eos", "99", clinical_dataM$nums)
clinical_dataM$nums <- ifelse(nchar(clinical_dataM$nums) < 3, 
                              paste0("0", clinical_dataM$nums), 
                              clinical_dataM$nums)
clinical_dataM$newID <- paste0(clinical_dataM$stdnr, "_m", clinical_dataM$nums)
View(clinical_dataM)
clinical_dataM$colname <- gsub("_([0-9]+)$", "", clinical_dataM$variable)
clinical_dataM$colname <- gsub("(_baseline|_eos|_year[0-9]+)", "", clinical_dataM$colname)


clinical_dataM_measured_GFR <- dcast(clinical_dataM, 
                                    stdnr + sex + dateofbirth + intnr + ethnicity + nums ~ colname, 
                                    value.var = "value", 
                                    fun.aggregate = function(x) x[1])

clinical_dataM_measured_GFR$measured_gfr <- as.numeric(as.character(clinical_dataM_measured_GFR$measured_gfr))
alldates <- grep("date", colnames(clinical_dataM_measured_GFR))
clinical_dataM_measured_GFR[, alldates] <- lapply(clinical_dataM_measured_GFR[, alldates], as.Date, origin = "1970-01-01")
clinical_dataM_measured_GFR$date_measured_gfr[which(clinical_dataM_measured_GFR$date_measured_gfr=="1966-03-22")]=NA

#Calculation of GFR slope

calculate_gfr_slope <- function(df, gfr_column, date_column) {
  df %>%
    filter(!is.na(!!sym(gfr_column)) & !is.na(!!sym(date_column))) %>%  # Remove NA values
    group_by(stdnr) %>%
    summarise(
      slope = {
        if(n() < 3) {
          NA  # If fewer than 3 samples, set slope to NA
        } else {
          # Check for unique measurements
          if(length(unique(!!sym(gfr_column))) == 1 || length(unique(!!sym(date_column))) == 1) {
            NA  # Don't calculate slope if all measurements are the same
          } else {
            # Fit the robust linear regression model
            model <- rlm(!!sym(gfr_column) ~ as.numeric(!!sym(date_column)), data = cur_data())
            slope_value <- coef(model)[2] * 365.25  # Calculate yearly slope
            slope_value  # Return the slope value
          }
        }
      },
      rmse = {
        if(n() < 3 || length(unique(!!sym(gfr_column))) == 1 || length(unique(!!sym(date_column))) == 1) {
          NA  # If conditions aren't met, set RMSE to NA
        } else {
          model <- rlm(!!sym(gfr_column) ~ as.numeric(!!sym(date_column)), data = cur_data())
          summary(model)$sigma  # Return RMSE
        }
      },
      n = n()  # Count of observations
    ) %>%
    ungroup()
}


#calculation of mGFR slope

measured_gfr_slope <- calculate_gfr_slope(clinical_dataM_measured_GFR, "measured_gfr", "date_measured_gfr")
measured_gfr_slope_new <- measured_gfr_slope %>%
  rename_with(~ paste0(., "_measured"), -stdnr)
View(measured_gfr_slope_new)

