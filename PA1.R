
library(knitr)
browseURL("PA1_template.Rmd")

knit2html("PA1_template.Rmd")

x <- as.Date("2012-02-04")
class(x)
x

weekdays(x, abbreviate=)
is.weekend(x)
is.holiday(x)




days <- function(x){
        if (x == "Sunday"){"weekend"}
        else if (x == "Saturday"){"weekend"}
        else {"Weekday"}
}