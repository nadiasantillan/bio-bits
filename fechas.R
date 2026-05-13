library(lubridate)

estacion <- function(fecha) {
  if (is.na(fecha)) {
    e <- NA    
  } else {
    m <- month(fecha)  
    d <- day(fecha)
    
    if (d >= 21 && m == 3 || m %in% c(4, 5) || d < 21 && m == 6) {
      e <- "Otoño"
    } else if (d >= 21 && m == 6 || m %in% c(7, 8) || d < 21 && m == 9) {
      e <- "Invierno"
    } else if (d >= 21 && m == 9 || m %in% c(10, 11) || d < 21 && m == 12) {
      e <- "Primavera"
    } else {
      e <- "Verano"
    }
    
  }
  e
}
