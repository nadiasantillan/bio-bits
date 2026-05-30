#-------------------- Utilidades ----------------------#
#-----------------Materia Optativa I-------------------#
#--------------Autores: -------------------------------#
#-----FERRAGUTTI - SANTILLAN - VILLARREAL--------------#
#-------------------Año: 2026 -------------------------#

# Ventana con detección de sistema operativo -----------------------------------
ventana <- function(...) {
  if (.Platform$OS.type == "windows") {
    window(...)
  } else {
    x11(...)
  }
}

# imagen <-function(
#     grafico,
#     archivo_nombre, 
#     width = 800, 
#     height = 600, 
#     img = F) {
#   if(img){
#     png(filename=paste0("img/", archivo_nombre), width = width, height = height)
#     grafico
#   } else {
#     print(grafico)
#   }
#   if(img) {
#     dev.off
#   }
# }
