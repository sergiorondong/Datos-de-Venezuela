#-Este programa permite descargar la información estadística disponible en el
#-portal web del Banco Central de Venezuela (BCV)

#-Se comienza leyendo el archivo 'url-archivos.txt'
lectura <- read.table(file = "url-archivos.txt", colClasses = "character")

lectura <- unlist(lectura)

#-Se crean los 'directorios' donde se guardarán los archivos
directorios <- grep(pattern = "http", x = lectura, value = TRUE, invert = TRUE)

#-Se extraen los 'vinculos' del objeto 'lectura'
vinculos <- lapply(X = directorios, FUN = function (directorio)
{
  # se crea 'elementos' para usar como pivote en el próximo loop
  inicio <- which(lectura == directorio) + 1

  final <- NROW(lectura)

  elementos <- c(inicio:final)

  # se extraen los 'vinculos' correspondiente a cada loop
  seleccion <- c()

  for (elemento in elementos)
  {
    if (!grepl(pattern = "http", x = lectura[elemento]))
    {
      break
    }

    seleccion <- c(seleccion, elemento)
  }

  lectura[seleccion]
})

names(vinculos) <- directorios

#-Se crea un menú para seleccionar los archivos a descargar
pregunta <- "¿Cuál grupo de archivos se desea descargar?"

opciones <- gsub(pattern = "[.]", replacement = "Billetes y Monedas",
                 x = sapply(X = directorios, FUN = dirname))

opciones <- c(levels(as.factor(opciones)), "Todos los anteriores")

seleccion <- menu(choices = opciones, title = pregunta)

#-Loop de descarga de los archivos
if (seleccion != 0)
{
  if (opciones[seleccion] != "Todos los anteriores")
  {
    directorios <- grep(pattern = opciones[seleccion], x = directorios,
                        value = TRUE)
  }

  for (directorio in directorios)
  {
    # se crea en el disco local el directorio de 'destino' de la descarga
    destino <- file.path("datos-crudos", directorio)

    dir.create(path = destino, showWarnings = FALSE, recursive = TRUE)

    # se realiza un loop para descargar los archivos
    seleccion  <- which(names(vinculos) == directorio)

    sapply(X = vinculos[[seleccion]], FUN = function(vinculo) {

      destino <- file.path(destino, basename(vinculo))

      download.file(url = vinculo, destfile = destino)
    })
  }
}

#rm (list = ls())  # eliminar variables
