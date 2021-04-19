#' get_cartography
#'
#' @param year string
#' @param import
#'
#' @return sf
#' @export
#' @importFrom magrittr %>%
#'
#' @examples
get_cartography <- function(year, import = TRUE){

  path <- file.path("data/INE/Cartography", year)


  if (file.exists(file.path(path, "Cartography.RDS"))){
    print(paste("Reading Cartography from", path))
    carto <- readRDS(file.path(path, "Cartography.RDS"))} else{
      dir.create(file.path(path), recursive = TRUE)

      url <- paste0("https://www.ine.es/prodyser/cartografia/seccionado_", year,".zip")
      utils::download.file(url, file.path(getwd(), path, "Cartography.zip"))

      utils::unzip(file.path(path, "Cartography.zip"),
                   exdir = path)
      file.remove(file.path(path, "Cartography.zip"))


      carto <- sf::read_sf(list.files(path, pattern = ".shp", full.names = TRUE)) %>%
        dplyr::rename(SECC=CUSEC)

      saveRDS(carto, file.path(path,"Cartography.RDS"))
    }
  return(carto)
}
