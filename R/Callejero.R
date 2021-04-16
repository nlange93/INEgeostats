#' Get Callejero
#'
#'Returns a data frame containing the complete spanish address catalogue.
#'For a given Address, the catalogue specifies among others:
#'Province Code, Municipality Code, District Code, Section Code, ZIP Code,
#'Street Name, Numeration...
#'
#'
#'For more information on the catalogue design, visit :
#'https://www.ine.es/ss/Satellite?c=Page&p=1259952026632&pagename=ProductosYServicios%2FPYSLayout&cid=1259952026632&L=1
#'
#'
#' @param year string
#' @param version string
#'
#' @return data.frame
#' @export
#'
#'
#' @examples
#' get_callejero(year = "2018", version = "01")
get_callejero <- function(year, version){
  #browser()
  vers <- paste0(year,version)
  path <- file.path("data/INE/Callejero", vers)

  if (file.exists(file.path(path, "Callejero.RDS"))){
    print(paste("Reading Callejero", year, "version", version, "from", path))
    callejero <- readRDS(file.path(path, "Callejero.RDS"))} else{
      dir.create(file.path(path), recursive = TRUE)
      url <- paste0("http://www.ine.es/prodyser/callejero/caj_esp/caj_esp_", version, year, ".zip")
      utils::download.file(url, file.path(getwd(), path, "callejero.zip"))

      utils::unzip(file.path(path, "callejero.zip"),
                   exdir = file.path("data/INE/Callejero", vers))

      files <- list.files(file.path(path))
      files <- files[grepl("TRAMOS|VIAS", files)]

      for (i in 1:2){
        utils::unzip(file.path(path, files[i]),
                     exdir = file.path("data/INE/Callejero", vers))
      }

      file.remove(list.files(path, pattern = ".zip", full.names = TRUE))
      names <- list.files(path, full.names = TRUE)
      newnames <- gsub('\\..*', ".txt", names)
      file.rename(names, newnames)

      #### create tramero df
      cat("Reading tramero, go get some coffee, this might take a while... ")
      tramero <- data.frame(utils::read.fwf(file.path(path, "TRAMOS-NAL.txt"),
                                            width = c(2,3,2,3,1,2,7,5,5,12,5,1,4,1,4,1,1,2,8,1,2,3,1,2,7,25,25,25,5,25),
                                            colClasses="character",
                                            n = 20,
                                            fileEncoding = "ISO-8859-1"))
      names(tramero) <- c("CPRO", "CMUN", "DIST", "SECC", "LSECC",
                          "SUBSC", "CUN",  "CVIA", "CPSVIA","MANZ",
                          "CPOS", "TINUM",   "EIN",  "CEIN", "ESN",  "CESN",
                          "TIPOINF",  "CDEV", "FVAR", "CVAR",
                          "DIST_I", "SECC_I", "LSECC_I",  "SUBSC_I",
                          "CUN_I",  "NENTCCC",  "NENTSIC",  "NNCLEC",   "CVIA_I",
                          "NVIAC")
      ### create vias
      cat("Done", "\n",  "Reading vias... ")
      vias <- data.frame(utils::read.fwf(file.path(path, "VIAS-NAL.txt"),
                                         width = c(2,3,5,1,2,8,1,5,5,1,50,25),
                                         colClasses="character",
                                         n=20,
                                         fileEncoding = "ISO-8859-1"))
      cat("Done")
      names(vias) <- c("CPRO", "CMUN", "CVIA", "TIPOINF", "CDEV",
                       "FVAR", "CVAR", "CVIA_I","TVIA",
                       "POS", "NVIA",   "NVIAC")


      callejero <- list(tramero = tramero, vias = vias )
      file.remove(list.files(path, pattern = ".txt", full.names = TRUE))
      saveRDS(callejero, file.path(path, "Callejero.RDS"))
      return(callejero)
    }

}

#' transform_callejero
#'
#' Takes list object containing tramero and vias and
#' returns joined dataset with the directory of spanish addresses
#'
#' @param object list containing data.frames tramero and vias
#'
#' @return data.frame
#' @export
#'
#' @examples
#' transform_callejero(get_callejero("2018", "01"))
transform_callejero <- function(object){

  tramero <- object$tramero
  tramero <- dplyr::select(tramero, -"LSECC", -"MANZ",
                           -"LSECC_I", -"TIPOINF",-"CVAR",
                           -"FVAR", -"DIST_I",  -"SECC_I",
                           -"SUBSC_I", -"CUN_I", -"CVIA_I")
  vias <- object$vias
  vias <- dplyr::select(vias, -"TIPOINF", -"CDEV", -"POS", -"FVAR", -"CVAR", -"CVIA_I")

  ### final dataset
  callejero <- dplyr::left_join(tramero,
                                vias,
                                by = c("CPRO", "CMUN", "CVIA", "NVIAC"))
  return(callejero)
}
