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
#' @param ver string
#'
#' @return data.frame
#' @export
#' @importFrom dplyr %>%
#'
#'
#' @examples
#' get_callejero(year = "2018", ver = "01")
get_callejero <- function(year, ver){
  #browser()

  path <- file.path("data/INE/Callejero", year, ver)

  if (file.exists(file.path(path, "Callejero.RDS"))){
    print(paste("Callejero", year, "version", ver, "in", path))
    callejero <- readRDS(file.path(path, "Callejero.RDS"))} else{
      dir.create(file.path(path), recursive = TRUE)
      url <- paste0("http://www.ine.es/prodyser/callejero/caj_esp/caj_esp_", ver, year, ".zip")
      utils::download.file(url, file.path(getwd(), path, "callejero.zip"))

      utils::unzip(file.path(path, "callejero.zip"),
            exdir = file.path("data/INE/Callejero", year, ver))

      files <- list.files(file.path(path))
      files <- files[grepl("TRAMOS|VIAS", files)]

      utils::unzip(file.path(path, files[1]),
            exdir = file.path("data/INE/Callejero", year, ver))

      utils::unzip(file.path(path, files[2]),
            exdir = file.path("data/INE/Callejero", year, ver))

      file.remove(list.files(path, pattern = ".zip", full.names = TRUE))

      names <- list.files(path, full.names = TRUE)
      newnames <- gsub('\\..*', ".txt", names)

      file.rename(names, newnames)

      #### create tramero df
      tramero <- utils::read.fwf(file.path(path, "TRAMOS-NAL.txt"),
                          width = c(2,3,2,3,1,2,7,5,5,12,5,1,4,1,4,1,1,2,8,1,2,3,1,2,7,25,25,25,5,25),
                          colClasses="character",
                          n = 20,
                          fileEncoding = "ISO-8859-1") %>% data.frame()

      names(tramero) <- c("CPRO", "CMUN", "DIST", "SECC", "LSECC",
                          "SUBSC", "CUN",  "CVIA", "CPSVIA","MANZ",
                          "CPOS", "TINUM",   "EIN",  "CEIN", "ESN",  "CESN",
                          "TIPOINF",  "CDEV", "FVAR", "CVAR",
                          "DIST_I", "SECC_I", "LSECC_I",  "SUBSC_I",
                          "CUN_I",  "NENTCCC",  "NENTSIC",  "NNCLEC",   "CVIA_I",
                          "NVIAC")

      tramero <- tramero %>% dplyr::select(-"LSECC", -"MANZ", -"LSECC_I", -"TIPOINF",
                                    -"CVAR", -"FVAR", -"DIST_I", -"SECC_I",
                                    -"SUBSC_I", -"CUN_I", -"CVIA_I")
      saveRDS(tramero, file.path(path, "Tramero.RDS"))

      ### create vias
      vias <- utils::read.fwf(file.path(path, "VIAS-NAL.txt"),
                       width = c(2,3,5,1,2,8,1,5,5,1,50,25),
                       colClasses="character",
                       n=20,
                       fileEncoding = "ISO-8859-1") %>% data.frame()

      names(vias) <- c("CPRO", "CMUN", "CVIA", "TIPOINF", "CDEV",
                       "FVAR", "CVAR", "CVIA_I","TVIA",
                       "POS", "NVIA",   "NVIAC")


      vias <- vias %>% dplyr::select(-"TIPOINF", -"CDEV", -"POS", -"FVAR", -"CVAR", -"CVIA_I")
      saveRDS(vias, file.path(path, "Vias.RDS"))

      ### final dataset
      callejero <- tramero %>% dplyr::left_join(vias, by = c("CPRO", "CMUN", "CVIA", "NVIAC"))
      saveRDS(callejero, file.path(path, "Callejero.RDS"))

      file.remove(list.files(path, pattern = ".txt", full.names = TRUE))
      return(callejero)
    }

}
