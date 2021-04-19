#' get_padron
#'
#' @param year string
#'
#' @return data.frame
#' @export
#' @importFrom magrittr %>%
#'
#' @examples
get_padron <- function(year){
  path <- file.path("data/INE/Padron", year)
  if (file.exists(file.path(path, "Padron.RDS"))){
    print(paste("Reading Padron from", path))
    padron <- readRDS(file.path(path, "Padron.RDS"))} else{
      dir.create(file.path(path), recursive = TRUE)
      padron <- list()

      stats <- list("0001","0002","0003","0004","0005", "0006")
      for (stat in stats){
        url <- paste0("https://www.ine.es/pcaxisdl/t20/e245/p07/a", year, "/l0/", stat,".px")
        utils::download.file(url, file.path(path, paste0(stat, ".px")))

        file <- pxR::read.px(file.path(path, paste0(stat,".px")))

        padron[[file[["TITLE"]][["value"]]]] <- file[["DATA"]]
      }
    }
  saveRDS(padron, file.path(path,"Padron.RDS"))
  return(padron)
}


#' Title
#'
#' @param object data.frame
#'
#' @return data.frame
#' @export
#' @importFrom magrittr %>%
#'
#' @examples
transform_padron <- function(object){
  #browser()
  out <- list()
  # we are only interested in 3 of the 6 statistics

  stat<- list(value = c(1, 2, 5),
               key = c("age", "nationality", "origin" ))


  for (i in 1:length(stat$key)){
    out[[stat$key[i]]] <- object[[stat$value[i]]][[1]] %>%
      dplyr::filter(.[,2] !="TOTAL", .[,1]!="Total", .[,3] == "Ambos Sexos")
    names(out[[stat$key[i]]])[1]<- "feature"
    names(out[[stat$key[i]]])[2]<- "SECC"
    out[[stat$key[i]]] <- out[[stat$key[i]]]%>%
      tidyr::pivot_wider(names_from = feature , values_from = value) %>%
      dplyr::select(-sexo)

  }

  out$pop <- object[[1]][[1]] %>%
    dplyr::filter(.[,2] != "TOTAL", .[,1] == "Total") %>%
    dplyr::select_at(-1) %>%
    tidyr::pivot_wider(names_from = sexo, values_from = value) %>%
    dplyr::rename(
      SECC = "sección",
      Population = "Ambos Sexos",
      Male = "Hombres",
      Female = "Mujeres")

  out <- purrr::reduce(out, dplyr::left_join, by = "SECC") %>%
    dplyr::mutate_at(dplyr::vars(-SECC,-Population), ~./Population) %>%
    select(SECC,Population,Male,Female, dplyr::everything())



  return(out)
}







