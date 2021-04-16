#' Title
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
get_atlas <- function(..., import = TRUE){

  path <- file.path("data/INE/Atlas_renta")


  if (file.exists(file.path(path, "Atlas.RDS"))){
    print(paste("Reading Atlas Renta from", path))
    atlas <- readRDS(file.path(path, "Atlas.RDS"))} else{
      dir.create(file.path(path), recursive = TRUE)

      stat = list(value = c("30824", "30825"), # "37677","30832"
                  key = c("Income_Distribution", "Income_Sources" )) # "Gini_P8020", "Demographics"

      for (i in 1:length(stat$key)){

        url <- paste0("https://www.ine.es/jaxiT3/files/t/es/csv/", stat$value[i], ".csv?nocab=1")
        utils::download.file(url, file.path(path, paste0(stat$key[i], ".csv")))

      }
      cat("Downloaded CSVs to", path)
      atlas <- list()
      if (import){
        for (i in 1:length(stat$key)){
          if (stat$key[i] %in% c("Income_Distribution", "Income_Sources")){
            file <- readr::read_delim(paste0("data/INE/Atlas_renta/", stat$key[i], ".csv"),
                                      "\t", escape_double = FALSE, trim_ws = TRUE,
                                      skip = 6)
          } else {
            file <- readr::read_delim(paste0("data/INE/Atlas_renta/", stat$key[i], ".csv"),
                                      "\t", escape_double = FALSE, trim_ws = TRUE)
          }
          file <- data.frame(file)
          atlas[[stat$key[i]]] <- file
        }
        saveRDS(atlas, file.path(path, "Atlas.RDS"))
        return(atlas)
      }
    }
}



transform_atlas <- function(object){
  data <- object$Income_Distribution
  data[data == "."] <- NA
  data <- mutate(data,
                 ANRPP = case_when(!is.na(X2017) ~ X2017,
                                   is.na(X2017) & !is.na(X2016) ~ X2016,
                                   TRUE ~ X2015),
                 ANRPH = case_when(!is.na(X2017_1) ~ X2017_1,
                                  is.na(X2017_1) & !is.na(X2016_1) ~ X2016_1,
                                  TRUE ~ X2015_1),
                 MRPCU = case_when(!is.na(X2017_2) ~ X2017_2,
                                   is.na(X2017_2) & !is.na(X2016_2) ~ X2016_2,
                                   TRUE ~ X2015_2)) %>% select(X1, ANRPP, ANRPH, MRPCU)



}

data %>% mutate(NRPP = case_when(!is.na(X2017) ~ X2017,
                                  is.na(X2017) & !is.na(X2016) ~ X2016,
                                  TRUE ~ X2015
                                  )) %>% View()

which(data$X2017)
