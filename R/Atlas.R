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

      stat = list(value = c("30824", "30825","37677","30832"), #
                  key = c("Income_Distribution", "Income_Sources", "Gini_P8020", "Demographics" )) #

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
                                      "\t", escape_double = FALSE, trim_ws = TRUE,
                                      skip = 6)
          }
          file <- data.frame(file)
          atlas[[stat$key[i]]] <- file
        }
        saveRDS(atlas, file.path(path, "Atlas.RDS"))
        return(atlas)
      }
    }
}



#' Title
#'
#' @param object
#'
#' @return
#' @export
#' @importFrom magrittr %>%
#'
#' @examples
transform_atlas <- function(object){
  income_distribution <- object$Income_Distribution
  income_distribution[income_distribution == "."] <- NA
  income_distribution <- dplyr::mutate(income_distribution,
                 ANRPP = dplyr::coalesce(X2017,X2016, X2015),
                 ANRPH = dplyr::coalesce(X2017_1,X2016_1, X2015_1),
                 MRPCU = dplyr::coalesce(X2017_2,X2016_2, X2015_2),
                 ABRPP = dplyr::coalesce(X2017_3,X2016_3, X2015_3),
                 ABRPH = dplyr::coalesce(X2017_4,X2016_4, X2015_4)) %>%

    dplyr::select(X1, ANRPP, ANRPH, MRPCU, ABRPP, ABRPH) %>%
    tidyr::separate(X1, c("geocode"),
                    sep = " ", extra = "drop") %>%
    mutate(type = case_when(stringr::str_length(geocode) == 5 ~ "municipality",
                            stringr::str_length(geocode) == 7 ~ "district",
                            stringr::str_length(geocode) == 10 ~ "section"),
           municipality_code = substr(geocode,1,5),
           district_code = substr(geocode,1,7)) %>%
    filter(!geocode %in% c("Notas:", "Estadística", "La", "Fuente:", NA))

  tmp_mun <- income_distribution %>%
    dplyr::filter(type == "municipality") %>%
    dplyr::select(-geocode,-type,-district_code) %>%
    dplyr::rename(ANRPPM=ANRPP,
           ANRPHM=ANRPH,
           MRPCUM=MRPCU,
           ABRPPM=ABRPP,
           ABRPHM=ABRPH)
  tmp_dis <- income_distribution %>%
    dplyr::filter(type == "district") %>%
    dplyr::select(-geocode,-type,-municipality_code)%>%
    dplyr::rename(ANRPPD=ANRPP,
           ANRPHD=ANRPH,
           MRPCUD=MRPCU,
           ABRPPD=ABRPP,
           ABRPHD=ABRPH)

  income_distribution <- income_distribution %>%
    dplyr::filter(type == "section") %>%
    dplyr::left_join(tmp_dis, by = "district_code") %>%
    dplyr::left_join(tmp_mun, by = "municipality_code") %>%
    dplyr::mutate(ANRPP = as.numeric(dplyr::coalesce(ANRPP,ANRPPD,ANRPPM)),
           ANRPH = as.numeric(dplyr::coalesce(ANRPH,ANRPHD,ANRPHM)),
           MRPCU = as.numeric(dplyr::coalesce(MRPCU,MRPCUD,MRPCUM)),
           ABRPP = as.numeric(dplyr::coalesce(ABRPP,ABRPPD,ABRPPM)),
           ABRPH = as.numeric(dplyr::coalesce(ABRPH,ABRPHD,ABRPHM))) %>%
    dplyr::select(geocode, ANRPP, ANRPH, MRPCU, ABRPP, ABRPH)

  #### income cources

  income_sources <- object$Income_Sources
  income_sources[income_sources == "."] <- NA
  income_sources <- dplyr::mutate(income_sources,
                                  salaries = dplyr::coalesce(X2017,X2016, X2015),
                                  pensions = dplyr::coalesce(X2017_1,X2016_1, X2015_1),
                                  unemployment = dplyr::coalesce(X2017_2,X2016_2, X2015_2),
                                  other_benefits = dplyr::coalesce(X2017_3,X2016_3, X2015_3),
                                  other_incomes = dplyr::coalesce(X2017_4,X2016_4, X2015_4)) %>%

    dplyr::select(X1, salaries, pensions, unemployment, other_benefits, other_incomes) %>%
    tidyr::separate(X1, c("geocode"),
                    sep = " ", extra = "drop") %>%
    mutate(type = case_when(stringr::str_length(geocode) == 5 ~ "municipality",
                            stringr::str_length(geocode) == 7 ~ "district",
                            stringr::str_length(geocode) == 10 ~ "section"),
           municipality_code = substr(geocode,1,5),
           district_code = substr(geocode,1,7)) %>%
    filter(!geocode %in% c("Notas:", "Estadística", "La", "Fuente:", NA))

  tmp_mun <- income_sources %>%
    dplyr::filter(type == "municipality") %>%
    dplyr::select(-geocode,-type,-district_code) %>%
    dplyr::rename(salariesM=salaries,
                  pensionsM=pensions,
                  unemploymentM=unemployment,
                  other_benefitsM=other_benefits,
                  other_incomesM=other_incomes)
  tmp_dis <- income_sources %>%
    dplyr::filter(type == "district") %>%
    dplyr::select(-geocode,-type,-municipality_code)%>%
    dplyr::rename(salariesD=salaries,
                  pensionsD=pensions,
                  unemploymentD=unemployment,
                  other_benefitsD=other_benefits,
                  other_incomesD=other_incomes)

  income_sources <- income_sources %>%
    dplyr::filter(type == "section") %>%
    dplyr::left_join(tmp_dis, by = "district_code") %>%
    dplyr::left_join(tmp_mun, by = "municipality_code") %>%
    dplyr::mutate(
      salaries = readr::parse_number(dplyr::coalesce(salaries,salariesD,salariesM),
                                     locale = locale(grouping_mark = ".")),
      pensions = readr::parse_number(dplyr::coalesce(pensions,pensionsD,pensionsM),
                                     locale = locale(grouping_mark = ".")),
      unemployment = readr::parse_number(dplyr::coalesce(unemployment,unemploymentD,unemploymentM),
                                         locale = locale(grouping_mark = ".")),
      other_benefits = readr::parse_number(dplyr::coalesce(other_benefits,other_benefitsD,other_benefitsM),
                                           locale = locale(grouping_mark = ".")),
      other_incomes = readr::parse_number(dplyr::coalesce(other_incomes,other_incomesD,other_incomesM),
                                          locale = locale(grouping_mark = "."))) %>%
    dplyr::select(geocode, salaries, pensions, unemployment, other_benefits, other_incomes)

  ####  Demographics


  demographics <- object$Demographics
  demographics[demographics == "."] <- NA
  demographics <- dplyr::mutate(demographics,
                                age_avg = dplyr::coalesce(X2017,X2016, X2015),
                                age_18 = dplyr::coalesce(X2017_1,X2016_1, X2015_1),
                                age_65 = dplyr::coalesce(X2017_2,X2016_2, X2015_2),
                                house_size = dplyr::coalesce(X2017_3,X2016_3, X2015_3),
                                house_uni = dplyr::coalesce(X2017_4,X2016_4, X2015_4),
                                population = dplyr::coalesce(X2017_5,X2016_5, X2015_5)) %>%

    dplyr::select(X1, population, age_avg, age_18, age_65, house_size,house_uni) %>%
    tidyr::separate(X1, c("geocode"),
                    sep = " ", extra = "drop") %>%
    mutate(type = case_when(stringr::str_length(geocode) == 5 ~ "municipality",
                            stringr::str_length(geocode) == 7 ~ "district",
                            stringr::str_length(geocode) == 10 ~ "section"),
           municipality_code = substr(geocode,1,5),
           district_code = substr(geocode,1,7)) %>%
    filter(!geocode %in% c("Notas:", "Estadística", "La", "Fuente:", NA))

  tmp_mun <- demographics %>%
    dplyr::filter(type == "municipality") %>%
    dplyr::select(-geocode,-type,-district_code, -population) %>%
    dplyr::rename(
      age_avgM=age_avg,
      age_18M=age_18,
      age_65M=age_65,
      house_sizeM=house_size,
      house_uniM=house_uni)
  tmp_dis <- demographics %>%
    dplyr::filter(type == "district") %>%
    dplyr::select(-geocode,-type,-municipality_code, -population)%>%
    dplyr::rename(
      age_avgD=age_avg,
      age_18D=age_18,
      age_65D=age_65,
      house_sizeD=house_size,
      house_uniD=house_uni)

  demographics <- demographics %>%
    dplyr::filter(type == "section") %>%
    dplyr::left_join(tmp_dis, by = "district_code") %>%
    dplyr::left_join(tmp_mun, by = "municipality_code") %>%
    dplyr::mutate(
      population = readr::parse_number(dplyr::coalesce(population),
                                       locale = locale(grouping_mark = ".")),
      age_avg = readr::parse_number(dplyr::coalesce(age_avg,age_avgD,age_avgM),
                                    locale = locale(grouping_mark = ".")),
      age_18 = readr::parse_number(dplyr::coalesce(age_18,age_18D,age_18M),
                                   locale = locale(grouping_mark = ".")),
      age_65 = readr::parse_number(dplyr::coalesce(age_65,age_65D,age_65M),
                                   locale = locale(grouping_mark = ".")),
      house_size = readr::parse_number(dplyr::coalesce(house_size,house_sizeD,house_sizeM),
                                       locale = locale(grouping_mark = ".")),
      house_uni = readr::parse_number(dplyr::coalesce(house_uni,house_uniD,house_uniM),
                                      locale = locale(grouping_mark = "."))) %>%
    dplyr::select(geocode, population, age_avg, age_18, age_65, house_size, house_uni)

  data <- income_distribution %>%
    left_join(income_sources, by = "geocode") %>%
    left_join(demographics, by = "geocode")

  return(data)

}
