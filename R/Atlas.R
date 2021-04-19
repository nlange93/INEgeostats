#' get_atlas
#'
#' @param import TRUE if want to return data.frame
#'
#' @return list
#' @export
#' @importFrom magrittr %>%
#'
#' @examples
#' get_atlas(import=FALSE)
get_atlas <- function(import = TRUE){

  path <- file.path("data/INE/Atlas")


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
            file <- readr::read_delim(paste0("data/INE/Atlas/", stat$key[i], ".csv"),
                                      "\t", escape_double = FALSE, trim_ws = TRUE,
                                      skip = 7)
          } else {
            file <- readr::read_delim(paste0("data/INE/Atlas/", stat$key[i], ".csv"),
                                      "\t", escape_double = FALSE, trim_ws = TRUE,
                                      skip = 7)
          }
          file <- data.frame(file)
          atlas[[stat$key[i]]] <- file
        }
        saveRDS(atlas, file.path(path, "Atlas.RDS"))
        return(atlas)
      }
    }
}



#' Returns Atlas, an experimentaö data frame with Income Information from the spanish Tax Office
#'
#' @param object data.frame
#'
#' @return data.frame
#' @export
#' @importFrom magrittr %>%
#'
#' @examples
#'
transform_atlas <- function(object){
  #browser()
  income_distribution <- object$Income_Distribution
  income_distribution[income_distribution == "."] <- NA
  income_distribution <- dplyr::mutate(income_distribution,
                 ANRPP = dplyr::coalesce(X2017,X2016, X2015),
                 ANRPH = dplyr::coalesce(X2017_1,X2016_1, X2015_1),
                 MRPCU = dplyr::coalesce(X2017_2,X2016_2, X2015_2),
                 ABRPP = dplyr::coalesce(X2017_3,X2016_3, X2015_3),
                 ABRPH = dplyr::coalesce(X2017_4,X2016_4, X2015_4)) %>%

    dplyr::select(X1, ANRPP, ANRPH, MRPCU, ABRPP, ABRPH) %>%
    tidyr::separate(X1, c("SECC"),
                    sep = " ", extra = "drop") %>%
    dplyr::mutate(type = dplyr::case_when(stringr::str_length(SECC) == 5 ~ "municipality",
                            stringr::str_length(SECC) == 7 ~ "district",
                            stringr::str_length(SECC) == 10 ~ "section"),
           municipality_code = substr(SECC,1,5),
           district_code = substr(SECC,1,7)) %>%
    dplyr::filter(!SECC %in% c("Notas:", "Estadística", "La", "Fuente:", NA))

  tmp_mun <- income_distribution %>%
    dplyr::filter(type == "municipality") %>%
    dplyr::select(-SECC,-type,-district_code) %>%
    dplyr::rename(ANRPPM=ANRPP,
           ANRPHM=ANRPH,
           MRPCUM=MRPCU,
           ABRPPM=ABRPP,
           ABRPHM=ABRPH)
  tmp_dis <- income_distribution %>%
    dplyr::filter(type == "district") %>%
    dplyr::select(-SECC,-type,-municipality_code)%>%
    dplyr::rename(ANRPPD=ANRPP,
           ANRPHD=ANRPH,
           MRPCUD=MRPCU,
           ABRPPD=ABRPP,
           ABRPHD=ABRPH)

  income_distribution <- income_distribution %>%
    dplyr::filter(type == "section") %>%
    dplyr::left_join(tmp_dis, by = "district_code") %>%
    dplyr::left_join(tmp_mun, by = "municipality_code") %>%
    dplyr::mutate(
      ANRPP = readr::parse_number(dplyr::coalesce(ANRPP,ANRPPD,ANRPPM),
                                  locale = readr::locale(grouping_mark = ".")),
      ANRPH = readr::parse_number(dplyr::coalesce(ANRPH,ANRPHD,ANRPHM),
                                  locale = readr::locale(grouping_mark = ".")),
      MRPCU = readr::parse_number(dplyr::coalesce(MRPCU,MRPCUD,MRPCUM),
                                  locale = readr::locale(grouping_mark = ".")),
      ABRPP = readr::parse_number(dplyr::coalesce(ABRPP,ABRPPD,ABRPPM),
                                  locale = readr::locale(grouping_mark = ".")),
      ABRPH = readr::parse_number(dplyr::coalesce(ABRPH,ABRPHD,ABRPHM),
                                  locale = readr::locale(grouping_mark = "."))) %>%
    dplyr::select(SECC, ANRPP, ANRPH, MRPCU, ABRPP, ABRPH)

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
    tidyr::separate(X1, c("SECC"),
                    sep = " ", extra = "drop") %>%
    dplyr::mutate(type = dplyr::case_when(stringr::str_length(SECC) == 5 ~ "municipality",
                            stringr::str_length(SECC) == 7 ~ "district",
                            stringr::str_length(SECC) == 10 ~ "section"),
           municipality_code = substr(SECC,1,5),
           district_code = substr(SECC,1,7)) %>%
    dplyr::filter(!SECC %in% c("Notas:", "Estadística", "La", "Fuente:", NA))

  tmp_mun <- income_sources %>%
    dplyr::filter(type == "municipality") %>%
    dplyr::select(-SECC,-type,-district_code) %>%
    dplyr::rename(salariesM=salaries,
                  pensionsM=pensions,
                  unemploymentM=unemployment,
                  other_benefitsM=other_benefits,
                  other_incomesM=other_incomes)
  tmp_dis <- income_sources %>%
    dplyr::filter(type == "district") %>%
    dplyr::select(-SECC,-type,-municipality_code)%>%
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
                                     locale = readr::locale(grouping_mark = ".")),
      pensions = readr::parse_number(dplyr::coalesce(pensions,pensionsD,pensionsM),
                                     locale = readr::locale(grouping_mark = ".")),
      unemployment = readr::parse_number(dplyr::coalesce(unemployment,unemploymentD,unemploymentM),
                                         locale = readr::locale(grouping_mark = ".")),
      other_benefits = readr::parse_number(dplyr::coalesce(other_benefits,other_benefitsD,other_benefitsM),
                                           locale = readr::locale(grouping_mark = ".")),
      other_incomes = readr::parse_number(dplyr::coalesce(other_incomes,other_incomesD,other_incomesM),
                                          locale = readr::locale(grouping_mark = "."))) %>%
    dplyr::select(SECC, salaries, pensions, unemployment, other_benefits, other_incomes)


  ####  Demographics


  demographics <- object$Demographics
  demographics[demographics == "."] <- NA
  demographics <- dplyr::mutate(demographics,
                                Atlas_age = dplyr::coalesce(X2017,X2016, X2015),
                                Atlas_age_18 = dplyr::coalesce(X2017_1,X2016_1, X2015_1),
                                Atlas_age_65 = dplyr::coalesce(X2017_2,X2016_2, X2015_2),
                                Atlas_house_size = dplyr::coalesce(X2017_3,X2016_3, X2015_3),
                                Atlas_uni_hh = dplyr::coalesce(X2017_4,X2016_4, X2015_4),
                                Atlas_population = dplyr::coalesce(X2017_5,X2016_5, X2015_5)) %>%

    dplyr::select(X1, Atlas_population, Atlas_age, Atlas_age_18, Atlas_age_65, Atlas_house_size,Atlas_uni_hh) %>%
    tidyr::separate(X1, c("SECC"),
                    sep = " ", extra = "drop") %>%
    dplyr::mutate(type = dplyr::case_when(stringr::str_length(SECC) == 5 ~ "municipality",
                            stringr::str_length(SECC) == 7 ~ "district",
                            stringr::str_length(SECC) == 10 ~ "section"),
           municipality_code = substr(SECC,1,5),
           district_code = substr(SECC,1,7)) %>%
    dplyr::filter(!SECC %in% c("Notas:", "Estadística", "La", "Fuente:", NA))

  tmp_mun <- demographics %>%
    dplyr::filter(type == "municipality") %>%
    dplyr::select(-SECC,-type,-district_code, -Atlas_population) %>%
    dplyr::rename(
      Atlas_ageM=Atlas_age,
      Atlas_age_18M=Atlas_age_18,
      Atlas_age_65M=Atlas_age_65,
      Atlas_house_sizeM=Atlas_house_size,
      Atlas_uni_hhM=Atlas_uni_hh)
  tmp_dis <- demographics %>%
    dplyr::filter(type == "district") %>%
    dplyr::select(-SECC,-type,-municipality_code, -Atlas_population)%>%
    dplyr::rename(
      Atlas_ageD=Atlas_age,
      Atlas_age_18D=Atlas_age_18,
      Atlas_age_65D=Atlas_age_65,
      Atlas_house_sizeD=Atlas_house_size,
      Atlas_uni_hhD=Atlas_uni_hh)

  demographics <- demographics %>%
    dplyr::filter(type == "section") %>%
    dplyr::left_join(tmp_dis, by = "district_code") %>%
    dplyr::left_join(tmp_mun, by = "municipality_code") %>%
    dplyr::mutate(
      Atlas_population = readr::parse_number(dplyr::coalesce(Atlas_population),
                                       locale = readr::locale(grouping_mark = ".")),
      Atlas_age = readr::parse_number(dplyr::coalesce(Atlas_age,Atlas_ageD,Atlas_ageM),
                                    locale = readr::locale(grouping_mark = ".")),
      Atlas_age_18 = readr::parse_number(dplyr::coalesce(Atlas_age_18,Atlas_age_18D,Atlas_age_18M),
                                   locale = readr::locale(grouping_mark = ".")),
      Atlas_age_65 = readr::parse_number(dplyr::coalesce(Atlas_age_65,Atlas_age_65D,Atlas_age_65M),
                                   locale = readr::locale(grouping_mark = ".")),
      Atlas_house_size = readr::parse_number(dplyr::coalesce(Atlas_house_size,Atlas_house_sizeD,Atlas_house_sizeM),
                                       locale = readr::locale(grouping_mark = ".")),
      Atlas_uni_hh = readr::parse_number(dplyr::coalesce(Atlas_uni_hh,Atlas_uni_hhD,Atlas_uni_hhM),
                                      locale = readr::locale(grouping_mark = "."))) %>%
    dplyr::select(SECC, Atlas_population, Atlas_age, Atlas_age_18, Atlas_age_65, Atlas_house_size, Atlas_uni_hh)

  data <- demographics %>%
    dplyr::left_join(income_distribution, by = "SECC") %>%
    dplyr::left_join(income_sources, by = "SECC") %>%
    dplyr::mutate(ABRPP_salaries = salaries/ABRPP,
                  ABRPP_pensions = pensions/ABRPP,
                  ABRPP_unemployment = unemployment/ABRPP,
                  ABRPP_other_benefits = other_benefits/ABRPP,
                  ABRPP_other_incomes = other_incomes/ABRPP) %>%
    dplyr::select(-salaries, -pensions, -unemployment,
           -other_benefits,-other_incomes)

  return(data)

}
