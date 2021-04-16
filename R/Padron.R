get_padron <- function(year=2020, municipality="02", stat="Age", language ="EN"){
  #browser()
  statistic <- case_when(stat == "Age" ~ "01",
                         stat == "Nationality" ~"02",
                         stat == "Origin" ~ "05")

  url <- paste0("https://servicios.ine.es/wstempus/js/", language, "/DATOS_TABLA/t20/e245/p07/a",
                year, "/l0/", municipality, statistic, ".px")


  data_ine <- httr::GET(url)

  data_ine <- data_ine$content %>%
    rawToChar() %>%
    fromJSON()

  values <- do.call(rbind, data_ine$Data)

  data_ine <- cbind(data_ine,values) %>%
    select(Nombre, Valor) %>%
    tidyr::separate(Nombre, c("Gender", "Section", stat), sep = ", ") %>%
    mutate(Municipality = municipality) %>%
    rename(Value = Valor)
  return(data_ine)}


get_data <- function(year=2019, language ="EN", municipalities){
  stats <- c("Age", "Nationality", "Origin")
  data <- list()
  #checks if the padron file already exists in the directory
  #creates a list with codes for the 52 municipalities
  if (missing(municipalities)){
    path <- file.path("data/INE/Padron", year, "Spain")
    municipalities <- c( "01", "02","02","03","04","05","06","07","08","09", as.character(rep(10:52)))
  } else {
    path <- file.path("data/INE/Padron", year, paste(municipalities, collapse = "_"))
  }

  if (file.exists(file.path(path,  "padron.RDS"))){
    data <- readRDS(file.path(path,  "padron.RDS"))} else{
      #browser()
      dir.create(file.path(path), recursive = TRUE)

      # get data
      for (stat in stats){
        out <- NULL
        for (municipality in municipalities){
          tmp <- get_padron(year=year, municipality=municipality, stat= stat, language = language)
          out <- rbind(out,tmp)}
        data[[stat]] <- out}
    }
  saveRDS(data, file.path(path, "padron.RDS"))
  return(data)
}


transform_padron <- function(x){
  #browser()

  stats <- c("Age", "Nationality", "Origin")
  data <- list()

  data$pop <- x$Age %>%
    filter(Section != "TOTAL", Age == "Total") %>%
    select(-Age, -Municipality) %>%
    pivot_wider(names_from = Gender, values_from = Value) %>%
    rename(Population = "Both sexes",
           Male = "Males",
           Female = "Females")

  for (stat in stats){
    data[[paste0(stat)]] <- x[[paste0(stat)]] %>%
      filter(Section !="TOTAL", !!as.name(stat)!="Total", Gender == "Both sexes") %>%
      pivot_wider(names_from = !!as.name(stat) , values_from = Value) %>%
      select(-Gender, -Municipality)}

  out <- reduce(data, left_join, by = "Section")

  return(out)
}







