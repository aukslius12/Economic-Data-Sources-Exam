###------------------- missingHours -----------
# Input:
#   * datetime formato vektorius (vector_datetime, POSIXct);
#   * pradin� valanda nuo kurios prasideda datos (starting_hour, integer);
#   * galima pasirinkti kad atspausdint� tarp koki� dat� tr�ko valand� (warnings_print, logical)
#   * galima pasirinkti kad gr��int� lentel� su praleistomis valandomis (warnings_return, logical)
#
# Output:
#   * warnings lentel� (warnings, tibble)
#
missingHours <-
  function(vector_datetime,
           starting_hour = 11,
           warnings_print = F,
           warnings_return = F) {
    hr = starting_hour
    
    warnings <-
      tibble(
        "start_date" = as_datetime(character()),
        "end_date" = as_datetime(character()),
        "time_difference" = character()
      )
    
    
    for (i in 1:length(vector_datetime)) {
      # Jei valanda i� vektoriaus neatitinka valandos, kuri turet� b�ti, �keliame dat� interval� � warnings.
      if (hour(vector_datetime[i]) != hr) {
        warnings <-
          add_row(
            warnings,
            start_date = vector_datetime[i - 1],
            end_date = vector_datetime[i],
            time_difference = as.character.Date(vector_datetime[i] - vector_datetime[i - 1])
          )
      }
      # Kai neatitinka, didiname valandas kol atitinka.
      while (hour(vector_datetime[i]) != hr) {
        hr = hr + 1
        if (hr == 24)
          hr = 0
      }
      
      hr = hr + 1
      if (hr == 24)
        hr = 0
    }
    
    if (nrow(warnings) != 0)
      warning("Vektoriuje yra praleistu datu.")
    
    if (warnings_print == T)
      print(warnings)
    
    if (warnings_return == T)
      return(warnings)
  }


###------------------- fixJson ---------
# Input:
#   * JSON(?) formato, bet sugadintas(?) failas (weird_json, raw text)
#
# Output:
#   * sutvarkyta lentel� (tibble)
#
fixJson <- function(weird_json) {
  # Pa�alina teksto dalis kurios lau�o kod�, gr��ina lentel�.
  return(
    str_replace_all(weird_json, "wup\\(|\\);", "") %>%
      str_replace(",\n\\}", "\n\\}") %>%
      fromJSON() %>%
      map(as.numeric) %>%
      as_tibble()
  )
}


###------------------- getWeatherCiurlionis ---------
# Input:
#   * github fail� pavadinim� vektorius, gautas prad�ioje (weather_path, character)
#
# Output:
#   * visos or� prognoz�s ciurlionio sto�iai, vienoje lentel�je (weather_table_final, tibble)
#
getWeatherCiurlionis <- function(weather_path) {
  # Atrenka fail� pavadinimus.
  weather_path_city <- weather_path %>%
    .[which(str_match(weather_path, "\\w*_") == "ciurlionis_")]
  
  weather_urls <-
    str_c(
      "https://raw.githubusercontent.com/vzemlys/eda_2017/master/data/",
      weather_path_city
    )
  
  weather_table_final <- tibble()
  
  # Parsiun�ia failus ir sudeda i lentel�.
  for (i in seq_along(weather_urls)) {
    weather_raw <- getURL(weather_urls[i])
    
    if (weather_raw != "") {
      weather_table_temp <- fixJson(weather_raw)
      
      weather_table_temp <- weather_table_temp %>%
        #Prideda laiko ir datos stulpelius, pagal failo pavadinim�.
        mutate(
          Date = ymd(str_match(
            weather_path[i], "\\d{4}-\\d{2}-\\d{2}"
          )),
          Time = str_match(weather_path[i], "\\d{2}:\\d{2}:\\d{2}")
        ) %>%
        # I�skirsto � HH:MM:SS format�, paver�ia � integer.
        separate(Time,
                 into = c("Hour", "Minute", "Second"),
                 sep = ":") %>%
        select(Date:Second, Temp:WD) %>%
        mutate(
          Hour = as.integer(Hour),
          Minute = as.integer(Minute),
          Second = as.integer(Second)
        )
      
    } else {
      next()
    }
    
    weather_table_final <- weather_table_final %>%
      bind_rows(weather_table_temp)
  }
  
  return(weather_table_final)
}


###------------------- getWeatherTable ---------
# Input:
#   * miesto pavadinimas (city, character)
#   * github fail� pavadinim� vektorius, gautas prad�ioje (weather_path, character)
#
# Output:
#   * visos oru prognoz�s tam tikram miestui, vienoje lentel�je (weather_table_final, tibble)
#
getWeatherTable <- function(city, weather_path) {
  # Tikrina ar miestas teisingai pasirinktas.
  city <- tolower(city)
  if (!(city %in%  c("vilnius", "kaunas", "klaipeda")))
    stop("Miestas nera is galimu variantu. (Vilnius, Kaunas, Klaipeda)")
  
  # Atrenka tik pasirinkto miesto failus.
  city_pattern <- str_c(city, "_")
  weather_path_city <- weather_path %>%
    .[which(str_match(weather_path, "\\w*_") == city_pattern)]
  
  weather_urls <-
    str_c(
      "https://raw.githubusercontent.com/vzemlys/eda_2017/master/data/",
      weather_path_city
    )
  
  # Kiekvienai nuorodai padaro po lentel� ir prijungia prie galutin�s.
  weather_table_final <- tibble()
  for (j in seq_along(weather_urls)) {
    weather_url <- getURL(weather_urls[j])
    
    # Patikrina ar failas ne tu��ias.
    if (weather_url == "")
      next()
    weather <- read_html(weather_url)
    
    # Atrenka oraiTable lenteles.
    weather_nodes <- weather %>%
      html_nodes(xpath = '//*[@class="weather_box"]//table')
    
    weather_table_raw <- weather_nodes %>%
      html_table()
  
    # Sutvarko visas html lenteles ir sudeda i vien�.
    weather_table_temp <- tibble()
    for (i in seq_along(weather_table_raw)) {
      temp_tbl <- as.tibble(weather_table_raw[[i]])
      temp_tbl_final <- temp_tbl %>%
        # Pa�alina iconeliu stulpel� (debesuota, saul�ta, etc.).
        select(-X2) %>%
        # Pervadina stulpelius atitinkamais pavadinimais.
        `colnames<-`(sapply(slice(., 2), enc2native)) %>%
        # Pa�alina eilut�, kuri� naudojo pavadinimams sud�ti.
        slice(3:n()) %>%
        # Prideda laiko ir datos eilutes.
        separate(Laikas,
                 into = c("Pradzia", "Pabaiga"),
                 sep = " - ") %>%
        mutate(Pradzia = str_sub(Pradzia, end = 2),
               Pabaiga = str_sub(Pabaiga, end = 2)) %>%
        # I� failo pavadinimo suranda dat�, prideda ciklo numeri kaip dien� (Prognoz�s yra 10d � priek�).
        mutate(Data = ymd(
          str_match(weather_path_city[j], pattern = "\\d{4}-\\d{2}-\\d{2}")
        ) + days(i - 1)) %>%
        select(Data, Pradzia:Krituliai)
      
      # Prideda sutvarkyt� lentel�.
      weather_table_temp <-
        rbind(weather_table_temp, temp_tbl_final)
    }
    
    # Tikrina ar lentele ne tu�cia.
    if (nrow(weather_table_temp) == 0)
      next()
  
    weather_table_final <- weather_table_final %>%
      bind_rows(weather_table_temp)
    
  }
  
  # Format� tvarkymas.
  weather_table_final <- weather_table_final %>%
    add_column("Miestas" = city) %>%
    mutate(Pradzia = as.integer(Pradzia),
           Pabaiga = as.integer(Pabaiga))
  
  return(weather_table_final)
}
