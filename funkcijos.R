###------------------- missingHours -----------
# Input:
#   * datetime formato vektorius (vector_datetime, POSIXct);
#   * pradine valanda nuo kurios prasideda datos (starting_hour, integer);
#   * galima pasirinkti kad atspausdintu tarp kokiu datu truko valandu (warnings_print, logical)
#   * galima pasirinkti kad grazintu lentele su praleistomis valandomis. (warnings_return, logical)
# 
# Output:
#   * warnings lentele (warnings, tibble)
#
missingHours <-
  function(vector_datetime,
           starting_hour = 11,
           warnings_print = F,
           warnings_return = F) {
    
    # Pradine valanda nuo kurios prasideda failai.
    hr = starting_hour
    
    # Lentele i kuria desime praleistus intervalus.
    warnings <-
      tibble(
        "start_date" = as_datetime(character()),
        "end_date" = as_datetime(character()),
        "time_difference" = character()
      )
    
    
    for (i in 1:length(vector_datetime)) {
      # Jei valanda is vektoriaus neatitinka valandos, kuri turetu buti, ikeliame datu intervala.
      if (hour(vector_datetime[i]) != hr) {
        warnings <-
          add_row(
            warnings,
            start_date = vector_datetime[i - 1],
            end_date = vector_datetime[i],
            time_difference = as.character.Date(vector_datetime[i] - vector_datetime[i - 1])
          )
      }
      # Kai neatitinka, didiname valandas kol atitinka. Jei 24, pradedame is naujo (0-23).
      while (hour(vector_datetime[i]) != hr) {
        hr = hr + 1
        if (hr == 24)
          hr = 0
      }
      
      # Jei viskas k, tesiame tikrinim±.
      hr = hr + 1
      if (hr == 24)
        hr = 0
    }
    
    # Jeigu kazkas negerai, pranesame vartotojui.
    if (nrow(warnings) != 0)
      warning("Vektoriuje yra praleistu datu.")
    
    if (warnings_print == T)
      print(warnings)
    
    if (warnings_return == T)
      return(warnings)
  }


###------------------- getSunriseTimes ---------
# Input:
#   * html failo lentelìs (orai_nodes, xml_nodeset).
# 
# Output:
#   * i¹ to failo i¹gautas saulìs tekìjimo ir leidimosi laikas. Nereikalingas, taèiau galimybì t± padaryti 
# yra (orai_sunrise_times, character).
#
getSunriseTimes <- function(orai_nodes){
  
  # I¹ metadata dalies, i¹renka laikus.
  orai_sunrise_times <- orai_nodes %>%
    html_nodes(xpath = '//tr//*[@class="day_metainfo"]//div') %>%
    html_text() %>%
    # Sutvarko netvarking± string'±.
    str_replace_all("[\r\n*{\t}]" , "") %>%
    str_split("                ")
  
  return(orai_sunrise_times)
}


###------------------ getRawDates (raw) --------
# Input:
#   * html failo lentelìs (orai_nodes, xml_nodeset).
# 
# Output:
#   * i¹ to failo i¹gautos datos, grynuoju character formatu. (orai_dates_text, character)
#
getRawDates <- function(orai_nodes){
  
  # Getting text from date nodes
  orai_dates_text <- orai_nodes %>%
    html_nodes(xpath = '//tr//*[@class="day_head"]') %>%
    html_text()
  
  return(orai_dates_text)
}


###------------------ getDatesEnglish -----------
# Input:
#   * html failo lentelìs (orai_nodes, xml_nodeset).
#   * parametras kuris leid¾ia datas gra¾inti character arba POSIXct formatu (as_string, logical)
# 
# Output:
#   * Datos paverstos i¹ grynù Lietuvi¹kù ç Anglu k., datos arba teksto formatu 
# (dates_final_char, tibble, character // dates_final, tibble, POSIIXct)
#
getDatesEnglish <- function(orai_nodes, as_string = TRUE){
  
  # Nuskaito grynasias datas
  dates_lith <- enc2native(tolower(getRawDates(orai_nodes)))
  
  # Mìnesiù pavadinimai. ISSUE: JEIGU NAUDOJI PER SOURCE, NENUSKAITO LIETUVISKU RAIDZIU
  months_lith <- c("sausio", "vasario", "kovo", "baland¾io", "gegu¾ìs", "bir¾elio", "liepos", "rugpjþèio", "rugsìjo", "spalio", "lapkrièio", "gruod¾io")
  months_en <- tolower(month.name)
  
  # Patternas, randantis angli¹kus lietuvi¹kù mìnesiù pavadinimù atitikmenis.
  pattern <- str_c(
    "(",
    str_c(months_lith, collapse = "|"),
    "){1}",
    '\\s\\d+'
  )
  
  # Sumatchina ir sudeda ç lentelê.
  dates_match <- as.tibble(str_match(dates_lith, pattern))
  
  # Sutvarko gryn±jç tekst± ir padaro jç datetime formatu.
  dates_final <- dates_match %>%
    mutate(
      # I¹traukia tik mìnesio dienù skaièiù.
      day_of_month = as.numeric(
        str_match(V1, "\\d+")
      ),
      # Pagal atitinkanèi± viet± vektoriuje, paverèia lietuvi¹kus mìnesius ç angli¹kus.
      month_en = months_en[match(dates_match$V2, months_lith)]) %>%
    # Sujungia ç mìnesis diena format±.
    mutate(date_final = str_c(month_en, day_of_month, sep = " ")) %>%
    # Paverèia ç datetime objekt±.
    mutate(date_final = parse_date_time(date_final, "%m %d", tz = Sys.timezone())) %>%
    select(date_final)
  
  # Gra¾inamas arba teksto arba datos formatu.
  if (as_string == TRUE){
    dates_final_char <- dates_final %>%
      mutate(date_final = strftime(date_final, format = "%B %d, %A")) %>%
      pull(date_final)
    return(dates_final_char)
  } else {
    return(dates_final %>% pull(date_final))
  }
}


###------------------- getWeatherTable ---------
# Input:
#   * miesto pavadinimas (city, character)
#   * github failù pavadinimù vektorius, gautas prad¾ioje (weather_path, character)
# 
# Output:
#   * visos orù prognozìs tam tikram miestui, vienoje lentelìje (weather_table_final, tibble)
#
getWeatherTable <- function(city, weather_path) {
  
  # Tikrina ar miestas teisingai pasirinktas.
  city <- tolower(city)
  if (!(city %in%  c("vilnius", "kaunas", "klaipeda")))
    stop("Miestas nìra i¹ galimù variantù. (Vilnius, Kaunas, Klaipeda)")
  
  city_pattern <- str_c(city, "_")
  
  # Atrenka tik pasirinkto miesto failus.
  weather_path_city <- weather_path %>%
    .[which(str_match(weather_path, "\\w*_") == city_pattern)]
  
  # Padaro i¹ jù nuorod± ç GitHub.
  weather_urls <-
    str_c(
      "https://raw.githubusercontent.com/vzemlys/eda_2017/master/data/",
      weather_path_city
    )
  
  
  weather_table_final <- tibble()
  
  # Kiekvienai nuorodai padarys po lentelê ir prijungs priei galutinìs
  for (weather_url in weather_urls) {
    
    # Nuskaito fail±.
    weather <- read_html(getURL(weather_url))
    
    # Atrenka oraiTable lenteles.
    weather_nodes <- weather %>%
      html_nodes(xpath = '//*[@class="weather_box"]//table')
    
    # html_table sudeda ç lenteles, bet prastai.
    weather_table_raw <- weather_nodes %>%
      html_table()
    
    
    
    ### Nuo èia kartojasi kodas, naudotas 7-8 u¾duotyje.
    
    weather_table_temp <- tibble()
    
    # Funkcij± kuri gra¾ins mums dat± (Detaliau yra apra¹yta paèiame faile getDatesEnglish.R).
    weather_dates <-
      getDatesEnglish(weather_nodes, as_string = T) #String formatas gra¾iau atrodo.
    
    # Sutvarko visas lenteles ir sudeda ç vien±.
    for (i in seq_along(weather_table_raw)) {
      
      temp_tbl <- as.tibble(weather_table_raw[[i]])
      temp_tbl_final <- temp_tbl %>%
        # Pa¹alina iconìliù stulpelç (debesuota, saulìta, etc.).
        select(-X2) %>%
        # Pervadina stulpelius atitinkamais pavadinimais.
        `colnames<-`(slice(., 2)) %>%
        # Pa¹alin± eilutê, kuri± naudojom pavadinimams sudìti.
        slice(3:n()) %>%
        # Prideda datos ir laiko eilutes.
        mutate("Data" = weather_dates[i]) %>%
        select(Data, Laikas:Krituliai) %>%
        separate(Laikas,
                 into = c("Pradzia", "Pabaiga"),
                 sep = " - ") %>%
        mutate(Pradzia = as.integer(str_sub(Pradzia, end = 2)),
               Pabaiga = as.integer(str_sub(Pabaiga, end = 2)))
      
      # Prideda sutvarkyt± lentelê.
      weather_table_temp <-
        rbind(weather_table_temp, temp_tbl_final)
    }
    
    # Prided± lenteliù lentelê prie galutinìs lentelìs.
    weather_table_final <- weather_table_final %>%
      bind_rows(weather_table_temp)
  }
  
  # Pridedu stulpeli su miesto pavadinimu
  return(weather_table_final %>% add_column("Miestas" = city))
}


###------------------- fixJson ---------
# Input:
#   * JSON(?) formato, bet sugadintas failas (weird_json, raw text)
# 
# Output:
#   * sutvarkyta lentelì (tibble)
#
fixJson <- function(weird_json) {
  
  # Pa¹alina teksto dalis kurios lau¾o kod±, gra¾ina lentelê.
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
#   * github failù pavadinimù vektorius, gautas prad¾ioje (weather_path, character)
# 
# Output:
#   * visos orù prognozìs ciurlionio stoèiai, vienoje lentelìje (weather_table_final, tibble)
#
getWeatherCiurlionis <- function(weather_path) {
  
  # Atrenka failù pavadinimus.
  weather_path_city <- weather_path %>%
    .[which(str_match(weather_path, "\\w*_") == "ciurlionis_")]
  
  # Padaro i¹ jù urls
  weather_urls <-
    str_c(
      "https://raw.githubusercontent.com/vzemlys/eda_2017/master/data/",
      weather_path_city
    )
  
  weather_table_final <- tibble()
  
  # Parsiunèia failus ir sudeda ç lentelê.
  for (i in seq_along(weather_urls)){
    
    weather_raw <- getURL(weather_urls[i])
    
    if (weather_raw != ""){
      # Sutvarko format± ir nuskaito kaip JSON, po to pakeièia ç numeric i¹ character.
      weather_table_temp <- fixJson(weather_raw)
      
      weather_table_temp <- weather_table_temp %>%
        #Prideda laiko ir datos stulpelius, pagal failo pavadinim±.
        mutate(Date = ymd(str_match(weather_path[i], "\\d{4}-\\d{2}-\\d{2}")),
               Time = str_match(weather_path[i], "\\d{2}:\\d{2}:\\d{2}")) %>%
        # I¹skirsto laik±.
        separate(Time, into = c("Hour", "Minute", "Second"), sep = ":") %>%
        # I¹dìlioja patogiu formatu.
        select(Date:Second, Temp:WD)
      
    } else {
      next()
    }
    
    # Sudeda ç lentelê.
    weather_table_final <- weather_table_final %>%
      bind_rows(weather_table_temp)
  }
  return(weather_table_final)
}
