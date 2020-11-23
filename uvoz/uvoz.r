# 2. faza: Uvoz podatkov

sl <- locale("sl", decimal_mark=",", grouping_mark=".")

#Funkcija ki uvozi nemških trgovskih partnerjev iz xlsx datoteko


UVOZI_TRGOVINSKE_PARTNERJE <- function(){
  stolpci <- c("Drzave_izvoz","izvoz_v_1000eur","Drzave_uvoz","uvoz_v_1000eur")
  uvoz <-read_xlsx("order-rank-germany-trading-partners-xls .xlsx",sheet="Ranking",
                   range = "B8:E251", col_names=stolpci,
                   na= "-",)
  uvoz <- uvoz[-c(240, 241), ]
  
  TRGOVINSKE_PARTNERJE_TIDY <- uvoz
  colnames(TRGOVINSKE_PARTNERJE_TIDY) <-  c("Drzave_izvoz","izvoz_v_1000eur","Drzave_uvoz","uvoz_v_1000eur")
  
  TRGOVINSKE_PARTNERJE_TIDY<-
  full_join(
    TRGOVINSKE_PARTNERJE_TIDY %>% select(Drzave_izvoz, izvoz_v_1000eur),
    TRGOVINSKE_PARTNERJE_TIDY %>% select(Drzave_uvoz,uvoz_v_1000eur),
    by = c('Drzave_izvoz' = 'Drzave_uvoz')
  )
  TRGOVINSKE_PARTNERJE_TIDY<- TRGOVINSKE_PARTNERJE_TIDY %>%
    rename(Drzave = Drzave_izvoz)
        
                
  
  return(TRGOVINSKE_PARTNERJE_TIDY)
}



TRGOVSKE_PARTNERJE<-UVOZI_TRGOVINSKE_PARTNERJE()


#Funkcija ki uvozi podatki o Izvoz in uvoz po razdelitvi klasifikacije proizvodov iz html





















# Funkcija, ki uvozi občine iz Wikipedije
uvozi.obcine <- function() {
  link <- "http://sl.wikipedia.org/wiki/Seznam_ob%C4%8Din_v_Sloveniji"
  stran <- html_session(link) %>% read_html()
  tabela <- stran %>% html_nodes(xpath="//table[@class='wikitable sortable']") %>%
    .[[1]] %>% html_table(dec=",")
  for (i in 1:ncol(tabela)) {
    if (is.character(tabela[[i]])) {
      Encoding(tabela[[i]]) <- "UTF-8"
    }
  }
  colnames(tabela) <- c("obcina", "povrsina", "prebivalci", "gostota", "naselja",
                        "ustanovitev", "pokrajina", "regija", "odcepitev")
  tabela$obcina <- gsub("Slovenskih", "Slov.", tabela$obcina)
  tabela$obcina[tabela$obcina == "Kanal ob Soči"] <- "Kanal"
  tabela$obcina[tabela$obcina == "Loški potok"] <- "Loški Potok"
  for (col in c("povrsina", "prebivalci", "gostota", "naselja", "ustanovitev")) {
    if (is.character(tabela[[col]])) {
      tabela[[col]] <- parse_number(tabela[[col]], na="-", locale=sl)
    }
  }
  for (col in c("obcina", "pokrajina", "regija")) {
    tabela[[col]] <- factor(tabela[[col]])
  }
  return(tabela)
}

# Funkcija, ki uvozi podatke iz datoteke druzine.csv
uvozi.druzine <- function(obcine) {
  data <- read_csv2("podatki/druzine.csv", col_names=c("obcina", 1:4),
                    locale=locale(encoding="Windows-1250"))
  data$obcina <- data$obcina %>% strapplyc("^([^/]*)") %>% unlist() %>%
    strapplyc("([^ ]+)") %>% sapply(paste, collapse=" ") %>% unlist()
  data$obcina[data$obcina == "Sveti Jurij"] <- iconv("Sveti Jurij ob Ščavnici", to="UTF-8")
  data <- data %>% pivot_longer(`1`:`4`, names_to="velikost.druzine", values_to="stevilo.druzin")
  data$velikost.druzine <- parse_number(data$velikost.druzine)
  data$obcina <- parse_factor(data$obcina, levels=obcine)
  return(data)
}

# Zapišimo podatke v razpredelnico obcine
obcine <- uvozi.obcine()

# Zapišimo podatke v razpredelnico druzine.
druzine <- uvozi.druzine(levels(obcine$obcina))

# Če bi imeli več funkcij za uvoz in nekaterih npr. še ne bi
# potrebovali v 3. fazi, bi bilo smiselno funkcije dati v svojo
# datoteko, tukaj pa bi klicali tiste, ki jih potrebujemo v
# 2. fazi. Seveda bi morali ustrezno datoteko uvoziti v prihodnjih
# fazah.
