# 2. faza: Uvoz podatkov

sl <- locale("sl", decimal_mark=",", grouping_mark=".")

#Funkcija ki uvozi nemških trgovskih partnerjev iz xlsx datoteko


UVOZ_TRGOVINSKE_PARTNERJE <- function(){
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



TRGOVSKE_PARTNERJE<-UVOZ_TRGOVINSKE_PARTNERJE()


#Funkcija ki uvozi podatki o Izvoz in uvoz po razdelitvi klasifikacije proizvodov iz html


UVOZ_RAZDELITVE<- function() {
      link <-"https://www.destatis.de/EN/Themes/Economy/Foreign-Trade/Tables/imports-exports.html?fbclid=IwAR2tVMBoA4bC6YXHvIVtzUXAD99eHUwEJLo6MAWLsQ31lm039Qm81uSTOFU"
      stran <- html_session(link) %>% read_html()
      tabela <- stran %>% html_nodes(xpath="//table[@class='wide']") %>%
        .[[1]] %>% html_table(dec=",", fill = TRUE)
      for (i in 1:ncol(tabela)) {
        if (is.character(tabela[[i]])) {
          Encoding(tabela[[i]]) <- "UTF-8"
        }
      }
      tabela$Division <- NULL
      tabela<-tabela[-c(1,2,33),]
      colnames(tabela) <- c("opis blaga","izvoz_mio","uvoz_mio")
      
      return(tabela)
}


razdelitve<-UVOZ_RAZDELITVE()


















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
