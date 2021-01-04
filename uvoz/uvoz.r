# 2. faza: Uvoz podatkov

sl <- locale("sl", decimal_mark=",", grouping_mark=".")

#Funkcija ki uvozi nemških trgovskih partnerjev iz xlsx datoteko


UVOZ_TRGOVINSKE_PARTNERJE <- function(){
  stolpci <- c("Drzave_izvoz","izvoz_v_1000eur","Drzave_uvoz","uvoz_v_1000eur")
  uvoz <-read_xlsx("podatki/order-rank-germany-trading-partners-xls .xlsx",sheet="Ranking",
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
        
 
  
  minus <- function(x) sum(x[1],na.rm=T) - sum(x[2],na.rm=T)
  plus <-  function(x) sum(x[1],na.rm=T) + sum(x[2],na.rm=T)
  
  NETO_TRGOVSKE <- TRGOVINSKE_PARTNERJE_TIDY %>% 
    mutate("Neto izvoz"=izvoz_v_1000eur - uvoz_v_1000eur) %>%
    rename(Izvoz=izvoz_v_1000eur, Uvoz=uvoz_v_1000eur) %>%
    pivot_longer(-Drzave, names_to="Podatek", values_to="Vrednost")
  
  
  
  return(NETO_TRGOVSKE)
}


# Zapišimo podatke v razpredelnico TRGOVSKE_PARTNERJE

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
      
      colnames(tabela) <- c("opis blaga","izvoz","uvoz")
      for (col in c("izvoz","uvoz")) {
        if (is.character(tabela[[col]])) {
          tabela[[col]] <- parse_number(tabela[[col]], na="-", locale=sl)
        }
      }
      
      minus <- function(x) sum(x[1],na.rm=T) - sum(x[2],na.rm=T)
      plus <-  function(x) sum(x[1],na.rm=T) + sum(x[2],na.rm=T)
      
      NETO_RAZDELITVE<-tabela %>% 
        mutate(
          'neto izvoz'=apply(tabela[,c('izvoz','uvoz')],1,minus) 
        )
      
      NETO_RAZDELITVE<-NETO_RAZDELITVE %>% 
        mutate(
          obrt=apply(tabela[,c('izvoz','uvoz')],1,plus) 
        )
      
      NETO_RAZDELITVE <- NETO_RAZDELITVE %>% 
        pivot_longer(-`opis blaga`, names_to="Podatek", values_to="Vrednost v milionih")
      
      
      
      return(NETO_RAZDELITVE)
}


# Zapišimo podatke v razpredelnico razdelitve

razdelitve<-UVOZ_RAZDELITVE()


#Funkcija ki uvozi podatke o splošni razvoj zunanje trgovine po letih iz pdf datoteke

Uvoz_pdf<-function(){
  PDF_uvoz <- pdf_text("podatki/overall-development-foreign-trade.pdf") %>% 
              str_split("\n")
          ## selecting podatke ki rabimo
  PDF_uvoz <- as.data.frame(PDF_uvoz[[2]][8:77]) %>% 
    ## poimenovanje za lazje delo
    rename(podatke = `PDF_uvoz[[2]][8:77]`)%>%
   
    
  ## ciscenje stolpce
  mutate(
    ## določite vzorec za povezavo pozitivnih in negativnih znakov z zadnjima dvema stolpcema
    ## (? <= [\\ + -]) je pogled nazaj ki nam kaze  "poiščite + ali -, ki je pred naslednjo vrednostjo, ki je več kot 1 presledek \\ s +
    ## (? = [0-9]) je pozitiven pogled naprej, ki pravi: "poiščite več presledkov, ki jim sledi številka"
    
    podatke = str_remove_all(podatke, "(?<=[\\+-])\\s+(?=[0-9])"),
    
    ## odstrani odvečne presledke in jih zamenja s podpičjem, da prepoznamo prelome stolpcev
    podatke = str_replace_all(podatke, "\\s{2,}", ";"),
    
    ## vejico v decimalnih številkah zamenjamo s piko 
    podatke = str_replace_all(podatke, ",", "\\."),
    
    ## odstrani nove vrstice, odvečne podpičja in pozitivne znake
    podatke = str_remove_all(podatke, "\\r|\\+|^;")
  ) %>% 
  separate(
    col = podatke ,
    
    ## preimenovanja stolpce
    into = c('leto', 'izvoz', 'uvoz', 'neto_izvoz', 'perc_change_ex','perc_change_im'),
    
    ## delimetar za stolpce
    sep = ";"
  ) %>% 
  ## končno čiščenje prostorov stolpcev in prisila v dvojni format.
  mutate(across(c(izvoz, uvoz, neto_izvoz ,perc_change_ex ,perc_change_im), ~ as.double(str_remove_all(.x, "\\s"))))
  PDF_uvoz<-PDF_uvoz[,-(4:6)]
  
  minus <- function(x) sum(x[1],na.rm=T) - sum(x[2],na.rm=T)
  plus <-  function(x) sum(x[1],na.rm=T) + sum(x[2],na.rm=T)
  
  pdf<-PDF_uvoz %>% 
    mutate(
      neto_izvoz=apply(PDF_uvoz[,c('izvoz','uvoz')],1,minus) 
    )
  
 pdf<-pdf %>% 
    mutate(
      obrt=apply(PDF_uvoz[,c('izvoz','uvoz')],1,plus) 
    )
  
 pdf$leto <- as.numeric(pdf$leto)
 
  
  pdf<-pdf%>% 
    pivot_longer(-leto, names_to="Podatek", values_to="Vrednost v milionih")
  return(pdf)
}

# Zapišimo podatke v razpredelnico pdf
pdf<-Uvoz_pdf()




# BDP po leti of 1991 nominalna cena baza 2010 v milionih
UVOZ_BDP <- function(){
  uvoz<-read.delim(file = 'podatki/BDP.tsv', sep = '\t', header =FALSE,)
  uvoz<-t(as.matrix(uvoz))
  rownames(uvoz) <- NULL
  uvoz<-uvoz[-1,]
  colnames(uvoz) <- c("Leti","BDP")
  uvoz.df <- as.data.frame(uvoz)
  
return(uvoz.df)
} 
BDP<-UVOZ_BDP()




















# Če bi imeli več funkcij za uvoz in nekaterih npr. še ne bi
# potrebovali v 3. fazi, bi bilo smiselno funkcije dati v svojo
# datoteko, tukaj pa bi klicali tiste, ki jih potrebujemo v
# 2. fazi. Seveda bi morali ustrezno datoteko uvoziti v prihodnjih
# fazah.
