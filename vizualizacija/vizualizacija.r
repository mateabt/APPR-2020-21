# 3. faza: Vizualizacija podatkov
source("lib/uvozi.zemljevid.r", encoding = "UTF-8")
source("lib/libraries.r", encoding = "UTF-8")
source("uvoz/uvoz.r", encoding = "UTF-8")
library(StandardizeText)

# Izračun neto izvoza(izvoz - uvoz) za posamezne države 

minus <- function(x) sum(x[1],na.rm=T) - sum(x[2],na.rm=T)

NETO_TRGOVSKE<-TRGOVSKE_PARTNERJE %>% 
  mutate(
    neto_izvoz=apply(TRGOVSKE_PARTNERJE[,c('izvoz_v_1000eur','uvoz_v_1000eur')],1,minus) 
)


NETO_TRGOVSKE$Drzave <-standardize.countrynames(NETO_TRGOVSKE$Drzave, suggest = "auto",print.changes = FALSE)

  
  zemljevid <- uvozi.zemljevid("https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/50m/cultural/ne_50m_admin_0_countries.zip",
                               
                               "ne_50m_admin_0_countries",mapa="./zemljevidi") %>% fortify()
  

  
  zemljevid_neto_izvoz <- ggplot() + 
    geom_polygon(data=left_join(zemljevid, NETO_TRGOVSKE %>% group_by(Drzave) %>% summarise(neto_izvoz=sum(neto_izvoz,na.rm=TRUE)),by=c("SOVEREIGNT"="Drzave")), 
                 aes(x=long, y=lat, group=group, fill=`neto_izvoz`), size=0.1) +
    labs(x="", y="", fill="Izvoz - Uvoz", title = "neto izvoz") + 
    theme_map(base_size = 20) +
    theme(legend.position = "bottom")
  
  