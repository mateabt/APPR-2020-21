# 3. faza: Vizualizacija podatkov


# Izračun neto izvoza(izvoz - uvoz) za posamezne države 

minus <- function(x) sum(x[1],na.rm=T) - sum(x[2],na.rm=T)

NETO_TRGOVSKE<-TRGOVSKE_PARTNERJE %>% 
  mutate(
    neto_izvoz=apply(TRGOVSKE_PARTNERJE[,c('izvoz_v_1000eur','uvoz_v_1000eur')],1,minus) 
)


#neto izvoz v milionih zemjevid


  
  zemljevid <- uvozi.zemljevid("https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/50m/cultural/ne_50m_admin_0_countries.zip",
                               
                               "ne_50m_admin_0_countries",mapa="./zemljevidi") %>% fortify()
  
  #da se imena ujemajo
  NETO_TRGOVSKE$Drzave <-standardize.countrynames(NETO_TRGOVSKE$Drzave, suggest = "auto", print.changes = FALSE)
  
  zemljevid$SOVEREIGNT <-standardize.countrynames(zemljevid$SOVEREIGNT, suggest = "auto", print.changes = FALSE)
  #napaka in razlike v razpredelnici
  
  NETO_TRGOVSKE$Drzave[NETO_TRGOVSKE$Drzave=="Lithuana"] <-"Lithuania"
  NETO_TRGOVSKE$Drzave[NETO_TRGOVSKE$Drzave=="Bangladesch"] <-"Bangladesh"
  NETO_TRGOVSKE$Drzave[NETO_TRGOVSKE$Drzave=="New Zeeland"] <-"New Zealand"
  

  plot_data <- NETO_TRGOVSKE %>% 
    group_by(Drzave) %>% 
    summarise(neto_izvoz=sum(neto_izvoz,na.rm=TRUE)) %>%
    left_join(zemljevid, ., by=c("SOVEREIGNT"="Drzave"))
  
  zemljevid_neto_izvoz <- 
    ggplot(plot_data, aes(x=long, y=lat, group=group, fill=`neto_izvoz`/1e6)) + 
    geom_polygon(data=, size=0.1,color = "white") +
    labs(x="", y="", fill="Izvoz - Uvoz", title = "neto izvoz  v milionih") + 
    theme_map(base_size = 10) +
    theme(legend.position = "right")+
    scale_fill_continuous(label=comma)+
    scale_fill_gradient(low="red3",high="yellow")
    
    
     
  
   
  
 plot1<-plot(zemljevid_neto_izvoz)
 
 
 
 #bubble aminimated zemjevid
 skupaj<-left_join(BDP,pdf,by=c("Leti"="leto"))
 skupaj$Leti<-as.numeric(skupaj$Leti)
 

 
 p<-ggplot(skupaj, aes(x=izvoz/1e3,y=uvoz/1e3 , size =neto_izvoz/1e3)) +
   geom_point(color="red2") +
   scale_x_log10() +
   theme_bw() +
   
   
   labs(title = 'Leto: {floor(frame_time)}',
        x = 'Uvoz v tisočih',
        y = 'Izvoz v tisočih') +
   scale_size(name="Neto izvoz v tisočih",range = c(1,10))+
   transition_time(Leti) +
   ease_aes('linear')+
   shadow_wake(0.2)
 
 
 
 
 renderer = gifski_renderer() 
 animate(p, renderer = gifski_renderer())

 