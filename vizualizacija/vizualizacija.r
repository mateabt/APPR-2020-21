# 3. faza: Vizualizacija podatkov




#neto izvoz v milionih zemjevid


  

  zemljevid <- uvozi.zemljevid("https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/50m/cultural/ne_50m_admin_0_countries.zip",
                               
                               "ne_50m_admin_0_countries",mapa="./zemljevidi") %>% fortify()
  
  #da se imena ujemajo
  TRGOVSKE_PARTNERJE$Drzave <-standardize.countrynames(TRGOVSKE_PARTNERJE$Drzave, suggest = "auto", print.changes = FALSE)
  
  zemljevid$SOVEREIGNT <-standardize.countrynames(zemljevid$SOVEREIGNT, suggest = "auto", print.changes = FALSE)
  #napaka in razlike v razpredelnici
  
  TRGOVSKE_PARTNERJE$Drzave[TRGOVSKE_PARTNERJE$Drzave=="Lithuana"] <-"Lithuania"
  TRGOVSKE_PARTNERJE$Drzave[TRGOVSKE_PARTNERJE$Drzave=="Bangladesch"] <-"Bangladesh"
  TRGOVSKE_PARTNERJE$Drzave[TRGOVSKE_PARTNERJE$Drzave=="New Zeeland"] <-"New Zealand"
  TRGOVSKE_PARTNERJE$Drzave[TRGOVSKE_PARTNERJE$Drzave=="Czech Republic"] <-"Czechia"
  plot_data <- TRGOVSKE_PARTNERJE %>% filter(Podatek == "Neto izvoz") %>%
    left_join(zemljevid, ., by=c("SOVEREIGNT"="Drzave"))
  
  zemljevid_neto_izvoz <- 
    ggplot(plot_data, aes(x=long, y=lat, group=group, fill=Vrednost/1e6)) + 
    geom_polygon(size=0.1, color="white") +
    labs(x="", y="", fill="Izvoz - Uvoz", title="Neto izvoz v milijonih") + 
    theme_map(base_size=10) +
    theme(legend.position="right") +
    scale_fill_gradient(low="red3", high="yellow")
  
    
    
     
  
   
  
 plot(zemljevid_neto_izvoz)
 
 
 
 #bubble aminimated zemjevid
 
 pdf1 <-pdf %>%pivot_wider( names_from = "Podatek",values_from = "Vrednost")
 skupaj<-left_join(BDP,pdf1,by=c("Leti"="leto"))
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

 
 
 
 
 #uvoz po klasifikacije pita
 razdelitve1 <-razdelitve %>%pivot_wider( names_from = "Podatek",values_from = "Vrednost")
 
 slices <- c(razdelitve1$uvoz_mio)
 lbls <- c(razdelitve1$`opis blaga`)
 pct <- round(slices/sum(slices)*100)
 pct1 <- paste(pct,"%",sep="")
 lbls <- paste(lbls, pct) # dodaj odstotke na labels 
 lbls <- paste(lbls,"%",sep="") # doda znak % 
 par(mai = c(0,0,1,3))

 
 
 pie_uvoz<-pie(slices, col=rainbow(length(lbls)),
     main="uvoz po razdelitve",clockwise=TRUE,cex=0.5,labels=pct1)
 legend("right", inset=c(-0.95,0),cex=0.5,legend =unique(lbls), bty="n",fill=rainbow(length(lbls)))
 
 #izvoz po klasifikacije
 
 
 slices <- c(razdelitve1$izvoz_mio)
 lbls <- c(razdelitve1$`opis blaga`)
 pct <- round(slices/sum(slices)*100)
 pct1 <- paste(pct,"%",sep="")
 lbls <- paste(lbls, pct) # dodaj odstotke na labels 
 lbls <- paste(lbls,"%",sep="") # doda znak % 
 par(mai = c(0,0,1,3))
 
 
 
 pie_izvoz<-pie(slices, col=rainbow(length(lbls)),
     main="izvoz po razdelitve",clockwise=TRUE,cex=0.5,labels=pct1)
 legend("right", inset=c(-0.95,0),cex=0.5,legend =unique(lbls), bty="n",fill=rainbow(length(lbls)))
 
 
 #