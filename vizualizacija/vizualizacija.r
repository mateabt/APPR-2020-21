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
  
    
    
 #bubble aminimated zemjevid
 
 pdf1 <-pdf %>%pivot_wider( names_from = "Podatek",values_from = "Vrednost v milionih")
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
 
 
 
 
 #renderer = gifski_renderer() 
 animacija<-animate(p, renderer = gifski_renderer())

 
 
 
 
 #lazji racun
 
 razdelitve1 <-razdelitve %>%pivot_wider( names_from = "Podatek",values_from = "Vrednost v milionih")
 

 
 
 
 #Uvoz in izvoz 2019
 U<-filter(pdf, leto == 2019, Podatek == "uvoz")
 I<-filter(pdf, leto == 2019, Podatek == "izvoz")
 
 
 data <- rbind(U,I)
 
 data[,1]<-NULL
 
 # izracun odtstotka
 data$odstotek <- data$`Vrednost v milionih` / sum(data$`Vrednost v milionih`)
 #preracun za pozicije legende
 data$ymax <- cumsum(data$odstotek)
 data$ymin <- c(0, head(data$ymax, n=-1))
 
 # pozicija legende
 data$pozicija <- (data$ymax + data$ymin) / 2
 
 # label
 data$label <- paste0(data$Podatek, "\n Vrednost v % ", round(data$odstotek,2)*100)
 
 # plot
 graf4<-ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=`Vrednost v milionih`)) +
   geom_rect() +
   coord_polar(theta="y") + 
   xlim(c(2, 4))+
   theme_void() +
   theme(legend.position = "none")+
   scale_fill_gradient(low="darkorange2", high="yellow")+
   geom_label( x=3.5, aes(y=pozicija, label=label), size=4)+
   ggtitle("Trgovina leta 2019",)+
   theme(
     plot.title = element_text(hjust = 0.5, size = 20))
 
 

 #geom_bar za uvoz po razdelitve

graf5<-ggplot(data=razdelitve1, aes(x=reorder(`opis blaga`,uvoz), y=uvoz)) +
  geom_bar(stat="identity", fill="darkorange2")+ coord_flip()+theme_minimal()+
  geom_text(aes(label=uvoz), size=3.5,hjust=0.5,position = position_dodge(width = 1))

stolpicni_uvoz<-graf5+xlab("panoge")
#geom_bar za izovz po razdelitve
graf6<-ggplot(data=razdelitve1, aes(x=reorder(`opis blaga`,izvoz), y=izvoz)) +
  geom_bar(stat="identity", fill="darkorange2")+ coord_flip()+theme_minimal()+
  geom_text(aes(label=uvoz),size=3.5,hjust=0.5,position = position_dodge(width = ))

stolpicni_izvoz<-graf6+xlab("panoge")





#bar_plot + liniski graf za neto izvoz po letih in bdp
df<-left_join(pdf1[pdf1$leto>=1991,],BDP, by=c("leto"="Leti"))
graf7<-ggplot(df, aes(leto)) +
  geom_bar(aes(y=neto_izvoz), fill="yellow",color="black",stat="identity") +ylab("vrednost v milionih")+  
  geom_line(aes(y=BDP), colour="red") +scale_y_continuous(labels=scales::comma)+theme_minimal()
  




#Izvoz EU
plot_data1 <- TRGOVSKE_PARTNERJE %>% filter(Podatek == "Izvoz")

zemljevid_evropa_izvoz <- ggplot() + 
  geom_polygon(data=left_join(zemljevid %>% filter(CONTINENT=="Europe"),plot_data1, by=c("SOVEREIGNT"="Drzave")),
               aes(x=long, y=lat, group=group, fill=Vrednost/1e6), size=0.1) +
  labs(x="", y="", fill="Izvoz") +   
  ggtitle("Količina izvoza v Evropi v milionih") +
  coord_cartesian(xlim=c(-27, 50), ylim=c(25, 80), expand = TRUE)  +
  theme_map(base_size = 20)+
  scale_fill_gradient(low="red3", high="yellow")

#Uvoz EU
  
plot_data2 <- TRGOVSKE_PARTNERJE %>% filter(Podatek == "Uvoz")

zemljevid_evropa_uvoz <- ggplot() + 
  geom_polygon(data=left_join(zemljevid %>% filter(CONTINENT=="Europe"),plot_data2, by=c("SOVEREIGNT"="Drzave")),
               aes(x=long, y=lat, group=group, fill=Vrednost/1e6), size=0.1) +
  labs(x="", y="", fill="Uvoz") +   
  ggtitle("Količina uvoza v Evropi v milionih") +
  coord_cartesian(xlim=c(-27, 50), ylim=c(25, 80), expand = TRUE)  +
  theme_map(base_size = 20)+
  scale_fill_gradient(low="red3", high="yellow")

#Izvoz svet

plot_data3 <- TRGOVSKE_PARTNERJE %>% filter(Podatek == "Izvoz")

zemljevid_svet_izvoz <- ggplot() + 
  geom_polygon(data=left_join(zemljevid,plot_data3, by=c("SOVEREIGNT"="Drzave")),
               aes(x=long, y=lat, group=group, fill=Vrednost/1e6), size=0.1) +
  labs(x="", y="", fill="Izvoz") +   
  ggtitle("Količina izvoza v Svetu v milionih") +
  theme_map(base_size = 20)+
  scale_fill_gradient(low="red3", high="yellow")

#Uvoz svet

plot_data4 <- TRGOVSKE_PARTNERJE %>% filter(Podatek == "Uvoz")

zemljevid_svet_uvoz <- ggplot() + 
  geom_polygon(data=left_join(zemljevid,plot_data4, by=c("SOVEREIGNT"="Drzave")),
               aes(x=long, y=lat, group=group, fill=Vrednost/1e6), size=0.1) +
  labs(x="", y="", fill="Uvoz") +   
  ggtitle("Količina uvoza v Svetu v milionih") +
  theme_map(base_size = 20)+
  scale_fill_gradient(low="red3", high="yellow")


#Afrika izvoz
plot_data5 <- TRGOVSKE_PARTNERJE %>% filter(Podatek == "Izvoz")

zemljevid_afrika_izvoz <- ggplot() + 
  geom_polygon(data=left_join(zemljevid %>% filter(CONTINENT=="Africa"),plot_data5, by=c("SOVEREIGNT"="Drzave")),
               aes(x=long, y=lat, group=group, fill=Vrednost/1e6), size=0.1) +
  labs(x="", y="", fill="Izvoz") +   
  ggtitle("Količina izvoza v Afriki v milionih") +
  coord_cartesian(xlim = c(-25,55),
                  ylim = c(-40,40), expand = TRUE)  +
  theme_map(base_size = 20)+
  scale_fill_gradient(low="red3", high="yellow")


#Afrika uvoz
plot_data6 <- TRGOVSKE_PARTNERJE %>% filter(Podatek == "Uvoz")

zemljevid_afrika_uvoz <- ggplot() + 
  geom_polygon(data=left_join(zemljevid %>% filter(CONTINENT=="Africa"),plot_data6, by=c("SOVEREIGNT"="Drzave")),
               aes(x=long, y=lat, group=group, fill=Vrednost/1e6), size=0.1) +
  labs(x="", y="", fill="Uvoz") +   
  ggtitle("Količina uvoza v Afriki v milionih") +
  coord_cartesian(xlim = c(-25,55),
                  ylim = c(-40,40), expand = TRUE)  +
  theme_map(base_size = 20)+
  scale_fill_gradient(low="red3", high="yellow")


#Juzna Amerika izvoz
plot_data2 <- TRGOVSKE_PARTNERJE %>% filter(Podatek == "Izvoz")

zemljevid_juznaamerika_izvoz <- ggplot() + 
  geom_polygon(data=left_join(zemljevid %>% filter(CONTINENT=="South America"),plot_data2, by=c("SOVEREIGNT"="Drzave")),
               aes(x=long, y=lat, group=group, fill=Vrednost/1e6), size=0.1) +
  labs(x="", y="", fill="Izvoz") +   
  ggtitle("Količina izvoza v Južni Ameriki v milionih") +
  coord_cartesian(xlim = c(-85, -35), ylim=c(-55,12), expand = TRUE)  +
  theme_map(base_size = 20)+
  scale_fill_gradient(low="red3", high="yellow")

#Juzna Amerika uvoz
plot_data2 <- TRGOVSKE_PARTNERJE %>% filter(Podatek == "Uvoz")

zemljevid_juznaamerika_uvoz <- ggplot() + 
  geom_polygon(data=left_join(zemljevid %>% filter(CONTINENT=="South America"),plot_data2, by=c("SOVEREIGNT"="Drzave")),
               aes(x=long, y=lat, group=group, fill=Vrednost/1e6), size=0.1) +
  labs(x="", y="", fill="Uvoz") +   
  ggtitle("Količina uvoza v Južni Ameriki v milionih") +
  coord_cartesian(xlim = c(-85, -35), ylim=c(-55,12), expand = TRUE)  +
  theme_map(base_size = 20)+
  scale_fill_gradient(low="red3", high="yellow")


#Severna Amerika izvoz
plot_data2 <- TRGOVSKE_PARTNERJE %>% filter(Podatek == "Izvoz")

zemljevid_severnaamerika_izvoz <- ggplot() + 
  geom_polygon(data=left_join(zemljevid %>% filter(CONTINENT=="North America"),plot_data2, by=c("SOVEREIGNT"="Drzave")),
               aes(x=long, y=lat, group=group, fill=Vrednost/1e6), size=0.1) +
  labs(x="", y="", fill="Izvoz") +   
  ggtitle("Količina izvoza v Severni Ameriki v milionih") +
  coord_cartesian(xlim = c(-170, -20),  ylim = c(10, 90), expand = TRUE)  +
  theme_map(base_size = 20)+
  scale_fill_gradient(low="red3", high="yellow")

#Severna Amerika uvoz
plot_data2 <- TRGOVSKE_PARTNERJE %>% filter(Podatek == "Uvoz")

zemljevid_severnaamerika_uvoz <- ggplot() + 
  geom_polygon(data=left_join(zemljevid %>% filter(CONTINENT=="North America"),plot_data2, by=c("SOVEREIGNT"="Drzave")),
               aes(x=long, y=lat, group=group, fill=Vrednost/1e6), size=0.1) +
  labs(x="", y="", fill="Uvoz") +   
  ggtitle("Količina uvoza v Severni Ameriki v milionih") +
  coord_cartesian(xlim = c(-170, -20),  ylim = c(10, 90), expand = TRUE)  +
  theme_map(base_size = 20)+
  scale_fill_gradient(low="red3", high="yellow")


#Avstralija in Oceanijo izvoz
plot_data2 <- TRGOVSKE_PARTNERJE %>% filter(Podatek == "Izvoz")

zemljevid_avstralija_izvoz <- ggplot() + 
  geom_polygon(data=left_join(zemljevid %>% filter(CONTINENT=="Oceania" | SOVEREIGNT=="Austrlia"),plot_data2, by=c("SOVEREIGNT"="Drzave")),
               aes(x=long, y=lat, group=group, fill=Vrednost/1e6), size=0.1) +
  labs(x="", y="", fill="Izvoz") +   
  ggtitle("Količina izvoza v Avstraliji v milionih") +
  coord_cartesian(xlim = c(110, 190), ylim = c(-50, 0), expand = TRUE)  +
  theme_map(base_size = 20)+
  scale_fill_gradient(low="red3", high="yellow")

#Avstralija in Oceanijo uvoz
plot_data2 <- TRGOVSKE_PARTNERJE %>% filter(Podatek == "Uvoz")

zemljevid_avstralija_uvoz <- ggplot() + 
  geom_polygon(data=left_join(zemljevid %>% filter(CONTINENT=="Oceania" | SOVEREIGNT=="Austrlia"),plot_data2, by=c("SOVEREIGNT"="Drzave")),
               aes(x=long, y=lat, group=group, fill=Vrednost/1e6), size=0.1) +
  labs(x="", y="", fill="Uvoz") +   
  ggtitle("Količina uvoza v Avstraliji v milionih") +
  coord_cartesian(xlim = c(110, 190), ylim = c(-50, 0), expand = TRUE)  +
  theme_map(base_size = 20)+
  scale_fill_gradient(low="red3", high="yellow")

#Azija izvoz
plot_data2 <- TRGOVSKE_PARTNERJE %>% filter(Podatek == "Izvoz")

zemljevid_azija_izvoz <- ggplot() + 
  geom_polygon(data=left_join(zemljevid %>% filter(CONTINENT=="Asia"),plot_data2, by=c("SOVEREIGNT"="Drzave")),
               aes(x=long, y=lat, group=group, fill=Vrednost/1e6), size=0.1) +
  labs(x="", y="", fill="Izvoz") +   
  ggtitle("Količina izvoza v Aziji v milionih") +
  coord_cartesian(xlim=c(20,160),ylim=c(-20,60), expand = TRUE)  +
  theme_map(base_size = 20)+
  scale_fill_gradient(low="red3", high="yellow")


#Azija uvoz
plot_data2 <- TRGOVSKE_PARTNERJE %>% filter(Podatek == "Uvoz")

zemljevid_azija_uvoz <- ggplot() + 
  geom_polygon(data=left_join(zemljevid %>% filter(CONTINENT=="Asia"),plot_data2, by=c("SOVEREIGNT"="Drzave")),
               aes(x=long, y=lat, group=group, fill=Vrednost/1e6), size=0.1) +
  labs(x="", y="", fill="Uvoz") +   
  ggtitle("Količina uvoza v Aziji v milionih") +
  coord_cartesian(xlim=c(20,160),ylim=c(-20,60), expand = TRUE)  +
  theme_map(base_size = 20)+
  scale_fill_gradient(low="red3", high="yellow")


#Antarktika uvoz
plot_data2 <- TRGOVSKE_PARTNERJE %>% filter(Podatek == "Izvoz")

zemljevid_antarktika_izvoz <- ggplot() + 
  geom_polygon(data=left_join(zemljevid %>% filter(CONTINENT=="Antarctica"),plot_data2, by=c("SOVEREIGNT"="Drzave")),
               aes(x=long, y=lat, group=group, fill=Vrednost), size=0.1) +
  labs(x="", y="", fill="Izvoz") +   
  ggtitle("Količina izvoza na Antarktiki") +
  coord_cartesian(xlim=c(-180,180),ylim=c(-140,0), expand = TRUE)  +
  theme_map(base_size = 20)+
  scale_fill_gradient(low="red3", high="yellow")


#Antarktika uvoz
plot_data2 <- TRGOVSKE_PARTNERJE %>% filter(Podatek == "Uvoz")

zemljevid_antarktika_uvoz <- ggplot() + 
  geom_polygon(data=left_join(zemljevid %>% filter(CONTINENT=="Antarctica"),plot_data2, by=c("SOVEREIGNT"="Drzave")),
               aes(x=long, y=lat, group=group, fill=Vrednost), size=0.1) +
  labs(x="", y="", fill="Uvoz") +   
  ggtitle("Količina uvoza na Antarktiki") +
  coord_cartesian(xlim=c(-180,180),ylim=c(-140,0), expand = TRUE)  +
  theme_map(base_size = 20)+
  scale_fill_gradient(low="red3", high="yellow")


#Tortni izvoz po vrsta blaga
pie1<- ggplot(razdelitve1 %>% mutate(pct=izvoz / sum(izvoz) * 100) %>%
                arrange(desc(`opis blaga`)) %>% mutate(pos=cumsum(pct) - pct/2), # položaji oznak
              aes(x=0, y=pct, fill=str_wrap(`opis blaga`, 30))) + # dolga imena naj se prelomijo
  coord_polar(theta="y") + geom_col(width=1) +
  geom_text(aes(y=pos, label=ifelse(pct < 1, "", paste0(round(pct), "%"))), # ne izpiši oznak za majhne rezine
            x=0.3, color="white") + ggtitle("Izvoz po vrsti blaga") +
  guides(fill=guide_legend("Vrsta blaga")) + xlab("") + ylab("") +
  scale_x_continuous(breaks=NULL)+ # os x naj se ne izriše
  theme(axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid  = element_blank())

#Tortni uvoz po vrsta blaga
pie2<- ggplot(razdelitve1 %>% mutate(pct=uvoz / sum(uvoz) * 100) %>%
              arrange(desc(`opis blaga`)) %>% mutate(pos=cumsum(pct) - pct/2), # položaji oznak
              aes(x=0, y=pct, fill=str_wrap(`opis blaga`, 30))) + # dolga imena naj se prelomijo
              coord_polar(theta="y") + geom_col(width=1) +
              geom_text(aes(y=pos, label=ifelse(pct < 1, "", paste0(round(pct), "%"))), # ne izpiši oznak za majhne rezine
                 x=0.3, color="white") + ggtitle("Uvoz po vrsti blaga") +
  guides(fill=guide_legend("Vrsta blaga")) + xlab("") + ylab("") +
  scale_x_continuous(breaks=NULL)+ # os x naj se ne izriše
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid  = element_blank())

# Najpomembni partnerji barplot

vrh <- TRGOVSKE_PARTNERJE %>% 
      filter((Podatek == "Uvoz"|  Podatek == "Izvoz") & ( Drzave == "United States" | Drzave == "China" | Drzave =="United Kingdom"))
                                                              

graf_vrh<- ggplot() +
           geom_bar(data = vrh, aes(x = Drzave, y = Vrednost, fill = Podatek), position = "dodge", stat = "identity")+
           ggtitle("Najpomembne trgovske partnerje") +scale_y_continuous(labels=scales::comma)+
          scale_fill_manual(values=c("yellow","red"))+ylab("Vrednost v tisočih")
          

  #proba

graf_gib <- ggplot(data = pdf , aes(x=leto, y=`Vrednost v milionih`/1e3,color=Podatek)) + geom_line(aes(frame=Podatek))
# ggplot2 ne prepozna frame plotly ga prepozna
graf_gib <- graf_gib + xlab("Leto") + ylab('Vrednost v milijardah')+ theme(legend.position = "none")
graf_gib <- ggplotly(graf_gib)
  