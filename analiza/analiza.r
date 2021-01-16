# 4. faza: Analiza podatkov

#linearni model s intervalom zaupanja

model1 <- lm(neto_izvoz ~ leto , data = pdf1)
prihodnost <- data.frame(leto = seq(2020,2024))

napoved1 <- prihodnost %>% mutate(neto_izvoz= predict(model1, .))


tabela.napoved1 <- bind_rows( (pdf1)[c(1,4)],napoved1)




graf.napoved1 <- ggplot(tabela.napoved1, aes(x = leto, y = neto_izvoz)) + 
  geom_point() + scale_x_continuous(name = "Leto", breaks = seq(1950,2024,2)) + ylab("Neto izvoz") +
  ggtitle("Napoved gibanja neto izvoza-linearna") +
  geom_smooth(method = 'lm', formula = y ~ x,col="red") +
  theme(axis.text.x=element_text(angle=90, vjust=0.5, hjust=1),
         panel.grid.minor = element_blank())+
  scale_y_continuous(labels=comma)+
  geom_point(data=napoved1, aes(x=leto, y=neto_izvoz), fill='yellow',shape=21, size=2)
  
  

#linearni model uvoz izvoz

model2 <- lm(izvoz ~ uvoz , data = pdf1)
novi.uvozi <- data.frame(uvoz = c(500000,1000000,1200000))

napoved2 <- novi.uvozi %>% mutate(izvoz= predict(model2, .))

tabela.napoved2 <- bind_rows( (pdf1)[c(2,3)],napoved2)

graf.napoved2 <- ggplot(tabela.napoved2, aes(x = uvoz, y = izvoz)) + 
  geom_point() + scale_x_continuous(name = "Uvoz",labels = scales::comma, breaks = seq(0,1300000,100000)) + ylab("Izvoz") +
  ggtitle("Napoved gibanja izvoza glede na uvoz") +
  geom_smooth(method = 'lm', formula = y ~ x,col="red") +
  theme(axis.text.x=element_text(angle=90, vjust=0.5, hjust=1),
        panel.grid.minor = element_blank())+
  scale_y_continuous(labels=comma)+
  geom_point(data=napoved2, aes(x=uvoz, y=izvoz), fill='yellow',shape=24, size=3.5)
