# 4. faza: Analiza podatkov

#linearni model s intervalom zaupanja neto izvoz po leti

model1 <- lm(neto_izvoz ~ leto , data = pdf1)
prihodnost <- data.frame(leto = seq(2020,2024))
celoten_interval<- data.frame(leto=c(2007:2024))


napoved1 <- prihodnost %>% mutate(neto_izvoz= predict(model1, .))


tabela.napoved1 <- bind_rows( (pdf1)[c(1,4)],napoved1)
tabela.napoved2 <- tabela.napoved1 %>% drop_na(neto_izvoz)



graf.napoved <- ggplot(tabela.napoved1, aes(x = leto, y = neto_izvoz)) + 
  geom_point() + scale_x_continuous(name = "Leto", breaks = seq(1950,2024,2)) + ylab("Neto izvoz") +
  ggtitle("Napoved gibanja neto izvoza-linearna regresija") +
  geom_smooth(method = 'lm', formula = y ~ x,col="red") +
  theme(axis.text.x=element_text(angle=90, vjust=0.5, hjust=1),
         panel.grid.minor = element_blank())+
  scale_y_continuous(labels=comma)
  

