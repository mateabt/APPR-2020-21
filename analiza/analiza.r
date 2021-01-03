# 4. faza: Analiza podatkov

#linearni model s intervalom zaupanja
lregr<-ggplot(pdf1, aes(x=leto, y=neto_izvoz/1e6)) + 
  geom_point(color='red', size = 2) + 
  geom_smooth(method=lm,formula=y~x, color='#2C3E50')+
  labs(title="Linearna regresija")+ylab("neto izvoz v milionih")



