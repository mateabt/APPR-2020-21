# Analiza zunanje trgovine Nemčije

Repozitorij z gradivi pri predmetu APPR v študijskem letu 2020/21

* [![Shiny](http://mybinder.org/badge.svg)](http://mybinder.org/v2/gh/mateabt/APPR-2020-21/master?urlpath=shiny/APPR-2020-21/projekt.Rmd) Shiny
* [![RStudio](http://mybinder.org/badge.svg)](http://mybinder.org/v2/gh/mateabt/APPR-2020-21/master?urlpath=rstudio) RStudio

## Tematika
Analizirala bom zunanjo trgovino Nemčije kot eno od najbolj razvitih svetovnih gospodarstev in hkrati precej odprto gospodarstvo.Pogledala 
si bom podatke o izvozu in uvozu po letih ter po posameznimi dejavnosti.Poleg tega bi tudi pogledala s katere države ima najobsežno trgovino

###### Tabele:

* `1.tabela` :xlsx datoteka (Vrstni red nemških trgovskih partnerjev -uvoz,izvoz)
Analizala bi neto izvoz in celotni promet posamezne države 
https://www.destatis.de/EN/Themes/Economy/Foreign-Trade/Tables/order-rank-germany-trading-partners.html

* `2.tabela` :html (Izvoz in uvoz po razdelitvi klasifikacije proizvodov)
Katere dejavnosti prinašajo največji uvoz/izvoz
https://www.destatis.de/EN/Themes/Economy/Foreign-Trade/Tables/imports-exports.html?fbclid=IwAR2tVMBoA4bC6YXHvIVtzUXAD99eHUwEJLo6MAWLsQ31lm039Qm81uSTOFU

* `3.tabela` :pdf (Splošni razvoj zunanje trgovine po letih)
Pogledala bi kako se je zunajna trgovina gibala po letih 
https://www.destatis.de/EN/Themes/Economy/Foreign-Trade/Tables/overall-development-foreign-trade.pdf?__blob=publicationFile&fbclid=IwAR1fflqYZaQy5LMYTNbA3XWQhij-ERHS0VkPFFRbfZpD4rYhQqecATqdPI4

* `4.tabela` :tsv (BDP po letih nominalne cene baza 2010 v milionih)
Povezanost BDP-ja z neto izvozom
https://ec.europa.eu/eurostat/databrowser/view/nama_10_gdp/default/table?lang=en



## Program

Glavni program in poročilo se nahajata v datoteki `projekt.Rmd`.
Ko ga prevedemo, se izvedejo programi, ki ustrezajo drugi, tretji in četrti fazi projekta:

* obdelava, uvoz in čiščenje podatkov: `uvoz/uvoz.r`
* analiza in vizualizacija podatkov: `vizualizacija/vizualizacija.r`
* napredna analiza podatkov: `analiza/analiza.r`

Vnaprej pripravljene funkcije se nahajajo v datotekah v mapi `lib/`.
Podatkovni viri so v mapi `podatki/`.
Zemljevidi v obliki SHP, ki jih program pobere,
se shranijo v mapo `../zemljevidi/` (torej izven mape projekta).

## Potrebni paketi za R

Za zagon tega vzorca je potrebno namestiti sledeče pakete za R:

* `knitr` - za izdelovanje poročila
* `rmarkdown` - za prevajanje poročila v obliki RMarkdown
* `shiny` - za prikaz spletnega vmesnika
* `DT` - za prikaz interaktivne tabele
* `rgdal` - za uvoz zemljevidov
* `rgeos` - za podporo zemljevidom
* `digest` - za zgoščevalne funkcije (uporabljajo se za shranjevanje zemljevidov)
* `readr` - za branje podatkov
* `rvest` - za pobiranje spletnih strani
* `tidyr` - za preoblikovanje podatkov v obliko *tidy data*
* `dplyr` - za delo s podatki
* `gsubfn` - za delo z nizi (čiščenje podatkov)
* `ggplot2` - za izrisovanje grafov
* `mosaic` - za pretvorbo zemljevidov v obliko za risanje z `ggplot2`
* `maptools` - za delo z zemljevidi
* `tmap` - za izrisovanje zemljevidov
* `extrafont` - za pravilen prikaz šumnikov (neobvezno)

## Binder

Zgornje [povezave](#analiza-podatkov-s-programom-r-202021)
omogočajo poganjanje projekta na spletu z orodjem [Binder](https://mybinder.org/).
V ta namen je bila pripravljena slika za [Docker](https://www.docker.com/),
ki vsebuje večino paketov, ki jih boste potrebovali za svoj projekt.

Če se izkaže, da katerega od paketov, ki ji potrebujete, ni v sliki,
lahko za sprotno namestitev poskrbite tako,
da jih v datoteki [`install.R`](install.R) namestite z ukazom `install.packages`.
Te datoteke (ali ukaza `install.packages`) **ne vključujte** v svoj program -
gre samo za navodilo za Binder, katere pakete naj namesti pred poganjanjem vašega projekta.

Tako nameščanje paketov se bo izvedlo pred vsakim poganjanjem v Binderju.
Če se izkaže, da je to preveč zamudno,
lahko pripravite [lastno sliko](https://github.com/jaanos/APPR-docker) z želenimi paketi.

Če želite v Binderju delati z git,
v datoteki `gitconfig` nastavite svoje ime in priimek ter e-poštni naslov
(odkomentirajte vzorec in zamenjajte s svojimi podatki) -
ob naslednjem zagonu bo mogoče delati commite.
Te podatke lahko nastavite tudi z `git config --global` v konzoli
(vendar bodo veljale le v trenutni seji).
