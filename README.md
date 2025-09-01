# Shiny web aplikacija za vjerojatnosne distribucije

Web aplikacija za vizualizaciju različitih parametarski zadanih vjerojatnosnih distribucija, dizajnirana u programskom jeziku R koristeći Shiny paket. Aplikacija je namijenjena kao dodatan alat za razumijevanje gradiva vjerojatnosti i statistike.

![](https://files.catbox.moe/7390bq.png)


---
**Upute za korištenje:**

Dovoljno je pristupiti stranici: https://lnol.shinyapps.io/vj_distribucije/

U slučaju da stranica nije dostupna, treba otvoriti `app.r` u Rstudio i kliknuti gumb "Run App" u gornjem desnom kutu okvira za programski kod. Ako nemate potrebne pakete, instaliraju se u konzoli s `install.packages(c("shiny", "shinydashboard", "reshape2", "ggplot2", "plotly", "rsconnect"))`

![](https://files.catbox.moe/m392dk.png)

**Korišteni paketi:**
- `shiny` - Glavni paket; namijenjen za izradu web aplikacija u području podatkovnih znanosti, podržava R i Python
- `shinydashboard` - Dodatan paket, odgovoran za "dashboard" vizualni dizajn aplikacije
- `reshape2` - potreban za backend funkcionalnosti
- `ggplot2, plotly` - paketi za grafove
- `rsconnect` - potreban za deploy

**Implementirane distribucije:**
- Diskretne: Bernoullijeva, Binomna, Poissonova, Uniformna
- Neprekidne: Normalna, Eksponencijalna, Uniformna, Chi-squared, Studentova, Fisherova

**Značajke:**
- Grafovi za svaku distribuciju s promjenjivim parametrima
- Grafički prikaz očekivanja, medijana, gornjeg i donjeg kvartila

<img src="https://files.catbox.moe/4s0dxc.png" width=50% height=50%>

- Prikaz vjerojatnosti po varijanci i standardnim devijacijama, ili po vlastitom intervalu
- matematički izrazi za funkciju gustoće, formule za očekivanje i varijancu
- trenutne vrijednosti očekivanja i varijance s obzirom na parametre grafa

**Napomene:**
- Prikaz vjerojatnosti na grafu zna biti netočan za početne parametre - u tom slučaju treba samo promijeniti jedan parametar (granicu intervala) i prikaz se ispravi (može se vratiti na početni interval za točan prikaz)
- Prikaz vjerojatnosti po varijanci/standardnim devijacijama je neprecizan kod Chi-squared i Studentove distribucije za k=1 tj. d_1=1
