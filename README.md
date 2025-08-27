# Shiny web aplikacija za vjerojatnosne distribucije

Web aplikacija za vizualizaciju različitih parametarski zadanih vjerojatnosnih distribucija, dizajnirana u programskom jeziku R koristeći Shiny paket. Aplikacija je namijenjena kao dodatan alat za razumijevanje gradiva vjerojatnosti i statistike.
---
**Upute za korištenje:**

Dovoljno je pristupiti stranici: https://lnol.shinyapps.io/vj_distribucije/
U slučaju da stranica nije dostupna, treba otvoriti `app.r` u Rstudio i kliknuti gumb "Run App" u gornjem desnom kutu okvira za programski kod.


**Korišteni paketi:**
- `shiny` - Glavni paket; namijenjen za izradu web aplikacija u području podatkovnih znanosti, podržava R i Python
- `shinydashboard` - Dodatan paket, odgovoran za "dashboard" vizualni dizajn aplikacije
- `reshape2` - potreban za backend funkcionalnosti
- `ggplot2, plotly` - paketi za grafove
- `rsconnect` - potreban za deploy
