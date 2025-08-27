library(shiny)
library(shinydashboard)
library(reshape2)

library(ggplot2)
library(plotly)
library(rsconnect)
#UI====

header_css <- '
.main-header .logo {
        font-family: "Times New Roman", serif;
        font-weight: bold;
        font-size: 15px;
'
ui <- dashboardPage(
    skin = "purple",
    dashboardHeader(title = "Vjerojatnosne distribucije"),
    dashboardSidebar(
        sidebarMenu(id='menus',
                    menuItem(text = "Diskretne" , tabName = "diskr"),
                    menuItem(text = "Neprekidne" , tabName = "nepr")

        ) #siderbarmenu
        
    ), #dashboardSidebar
    
    dashboardBody(
      tags$head(tags$style(type='text/css', header_css)),
      tabItems(
        tabItem("diskr",
                fluidRow( #tabovi
                  tabsetPanel(id="tabs_diskr",
                    tabPanel(title='Bernoullijeva',value='bern',fluidRow(
                      column(8, plotlyOutput('plotbern')),
                      column(3, wellPanel(
                        tags$p("Izaberite parametre distribucije:", style = "font-weight: bold; margin-bottom: 5px;"),
                        sliderInput("pbern", "Vjerojatnost uspjeha (p)", min = 0, max = 1, value = 0.5, step = 0.01),
                        actionButton("resetbern", "Reset"),
                        checkboxInput("showexp_diskr", "Prikaži očekivanje", value = FALSE),
                        checkboxInput("showmedian_diskr", "Prikaži medijan", value = FALSE),
                        checkboxInput("showquartiles_diskr", "Prikaži kvartile", value = FALSE)
                      ))
                    )
                    ), #Bernoullijeva
                    tabPanel(title='Binomna',value='binom',fluidRow(
                      column(8, plotlyOutput('plotbinom')),
                      column(3, wellPanel(
                        tags$p("Izaberite parametre distribucije:", style = "font-weight: bold; margin-bottom: 5px;"),
                        sliderInput("nbinom", "Broj ponavljanja (n)", min = 1, max = 25, value = 10, step = 1),
                        sliderInput("pbinom", "Vjerojatnost uspjeha (p)", min = 0, max = 1, value = 0.5, step = 0.01),
                        actionButton("resetbinom", "Reset"),
                        checkboxInput("showexp_diskr", "Prikaži očekivanje", value = FALSE),
                        checkboxInput("showmedian_diskr", "Prikaži medijan", value = FALSE),
                        checkboxInput("showquartiles_diskr", "Prikaži kvartile", value = FALSE),
                        selectInput("sd_interval_k_diskr", "Prikaži interval μ ± k·σ:",
                                    choices = c("Ne prikazuj" = 0, "k = 1" = 1, "k = 2" = 2, "k = 3" = 3),
                                    selected = 0),
                        sliderInput("discrete_interval", "Izaberi interval za izračun vjerojatnosti:", min = 0, max = 25, value = c(3, 7), step = 0.1),
                        checkboxInput("show_discrete_prob", "Prikaži vjerojatnost (P)", value = FALSE)
                      ))
                    )), #Binomna
                    tabPanel(title='Poissonova',value='pois',fluidRow(
                      column(8, plotlyOutput('plotpois')),
                      column(3, wellPanel(
                        tags$p("Izaberite parametre distribucije:", style = "font-weight: bold; margin-bottom: 5px;"),
                        sliderInput("lambdapois", "λ", min = 0.1, max = 20, value = 3, step = 0.1),
                        actionButton("resetpois", "Reset"),
                        checkboxInput("showexp_diskr", "Prikaži očekivanje", value = FALSE),
                        checkboxInput("showmedian_diskr", "Prikaži medijan", value = FALSE),
                        checkboxInput("showquartiles_diskr", "Prikaži kvartile", value = FALSE),
                        selectInput("sd_interval_k_diskr", "Prikaži interval μ ± k·σ:",
                                    choices = c("Ne prikazuj" = 0, "k = 1" = 1, "k = 2" = 2, "k = 3" = 3),
                                    selected = 0),
                        sliderInput("discrete_interval", "Izaberi interval za izračun vjerojatnosti:", min = 0, max = 35, value = c(1, 7), step = 0.1),
                        checkboxInput("show_discrete_prob", "Prikaži vjerojatnost (P)", value = FALSE)
                      ))
                    )), #Poissonova
                    tabPanel(title='Uniformna',value='unid',fluidRow(
                      column(8, plotlyOutput('plotunid')),
                      column(3, wellPanel(
                        tags$p("Izaberite interval distribucije:", style = "font-weight: bold; margin-bottom: 5px;"),
                        sliderInput("aunid", "Početak intervala (a)", min = 0, max = 20, value = 0, step = 1),
                        sliderInput("bunid", "Kraj intervala (b)", min = 1, max = 25, value = 5, step = 1),
                        actionButton("resetunid", "Reset"),
                        checkboxInput("showexp_diskr", "Prikaži očekivanje", value = FALSE),
                        checkboxInput("showmedian_diskr", "Prikaži medijan", value = FALSE),
                        checkboxInput("showquartiles_diskr", "Prikaži kvartile", value = FALSE),
                        selectInput("sd_interval_k_diskr", "Prikaži interval μ ± k·σ:",
                                    choices = c("Ne prikazuj" = 0, "k = 1" = 1, "k = 2" = 2, "k = 3" = 3),
                                    selected = 0),
                        sliderInput("discrete_interval", "Izaberi interval za izračun vjerojatnosti:", min = 0, max = 25, value = c(1, 4), step = 0.1),
                        checkboxInput("show_discrete_prob", "Prikaži vjerojatnost (P)", value = FALSE)
                      ))
                    )) #uniformna d.
                  )#tabsetPanel
                ), #fluidRow - tabovi
                fluidRow(br()),
                fluidRow(
                  box(uiOutput("dense_diskr"), title = strong("Funkcija gustoće"), status = 'primary', solidHeader = TRUE),
                  infoBoxOutput("exp_diskr", width = 3),
                  infoBoxOutput("var_diskr", width = 3)
                ),
                fluidRow(
                  box(uiOutput("expected_diskr"), title = strong("Formula za očekivanje"), status = 'primary', solidHeader = TRUE),
                  box(uiOutput("variance_diskr"), title = strong("Formula za varijancu"), status = 'primary', solidHeader = TRUE)
                )
        ), #tabItem - diskretne
        tabItem("nepr", 
                fluidRow(
                  tabsetPanel(id="tabs",
                              tabPanel(title='Normalna',value='nrm',fluidRow(
                                column(8, plotlyOutput('plotnrm')),
                                column(3,wellPanel(
                                  tags$p("Izaberite parametre distribucije:", style = "font-weight: bold; margin-bottom: 5px;"),
                                  sliderInput(inputId = "mu",label = "očekivanje (μ)",min = -10,max = 10,value = 0,step = 0.2),
                                  sliderInput(inputId = "var",label = withMathJax('$$Varijanca (\\sigma^2 )$$'),min = 0,max = 10,value = 1,step = 0.2),
                                  actionButton(inputId = 'resetnrm',label = "reset"),
                                  checkboxInput(inputId = "showexp", label = "Prikaži očekivanje", value = FALSE),
                                  checkboxInput(inputId = "showmedian", label = "Prikaži medijan", value = FALSE),
                                  checkboxInput(inputId = "showquartiles", label = "Prikaži kvartile", value = FALSE),
                                  selectInput("sd_interval_k", "Prikaži interval μ ± k·σ:",
                                              choices = c("Ne prikazuj" = 0, "k = 1" = 1, "k = 2" = 2, "k = 3" = 3),
                                              selected = 0),
                                  sliderInput("interval", "Izaberi interval za izračun vjerojatnosti:", min = -10, max = 10, value = c(-1, 1), step = 0.1),
                                  checkboxInput("show_area", "Prikaži vjerojatnost (P)", value = FALSE))
                                  
                                ))), #Normalna
                              tabPanel(title='Eksponencijalna',value='eksp',fluidRow(
                                column(8, plotlyOutput('ploteksp')),
                                column(3,wellPanel(
                                  tags$p("Izaberite parametre distribucije:", style = "font-weight: bold; margin-bottom: 5px;"),
                                  sliderInput(inputId = "lamb",label = "λ",min = 0,max = 10,value = 1,step = 0.5),
                                  actionButton(inputId = 'reseteksp',label = "reset"),
                                  checkboxInput(inputId = "showexp", label = "Prikaži očekivanje", value = FALSE),
                                  checkboxInput(inputId = "showmedian", label = "Prikaži medijan", value = FALSE),
                                  checkboxInput(inputId = "showquartiles", label = "Prikaži kvartile", value = FALSE),
                                  selectInput("sd_interval_k", "Prikaži interval μ ± k·σ:",
                                              choices = c("Ne prikazuj" = 0, "k = 1" = 1, "k = 2" = 2, "k = 3" = 3),
                                              selected = 0),
                                  sliderInput("interval", "Izaberi interval za izračun vjerojatnosti:", min = 0.1, max = 10, value = c(0.1, 1), step = 0.1),
                                  checkboxInput("show_area", "Prikaži vjerojatnost (P)", value = FALSE))
                                  
                                ))), #Eskponencijalna
                              tabPanel(title='Uniformna',value='unin',fluidRow(
                                column(8, plotlyOutput('plotunin'),),
                                column(3,wellPanel(
                                  tags$p("Izaberite interval distribucije:", style = "font-weight: bold; margin-bottom: 5px;"),
                                  sliderInput(inputId = "inf",label = "Početak intervala (a)",min = -10,max = 10,value = 0,step = 0.2),
                                  sliderInput(inputId = "sup",label = "Kraj intervala (b)",min = -10,max = 10,value = 1,step = 0.2),
                                  actionButton(inputId = 'resetunin',label = "reset"),
                                  checkboxInput(inputId = "showexp", label = "Prikaži očekivanje", value = FALSE),
                                  checkboxInput(inputId = "showmedian", label = "Prikaži medijan", value = FALSE),
                                  checkboxInput(inputId = "showquartiles", label = "Prikaži kvartile", value = FALSE),
                                  selectInput("sd_interval_k", "Prikaži interval μ ± k·σ:",
                                              choices = c("Ne prikazuj" = 0, "k = 1" = 1, "k = 2" = 2, "k = 3" = 3),
                                              selected = 0),
                                  sliderInput("interval", "Izaberi interval za izračun vjerojatnosti:", min = -10, max = 10, value = c(0, 1), step = 0.1),
                                  checkboxInput("show_area", "Prikaži vjerojatnost (P)", value = FALSE))
                                  
                                ))), #Uniformna n.
                              tabPanel(title=withMathJax("\\(\\chi^2\\)"),value='chisq',fluidRow(
                                column(8, plotlyOutput('plotchisq')),
                                column(3,wellPanel(
                                  tags$p("Izaberite parametre distribucije:", style = "font-weight: bold; margin-bottom: 5px;"),
                                  sliderInput(inputId = "kkhi",label = "Stupanj slobode (k)",min = 1,max = 20,value = 1,step = 1),
                                  actionButton(inputId = 'resetchisq',label = "reset"),
                                  checkboxInput(inputId = "showexp", label = "Prikaži očekivanje", value = FALSE),
                                  checkboxInput(inputId = "showmedian", label = "Prikaži medijan", value = FALSE),
                                  checkboxInput(inputId = "showquartiles", label = "Prikaži kvartile", value = FALSE),
                                  selectInput("sd_interval_k", "Prikaži interval μ ± k·σ:",
                                              choices = c("Ne prikazuj" = 0, "k = 1" = 1, "k = 2" = 2, "k = 3" = 3),
                                              selected = 0),
                                  sliderInput("interval", "Izaberi interval za izračun vjerojatnosti:", min = 0, max = 20, value = c(0, 1), step = 0.1),
                                  checkboxInput("show_area", "Prikaži vjerojatnost (P)", value = FALSE))
                                  
                                ))), #chisq
                              tabPanel(title='Studentova',value='stud',fluidRow(
                                column(8, plotlyOutput('plotstud')),
                                column(3,wellPanel(
                                  tags$p("Izaberite parametre distribucije:", style = "font-weight: bold; margin-bottom: 5px;"),
                                  sliderInput(inputId = "kstud",label = withMathJax("Stupanj slobode (\\(\\nu\\))"),min = 3,max = 100,value = 3,step = 1),
                                  actionButton(inputId = 'resetstud',label = "reset"),
                                  checkboxInput(inputId = "showexp", label = "Prikaži očekivanje", value = FALSE),
                                  checkboxInput(inputId = "showmedian", label = "Prikaži medijan", value = FALSE),
                                  checkboxInput(inputId = "showquartiles", label = "Prikaži kvartile", value = FALSE),
                                  selectInput("sd_interval_k", "Prikaži interval μ ± k·σ:",
                                              choices = c("Ne prikazuj" = 0, "k = 1" = 1, "k = 2" = 2, "k = 3" = 3),
                                              selected = 0),
                                  sliderInput("interval", "Izaberi interval za izračun vjerojatnosti:", min = -10, max = 20, value = c(-1, 1), step = 0.1),
                                  checkboxInput("show_area", "Prikaži vjerojatnost (P)", value = FALSE))
                                  
                                ))), #Studentova
                              tabPanel(title='Fisherova',value='fish',fluidRow(
                                column(8, plotlyOutput('plotfish')),
                                column(3,wellPanel(
                                  tags$p("Izaberite parametre distribucije:", style = "font-weight: bold; margin-bottom: 5px;"),
                                  sliderInput(inputId = "d1", label = HTML("Stupanj slobode 1 (\\(d_1\\))"), min = 1, max = 100, value = 1, step = 1),
                                  sliderInput(inputId = "d2",label = HTML("Stupanj slobode 2 (\\(d_2\\))"),min = 5,max = 100,value = 5,step = 1),
                                  actionButton(inputId = 'resetfish',label = "reset"),
                                  checkboxInput(inputId = "showexp", label = "Prikaži očekivanje", value = FALSE),
                                  checkboxInput(inputId = "showmedian", label = "Prikaži medijan", value = FALSE),
                                  checkboxInput(inputId = "showquartiles", label = "Prikaži kvartile", value = FALSE),
                                  selectInput("sd_interval_k", "Prikaži interval μ ± k·σ:",
                                              choices = c("Ne prikazuj" = 0, "k = 1" = 1, "k = 2" = 2, "k = 3" = 3),
                                              selected = 0),
                                  sliderInput("interval", "Izaberi interval za izračun vjerojatnosti:", min = 0, max = 20, value = c(0, 1), step = 0.1),
                                  checkboxInput("show_area", "Prikaži vjerojatnost (P)", value = FALSE))

                                  
                                ))) #Fisherova
                  )#tabsetPanel
                ), #fluidRow
                fluidRow(br()),
                fluidRow(box(uiOutput('dense'),title = strong('Funkcija gustoće'), status='primary',color='purple', solidHeader=T),
                         infoBoxOutput(outputId = "exp",width = 3),
                         infoBoxOutput(outputId = "var",width = 3)),
                fluidRow(box(uiOutput('expected'),title = strong('Formula za očekivanje'), status='primary',color='purple', solidHeader=T),
                         box(uiOutput('variance'),title = strong('Formula za varijancu'), status='primary',color='purple', solidHeader=T)),
                fluidRow(#infoBoxOutput(outputId = "exp",width = 3),
                         #infoBoxOutput(outputId = "var",width = 3)
                  )
        ) #tabItem - neprekidne
      ) #tabItems
    ) # dashboardBody
    
) # ui <- dashboardPage


#SERVER====
server <- function(input, output, session) {
  #x parametri za (zasad samo neprekidne) distribucije
  x <- reactive({switch (input$tabs,
                         'unin' = seq(-10,10,0.1),
                         'nrm' = seq(input$mu - 10, input$mu + 10, 0.1),
                         'eksp' = seq(0,20,0.1),
                         'chisq' = seq(0,20,0.1),
                         'stud' = seq(-5,5,.1),
                         'fish' = seq(0,5,.01)
  )})#x reactive (zasad samo parametri neprekidnih)
  
  #defaultne gustoce za distribucije
  data <- reactive({switch(input$tabs,
                           'unin' = dunif(x(),0,1),
                           'nrm' = dnorm(x(),0,1),
                           'eksp' = dexp(x(),1),
                           'chisq' = dchisq(x(),df = 1),
                           'stud' = dt(x(),1),
                           'fish' = df(x(),1,1)
  )}) # data reactive - defaultne gustoce
  
  # rucni parametri gustoce za distribucije
  data2 <- reactive({switch(input$tabs,
                            'unin' = dunif(x(),min(input$inf, input$sup),max(input$inf,input$sup)),
                            'nrm' = dnorm(x(), input$mu, sqrt(input$var)) ,
                            'eksp' = dexp(x(),input$lamb),
                            'chisq' = dchisq(x(),input$kkhi),
                            'stud' = dt(x(), input$kstud),
                            'fish' = df(x(),input$d1,input$d2)
  )}) # data2 reactive - rucni parametri
  
  #max/min vrijednosti na grafu
  minimumx <- reactive({switch(input$tabs,
                               'unin' = -10,
                               'nrm' = input$mu-10,
                               'eksp' = 0,
                               'chisq' = 0,
                               'stud' = -5,
                               'fish' = 0
  )})#min x
  
  maximumx <- reactive({switch (input$tabs,
                                'unin' = 10,
                                'nrm' = input$mu+10,
                                'eksp' = 20,
                                'chisq' = 20,
                                'stud' = 5,
                                'fish' = 5
  )})# max x
  
  minimumy <- reactive({switch(input$tabs,
                               'unin' = 0,
                               'nrm' = 0,
                               'eksp' = 0,
                               'chisq' = 0,
                               'stud' = 0,
                               'fish' = 0
  )})# min y
  
  maximumy <- reactive({switch (input$tabs,
                                'unin' = max(1/(max(input$sup,input$inf)-min(input$sup,input$inf))+0.1, 1),
                                'nrm' = 1,
                                'eksp' = input$lamb+1,
                                'chisq' = .5,
                                'stud' = .5,
                                'fish' = 2.5
  )}) #max y
  
  maximumy_diskr <- reactive({
    switch(input$tabs_diskr,
           'bern' = 1,
           'binom' = {
             x_vals <- 0:input$nbinom
             max(dbinom(x_vals, size = input$nbinom, prob = input$pbinom)) * 1.2
           },
           'pois' = {
             lambda <- input$lambdapois
             x_vals <- 0:qpois(0.999, lambda)
             max(dpois(x_vals, lambda = lambda)) * 1.2
           },
           'unid' = {
             a <- min(input$aunid, input$bunid)
             b <- max(input$aunid, input$bunid)
             1 / (b - a + 1)
           }
    )
  })
  
  #vraca ocekivanu vrijednost distribucije
  expinput <- reactive({switch (input$tabs,
                                'unin' = (input$inf+input$sup)/2,
                                'nrm' = input$mu,
                                'eksp' = 1/input$lamb,
                                'chisq' = input$kkhi,
                                'stud' = {if(input$kstud == 1){'Non defined'} else{0}},
                                'fish' = {if(input$d2<=2){'Non defined'} else{input$d2/(input$d2-2)}}
  )}) #expinput - ocekivana vrijednost
  
  expinput_diskr <- reactive({switch (input$tabs_diskr,
                                      'bern' = input$pbern,
                                      'binom' = input$nbinom * input$pbinom,
                                      'pois' = input$lambdapois,
                                      'unid' = (min(input$aunid, input$bunid) + max(input$aunid, input$bunid)) / 2
  )})
  
  #vraca varijancu distribucije
  varinput <- reactive({switch (input$tabs,
                                'unin' = (input$sup-input$inf)**2/12,
                                'nrm' = input$var,
                                'eksp' = 1/input$lamb**2,
                                'chisq' = 2*input$kkhi,
                                'stud' = {if(input$kstud==1){'Non defined'}else if(input$kstud==2){'Infinity'} else {input$kstud/(input$kstud-2)}},
                                'fish' = {ifelse(input$d2<=4,'Non defined',2*input$d2**2*(input$d1+input$d2-2)/(input$d1*(input$d2-2)**2*(input$d2-4)))}
  )})# varinput - varijanca
  
  varinput_diskr <- reactive({switch (input$tabs_diskr,
                                      'bern' = input$pbern * (1 - input$pbern),
                                      'binom' = input$nbinom * input$pbinom * (1 - input$pbinom),
                                      'pois' = input$lambdapois,
                                      'unid' = {
                                        a <- min(input$aunid, input$bunid)
                                        b <- max(input$aunid, input$bunid)
                                        ((b - a + 1)^2 - 1) / 12
                                      }
  )}) 
  
  #vraca medijan distribucije
  medianinput <- reactive({switch(input$tabs,
           'unin' = (input$inf + input$sup) / 2,
           'nrm' = input$mu,
           'eksp' = log(2) / input$lamb,
           'chisq' = qchisq(0.5, input$kkhi),
           'stud' = 0,
           'fish' = qf(0.5, input$d1, input$d2)
    )
  })#medianinput
  
  medianinput_diskr <- reactive({
    switch(input$tabs_diskr,
           'bern' = if (input$pbern < 0.5) 0 else if (input$pbern > 0.5) 1 else 0.5,
           'binom' = qbinom(0.5, input$nbinom, input$pbinom),
           'pois' = qpois(0.5, input$lambdapois),
           'unid' = floor((input$aunid +input$bunid) / 2)
    )
  })
  
  #vraca prvi kvartil distribucije
  q1input <- reactive({
    switch(input$tabs,
           'unin' = input$inf + 0.25 * (input$sup - input$inf),
           'nrm' = qnorm(0.25, input$mu, sqrt(input$var)),
           'eksp' = qexp(0.25, input$lamb),
           'chisq' = qchisq(0.25, input$kkhi),
           'stud' = qt(0.25, input$kstud),
           'fish' = qf(0.25, input$d1, input$d2)
    )
  }) #q1input
  
  q1input_diskr <- reactive({
    switch(input$tabs_diskr,
           'bern' = qbinom(0.25, 1, input$pbern),
           'binom' = qbinom(0.25, input$nbinom, input$pbinom),
           'pois' = qpois(0.25, input$lambdapois),
           'unid' = quantile(min(input$aunid, input$bunid):max(input$aunid, input$bunid), 0.25, type = 1)
    )
  })
  
  #vraca treci kvartil distribucije
  q3input <- reactive({
    switch(input$tabs,
           'unin' = input$inf + 0.75 * (input$sup - input$inf),
           'nrm' = qnorm(0.75, input$mu, sqrt(input$var)),
           'eksp' = qexp(0.75, input$lamb),
           'chisq' = qchisq(0.75, input$kkhi),
           'stud' = qt(0.75, input$kstud),
           'fish' = qf(0.75, input$d1, input$d2)
    )
  }) #q3input
  
  q3input_diskr <- reactive({
    switch(input$tabs_diskr,
           'bern' = qbinom(0.75, 1, input$pbern),
           'binom' = qbinom(0.75, input$nbinom, input$pbinom),
           'pois' = qpois(0.75, input$lambdapois),
           'unid' = quantile(min(input$aunid, input$bunid):max(input$aunid, input$bunid), 0.75, type = 1)
    )
  })
  
  
  #output ocekivane vrijednosti i varijance
  output$exp <- renderInfoBox({infoBox(title='Očekivanje', value=paste(as.character(expinput())), color = 'purple')})
  output$var <- renderInfoBox({infoBox(title='Varijanca ', value={paste(as.character(varinput()))},color='purple')})
  output$exp_diskr <- renderInfoBox({infoBox("Očekivanje", value={paste(as.character(expinput_diskr()))}, color = "purple")})
  output$var_diskr <- renderInfoBox({infoBox("Varijanca", value={paste(as.character(varinput_diskr()))}, color = "purple")})
  
  
  #resetiranje vrijednosti
  observeEvent(input$resetunin, {updateSliderInput(session, 'inf',value=0); updateSliderInput(session, 'sup',value=1)})
  observeEvent(input$resetnrm, {updateSliderInput(session, 'mu',value=0); updateSliderInput(session, 'var',value=1)})
  observeEvent(input$reseteksp, {updateSliderInput(session, 'lamb',value=1)})
  observeEvent(input$resetchisq, {updateSliderInput(session = session, 'kkhi', value=1)})

  
  # observeEvent(input$resetchisq, {
  #   updateSliderInput(session, "kkhi", value = 1)
  #   updateCheckboxInput(session, "showexp", value = FALSE)
  #   updateCheckboxInput(session, "showvar", value = FALSE)
  #   updateCheckboxInput(session, "showmedian", value = FALSE)
  #   updateCheckboxInput(session, "showquartiles", value = FALSE)
  # })
  observeEvent(input$resetstud, {updateSliderInput(session = session, 'kstud', value=1)})
  observeEvent(input$resetfish, {updateSliderInput(session, 'd1', value=1);updateSliderInput(session, 'd2', value=1)})
  observeEvent(input$resetbern, {updateSliderInput(session, "pbern", value = 0.5)})
  observeEvent(input$resetbinom, {updateSliderInput(session, "nbinom", value = 10);updateSliderInput(session, "pbinom", value = 0.5)})
  observeEvent(input$resetpois, {updateSliderInput(session, "lambdapois", value = 3)})
  observeEvent(input$resetunid, {updateSliderInput(session, "aunid", value = 0);updateSliderInput(session, "bunid", value = 5)})
  
  #grafovi
  
  #povrsina ispod grafa (vjerojatnost)
  intervalProb <- reactive({
    req(input$show_area)
    a <- input$interval[1]
    b <- input$interval[2]
    
    switch(input$tabs,
           'unin' = punif(b, input$inf, input$sup) - punif(a, input$inf, input$sup),
           'nrm' = pnorm(b, input$mu, sqrt(input$var)) - pnorm(a, input$mu, sqrt(input$var)),
           'eksp' = pexp(b, input$lamb) - pexp(a, input$lamb),
           'chisq' = pchisq(b, input$kkhi) - pchisq(a, input$kkhi),
           'stud' = pt(b, input$kstud) - pt(a, input$kstud),
           'fish' = pf(b, input$d1, input$d2) - pf(a, input$d1, input$d2)
    )
    
  })
  

  
  output$plotunin <- output$plotnrm <- output$ploteksp <- output$plotchisq <- output$plotstud <- output$plotfish <- 
    renderPlotly({
      validate(
        need(input$inf <= input$sup, "Greška: Parametar 'a' mora biti manji ili jednak od 'b'.")
      )
      #df <- melt(data.frame(x(),data(),data2()), id='x..') # data() sadrzava default vrijednosti
      df <- melt(data.frame(x(),data2()), id='x..')
      p <- ggplot(data=df, aes(x=x.., y=value, colour=variable)) + geom_line() + xlim(minimumx(),maximumx()) + ylim(minimumy(),maximumy()) + theme(legend.position = 'none')
      
      #crtanje ocekivanja
      if (input$showexp && is.numeric(expinput())) {
        y_val_exp <- data2()[which.min(abs(x() - expinput()))]
        p <- p + geom_segment(aes(x = expinput(), xend = expinput(), y = 0, yend = y_val_exp), 
                              linetype = "dashed", color = "blue") +
          annotate("text", x = expinput(), y = y_val_exp * 1.10, label = "E[X]", vjust = -0.5, hjust = -0.1, color = "blue", size=3)
      }
      
      # #prikaz varijance sa sd
      # 
      # if (input$showvar && is.numeric(expinput()) && is.numeric(varinput())) {
      #   exp_val <- expinput()
      #   std_dev <- sqrt(varinput())
      #   
      #   # izracun intervala
      #   intervals <- c(
      #     exp_val - 3*std_dev, exp_val - 2*std_dev, exp_val - std_dev,
      #     exp_val + std_dev, exp_val + 2*std_dev, exp_val + 3*std_dev
      #   )
      #   
      #   # izledi za različite st, devijacije
      #   colors <- c("red", "orange", "mediumseagreen", "mediumseagreen", "orange", "red")
      #   linetypes <- c("dotted", "dotted", "dotted", "dotted", "dotted", "dotted")
      #   labels <- c("μ-3σ", "μ-2σ", "μ-σ", "μ+σ", "μ+2σ", "μ+3σ")
      #   
      #   # vertikalni pravci za svaki sd interval
      #   
      #   for (i in seq_along(intervals)) {
      #     x_val <- intervals[i]
      #     y_val <- data2()[which.min(abs(x() - x_val))]
      #     
      #     p <- p + geom_segment(data = data.frame(x_val = x_val, y_val = y_val),
      #                           mapping = aes(x = x_val, xend = x_val, y = 0, yend = y_val),
      #                           linetype = linetypes[i], color = colors[i], alpha = 0.7,
      #                           inherit.aes = FALSE)
      #     
      #         p <- p + annotate("text",
      #                           x = intervals[i],
      #                           y = maximumy() * 0.9,
      #                           label = labels[i],
      #                           vjust = -0.5,
      #                           hjust = 0.5,
      #                           color = colors[i],
      #                           size = 3,
      #                           angle = 90)
      #   }
      
      #prikaz varijance sa intervalima
      
      # Dodaj sjenčanje ako je odabrano k > 0
      if (as.numeric(input$sd_interval_k) > 0) {
        k <- as.numeric(input$sd_interval_k)
        
        if (input$tabs == "nrm" && is.numeric(input$mu) && is.numeric(input$var)) {
          mu <- input$mu
          sigma <- sqrt(input$var)
          rate_param <- NULL
        } else if (input$tabs == "unin" && is.numeric(input$inf) && is.numeric(input$sup)) {
          mu <- (input$inf + input$sup)/2
          sigma <- (input$sup - input$inf)/sqrt(12)
          rate_param <- NULL
        } else if (input$tabs == "eksp" && is.numeric(input$lamb)) {
          mu <- 1 / input$lamb
          sigma <- 1 / input$lamb
          rate_param <- input$lamb
        } else if (input$tabs == "chisq" && is.numeric(input$kkhi)) {
          mu <- input$kkhi
          sigma <- sqrt(2 * input$kkhi)
          rate_param <- NULL
        } else if (input$tabs == "stud" && is.numeric(input$kstud)) {
          mu <- 0
          sigma <- sqrt(input$kstud / (input$kstud - 2))
          rate_param <- NULL
        } else if (input$tabs == "fish" && is.numeric(input$d1) && is.numeric(input$d2) && input$d2 > 2) {
          mu <- input$d2 / (input$d2 - 2)
          sigma <- sqrt((2 * input$d2^2 * (input$d1 + input$d2 - 2)) / (input$d1 * (input$d2 - 2)^2 * (input$d2 - 4)))
          rate_param <- NULL
        } else {
          return(p)
        }
        
        if (!is.finite(mu) || !is.finite(sigma)) return(p)
        
        a <- mu - k * sigma
        b <- mu + k * sigma
        
        # Ensure non-negative bounds for distributions that start at 0
        if (input$tabs %in% c("eksp", "chisq", "fish")) {
          a <- max(0, a)
        }
        
        # Ensure the interval is within the plot range
        a <- max(a, minimumx())
        b <- min(b, maximumx())
        
        if (a >= b) return(p)  # Invalid interval
        
        # Create sequence of x values for smooth shading
        x_seq <- seq(a, b, length.out = 100)
        
        # Calculate corresponding y values using the appropriate density function
        y_seq <- switch(input$tabs,
                        'unin'  = dunif(x_seq, input$inf, input$sup),
                        'nrm'   = dnorm(x_seq, input$mu, sqrt(input$var)),
                        'eksp'  = dexp(x_seq, rate = input$lamb),
                        'chisq' = dchisq(x_seq, df = input$kkhi),
                        'stud'  = dt(x_seq, df = input$kstud),
                        'fish'  = df(x_seq, df1 = input$d1, df2 = input$d2),
                        rep(0, length(x_seq))
        )
        
        # Create proper polygon for shading
        shade_df <- data.frame(
          x = c(a, x_seq, b, b, a),
          y = c(0, y_seq, 0, 0, 0)
        )
        
        # Add the shaded area
        p <- p + geom_polygon(data = shade_df, aes(x = x, y = y), inherit.aes = FALSE, fill = "orchid", alpha = 0.4)
        #p <- p + annotate("geom_polygon",data = shade_df, x = x, y = y, inherit.aes = FALSE, fill = "orchid", alpha = 0.4)
        
        # Add vertical lines at interval boundaries
        y_at_a <- switch(input$tabs,
                         'unin'  = dunif(a, input$inf, input$sup),
                         'nrm'   = dnorm(a, input$mu, sqrt(input$var)),
                         'eksp'  = dexp(a, rate = input$lamb),
                         'chisq' = dchisq(a, df = input$kkhi),
                         'stud'  = dt(a, df = input$kstud),
                         'fish'  = df(a, df1 = input$d1, df2 = input$d2),
                         0
        )
        
        y_at_b <- switch(input$tabs,
                         'unin'  = dunif(b, input$inf, input$sup),
                         'nrm'   = dnorm(b, input$mu, sqrt(input$var)),
                         'eksp'  = dexp(b, rate = input$lamb),
                         'chisq' = dchisq(b, df = input$kkhi),
                         'stud'  = dt(b, df = input$kstud),
                         'fish'  = df(b, df1 = input$d1, df2 = input$d2),
                         0
        )
        
        # p <- p + geom_segment(aes(x = a, xend = a, y = 0, yend = y_at_a),
        #                       linetype = "dotted", color = "red") +
        #   geom_segment(aes(x = b, xend = b, y = 0, yend = y_at_b),
        #                linetype = "dotted", color = "red")
        
        # Calculate probability using correct parameters
        prob <- switch(input$tabs,
                       'unin'  = punif(b, input$inf, input$sup) - punif(a, input$inf, input$sup),
                       'nrm'   = pnorm(b, input$mu, sqrt(input$var)) - pnorm(a, input$mu, sqrt(input$var)),
                       'eksp'  = pexp(b, rate = input$lamb) - pexp(a, rate = input$lamb),
                       'chisq' = pchisq(b, df = input$kkhi) - pchisq(a, df = input$kkhi),
                       'stud'  = pt(b, df = input$kstud) - pt(a, df = input$kstud),
                       'fish'  = pf(b, df1 = input$d1, df2 = input$d2) - pf(a, df1 = input$d1, df2 = input$d2),
                       NA)
        
        if (is.finite(prob)) {
          p <- p + annotate("text", x = mean(c(minimumx(), maximumx())), y = maximumy()*0.78,
                            label = paste0("P(μ-", k, "σ < X < μ+", k, "σ) ≈", round(prob, 4)),
                            color = "darkred", fontface = "bold", size = 4)
        }
      }
      
      # crtanje medijana
      if (input$showmedian && is.numeric(medianinput())) {
        y_val_med <- data2()[which.min(abs(x() - medianinput()))]
        p <- p + geom_segment(aes(x = medianinput(), xend = medianinput(), y = 0, yend = y_val_med), 
                              linetype = "dashed", color = "purple", size = 1) +
          annotate("text", x = medianinput(), y = y_val_med * 1.10, 
                   label = "Medijan", vjust = -0.5, hjust = -0.1, color = "purple", fontface = "bold", size=3)
      } #medijan
      
      # crtanje kvartila
      if (input$showquartiles && is.numeric(q1input()) && is.numeric(q3input())) {
        y_q1 <- data2()[which.min(abs(x() - q1input()))]
        y_q3 <- data2()[which.min(abs(x() - q3input()))]

        # crtanje prvog kvartila
        p <- p + geom_segment(aes(x = q1input(), xend = q1input(), y = 0, yend = y_q1),
                              linetype = "dotdash", color = "darkgreen", size = 0.8) +
          annotate("text", x = q1input(), y = y_q1 * 1.2,
                   label = "Q(0.25)", vjust = -0.5, hjust = -0.1, color = "darkgreen", fontface = "bold", size = 3.2)

        # crtanje treceg kvartila
        p <- p + geom_segment(aes(x = q3input(), xend = q3input(), y = 0, yend = y_q3),
                              linetype = "dotdash", color = "darkgreen", size = 0.8) +
          annotate("text", x = q3input(), y = y_q3 * 1.2,
                   label = "Q(0.75)", vjust = -0.5, hjust = -0.1, color = "darkgreen", fontface = "bold", size=3.2)
        
        # a <- q1input()
        # b <- q3input()
        # 
        # x_fill <- x()[x() >= a & x() <= b]
        # y_fill <- data2()[x() >= a & x() <= b]
        # shade_df <- data.frame(x = x_fill, y = y_fill)
        # 
        # p <- p + geom_area(data = shade_df, inherit.aes = FALSE, mapping = aes(x = x, y = y), fill = "palegreen", alpha = 0.5)
        # 
        # # Dodaj tekst s prikazom vjerojatnosti interkvartilnog raspona
        # prob_quartile <- round(switch(input$tabs,
        #                               'unin' = punif(b, input$inf, input$sup) - punif(a, input$inf, input$sup),
        #                               'nrm' = pnorm(b, input$mu, sqrt(input$var)) - pnorm(a, input$mu, sqrt(input$var)),
        #                               'eksp' = pexp(b, input$lamb) - pexp(a, input$lamb),
        #                               'chisq' = pchisq(b, input$kkhi) - pchisq(a, input$kkhi),
        #                               'stud' = pt(b, input$kstud) - pt(a, input$kstud),
        #                               'fish' = pf(b, input$d1, input$d2) - pf(a, input$d1, input$d2)
        # ), 4)
        # 
        # p <- p + annotate("text", x = mean(c(minimumx(), maximumx())), maximumy()*0.87,
        #                   label = paste0("P(Q1 ≤ X ≤ Q3) ≈ ", prob_quartile),
        #                   size = 4, color = "darkgreen", fontface = "bold")
        
      } #kvartili
      
      #povrsina ispod grafa
      if (input$show_area) {
        a <- input$interval[1]
        b <- input$interval[2]
        x_fill <- x()[x() >= a & x() <= b]
        y_fill <- data2()[x() >= a & x() <= b]
        shade_df <- data.frame(x = x_fill, y = y_fill)
        
        p <- p + geom_area(data = shade_df,inherit.aes = FALSE, mapping = aes(x = x, y = y), fill = "skyblue", alpha = 0.5)
        #p <- p + annotate("geom_area", data = shade_df, inherit.aes = FALSE, mapping = aes(x = x, y = y), fill = "skyblue", alpha = 0.5)
        
        # Dodaj tekst s prikazom vjerojatnosti
        prob_label <- paste0("P(", a, " ≤ X ≤ ", b, ") = ", round(intervalProb(), 4))
        p <- p + annotate("text", x = mean(c(minimumx(), maximumx())), y = maximumy()*0.95, label = prob_label, size = 4, color = "black", fontface = "bold")
      }
      
      p <- p + labs(x = 'x', y = 'f(x)') + scale_fill_discrete(name = "Values", labels = c("Reference", "New"))
      print(ggplotly(p,tooltip = c('y')))
    }) #renderPlotly
  
  #bernoulli graf
  output$plotbern <- renderPlotly({
    x_vals <- c(0, 1)
    probs <- c(1 - input$pbern, input$pbern)
    df <- data.frame(x = x_vals, y = probs)
    
    p <- ggplot(df, aes(x = x, y = y)) +
      geom_linerange(aes(x = x, ymin = 0, ymax = y), color = "steelblue", linewidth = 1) +
      ylim(0, 1) +
      xlab("x") + ylab("f(x)") +
      theme_minimal()
    
    if (input$showexp_diskr) {
      p <- p + geom_point(aes(x = as.numeric(expinput_diskr()), y = 0), 
                          color = "blue", size = 3) +
        geom_text(aes(x = as.numeric(expinput_diskr()), y = 0.05), 
                  label = "E[X]", color = "blue", vjust = -1, size = 3)
    }
    
    if (input$showmedian_diskr) {
      p <- p + geom_point(aes(x = as.numeric(medianinput_diskr()), y = 0), 
                          color = "purple", size = 3) +
        geom_text(aes(x = as.numeric(medianinput_diskr()), y = 0.05), 
                  label = "Medijan", color = "purple", vjust = -1, size = 3)
    }
    
    if (input$showquartiles_diskr) {
      p <- p + 
        geom_point(aes(x = as.numeric(q1input_diskr()), y = 0), 
                   color = "darkgreen", size = 3) +
        geom_point(aes(x = as.numeric(q3input_diskr()), y = 0), 
                   color = "darkgreen", size = 3) +
        geom_text(aes(x = as.numeric(q1input_diskr()), y = 0.05), 
                  label = "Q(0.25)", color = "darkgreen", vjust = -1, size = 3) +
        geom_text(aes(x = as.numeric(q3input_diskr()), y = 0.05), 
                  label = "Q(0.75)", color = "darkgreen", vjust = -1, size = 3)
    }
    ggplotly(p)
  }) #bernoullijev graf
  
  #binomni graf
  output$plotbinom <- renderPlotly({
    x_vals <- 0:input$nbinom
    probs <- dbinom(x_vals, size = input$nbinom, prob = input$pbinom)
    df <- data.frame(x = x_vals, y = probs)
    
    p <- ggplot(df, aes(x = x, y = y)) +
      geom_linerange(aes(x = x, ymin = 0, ymax = y), color = "steelblue", linewidth = 1) +
      ylim(0, 1) +
      xlab("x") + ylab("f(x)") +
      theme_minimal()
    if (input$showexp_diskr) {
      p <- p + geom_point(aes(x = as.numeric(expinput_diskr()), y = 0), 
                          color = "blue", size = 3) +
        geom_text(aes(x = as.numeric(expinput_diskr()), y = 0.05), 
                  label = "E[X]", color = "blue", vjust = -1, size = 3)
    }
    
    if (input$showmedian_diskr) {
      p <- p + geom_point(aes(x = as.numeric(medianinput_diskr()), y = 0), 
                          color = "purple", size = 3) +
        geom_text(aes(x = as.numeric(medianinput_diskr()), y = 0.05), 
                  label = "Medijan", color = "purple", vjust = -1, size = 3)
    }
    
    if (input$showquartiles_diskr) {
      p <- p + 
        geom_point(aes(x = as.numeric(q1input_diskr()), y = 0), 
                   color = "darkgreen", size = 3) +
        geom_point(aes(x = as.numeric(q3input_diskr()), y = 0), 
                   color = "darkgreen", size = 3) +
        geom_text(aes(x = as.numeric(q1input_diskr()), y = 0.05), 
                  label = "Q(0.25)", color = "darkgreen", vjust = -1, size = 3) +
        geom_text(aes(x = as.numeric(q3input_diskr()), y = 0.05), 
                  label = "Q(0.75)", color = "darkgreen", vjust = -1, size = 3)
      
    }
    
    #izracun vjerojatnosti
    if (input$show_discrete_prob) {
      a <- input$discrete_interval[1]
      b <- input$discrete_interval[2]
      
      x_vals <- a:b
      y_vals <- dbinom(x_vals, size = input$nbinom, prob = input$pbinom) 
      x_center <- round((input$nbinom) / 2)
      interval_pts <- data.frame(x = c(a, b), y = 0)
      
      # Dodaj točke za krajeve intervala
      p <- p +
        geom_point(data = interval_pts,inherit.aes = FALSE,
                   aes(x = x, y = y),
                   color = "darkred", size = 3) +
        geom_segment(data = data.frame(x = a, xend =b,y = 0, yend = 0),
                     aes(x = x, xend = xend, y = y, yend = yend),
                     inherit.aes = FALSE,
                     color = "darkred", linetype = "dashed", linewidth = 1) +
        annotate("text", x = x_center, y =  0.95,
                 label = paste0("P(", a, " ≤ X ≤ ", b, ") = ", 
                                round(sum(dbinom(x_vals, size = input$nbinom, prob = input$pbinom)), 4)),
                 size = 4, color = "darkred", fontface = "bold")
    }
    
    # μ ± k·σ interval
    if (as.numeric(input$sd_interval_k_diskr) > 0) {
      k <- as.numeric(input$sd_interval_k_diskr)
      mu <- input$nbinom * input$pbinom
      sigma <- sqrt(input$nbinom * input$pbinom * (1 - input$pbinom))
      
      a <- ceiling(mu - k * sigma)
      b <- floor(mu + k * sigma)
      x_center <- round((input$nbinom) / 2)
      
      x_range <- a:b
      valid_vals <- x_range[x_range >= 0 & x_range <= input$nbinom]
      prob <- if (length(valid_vals) > 0) sum(dbinom(valid_vals, size = input$nbinom, prob = input$pbinom)) else 0
      
      # Dodaj horizontalnu crtu i tekst
      p <- p +
        geom_segment(aes(x = a, xend = b, y = 0, yend = 0),
                     color = "orchid", linetype = "dotted", linewidth = 1.2) +
        geom_point(aes(x = a, y = 0), color = "orchid", size = 3) +
        geom_point(aes(x = b, y = 0), color = "orchid", size = 3) +
        annotate("text", x = x_center, y = 0.88,
                 label = paste0("P(μ-", k, "σ ≤ X ≤ μ+", k, "σ) ≈ ", round(prob, 4)),
                 color = "mediumpurple4", fontface = "bold", size = 4)
    }
    
    ggplotly(p)
  }) #binomni graf
  
  #poisson graf
  output$plotpois <- renderPlotly({
    lambda <- input$lambdapois
    x_vals <- 0:qpois(0.999, lambda)
    probs <- dpois(x_vals, lambda = lambda)
    df <- data.frame(x = x_vals, y = probs)
    
    p <- ggplot(df, aes(x = x, y = y)) +
      geom_linerange(aes(x = x, ymin = 0, ymax = y), color = "steelblue", linewidth = 1) +
      ylim(0, max(probs) * 1.1) +
      xlab("x") + ylab("f(x)") +
      theme_minimal()
    
    if (input$showexp_diskr) {
      p <- p + geom_point(aes(x = as.numeric(expinput_diskr()), y = 0), 
                          color = "blue", size = 3) +
        geom_text(aes(x = as.numeric(expinput_diskr()), y = 0.01), 
                  label = "E[X]", color = "blue", vjust = -1, size = 3)
    }
    
    if (input$showmedian_diskr) {
      p <- p + geom_point(aes(x = as.numeric(medianinput_diskr()), y = 0), 
                          color = "purple", size = 3) +
        geom_text(aes(x = as.numeric(medianinput_diskr()), y = 0.01), 
                  label = "Medijan", color = "purple", vjust = -1, size = 3)
    }
    
    if (input$showquartiles_diskr) {
      p <- p + 
        geom_point(aes(x = as.numeric(q1input_diskr()), y = 0), 
                   color = "darkgreen", size = 3) +
        geom_point(aes(x = as.numeric(q3input_diskr()), y = 0), 
                   color = "darkgreen", size = 3) +
        geom_text(aes(x = as.numeric(q1input_diskr()), y = 0.01), 
                  label = "Q(0.25)", color = "darkgreen", vjust = -1, size = 3) +
        geom_text(aes(x = as.numeric(q3input_diskr()), y = 0.01), 
                  label = "Q(0.75)", color = "darkgreen", vjust = -1, size = 3)
    }
    
    if (input$show_discrete_prob) {
      a <- input$discrete_interval[1]
      b <- input$discrete_interval[2]
      
      x_range <- a:b
      y_vals <- dpois(x_range, lambda = lambda)
      x_center <- round(lambda)
      
      interval_pts <- data.frame(x = c(a, b) , y = 0)

      p <- p +
        geom_point(data = interval_pts, inherit.aes = FALSE,
                   aes(x = x, y = y),
                   color = "darkred", size = 3) +
        geom_segment(data = data.frame(x = a,
                                       xend = b,
                                       y = 0, yend = 0),
                     aes(x = x, xend = xend, y = y, yend = yend),
                     inherit.aes = FALSE,
                     color = "darkred", linetype = "dashed", linewidth = 1) +
        annotate("text", x = x_center, y = max(probs)*1.05,
                 label = paste0("P(", a, " ≤ X ≤ ", b, ") = ", 
                                round(sum(y_vals), 4)),
                 size = 4, color = "darkred", fontface = "bold")
    }
    
    # μ ± k·σ interval za Poisson
    if (as.numeric(input$sd_interval_k_diskr) > 0) {
      k <- as.numeric(input$sd_interval_k_diskr)
      mu <- lambda
      sigma <- sqrt(lambda)
      x_center <- round(lambda)
      
      a <- ceiling(mu - k * sigma)
      b <- floor(mu + k * sigma)
      
      x_range <- a:b
      valid_vals <- x_range[x_range >= 0]
      prob <- if (length(valid_vals) > 0) sum(dpois(valid_vals, lambda = lambda)) else 0
      
      p <- p +
        geom_segment(aes(x = a, xend = b, y = 0, yend = 0),
                     color = "orchid", linetype = "dotted", linewidth = 1.2) +
        geom_point(aes(x = a, y = 0), color = "orchid", size = 3) +
        geom_point(aes(x = b, y = 0), color = "orchid", size = 3) +
        annotate("text", x = x_center, y = max(probs)*0.98,
                 label = paste0("P(μ-", k, "σ ≤ X ≤ μ+", k, "σ) ≈ ", round(prob, 4)),
                 color = "mediumpurple4", fontface = "bold", size = 4)
    }
    ggplotly(p)
  }) # poisson graf
  
  #unif. d. graf
  output$plotunid <- renderPlotly({
    validate(
      need(input$aunid <= input$bunid, "Greška: Parametar 'a' mora biti manji ili jednak od 'b'.")
    )
    a <- min(input$aunid, input$bunid)
    b <- max(input$aunid, input$bunid)
    x_vals <- a:b
    probs <- rep(1 / (b - a + 1), length(x_vals))
    df <- data.frame(x = x_vals, y = probs)
    
    p <- ggplot(df, aes(x = x, y = y)) +
      geom_linerange(aes(x = x, ymin = 0, ymax = y), color = "steelblue", linewidth = 1) +
      ylim(0, 1) +
      xlab("x") + ylab("f(x)") +
      theme_minimal()
    
    if (input$showexp_diskr) {
      p <- p + geom_point(aes(x = as.numeric(expinput_diskr()), y = 0), 
                          color = "blue", size = 3) +
        geom_text(aes(x = as.numeric(expinput_diskr()), y = 0.05), 
                  label = "E[X]", color = "blue", vjust = -1, size = 3)
    }
    
    if (input$showmedian_diskr) {
      p <- p + geom_point(aes(x = as.numeric(medianinput_diskr()), y = 0), 
                          color = "purple", size = 3) +
        geom_text(aes(x = as.numeric(medianinput_diskr()), y = 0.05), 
                  label = "Medijan", color = "purple", vjust = -1, size = 3)
    }
    
    if (input$showquartiles_diskr) {
      p <- p + 
        geom_point(aes(x = as.numeric(q1input_diskr()), y = 0), 
                   color = "darkgreen", size = 3) +
        geom_point(aes(x = as.numeric(q3input_diskr()), y = 0), 
                   color = "darkgreen", size = 3) +
        geom_text(aes(x = as.numeric(q1input_diskr()), y = 0.05), 
                  label = "Q(0.25)", color = "darkgreen", vjust = -1, size = 3) +
        geom_text(aes(x = as.numeric(q3input_diskr()), y = 0.05), 
                  label = "Q(0.75)", color = "darkgreen", vjust = -1, size = 3)
    }
    
    if (input$show_discrete_prob) {
      a <- input$discrete_interval[1]
      b <- input$discrete_interval[2]
      a_val <- min(input$aunid, input$bunid)
      b_val <- max(input$aunid, input$bunid)
      
      x_vals <- a:b
      valid_vals <- x_vals[x_vals >= a_val & x_vals <= b_val]
      prob <- if (length(valid_vals) > 0) length(valid_vals) / (b_val - a_val + 1) else 0
      y_val <- 1 / (b_val - a_val + 1)
      
      x_center <- floor((min(input$aunid, input$bunid) + max(input$aunid, input$bunid)) / 2)
      interval_pts <- data.frame(x = c(a, b), y = 0)
      
      p <- p +
        geom_point(data = interval_pts, inherit.aes = FALSE,
                   aes(x = x, y = y),
                   color = "darkred", size = 3) +
        annotate("segment", x = a, xend = b, y = 0, yend = 0,
                     color = "darkred", linetype = "dashed", linewidth = 1) +
        annotate("text", x = x_center, y = y_val * 2,
                 label = paste0("P(", a, " ≤ X ≤ ", b, ") = ", round(prob, 4)),
                 size = 4, color = "darkred", fontface = "bold")
    }
    # μ ± k·σ interval za diskretnu uniformnu
    if (as.numeric(input$sd_interval_k_diskr) > 0) {
      k <- as.numeric(input$sd_interval_k_diskr)
      a_val <- min(input$aunid, input$bunid)
      b_val <- max(input$aunid, input$bunid)
      x_center <- floor((min(input$aunid, input$bunid) + max(input$aunid, input$bunid)) / 2)
      y_val <- 1 / (b_val - a_val + 1)
      mu <- (a_val + b_val) / 2
      sigma <- sqrt(((b_val - a_val + 1)^2 - 1) / 12)
      
      a <- ceiling(mu - k * sigma)
      b <- floor(mu + k * sigma)
      
      x_range <- a:b
      valid_vals <- x_range[x_range >= a_val & x_range <= b_val]
      prob <- if (length(valid_vals) > 0) length(valid_vals) / (b_val - a_val + 1) else 0
      
      p <- p +
        geom_segment(aes(x = a, xend = b, y = 0, yend = 0),
                     color = "orchid",linetype = "dotted", linewidth = 1.2) +
        geom_point(aes(x = a, y = 0), color = "orchid", size = 3) +
        geom_point(aes(x = b, y = 0), color = "orchid", size = 3) +
        annotate("text", x = x_center, y = y_val * 1.5,
                 label = paste0("P(μ-", k, "σ ≤ X ≤ μ+", k, "σ) ≈ ", round(prob, 4)),
                 color = "mediumpurple4", fontface = "bold", size = 4)
    }
    
    ggplotly(p)
  })#unif. d. graf
  
  #mat. izrazi ocekivanja
  output$expected <- renderUI({p({switch(input$tabs,'unin' = withMathJax('$$\\mathbb{E}[X]=\\dfrac{a+b}{2}$$'),
                                         'nrm' = withMathJax('$$\\mathbb{E}[X]=\\mu$$'),
                                         'eksp' = withMathJax('$$\\mathbb{E}[X]=\\dfrac{1}{\\lambda}$$'),
                                         'chisq' = withMathJax('$$\\mathbb{E}[X]=k$$'),
                                         'stud' = withMathJax('$$\\mathbb{E}[X]=0 \\textrm{, } k>1$$'),
                                         'fish' = withMathJax('$$\\mathbb{E}[X]=\\dfrac{d_2}{d_2-2},\\ d_2>2$$')
  )})}) #renderUI
  
  output$expected_diskr<- renderUI({p({switch(input$tabs_diskr,'bern' = withMathJax('$$\\mathbb{E}[X] = p$$'),
                                              'binom' = withMathJax('$$\\mathbb{E}[X] = np$$'),
                                              'pois' = withMathJax('$$\\mathbb{E}[X] = \\lambda$$'),
                                              'unid' = withMathJax('$$\\mathbb{E}[X] = \\frac{a + b}{2}$$')
  )})})
  
  #mat. izrazi gustoće
  output$dense <- renderUI({p({switch(input$tabs,'unin' = withMathJax('$$f(x;a,b)=\\dfrac{1}{b-a}\\text{,   }\\ a\\leq x \\leq b\\ $$'),
                                      'nrm' = withMathJax('$$f( x;\\sigma^2,\\mu)=\\dfrac{1}{\\sigma\\sqrt{2\\pi}}e^{-\\frac{(x-\\mu)^2}{2\\sigma^2}},\\quad x\\in\\mathbb{R}$$'),
                                      'eksp' = withMathJax('$$f( x;\\lambda)=\\lambda e^{-\\lambda x}\\text{,   }\\, x \\geq 0$$'),
                                      'chisq' = withMathJax('$$f(x;k)=\\dfrac{\\left(\\frac{1}{2}\\right)^{\\frac{k}{2}}}{\\Gamma\\left(\\frac{k}{2}\\right)}x^{\\frac{k}{2}-1}e^{-\\frac{x}{2}},\\ x > 0$$'),
                                      'stud' = withMathJax('$$f(x;\\nu)=\\dfrac{1}{\\sqrt{\\nu\\pi}}\\dfrac{\\Gamma\\left(\\frac{\\nu+1}{2}\\right)}{\\Gamma\\left(\\frac{\\nu}{2}\\right)}\\left(1+\\dfrac{x^2}{\\nu}\\right)^{-\\frac{\\nu+1}{2}},\\quad x\\in\\mathbb{R}$$'),
                                      'fish' = withMathJax('$$f(x;d1,d2)=\\dfrac{\\sqrt{\\dfrac{(d_1x)^{d_1}d_2^{d_2}}{(d_1x+d_2)^{d_1+d_2}}}}{x\\textbf{B}\\left(\\frac{d_1}{2},\\frac{d_2}{2}\\right)}\\text{,   }\\, x \\ > 0,\\textbf{B}(x,y)=\\dfrac{\\Gamma(x)\\Gamma(y)}{\\Gamma(x+y)}$$')
  )})}) #renderUI
  
  output$dense_diskr<- renderUI({p({switch(input$tabs_diskr,'bern' = withMathJax('$$f(x;p) = p^x (1-p)^{1-x},\\quad x \\in \\{0,1\\}$$'),
                                           'binom' = withMathJax('$$f(x;n,p) = \\binom{n}{x}p^x(1-p)^{n-x},\\quad x = 0,1,\\dots,n$$'),
                                           'pois' = withMathJax('$$f(x;\\lambda) = \\dfrac{\\lambda^x e^{-\\lambda}}{x!},\\quad x \\in \\mathbb{N}_0$$'),
                                           'unid' = withMathJax('$$f(x;a,b) = \\dfrac{1}{b - a + 1},\\quad x = a,a+1,\\dots,b$$')
  )})})
  
  #mat. izrazi varijance
  output$variance <- renderUI({p({switch(input$tabs,'unin' = withMathJax('$$\\mathbb{Var} X=\\dfrac{(b-a)^2}{12}$$'),
                                         'nrm' = withMathJax('$$\\mathbb{Var}[X]=\\sigma^2$$'),
                                         'eksp' = withMathJax('$$\\mathbb{Var}[X]=\\dfrac{1}{\\lambda^2}$$'),
                                         'chisq' = withMathJax('$$\\mathbb{Var}[X]=2k$$'),
                                         'stud' = withMathJax('$$\\mathbb{Var}[X]=\\dfrac{k}{k-2},\\ k>2$$'),
                                         'fish' = withMathJax('$$\\mathbb{Var}[X]=\\dfrac{2d_2^2(d_1+d_2-2)}{d_1(d_2-2)^2(d_2-4)},\\ d_2>4$$')
  )})})
  
  output$variance_diskr<- renderUI({p({switch(input$tabs_diskr,'bern' = withMathJax('$$\\mathbb{Var}(X) = p(1-p)$$'),
                                              'binom' = withMathJax('$$\\mathbb{Var}(X) = np(1-p)$$'),
                                              'pois' = withMathJax('$$\\mathbb{Var}(X) = \\lambda$$'),
                                              'unid' = withMathJax('$$\\mathbb{Var}(X) = \\frac{(b - a + 1)^2 - 1}{12}$$')
  )})})
  
} #server



shinyApp(ui = ui, server = server)