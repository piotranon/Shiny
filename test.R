setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(tm);
library(shiny);
library(shinythemes);
library(wordcloud2);
library(DT);

# zaladowanie funkcji dotyczacych bazy solr
source("solr.R")
# zaladowanie funkcji dotyczacych dzialan na danych
source("functions.R")

# dodanie danych do bazy jesli jeszcze ich tam nie ma
#aktualizacjaDanychSolr();

# chmura slow tytuly
#wordcloud2(chmuraSlow(titles),size=0.5)

# chmura slow opisy
#wordcloud2(chmuraSlow(contents),size=0.5)

maxDate<-as.Date(getMaxDate()[[1]])
minDate<-as.Date(getMinDate()[[1]])
  
#maxCzestotliwosc<-
#minCzestotliwosc<-


ui <- fluidPage(theme = shinytheme("sandstone"),
  navbarPage(h3("Aplikacja do akwizycji i analizy danych"),
             tabPanel(h3("Czestosc wystepowania"),
                      sidebarLayout(
                        sidebarPanel(
                          column(12, align="center",h2("Częstośc występowania słów")),
                          selectInput("wystepowanieSelect",h4("Wybierz dane"),c("Zawartosc"="Zawartosc","Tytul"="Tytul"),F),
                          dateRangeInput('wystepowaniedateRange',
                                         label = paste('Zakres datowy'),
                                         start = minDate, end = maxDate,
                                         min = minDate, max = maxDate,
                                         separator = " - ", format = "yyyy-mm-dd",
                                         startview = 'day', weekstart = 1
                          ),
                          sliderInput("wystepowanieProg", h4("Prog wystepowania"), value = 1.0, min = 1.0, max = 1, step = 1),
                          sliderInput("maksymalnaIloscCzestotliwosc", h4("Maksymalna ilosc"), value = 1.0, min = 1.0, max = 1, step = 1)
                        ),
                        mainPanel(
                          plotOutput("WykresCzestotliwosci")
                        )
                      )
             ),
             tabPanel(h3("Chmura Slow"),
                      sidebarLayout(
                        sidebarPanel(
                            column(12, align="center",h2("Chmura slow")),
                            selectInput("chmuraSelect",h4("Wybierz dane"),c("Zawartosc"="Zawartosc","Tytul"="Tytul"),F),
                            dateRangeInput('chmuradateRange',
                                           label = paste('Zakres datowy'),
                                           start = minDate, end = maxDate,
                                           min = minDate, max = maxDate,
                                           separator = " - ", format = "yyyy-mm-dd",
                                           startview = 'day', weekstart = 1
                            ),
                            sliderInput("chmuraSize", h4("Rozmiar"), value = 0.5, min = 0.4, max = 1, step = 0.1),
                            sliderInput("chmuraGrid", h4("Odstep miedzy literami"), value = 5, min = 0, max = 25, step = 5),
                            sliderInput("chmuraRotate", h4("Pochylenie"), value = 1, min = 0, max = 2, step = 0.2),
                            selectInput("chmuraShape",label=h4("Wybierz ksztalt"),c("Okrag"="circle","Gwiazda"="star","Trojkat"="triangle","Diament"="diamond","cardioid"="cardioid","pentagon"="pentagon"),F),
                        ),
                        mainPanel(
                          wordcloud2Output("chmura")
                        )
                      )
             ),tabPanel(h3("Klasteryzacja"),
                        sidebarLayout(
                          sidebarPanel(
                            fluidRow(column(12, align="center",h2("Klasteryzacja"))),
                            selectInput("klasteryzacjaSelect",h4("Wybierz dane"),c("Zawartosc"="Zawartosc","Tytul"="Tytul"),F),
                            dateRangeInput('klasteryzacjadateRange',
                                           label = paste('Zakres datowy'),
                                           start = minDate, end = maxDate,
                                           min = minDate, max = maxDate,
                                           separator = " - ", format = "yyyy-mm-dd",
                                           startview = 'day', weekstart = 1
                            ),
                            selectInput(inputId="klasteryzacjaMetryka",label=h4("Metryka"),choices = c("euclidean"="euclidean","manhattan"="manhattan"),selected = "euclidean",multiple = F),
                            sliderInput("klasteryzacjaKlastry", h4("Ilosc klastrow"), value = 2, min = 2, max = 10, step = 1),
                          ),
                          mainPanel(
                            fluidRow(column(12, align="center",DTOutput("klasteryzacjaDane"))),
                          )
                        )
             ),
             tabPanel(h3("Dane"),
                      sidebarLayout(
                        sidebarPanel(
                          fluidRow(column(12, align="center",h2("Opcje"))),
                          fluidRow(column(12, align="center",actionButton("getData", "zaktualizuj dane z kanalem RSS"))),
                        ),
                        mainPanel(
                          #tags$style(HTML('table.dataTable th {color: white !important;}')),
                          #tags$style(HTML('.dataTables_filter {color: white !important;}')),
                          DTOutput("allData")
                        )
                      )
             )
  ),
  hr(),
  fluidRow(column(12, align="center",h2("Piotr Długosz 3 rok informatyki inżynierskie Uniwersytet Rzeszowski r.2020/2021"))),
)

server <- function(input, output, session) {
  daneCzestotliwosc<-1;
  observeEvent(list(input$chmuraSelect,input$chmuraSize,input$chmuraGrid,input$chmuraShape,input$chmuraRotate,input$chmuradateRange),{
    print(input$chmuraKrztalt);
    output$chmura = renderWordcloud2({
        if(input$chmuraSelect=="Tytul"){
          chmuraDane<-pobranieTytulowSolrOdDo(od=input$chmuradateRange[1],do=input$chmuradateRange[2]);
          dane = chmuraSlow(chmuraDane);
        }
        else{
          chmuraDane<-pobranieZawartosciSolrOdDo(od=input$chmuradateRange[1],do=input$chmuradateRange[2]);
          dane = chmuraSlow(chmuraDane);
        }
        print("odswierzenie chmury");
        wordcloud2(dane,color = "random-light",rotateRatio=input$chmuraRotate ,size = input$chmuraSize,gridSize = input$chmuraGrid,backgroundColor ='rgba(255,255,255,0.1)',shape = input$chmuraShape)
      })
    });
  
  
  observeEvent(list(input$wystepowanieSelect,input$wystepowaniedateRange),{
    print("Update danych Czestotliwosc");
    if(input$wystepowanieSelect=="Tytul"){
      daneCzestotliwosc<<-wykresCzestotoliwosci(pobranieTytulowSolrOdDo(od=input$wystepowaniedateRange[1],do=input$wystepowaniedateRange[2]));
      #daneCzestotliwosc<<-wykresCzestotoliwosci(pobranieTytulowSolrOdDo(od=as.Date("2020-09-10"),do=as.Date("2020-11-23")));
    }
    else{
      daneCzestotliwosc<<-wykresCzestotoliwosci(pobranieZawartosciSolrOdDo(od=input$wystepowaniedateRange[1],do=input$wystepowaniedateRange[2]));
    }
    
    updateSliderInput(session, "wystepowanieProg", value = input$wystepowanieProg, min = 1.0, max = daneCzestotliwosc[[1]], step = 1);
    updateSliderInput(session, "maksymalnaIloscCzestotliwosc", value = 10, min = 1.0, max = length(daneCzestotliwosc), step = 1);
  
    print(as.Date(input$wystepowaniedateRange[1]));
    print(as.Date(input$wystepowaniedateRange[2]));
  });
  
  observeEvent(list(input$wystepowanieProg,input$maksymalnaIloscCzestotliwosc),{
      print("Update wykresu od progu");
      koniecCzestotliwosc<-length(daneCzestotliwosc);
      
      #znalezieni progu w danych
      for(d in length(daneCzestotliwosc):1)
      {
        if(daneCzestotliwosc[[d]]>=input$wystepowanieProg)
        {
          koniecCzestotliwosc<-d;
          break;
        }
      }
      
      #przetworzenie na data frame potrzebny do wykresu
      print(data.frame(words=names(daneCzestotliwosc),freq=daneCzestotliwosc));
      print("po data frame");
      daneCzestotliwosc2 <- data.frame(words=names(daneCzestotliwosc),freq=daneCzestotliwosc);
      print("po przypisaniu");
      updateSliderInput(session, "maksymalnaIloscCzestotliwosc", value = input$maksymalnaIloscCzestotliwosc, min = 1.0, max = koniecCzestotliwosc, step = 1);
      print("po update");
      ilosc<-koniecCzestotliwosc;
      if(input$maksymalnaIloscCzestotliwosc<ilosc)
      {
        ilosc<-input$maksymalnaIloscCzestotliwosc;
      }
      
      print("po ilosci");
      output$WykresCzestotliwosci<-renderPlot({
        barplot(daneCzestotliwosc2[1:ilosc,]$freq, names.arg = daneCzestotliwosc2[1:ilosc,]$word,
                col ="#4e5d6c", main =paste("Najczesciej wystepujace slowa powyzej progu:", input$wystepowanieProg,"maksymalna ilosc:",input$maksymalnaIloscCzestotliwosc,"zakres datowy:",input$dateRange[1],"-",input$dateRange[2], sep=" "),
                ylab = "Ilosc wystapien", ylim = c(0,daneCzestotliwosc[[1]]),
                xlab = "Slowo")
      })
      print("po wykresie");
  });
  
  
  observeEvent(list(input$getData),{
    print("Aktualizacja danych RSS");
    aktualizacjaDanychSolr();
    
    daneALL<-getAllData();
    
    formated<-data.frame(title=c(daneALL$title),content=c(daneALL$content),dateTime=c(daneALL$date));
    
    output$allData = renderDT(formated, options = list(pageLength = 20,lengthChange = FALSE));
  });
  
  
  observeEvent(list(input$klasteryzacjaSelect,input$klasteryzacjaMetryka,input$klasteryzacjaKlastry,input$klasteryzacjadateRange),{
    
    if(input$klasteryzacjaSelect=="Tytul"){
      dane = pobranieTytulowSolrOdDo(od=input$klasteryzacjadateRange[1],do=input$klasteryzacjadateRange[2]);
    }
    else{
      dane = pobranieZawartosciSolrOdDo(od=input$klasteryzacjadateRange[1],do=input$klasteryzacjadateRange[2]);
    }
    
    daneKlaster<-klasteryzacja(dane=dane,metryka=input$klasteryzacjaMetryka,iloscKlastrow=input$klasteryzacjaKlastry)
    
    formated<-data.frame(words=c(daneKlaster$words),freq=c(daneKlaster$freq),klastry=c(daneKlaster$klaster));
    
    ordered<-formated[order(-formated$klastry),];
      
    output$klasteryzacjaDane = renderDT(ordered, options = list(pageLength = 20,lengthChange = FALSE));
  })

}
# Create Shiny app ----
shinyApp(ui = ui, server = server)

# max_value = xd[[1]]
#amount_of_records = length(xd)