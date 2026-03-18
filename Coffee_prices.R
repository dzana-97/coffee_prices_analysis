#Esercitazione 1

#1. Importo file excel coffee_prices, ispeziono la struttura e creo il dataframe
coffee <- read_excel("C:/Users/dario/Desktop/Uni/2° anno/Analisi serie storiche 8cfu/Datasets-20260226/coffee_prices.xlsx", sheet = "Data")
str(coffee)
coffee <- as.data.frame(coffee)

#ispezione della struttura
str(coffee)

#1. Conversione colonna time in un oggetto Date
coffee$date = as.Date(coffee$date)
coffee$Mese = months(coffee$date)
coffee$Anno = as.numeric(format(coffee$date, format = "%Y"))
 
#2.  Creare due oggetti ts con il prezzo del caffè di qualità Arabica e Robusta, 
#associando il mese a cui i valori si riferiscono. 
#Inoltre, convertire i prezzi del caffè da USD centesimi per libbra (pound) a USD
#per kg(1libbra = 0.453592 kg)

Arabica = ts(data=coffee$PCOFFOTMUSDM/(100*0.453592), start = c(2020,01), frequency = 12)

Robusta = ts(data=coffee$PCOFFROBUSDM/(100*0.453592), start = c(2020, 01), frequency = 12)

#3. Estrarre solo le osservazioni da gennaio 2021 a maggio 2021

window(Arabica, start = c(2021,01), end = c(2021,05))
window(Robusta, start = c(2021,01), end = c(2021,05))

#4. Calcolare la differenza del primo ordine per entrambe le serie di prezzi del caffè

diff1_Arabica = diff(Arabica)
diff1_Arabica

diff1_Robusta = diff(Robusta)
diff1_Robusta

#5.  Calcolare le serie a ritardo di 1 mese per entrambe le serie di prezzi del caffè

lag1_Arabica = lag(Arabica, k = 1)
lag1_Arabica

lag1_Robusta = lag(Robusta, k = 1)
lag1_Robusta

#6. Creare un grafico delle serie temporali dei prezzi del caffè Arabica e Robusta, 
#aggiungendo un titolo e le etichette degli assi appropriate.

autoplot(Robusta) +
  labs(Title = "Prezzo caffè qualità Robusta",
       Subtitle = "Serie da gennaio 2020 a dicembre 2024",
       x = "Year", y = "USD/Kg")

autoplot(Arabica) +
  labs(Title = "Prezzo caffè qualità Arabica",
       Subtitle = "Serie da gennaio 2020 a dicembre 2024",
       x = "Year", y = "USD/Kg")

#7. Creare un grafico delle serie temporali dei prezzi del caffè Arabica e Robusta, 
#rappresentando entrambeleserie sullo stesso grafico. Confrontare l’andamento 
#dei prezzi del caffè per le diverse qualità.

TS = ts.union(Arabica, Robusta)

autoplot(TS) +
  labs(title = "Qualità di caffè a confronto",
       subtitle = "2020-2025",
    x = NULL, y = "USD/Kg")

#8. Creare un grafico delle variazioni percentuali dei prezzi del caffè Arabica e Robusta, 
#rappresentando entrambe le serie sullo stesso grafico. Da tale grafico si 
#notano andamenti differenti nella dinamica dei prezzi?

T = length(Arabica)

v1 = diff(Arabica)/Arabica[1:(T-1)]*100

v2 = diff(Robusta)/Robusta[1:(T-1)]*100

v1v2 = ts.union(v1, v2)

autoplot(v1v2) +
  geom_hline(yintercept = 0, lty = 2) +
  scale_y_continuous(n.breaks = 7) +
  labs(title = "Variazioni percentuali delle qualità arabica e robusta a confronto",
       x = NULL, y = "v")

#9. Creare un seasonal plot dei prezzi del caffè Arabica e Robusta. 
#Si osserva qualche andamento stagionale?

ggseasonplot(Arabica, year.labels = TRUE, col = 2) 

ggseasonplot(Robusta, year.labels = TRUE, col = 3)

#Oppure

ggseasonplot(Arabica, year.labels = TRUE, col = 2, polar = TRUE) 

ggseasonplot(Robusta, year.labels = FALSE, polar = TRUE)

