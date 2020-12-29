library(rlist);
library(stringi);
library(httr);
library(XML);
library(cluster);

lematyzajca<-function(tekst)
{
  parametry<-list(lpmn="any2txt|wcrft2", text=tekst, user="piotranon.1@gmail.com");
  odpowiedz<-POST("http://ws.clarin-pl.eu/nlprest2/base/process", body=parametry, encode="json", verbose());
  zawartosc<-content(odpowiedz,"text",encoding="UTF-8");
  xml<-xmlParse(zawartosc, encoding="UTF-8");
  slowa<-xpathSApply(xml, '//chunkList/chunk/sentence/tok/lex/base', xmlValue, encoding="UTF-8");
  
  return(paste(slowa, collapse=" "));
}

usuniecieSmieci<-function(dane)
{
  # slowa stopu
  stop<-as.vector(unlist(read.csv(file="stop_words_pl.txt", header=FALSE, sep=",", fileEncoding="UTF-8")));
  
  # konwersja do utf8 nastepnie do wektora nastepnie do kolekcji dokumentów
  dokumenty<-Corpus(VectorSource(stri_enc_toutf8(dane)));
  # usuniecie napisu z konca 
  dokumenty<-tm_map(dokumenty, function(x) gsub("(czytaj wiecej - www.gofin.pl/prawnik-radzi/)","",x))
  # usuniecie znakow interpunkcyjnych
  dokumenty<-tm_map(dokumenty, removePunctuation); 
  # do malych znakow
  dokumenty<-tm_map(dokumenty, tolower); 
  # usuniecie liczb
  dokumenty<-tm_map(dokumenty, removeNumbers); 
  # usuniecie slow znajdujacych sie w stop
  dokumenty<-tm_map(dokumenty, removeWords, stop);
  # usuniecie znaków z tablicy
  dokumenty<-tm_map(dokumenty, function(x) gsub("[-“„]","",x));
  
  for( d in 1:length(dokumenty))
  {
    dokumenty[[d]]$content<-lematyzajca(dokumenty[[d]]$content);
    dokumenty[[d]]$content<-stri_enc_toutf8(dokumenty[[d]]$content);
  }
  
  return(dokumenty);
}

chmuraSlow<-function(dane)
{
  dokumenty<-usuniecieSmieci(dane = dane);
  
  tdm<-TermDocumentMatrix(dokumenty);
  m<-as.matrix(tdm);
  
  v<-sort(rowSums(m), decreasing = TRUE);
  
  d<-data.frame(words=names(v), freq=v);
  
  return(d);
}

wykresCzestotoliwosci<-function(dane)
{
  dokumenty<-usuniecieSmieci(dane = dane);
  
  tdm1<-TermDocumentMatrix(dokumenty);
  m1<-as.matrix(tdm1);
  v<-sort(rowSums(m1),decreasing=TRUE);
  return(v);
}

klasteryzacja<-function(dane,metryka,iloscKlastrow)
{
  
  dokumenty<-usuniecieSmieci(dane = dane);
  
  tdm<-TermDocumentMatrix(dokumenty);
  m<-as.matrix(tdm);
  
  v<-sort(rowSums(m), decreasing = TRUE);
  
  d<-data.frame(words=names(v), freq=v);
  
  
  klasteryzacja.h<-agnes(d, metric=paste(metryka), method="average");
  klaster<-cutree(klasteryzacja.h, k=paste(iloscKlastrow));
  tabela.wynikowa.h<-cbind(d, klaster);
  
  return (tabela.wynikowa.h);
}


#max_value = xd[[1]]
#amount_of_records = length(xd)
