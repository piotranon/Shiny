library(tidyRSS);
library(solrium);
library(gsubfn); # do wyciagniecia daty z zawartosci

# to do poszukac facet solrium
# query from datetime to dateTime

port<-"8984";


aktualizacjaDanychSolr<-function()
{
  # klient solr
  connection <-SolrClient$new(host="127.0.0.1", port = port, path = "/solr/projekt/select");
  
  # pobranie danych z kanalu
  news <- tidyfeed(feed="http://www.rss.gofin.pl/prawnikradzi.xml")
  
  connection <-SolrClient$new(host="127.0.0.1", port = port, path = "/solr/projekt/select");
  
  for(i in 1:nrow(news))
  {
    res="xd";
    tryCatch({
      res<-solrium::solr_search(conn = connection,params=list(q=paste("title:\"",news$entry_title[i],"\" AND content:\"",news$entry_content[i],"\"")))
    },error=function(cond){
      message(cond);
      #create all
      
      dataToSave = data.frame(matrix(ncol=3,nrow=10))
      colnames(dataToSave)[1] <- "title";
      colnames(dataToSave)[2] <- "content";
      colnames(dataToSave)[3] <- "date";
      
      dataToSave$title<-c(news$entry_title);
      dataToSave$content<-c(news$entry_content);
      date<-strapplyc(news$entry_content, "\\d+.\\d+.\\d+ r.", simplify = TRUE)
      dataToSave$date<-c(as.Date(substr(date,1,nchar(date)-3),format = "%d.%m.%Y"));
      
      solrium::add(x=dataToSave, conn=connection, name="projekt", commit=TRUE);
    }
    );
    
    # jezeli nie istnieje dodaj
    if(nrow(res)==0)
    {
      # utworzenie dataframu takiego jak w bazie
      dataToSave = data.frame(matrix(ncol=3))
      colnames(dataToSave)[1] <- "title";
      colnames(dataToSave)[2] <- "content";
      colnames(dataToSave)[3] <- "date";
      
      dataToSave$title<-c(news$entry_title[i]);
      dataToSave$content<-c(news$entry_content[i]);
      date<-strapplyc(news$entry_content[i], "\\d+.\\d+.\\d+ r.", simplify = TRUE)
      dataToSave$date<-c(as.Date(substr(date,1,nchar(date)-3),format = "%d.%m.%Y"));
      
      # dodanie do bazy
      solrium::add(x=dataToSave, conn=connection, name="projekt", commit=TRUE);
      
      
    }
  }
}

pobranieTytulowSolr<-function()
{
  # klient solr
  connection <-SolrClient$new(host="127.0.0.1", port = port, path = "/solr/projekt/select");
  
  titles<-solr_search(conn = connection, params = list(q="*:*",fl=paste("title"), rows=-1));
  titles<-unique(titles);
  return(titles);
}

pobranieTytulowSolrOdDo<-function(od,do)
{
  # klient solr
  connection <-SolrClient$new(host="127.0.0.1", port = port, path = "/solr/projekt/select");
  
  titles<-solr_search(conn = connection,params = list(q=paste("date:[",od,"T00:00:00Z TO ",do,"T00:00:00Z]",sep=""),fl=paste("title"), rows=-1))
  titles<-unique(titles);
  return(titles);
}

pobranieZawartosciSolr<-function()
{
  # klient solr
  connection <-SolrClient$new(host="127.0.0.1", port = port, path = "/solr/projekt/select");
  
  content<-solr_search(conn = connection, params = list(q="*:*",fl=paste("content"), rows=-1));
  content<-unique(content);
  return(content);
}

pobranieZawartosciSolrOdDo<-function(od,do)
{
  # klient solr
  connection <-SolrClient$new(host="127.0.0.1", port = port, path = "/solr/projekt/select");
  
  content<-solr_search(conn = connection, params = list(q=paste("date:[",od,"T00:00:00Z TO ",do,"T00:00:00Z]",sep=""),fl=paste("content"), rows=-1));
  content<-unique(content);
  return(content);
}

getAllData<-function()
{
  connection <-SolrClient$new(host="127.0.0.1", port = port, path = "/solr/projekt/select");
  return(solr_search(conn = connection, params = list(q="*:*", rows=-1)));
}

getMaxDate<-function()
{
  connection <-SolrClient$new(host="127.0.0.1", port = port, path = "/solr/projekt/select");
  return(solr_search(conn = connection, params = list(q="*:*",fl=paste("date"),sort="date desc", rows=1))[1]);
}

getMinDate<-function()
{
  connection <-SolrClient$new(host="127.0.0.1", port = port, path = "/solr/projekt/select");
  return(solr_search(conn = connection, params = list(q="*:*",fl=paste("date"),sort="date asc", rows=1))[1]);
}

# date range
# solr_search(conn = connection,params = list(q="date:[2015-09-01T00:00:00Z TO 2021-09-01T23:59:59Z]",fl=paste("content"), rows=-1))
