# Clean script for first try 
# Key value system 

library(plyr)
library(rvest)


Unaccent <- function(text) {
  text <- gsub("['`^~\"]", " ", text)
  text <- iconv(text, to="ASCII//TRANSLIT//IGNORE")
  text <- gsub("['`^~\"]", "", text)
  return(text)
}

get_desc <- function(url_df){
  result = data.frame()
  
  xpath_value = '//*[contains(concat( " ", @class, " " ), concat( " ", "value", " " ))]'
  xpath_property = '//*[contains(concat( " ", @class, " " ), concat( " ", "property", " " ))]'
  xpath_title = '//*[contains(concat( " ", @class, " " ), concat( " ", "no-border", " " ))]'
  xpath_photos =  '//*[contains(concat( " ", @class, " " ), concat( " ", "item_photo", " " ))]'
  xpath_MEL = '//*[contains(concat( " ", @class, " " ), concat( " ", "line_pro", " " ))]'
  
  
  for (i in url_df[,]){
    url = paste('https:', as.character(i), sep = "")
    
    exc <- try(read_html(url))
    
    if (class(exc[1]) != "try-error"){
      url %>%
        read_html() -> url_page
      url_page%>%
        html_nodes(xpath = xpath_property)%>%
        html_text()%>%
        repair_encoding('UTF-8')%>%
        (function(x)gsub("[^[:alnum:][:space:]']", "", x)) %>%
        Unaccent()%>%
        trimws()-> property


      url_page%>%
        html_nodes(xpath = xpath_value)%>%
        html_text()%>%
        repair_encoding('utf-8')%>%
        trimws()%>% 
        t()%>% 
        data.frame()-> value
      
      
      url_page%>%
        html_nodes(xpath = xpath_title)%>%
        html_text()%>%
        repair_encoding('utf-8')%>%
        trimws()-> titre
      
      
      url_page%>%
        html_nodes(xpath = xpath_photos)%>%
        html_text()%>%
        repair_encoding('utf-8')%>%
        trimws()-> photos
      
      url_page%>%
        html_nodes(xpath = xpath_MEL)%>%
        html_text()%>%
        repair_encoding('utf-8')%>%
        trimws()-> MEL

      
      colnames(value) = property
      colnames(value)[is.na(colnames(value))] = 'NA'
      value$Titre = titre
#      value$Photos = photos
      value$MEL = MEL[1]
      value$User = MEL[2]

      result = rbind.fill(result, value)
      }else{
      print('Adress Error')
    }

  }
  return(result)
}


extract_LBC = function(nb_page){
  
  cat = 'voitures'
  lbc_url = 'https://www.leboncoin.fr/'
  search_url= 'offres/haute_normandie/occasions/'
  
  lbc_extract = data.frame()
  
  for (i in seq(nb_page)){
    
    print(i)
    url_lbc = paste(lbc_url, cat,'/',search_url, '?o=',i, sep = "")
    

    css_page = 'a.list_item.clearfix.trackable'
    
    res <- try(read_html(url_lbc))
    
    if (class(res[1]) != "try-error"){
      url_lbc %>%
        read_html() %>%
        html_nodes(css_page)%>%
        html_attr('href')%>%
        data.frame()-> url_df
      
      url_df
      result = get_desc(url_df)
      lbc_extract = rbind.fill(lbc_extract, result)
    }else{
      print('Page Error')
    }
    
  }
  return(lbc_extract)
}

parse_df <- function(df){

  colnames(df)[5] <- 'Annee'
  colnames(df)[8] <- 'Transmission'
  
  ### Retrait des lignes avec prix manquants
  df = df[!is.na(df$Prix),]
  
  df$Prix = lapply(df$Prix, function(x) gsub(' |€', '',x))
  df$Prix = as.numeric(df$Prix)
  
  ### Retrait des prix trop faibles (<100 €)
  df = df[df$Prix>100,]
  
  df$Kilométrage = lapply(df$Kilométrage, function(x) gsub(' |KM', '',x))
  df$Kilométrage = as.numeric(df$Kilométrage)
  
  df$Annee = as.numeric(as.character(df$Annee))
  return(df)
}

