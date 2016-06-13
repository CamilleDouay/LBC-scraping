# Clean script for first try 
# Key value system 

library(plyr)
library(rvest)


get_desc <- function(url_df){
  result = data.frame()
  
  
  'span.property' -> css_key
  'span.value' -> css_value
  
  
  for (i in url_df[,]){
    url = paste('https:', as.character(i), sep = "")
    
    exc <- try(read_html(url))
    
    if (class(exc) != "try-error"){
      url %>%
        read_html() %>%
        html_nodes(css_key)%>%
        html_text()%>%
        repair_encoding('utf-8')%>%
        data.frame()-> key_page
    
      key_strip = apply(key_page, 2, function(x) trimws(x))
      
      url %>%
        read_html() %>%
        html_nodes(css_value)%>%
        html_text()%>%
        repair_encoding('utf-8')%>%
        data.frame()-> value_page
      
      value_strip = apply(value_page, 2, function(x) trimws(x))
      
      value_strip
    
      df = data.frame(t(value_strip))
      colnames(df) = t(key_strip)
      result = rbind.fill(result, df)
      }else{
      print('Adress Error')
    }
    #print(url)
    #print(url_split)
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
    
    if (class(res) != "try-error"){
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


rawdf = extract_LBC(1000)
head(rawdf)
dim(rawdf)
df = parse_df(rawdf)

summary(df)
head(df)

write.csv(df, 'LBC_cars_200.csv')
