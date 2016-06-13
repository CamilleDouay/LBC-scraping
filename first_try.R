library(rvest)

lbc <-  read_html("https://www.leboncoin.fr/voitures/offres/haute_normandie/?o=1&brd=Audi")
lbc %>%
  html_node('a.list_item.clearfix.trackable')%>%
  html_attr('href') %>%
  print()


lbc_href <- lbc %>%   html_node('a.list_item.clearfix.trackable')

lapply(link, FUN = function(lbc){
  return( html_node('a.list_item.clearfix.trackable'))
})

"https://www.leboncoin.fr/voitures/offres/haute_normandie/?o=1&brd=Audi" -> url
'a.list_item.clearfix.trackable' -> css_page
url %>%
  read_html(encoding = "UTF-8") %>%
  html_nodes(css_page)%>%
  html_attr('href')%>%
  data.frame()-> url_df

url_df

paste('https:', as.character(url_df[1,1]), sep = "") -> url 
'.value' -> css_page
url %>%
  read_html() %>%
  html_nodes(css_page)%>%
  html_text()%>%
  repair_encoding()%>%
  data.frame()-> url_page

url_page
summary(url_page)

url_split = apply(url_page, 2, function(x) trimws(x))
url_split

str = as.character(url_page[2,1])
str
trimws(str)

grep('5', str,perl = TRUE, value = T)

'.value' -> css_page
url %>%
  read_html %>%
  html_nodes(css_page)%>%
  html_text()%>%
  data.frame()-> url_desc

url_desc

