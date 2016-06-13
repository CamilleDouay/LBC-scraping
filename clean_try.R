# Clean script for first try 

cat = 'voitures/'
brand = 'Audi'
lbc_url = 'https://www.leboncoin.fr/'
search_url= 'offres/haute_normandie/'

result = data.frame()


### Extract url from search page 
url = paste(lbc_url, cat,search_url, '?o=1', sep = "")

css_page = 'a.list_item.clearfix.trackable'

url %>%
  read_html() %>%
  html_nodes(css_page)%>%
  html_attr('href')%>%
  data.frame()-> url_df

url_df

'span.value' -> css_page

for (i in url_df[1:38,]){
  url = paste('https:', as.character(i), sep = "")
  print(url)
  
  url %>%
    read_html() %>%
    html_nodes(css_page)%>%
    html_text()%>%
    repair_encoding()%>%
    data.frame()-> url_page
  
  url_split = apply(url_page, 2, function(x) trimws(x))
  #print(url)
  #print(url_split)
  result = rbind(result, t(url_split))
}

result
t(url_split)
