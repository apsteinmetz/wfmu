x_str <- paste('<table><tbody><tr><td>thing</td></tr>',
                      '<tr><td>random thing</td></tr>',
                     '<tr><th class="item">name</th><th class="item">size</th></tr>',
                      '<tr><td class="item">car</td><td class="item">big</td></tr>',
                     '<tr><td class="item">mouse</td><td class="item">small</td></tr>',
                     '</tbody></table>',sep='')

x<-read_html(x_str)

#don't want top rows
x %>% html_table(fill=T)

#assume first field is title and second is artist
title_artist<-wholepage %>% 
  html_nodes(xpath="//table[2]") %>% 
  html_text() %>% 
  str_extract_all("\n[\\S ]+\n-\n[\\S ]+\n") %>% 
  .[[1]] 


Title<-title_artist %>% 
  str_extract_all("\n[-A-Za-z1-9 ]+\n-") %>% 
  str_replace_all("\n(-)?","")

