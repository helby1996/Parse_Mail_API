library(gmailr)
library(data.table)
library(httr)
library(DBI)
library(RMySQL)
library(stringi)
library(lubridate)
library(anytime)
library(vroom)
library(tidyverse)
library(parallel)
library(RCurl)

path <- '/Users/admin/Downloads/client_secret_155938939393_dechm9o534tvupcu3nt34ivfcr2greqq_apps.json'

find_messages <- function(path){
  gm_auth_configure(path = path)
  gm_auth()   
  gm_labels()
  my_m <- gm_messages(search="")
  gm_modify_message(gm_id(my_m), add_labels = 'Label_1', remove_labels = 'INBOX')
  data<- gm_message(new$id)$payload$parts[[2]]$filename
  fwrite(data.table(id=c(new)), append = TRUE, file = "/Users/admin/Downloads/PAZ-cache2.csv")
  return(data)
}

data_processing <- function(data){
  data <- gm_message(new$id)$payload$parts[[2]]$filename %>%
  vroom(col_names = c("deviceId", "meterID", "date", "time", "ws", "energy"), col_types = "character" )%>%
  .[, dateTime := ymd_hms(paste(date, time) )] %>%
  .[, dateTime := format(dateTime, "%Y/%m/%d %H:%M")]  %>%
  .[, energy := as.numeric(stri_replace_all_fixed(energy, pattern = "(.+)-", replacement = "-\1"))]%>%
  .[, c('dateTime', 'deviceId', 'energy')]
return(data)
}

sql_connect <- function(sql_query){
  connection <- dbConnect(MySQL(), port= ,database = '' , user = '', password = '', host='')
  data <- dbGetQuery(connection, sql_query)
  dbDisconnect()
  return(data)
} 

email_sender <- function(sender_email_id,sender_password,email_message,recipients,host,subject){
  body = email_message
  msg <- gm_mime()%>%
    gm_subject(paste0(host, " - ", subject))%>%
    gm_from(sender_email_id)%>%
    gm_to(paste0(", ", recipients, ","))%>%
    gm_attach_file(gm_text_body(body, 'plain'))%>%
    gm_auth_configure(path = '/Users/admin/Downloads/client_secret_155938939393_dechm9o534tvupcu3nt34ivfcr2greqq_apps.json')%>%
    gm_auth()%>%
    1%>%
    gm_send_message() 
}

runjob <- function(){
  resp <- POST(url = '' ,
               add_headers(
                 'Content-Type'= 'application/json'
               ), body='{ "login": "", "password": "" }')


  token<-content(resp)
  toke<-token$id_token
  resp2 <- GET('',
              add_headers('Authorization'=paste0('Bearer ',toke), 'accept'= '*/*') )%>%
  status_code()
}

