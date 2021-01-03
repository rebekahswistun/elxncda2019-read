library(XML)
library(rvest)
library(rlist)
library(data.table)
library(RCurl)

####Enter elxn number####

elxn_num <- 

####Generate elections canada url####

cr_elxn_lnk <- function(elxn_num){
	ifelse(elxn_num<=41, 
	       paste0("https://www.elections.ca/content.aspx?section=res&dir=rep/off/",elxn_num,"gedata&document=byed&lang=e"),
	       paste0("https://www.elections.ca/content.aspx?section=res&dir=rep/off/",elxn_num,"gedata&document=reg&lang=e")
	        )               
                          }

####Generate url for elxn ####
elxn_url <- cr_elxn_lnk(elxn_num)


###Generate urls for range of elxns#### 
elxn_url <- lapply(elxn_num, cr_elxn_lnk)


####Reads riding tables into R to get each unique riding code####
get_riding_code <-     function(elxn_url){
	                    webpage <- xml2::read_html(elxn_url)
	                     riding <- webpage %>%
	                               rvest::html_nodes("table")%>%
	                               rvest::html_table()%>%
	                               data.table::rbindlist()
	                                
	                    subset(riding, select = c("Code", "Federal Electoral Districts"))
	                                  }
####Elxn 38 to 43
riding_codes <- get_riding_code(elxn_url)

                         
####Uses Elxn Cda riding codes to create unique url for each riding ####
create_url_elxncda <- function(ridingcode){
	           paste0("https://www.elections.ca/res/rep/off/ovr2019app/51/data_donnees/pollresults_resultatsbureau",ridingcode ,".csv")
	           }

created_url <- lapply(riding_codes$Code, create_url_elxncda)

####Gets data for each unique URL without downloading CSV file####
download_riding_data <- function(unique_url) {
                                             RCurl::getURL(unique_url)
                                             }

downloaded_riding_dt <- lapply(created_url, download_riding_data)


####Reads the complete CSV file (300+ ridings) into R####
riding_data_csv <- function(file){
	read.csv(text = file, stringsAsFactors = FALSE)
}

data <- lapply(downloaded_riding_dt, riding_data_csv)%>%
        data.table::rbindlist()%>%
        setnames("Polling.Station.Number.NumÃ.ro.du.bureau.de.scrutin", "PollStn")


####Clean up poll_stn column####
##Create new column for polling stations containing A/B/C or SR/1 or 2##
data$PollStn_split <- ifelse(grepl("A",data$PollStn),"A",ifelse(grepl("B",data$PollStn),"B",ifelse(grepl("C",data$PollStn),"C",ifelse(grepl("S/R 1",data$PollStn),"S/R 1",ifelse(grepl("S/R 2",data$PollStn),"S/R 2", NA)))))

##remove A/B/C and SR/1 or 2 from PollStn col and change to numeric##
data$PollStn <- gsub("[a-zA-Z ]", "", data$PollStn)%>%
                as.numeric()

####filter dataset on advanced poll numbers####

x <- as.numeric(600)
advanced_poll_data <- subset(data, PollStn >= x)

##Inspect advance poll data set##
head(advanced_poll_data, n=50)
tail(advanced_poll_data, n=50)
str(advanced_poll_data)
dim(advanced_poll_data)
                      
