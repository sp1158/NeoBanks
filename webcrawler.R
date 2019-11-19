library(rvest)
library(TrustpilotR)



monzo <- TrustpilotR::trustpilot("www.monzo.com")
starling <- TrustpilotR::trustpilot("starlingbank.com")
atom <- TrustpilotR::trustpilot("atombank.co.uk")
monese <- TrustpilotR::trustpilot("www.monese.com")
revolut <- TrustpilotR::trustpilot("www.revolut.com")
n26 <- TrustpilotR::trustpilot("n26.com")

write.csv(monzo,"/Users/showrooppokhrel/Documents/GitHub/NeoBanks/data/monzo.csv")
write.csv(starling,"/Users/showrooppokhrel/Documents/GitHub/NeoBanks/data/starling.csv")
write.csv(atom,"/Users/showrooppokhrel/Documents/GitHub/NeoBanks/data/atom.csv")
write.csv(monese,"/Users/showrooppokhrel/Documents/GitHub/NeoBanks/data/monese.csv")
write.csv(revolut,"/Users/showrooppokhrel/Documents/GitHub/NeoBanks/data/revolut.csv")
write.csv(n26,"/Users/showrooppokhrel/Documents/GitHub/NeoBanks/data/n26.csv")
























# 
# 
# revolut <- "https://play.google.com/store/apps/details?id=com.revolut.revolut&hl=en_US"
# monzo <- "https://play.google.com/store/apps/details?id=co.uk.getmondo&hl=en_US"
# monese <- "https://play.google.com/store/apps/details?id=com.monese.monese.live&hl=en_US"
# atom <- "https://play.google.com/store/apps/details?id=com.atombank.release&hl=en_GB"
# starling <- "https://play.google.com/store/apps/details?id=com.starlingbank.android&hl=en_US"
# 
# 
# #Dependencies: Need to run java -jar selenium-server-standalone-3.141.59.jar on terminal first
# 
# 
# 
# 
# 
# 
# 
# # starting local RSelenium (this is the only way to start RSelenium that is working for me atm)
# # selCommand <- wdman::selenium(jvmargs = c("-Dwebdriver.chrome.verboseLogging=true"), retcommand = TRUE)
# # system(selCommand, wait = FALSE)
#   
#   
#   
# # go to website
# remDr <-remoteDriver(browser="firefox")
# remDr$open()
# remDr$navigate(monese)
# # get page source and save it as an html object with rvest
# html_obj <- remDr$getPageSource(header = TRUE)[[1]] %>% read_html()
# # 1) name field (assuming that with 'name' you refer to the name of the reviewer)
# names <- html_obj %>% html_nodes(".kx8XBd .X43Kjb") %>% html_text()
# # 2) How much star they got 
# stars <- html_obj %>% html_nodes(".nt2C1d .pf5lIe [role='img']") %>% html_attr("aria-label")
# # 3) review they wrote
# reviews <- html_obj %>% html_nodes(".UD7Dzf") %>% html_text()
# # 4) date
# date <- html_obj%>%html_nodes(".kx8XBd .p2TkOb")%>%html_text()
# # create the df with all the info
# monese <- data.frame(date=date,names = names, stars = stars, reviews = reviews, stringsAsFactors = F)
#   
# #write.csv(monese,"~/Desktop/monese.csv",row.names = F)
# 
# 
# 
# 
# # go to website
# remDr <-rsDriver(browser="firefox",geckover="0.26.0",extraCapabilities = list(marionette = TRUE))
# remDr$open()
# remDr$navigate(atom)
# # get page source and save it as an html object with rvest
# html_obj <- remDr$getPageSource(header = TRUE)[[1]] %>% read_html()
# # 1) name field (assuming that with 'name' you refer to the name of the reviewer)
# names <- html_obj %>% html_nodes(".kx8XBd .X43Kjb") %>% html_text()
# # 2) How much star they got 
# stars <- html_obj %>% html_nodes(".nt2C1d .pf5lIe [role='img']") %>% html_attr("aria-label")
# # 3) review they wrote
# reviews <- html_obj %>% html_nodes(".UD7Dzf") %>% html_text()
# # 4) date
# date <- html_obj%>%html_nodes(".kx8XBd .p2TkOb")%>%html_text()
# # create the df with all the info
# atom <- data.frame(date=date,names = names, stars = stars, reviews = reviews, stringsAsFactors = F)
# 
# write.csv(starling,"~/Desktop/starling.csv",row.names = F)
# 
# 
# 
# 
# 
# rD <- RSelenium::rsDriver(browser = "chrome",port=5556L,
#                           chromever =
#                             system2(command ="/Applications/Google\ Chrome.app/Contents/MacOS/Google\ Chrome",
#                                     args = "--version",
#                                     stdout = TRUE,
#                                     stderr = TRUE) %>%
#                             stringr::str_extract(pattern = "(?<=Chrome )\\d+\\.\\d+\\.\\d+\\.") %>%
#                             magrittr::extract(!is.na(.)) %>%
#                             stringr::str_replace_all(pattern = "\\.",
#                                                      replacement = "\\\\.") %>%
#                             paste0("^",  .) %>%
#                             stringr::str_subset(string =
#                                                   binman::list_versions(appname = "chromedriver") %>%
#                                                   dplyr::last()) %>%
#                             as.numeric_version() %>%
#                             max() %>%
#                             as.character())
# 
# 
# rD$open()
# rD$navigate(starling)
# 
# 
# 
# 
# 
# 
# 
# 
# selServ <- RSelenium::startServer(javaargs = c("-Dwebdriver.gecko.driver=\"~/Desktop/geckodriver.exe\""))
# remDr <- remoteDriver(browser="firefox",extraCapabilities = list(marionette = TRUE), port=5556)
# remDr$open()
# ....
# ....
# remDr$close()
# selServ$stop()
# 
# 
# 
