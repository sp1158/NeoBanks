library(textclean)
library(stringi)
library(qdapRegex)
library(udpipe)
library(sentimentr)
library(syuzhet)
library(tidytext)
library(textdata)
library(dplyr)
library(stringr)

# ============================================================================================================================
#-----------------------------------------  STEP 1: READING DATA AND PREROCESSING --------------------------------------------
# ============================================================================================================================
monese <- read.csv("~/Downloads/19Fall/FinTech/NeoBanks/data/monese.csv", header = TRUE, stringsAsFactors = F)

#Creating an "id" variable
monese$id <- c(1:nrow(monese))

#Cleaning for html tags, emoticons, emoji, extraneous white space, punctuation, numbers
monese$content <- qdapRegex::rm_emoticon(monese$content)%>%gsub("[^\x01-\x7F]", "",.)%>%stri_trim()%>%stri_trans_tolower()%>%stri_replace_all(.,"",regex = "[0-9]")



# ============================================================================================================================
#-----------------------------------------  STEP 2: CREATING LIST OF ASPECTS -------------------------------------------------
# ============================================================================================================================

#For now, we are looking at ten aspects, namely account, ui, fees, app, customer service, fast, card, safety, other, bank_overall

account.asp <- c("account","spend","current","debit","money","credit","overdraft","withdrawal","business","pot","finance","invest","saving","limit","wallet","interest","budget","cheque")
ui.asp <- c("login","payments","person","efficient","inefficient","easy","problem","difficult","transparent","ui","user","interface","experience")
fees.asp <- c("transfer","salary","fee","payment","cash","international","foreign","currency","travel","pay","send","value","free","interest","bill")
customer_service.asp <- c("customer service","customer support","experience","complain","hassle","issue")
fast.asp <- c("quick","instant","fast","time")
card.asp <- c("card","mastercard","limit")
safety.asp <- c("biometric","recog","face","voice","safe","fraud","verification","verify")
other.asp <- c("alternative","transparent")
bank_overall.asp <- c("bank","monese")
app.asp <- c("app","function","notification","notify","issue")



# ============================================================================================================================
#-----------------------------------  STEP 3: SUBSETTING THE ORIGINAL DATA BY ASPECT ----------------------------------------
# ============================================================================================================================

#Given a dataframe (Eg: "monese" from Step 1) and a vector of aspect (Eg: "account.asp" or "ui.asp"), this function returns a subset of the original dataframe
# where the users have mentioned that particular aspect in their reviews
get_aspect_df <- function(bank,aspect_syn){
  #Filtering the dataframe for the cases only when the aspect exists
  bank <- bank[grepl(paste(aspect_syn,collapse = "|"),bank$content),]
  #From the reviews, extracting only the sentences where people have talked about the given feature/aspect
  for(i in c(1:nrow(bank))){
    print(i)
    cur_rev <- str_split(bank$content[i],pattern = "\\.")%>%unlist()
    sentence_filt <- grepl(paste(aspect_syn,collapse = "|"),cur_rev)
    cur_rev <- cur_rev[sentence_filt]%>%paste(.,collapse = ".")
    bank$content[i] <- cur_rev
  }
  return(bank)
}


#Getting data by aspect
acct <- get_aspect_df(monese,account.asp)
ui <- get_aspect_df(monese,ui.asp)
fees <- get_aspect_df(monese,fees.asp)
cust_service <- get_aspect_df(monese,customer_service.asp)
card <- get_aspect_df(monese,card.asp)
#and so on





# ============================================================================================================================
#-----------------------------------  STEP 4: SENTIMENT ANALYSIS BY ASPECT ---------------------------------------------------
# ============================================================================================================================

#Given a aspect dataframe (Eg: "acct" or "ui" from Step 3, this function returns a overall sentiment score (between -5 and 5) for each review)
get_sentiment <- function(df){
  sent <- syuzhet::get_sentiment(df$content)
  range <- max(sent)-min(sent)
  sentiment <- (((sent-min(sent))*10)/range)+(-5)
  return(sentiment)
}

#Adding the sentiment column to each aspect dataframe
acct$sentiment <- get_sentiment(acct)
ui$sentiment <- get_sentiment(ui)
fees$sentiment <- get_sentiment(fees)
cust_service$sentiment <- get_sentiment(cust_service)
card$sentiment <- get_sentiment(card)





# ============================================================================================================================
#---------------------  STEP 5: FINDING POSITIVE AND NEGATIVE ADJECTIVES TO DESCRIBE AN ASPECT---------------------------------
# ============================================================================================================================

#For this part, we'll be using a library called "udpipe". It helps us parse sentences as a human would do. 

#To begin you need to download the udpipe model for english language 
model    <- udpipe_download_model(language = "english-ewt")
if(!model$download_failed){
  ud_eng <- udpipe_load_model(model)}


#Next, creating a udpipe object (might take a couple of minutes to run).
#Also, calling the object as acct.ud to indicate that it is made out of the account dataframe and is a udpipe object
acct.ud <- udpipe::udpipe(acct$content,ud_eng, doc_id = acct$id)%>%as.data.frame()
ui.ud <- udpipe::udpipe(ui$content,ud_eng, doc_id = ui$id)%>%as.data.frame()
fees.ud <- udpipe::udpipe(fees$content,ud_eng, doc_id = fees$id)%>%as.data.frame() 
card.ud <- udpipe::udpipe(card$content,ud_eng, doc_id = card$id)%>%as.data.frame()


#Given a udpipe object, this function returns top 5 positive and negative keywords people used to describe that feature
get_keywords <- function(udpipe.obj){
  stats <- keywords_rake(x = udpipe.obj, term = "token", group = "doc_id", ngram_max = 4,relevant = udpipe.obj$upos %in% c("NOUN", "ADJ"))%>%data.frame()
  j <- stats[stats$ngram==3,]
  j$sentiment <- syuzhet::get_sentiment(j$keyword)
  j.pos <- j[j$sentiment>0,]%>%arrange(.,desc(sentiment))
  j.neg <- j[j$sentiment<0,]%>%dplyr::arrange(.,sentiment)
  
  pos <- j.pos$keyword
  neg <- j.neg$keyword
  if(length(pos)<5){pos <- c(pos,rep(NA,(5-length(pos))))}
  else{pos <- pos[1:5]}
  if(length(neg)<5){neg <- c(neg,rep(NA,(5-length(neg))))}
  else{neg<-neg[1:5]}
  
  df <- data.frame(pos_words=pos,neg_words=neg)
  return(df)
}


#Finding positive and negative keywords
acct.keywords <- get_keywords(acct.ud)
ui.keywords <- get_keywords(ui.ud)
fees.keywords <- get_keywords(fees.ud)
card.keywords <- get_keywords(card.ud)

acct.keywords
ui.keywords
fees.keywords
card.keywords

#Exporting csv files
write.csv(acct,"~/Downloads/19Fall/FinTech/NeoBanks/results/monese/acct.csv", row.names=F)


#Grouping by aspects
# #Return a dataframe of aspect
# 
# get_aspect_df <- function(bank,aspect,aspect_syn){
#   df <- bank[bank[[aspect]]==1,]
#   for(i in c(1:nrow(df))){
#     print(i)
#     browser()
#     cur_rev <- str_split(df$content[i],pattern = "\\.")%>%unlist()
#     sentence_filt <- grepl(paste(aspect_syn,collapse = "|"),cur_rev)
#     cur_rev <- cur_rev[sentence_filt]%>%paste(.,collapse = ".")
#     df$content[i] <- cur_rev
#   }
#   return(df)
# }
# 
# acct <- get_aspect_df(monese,"account",account)
# ui <- get_aspect_df(monese,"ui",ui)
# fees <- get_aspect_df(monese,"fees",fees)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# monese.ud.sub <- cbind(monese.ud.sub,aspects)
# 
# j <- monese.ud.sub[monese.ud.sub$dep_rel=="obl",]
# k <- table(j$lemma)%>%data.frame()%>%dplyr::arrange(.,desc(Freq))
# 
# 
# stats <- keywords_collocation(x = monese.ud.sub, 
#                      term = "token", group = c("doc_id","sentence_id"),
#                      ngram_max = 4)%>%data.frame()%>%dplyr::arrange(.,desc(freq))
# 
# 
# 
# #grouping similar topics
# 
# s <- get_sentences(monese.rev)
# 
# sent <- sentiment(s)
# 
# 
# 
# #!/bin/bash
# #SBATCH --job-name="sp1158"
# #SBATCH --output="sp1158.o%j"
# #SBATCH --nodes=1
# #SBATCH --ntasks=1
# #SBATCH --cpus-per-task=1
# #SBATCH --time=05:00:00
# #SBATCH --mem=3G
# 
# module load stata
# 
# stata-se -b do foo
# 
# 
# 
# 
# 
# #Plotting
# tiff("~/Desktop/nov11/monese_pos.tiff", units = "in", width=6, height=12, res=300)
# g <- ggplot2::ggplot()
# g <- g + geom_col(mapping = aes(y=rake, x=reorder(keyword,rake)),col="black",fill="gold3",alpha=0.5,data = stats)
# g <- g + coord_flip()
# g <- g + theme_minimal()
# g <- g + ggtitle("Reviews")
# g <- g + theme(axis.text.y = element_text(size=14,colour="black",face="bold"),axis.title.y=element_blank(),
#                axis.title.x =element_blank())
# print(g)
# dev.off()
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# monese <- read.csv("~/Desktop/monese.csv",header = T, stringsAsFactors = F)
# monese$id <- c(1:nrow(monese))
# monese$rating <- as.numeric(str_extract(monese$stars,"[1-9]"))
# date <- lubridate::mdy(monese$date)
# monese$day <- day(date)
# monese$month <- month(date)
# monese$year <- year(date)
# 
# 
# monese <- read.csv("~/Desktop/monese.csv",header = T, stringsAsFactors = F)
# monese$id <- c(5000:(4999+nrow(monese)))
# monese$rating <- as.numeric((str_extract(monese$stars,"[1-9]")))
# date <- lubridate::dmy(monese$date)
# monese$day <- day(date)
# monese$month <- month(date)
# monese$year <- year(date)
# 
# 
# 
# # PLOTTING REVIEWS OVER TIME
# 
# #FOR MONESE
# by_yr_mon <- dplyr::group_by(monese,year,month)
# sum_by_yr_mon <- dplyr::summarise(by_yr_mon,percent_5_star=(sum(rating==5)/n()))%>%data.frame()
# sum_by_yr_mon$date <- (paste(sum_by_yr_mon$year,sum_by_yr_mon$month,"01",sep = "-"))
# sum_by_yr_mon <- dplyr::select(sum_by_yr_mon,-year,-month)
# sum_by_yr_mon$date <- lubridate::ymd(sum_by_yr_mon$date)
# 
# mon.rating.ts <- xts::xts(sum_by_yr_mon$percent_5_star, order.by = sum_by_yr_mon$date)
# 
# 
# 
# #FOR monese
# by_yr_mon <- dplyr::group_by(monese,year,month)
# sum_by_yr_mon <- dplyr::summarise(by_yr_mon,percent_5_star=(sum(rating==5)/n()))%>%data.frame()
# sum_by_yr_mon$date <- (paste(sum_by_yr_mon$year,sum_by_yr_mon$month,"01",sep = "-"))
# sum_by_yr_mon <- dplyr::select(sum_by_yr_mon,-year,-month)
# sum_by_yr_mon$date <- lubridate::ymd(sum_by_yr_mon$date)
# 
# monese.rating.ts <- xts::xts(sum_by_yr_mon$percent_5_star, order.by = sum_by_yr_mon$date)
# 
# tiff("~/Desktop/nov11/ts.tiff", units = "in", width=12, height=6, res=300)
# plot.xts(mon.rating.ts, col = "black", ylim=c(0,1), main="Percentage of monthly 5 star ratings")
# lines(monese.rating.ts, col = "red",lwd=2)
# addLegend(legend.loc = 'top', legend.names = c("Monese", "monese"),lty = 1, col = c("black","red"))
# dev.off()
# 
# 
# 
# 
# 
# 
# ## ================ STEP 1 #WHAT ARE PEOPLE WHO ARE GIVING POSITIVE REVIEWS THINKING ABOUT ==================
# 
# monese.pos <- monese[as.numeric(monese$rating)>=4,]
# monese.posrev <- monese.pos$reviews
# monese.pos <- monese[as.numeric(monese$rating)>=4,]
# monese.posrev <- monese.pos$reviews
# 
# 
# #Cleaning for html tags, emoticons, emoji, extraneous white space, punctuation, numbers
# monese.posrev <- stri_replace_all(monese.posrev,"",regex = "<.*?>")%>% qdapRegex::rm_emoticon()%>%
#   gsub("[^\x01-\x7F]", "",.)%>%stri_trim()%>%stri_trans_tolower()%>%stri_replace_all(.,"",regex = "[:punct:]")%>%stri_replace_all(.,"",regex = "[0-9]")
# 
# monese.posrev <- stri_replace_all(monese.posrev,"",regex = "<.*?>")%>% qdapRegex::rm_emoticon()%>%
#   gsub("[^\x01-\x7F]", "",.)%>%stri_trim()%>%stri_trans_tolower()%>%stri_replace_all(.,"",regex = "[:punct:]")%>%stri_replace_all(.,"",regex = "[0-9]")
# 
# 
# #Downloading udpipe model 
# model    <- udpipe_download_model(language = "english-ewt")
#  if(!model$download_failed){
#    ud_eng <- udpipe_load_model(model)}
# 
# #Creating udpipe objects
# mon.ud <- udpipe::udpipe(monese.posrev, ud_eng, doc_id = monese.pos$id)%>%as.data.frame()
# monese.ud <- udpipe::udpipe(monese.posrev, ud_eng, doc_id = monese.pos$id)%>%as.data.frame()
# 
# 
# # IDENTIFYING KEY PHRASES (with respect to features)
# 
# # all_features <- "mobile, money, quick, service, alternative, transparent, current, transfer, salary, fee
# # cards, mastecards, debits, credits, overdrafts, account, cash, withdrawal, international, 
# # foreign, currency, business, travel, customer, support, finance, invest, saving, pot, 
# # pay, manage, instant, paypal, wallet, british, airway, executive, friend, send, value,
# # immigrants, refugee, free, limit, price, interest, payment, login, personalize, safe, 
# # inefficient, hidden, opaque, expectation"
# # all_features <- stringr::str_replace_all(all_features,",","")
# # all_feat <- udpipe(all_features,ud_eng)
# # relevant_features <- all_feat$lemma
# # 
# # filter <- is.element(mon.ud$lemma,relevant_features)
# 
# #For monese
# stats.mon <- merge(mon.ud, mon.ud, 
#                by.x = c("doc_id", "paragraph_id", "sentence_id", "head_token_id"),
#                by.y = c("doc_id", "paragraph_id", "sentence_id", "token_id"),
#                all.x = TRUE, all.y = FALSE, 
#                suffixes = c("", "_parent"), sort = FALSE)
# stats.mon <- subset(stats.mon, dep_rel %in% "nsubj" & upos %in% c("NOUN") & upos_parent %in% c("ADJ"))
# stats.mon$term <- paste(stats.mon$lemma_parent, stats.mon$lemma, sep = " ")
# stats.mon <- txt_freq(stats.mon$term)
# stats.mon <- stats.mon[stats.mon$freq>1,]
# 
# #Plotting
# tiff("~/Desktop/nov11/monese_pos.tiff", units = "in", width=6, height=12, res=300)
# g <- ggplot2::ggplot()
# g <- g + geom_col(mapping = aes(y=freq, x=reorder(key,freq)),col="black",fill="olivedrab",alpha=0.5,data = stats.mon)
# g <- g + coord_flip()
# g <- g + theme_minimal()
# g <- g + ggtitle("MONESE: Positive Reviews")
# g <- g + theme(axis.text.y = element_text(size=14,colour="black",face="bold"),axis.title.y=element_blank(),
#                axis.title.x=element_blank())
# print(g)
# dev.off()
# 
# 
# #For monese
# 
# #Using RAKE
# stats.monese <- keywords_rake(x = monese.ud, term = "lemma", group = "doc_id", 
#                        relevant = monese.ud$upos %in% c("NOUN", "ADJ"))
# stats.monese$key <- factor(stats.monese$keyword, levels = rev(stats$keyword))
# stats.monese <-  head(subset(stats.monese, freq > 3), 20)
# 
# 
# #Plotting
# tiff("~/Desktop/nov11/monese_pos.tiff", units = "in", width=6, height=12, res=300)
# g <- ggplot2::ggplot()
# g <- g + geom_col(mapping = aes(y=rake, x=reorder(keyword,rake)),col="black",fill="gold3",alpha=0.5,data = stats.monese)
# g <- g + coord_flip()
# g <- g + theme_minimal()
# g <- g + ggtitle("monese: Positive Reviews")
# g <- g + theme(axis.text.y = element_text(size=14,colour="black",face="bold"),axis.title.y=element_blank(),
#                axis.title.x =element_blank())
# print(g)
# dev.off()
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# ## ================ STEP 1 #WHAT ARE PEOPLE WHO ARE GIVING NEGATIVE REVIEWS SAYING ==================
# 
# monese.neg <- monese[monese$rating<3,]
# monese.neg <- monese[monese$reviews<3,]
# 
# 
# monese.negrev <- monese.neg$reviews
# monese.negrev <- monese.neg$reviews
# 
# 
# 
# #Cleaning for html tags, emoticons, emoji, extraneous white space, punctuation, numbers
# monese.negrev <- stri_replace_all(monese.negrev,"",regex = "<.*?>")%>% qdapRegex::rm_emoticon()%>%
#   gsub("[^\x01-\x7F]", "",.)%>%stri_trim()%>%stri_trans_tolower()%>%stri_replace_all(.,"",regex = "[:punct:]")%>%stri_replace_all(.,"",regex = "[0-9]")
# 
# monese.negrev <- stri_replace_all(monese.negrev,"",regex = "<.*?>")%>% qdapRegex::rm_emoticon()%>%
#   gsub("[^\x01-\x7F]", "",.)%>%stri_trim()%>%stri_trans_tolower()%>%stri_replace_all(.,"",regex = "[:punct:]")%>%stri_replace_all(.,"",regex = "[0-9]")
# 
# 
# #Creating udpipe objects
# mon.neg.ud <- udpipe::udpipe(monese.negrev, ud_eng, doc_id = monese.neg$id)%>%as.data.frame()
# monese.neg.ud <- udpipe::udpipe(monese.negrev, ud_eng, doc_id = monese.neg$id)%>%as.data.frame()
# 
# 
# #For monese
# stats.mon <- merge(mon.neg.ud, mon.neg.ud, 
#                    by.x = c("doc_id", "paragraph_id", "sentence_id", "head_token_id"),
#                    by.y = c("doc_id", "paragraph_id", "sentence_id", "token_id"),
#                    all.x = TRUE, all.y = FALSE, 
#                    suffixes = c("", "_parent"), sort = FALSE)
# stats.mon <- subset(stats.mon, dep_rel %in% "nsubj" & upos %in% c("NOUN") & upos_parent %in% c("ADJ"))
# stats.mon$term <- paste(stats.mon$lemma_parent, stats.mon$lemma, sep = " ")
# stats.mon <- txt_freq(stats.mon$term)
# stats.mon <- dplyr::select(stats.mon,-freq_pct)
# 
# 
# wc.mon.neg <- wordcloud2::wordcloud2(stats.mon,size=0.22,shape = "pentagon",color="random-dark",ellipticity = 1)
# htmlwidgets::saveWidget(wc.mon.neg,"tmp.html",selfcontained = F)
# webshot::webshot("tmp.html","monnese_neg_rev.pdf",delay=5,vwidth = 500,vheight = 500)
# 
# 
# 
# #For monese
# 
# #Using RAKE
# stats.monese <- keywords_rake(x = monese.neg.ud, term = "lemma", group = "doc_id", 
#                             relevant = monese.neg.ud$upos %in% c("NOUN", "ADJ"))
# stats.monese$key <- factor(stats.monese$keyword, levels = rev(stats.monese$keyword))
# stats.monese <- subset(stats.monese, freq > 2)
# 
# stats.monese <- merge(monese.neg.ud, monese.neg.ud, 
#                    by.x = c("doc_id", "paragraph_id", "sentence_id", "head_token_id"),
#                    by.y = c("doc_id", "paragraph_id", "sentence_id", "token_id"),
#                    all.x = TRUE, all.y = FALSE, 
#                    suffixes = c("", "_parent"), sort = FALSE)
# stats.monese <- subset(stats.monese, dep_rel %in% "nsubj" & upos %in% c("NOUN") & upos_parent %in% c("ADJ"))
# stats.monese$term <- paste(stats.monese$lemma_parent, stats.monese$lemma, sep = " ")
# stats.monese <- txt_freq(stats.monese$term)
# stats.monese <- dplyr::select(stats.monese,-freq_pct)
# 
# wc.monese.neg <- wordcloud2::wordcloud2(stats.monese,size=0.1,shape = "pentagon",ellipticity = 0.5)
# htmlwidgets::saveWidget(wc.monese.neg,"tmp.html",selfcontained = F)
# webshot::webshot("tmp.html","monesem_neg_rev.pdf",delay=5,vwidth = 500,vheight = 500)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# ## Collocation (words following one another)
# stats <- keywords_collocation(x = ud.mon, 
#                               term = "token", group = c("doc_id", "paragraph_id", "sentence_id"),
#                               ngram_max = 4)
# 
# ## Co-occurrences: How frequent do words occur in the same sentence, in this case only nouns or adjectives
# stats <- cooccurrence(x = subset(monese.neg.ud, upos %in% c("NOUN", "ADJ")), 
#                       term = "lemma", group = c("doc_id", "paragraph_id", "sentence_id"))
# 
# ## Co-occurrences: How frequent do words follow one another
# stats <- cooccurrence(x = monese.neg.ud$lemma, 
#                       relevant = monese.neg.ud$upos %in% c("NOUN", "ADJ"))
# 
# ## Co-occurrences: How frequent do words follow one another even if we would skip 2 words in between
# stats <- cooccurrence(x = monese.neg.ud$lemma, 
#                       relevant = monese.neg.ud$upos %in% c("NOUN", "ADJ"), skipgram = 3)
# head(stats)
# 
# 
# 
# 
# 
# 
# wordnetwork <- head(stats,70)
# wordnetwork <- graph_from_data_frame(wordnetwork)
# ggraph(wordnetwork, layout = 'graphopt') +
#   geom_edge_link2(arrow = arrow(length = unit(2, 'mm')),edge_colour = "blue") +
#   geom_node_text(aes(label = name), col = "blue", size = 4) +
#   geom_node_point(size=2)+
#   theme_graph(base_family = "Arial Narrow") +
#   theme(legend.position = "none") +
#   labs(title = "Cooccurrences within 3 words distance", subtitle = "Nouns & Adjective")
# 
# 
# 
# 
# 
# 
# 
# ## Build document term matrix on nouns/adjectives 
# monese.neg.ud$topic_level_id <- unique_identifier(monese.neg.ud, fields = c("doc_id", "sentence_id"))
# dtf <- subset(monese.neg.ud, upos %in% c("NOUN", "ADJ"))
# dtf <- document_term_frequencies(dtf, document = "topic_level_id", term = "lemma")
# dtm <- document_term_matrix(x = dtf)
# dtm_clean <- dtm_remove_lowfreq(dtm, minfreq = 5)
# ## Build topic model + get topic terminology
# n_k <- 22
# m <- LDA(dtm_clean, k = n_k, method = "Gibbs", 
#          control = list(nstart = 5, burnin = 2000, best = TRUE, seed = 1:5))
# topicterminology <- predict(m, type = "terms", min_posterior = 0.025, min_terms = 5)
# 
# scores <- predict(m, newdata = dtm, type = "topics")
# x_topics <- merge(monese.neg.ud, scores, by.x="topic_level_id", by.y="doc_id")
# 
# topicterminology <- predict(m, type = "terms", min_posterior = 0.05, min_terms = 10)
# termcorrs <- subset(x_topics, topic %in% c(1:n_k) & lemma %in% topicterminology[[1]]$term)
# termcorrs <- document_term_frequencies(termcorrs, document = "topic_level_id", term = "lemma")
# termcorrs <- document_term_matrix(termcorrs)
# termcorrs <- dtm_cor(termcorrs)
# termcorrs[lower.tri(termcorrs)] <- NA
# diag(termcorrs) <- NA
# library(qgraph)
# tiff("~/Desktop/nov11/network.tiff", units = "in", width=12, height=8, res=300)
# qgraph(termcorrs, layout = "spring", labels = colnames(termcorrs), directed = FALSE,
#        borders = FALSE, label.scale = FALSE, label.cex = 1, node.width = 0.5)
# dev.off()
# 
# 
# 
# monese.neg.ud$topic_level_id <- unique_identifier(monese.neg.ud, fields = c("doc_id", "sentence_id"))
# 
# 
# 
# #dtf <- subset(monese.neg.ud, upos %in% c("NOUN", "ADJ"))
# dtf <- document_term_frequencies(stats.monese, term = "keyword")
# dtm <- document_term_matrix(x = dtf)
# dtm_clean <- dtm_remove_lowfreq(dtm, minfreq = 5)
# 
# ## Build topic model + get topic terminology
# num_k <- 8
# m <- LDA(dtm_clean, k = num_k, method = "Gibbs", 
#          control = list(nstart = 5, burnin = 2000, best = TRUE, seed = 1:5))
# topicterminology <- predict(m, type = "terms", min_posterior = 0.025, min_terms = 5)
# 
# 
# #x_topics <- merge(stats.monese, scores, by.x="topic_level_id", by.y="doc_id")
# scores <- predict(m, dtm, type = "topics")
# stats.monese <- dplyr::rename(stats.monese,topic=topic_level_id)
# x_topics <- merge(stats.monese, scores, by.x="topic", by.y="doc_id")
# 
# termcorrs <- subset(stats.monese, keyword %in% topicterminology[[1]]$term)
# termcorrs <- document_term_frequencies(termcorrs, term = "keyword")
# termcorrs <- document_term_matrix(termcorrs)
# termcorrs <- dtm_cor(termcorrs)
# termcorrs[lower.tri(termcorrs)] <- NA
# diag(termcorrs) <- NA
# library(qgraph)
# qgraph(termcorrs, layout = "spring", labels = colnames(termcorrs), directed = FALSE,
#        borders = FALSE, label.scale = FALSE, label.cex = 1, node.width = 0.5)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# all_features <- "mobile, money, quick, service, alternative,transparent,current, transfer, salary, fee
# cards, mastecards, debits, credits, overdrafts, account, cash, withdrawal, international, 
# foreign, currency, business, travel, customer, support, finance, invest, saving, pot, 
# pay, manage, instant, paypal, wallet, british, airway, executive, friend, send, value,
# immigrants, refugee, free, limit, price, interest, payment, login, personalize, safe, 
# inefficient, hidden, opaque, expectation"
# all_features <- stringr::str_replace_all(all_features,",","")
# all_feat <- udpipe(all_features,ud_eng)
# relevant_features <- all_feat$lemma
# 
# filter <- is.element(ud.mon$lemma,relevant_features)
# 
# ud.mon <- ud.mon[filter,]
# 
# 
# 
# 
# 
# ## Collocation (words following one another)
# stats <- keywords_collocation(x = ud.mon, 
#                               term = "token", group = c("doc_id", "paragraph_id", "sentence_id"),
#                               ngram_max = 4)
# 
# ## Co-occurrences: How frequent do words occur in the same sentence, in this case only nouns or adjectives
# stats <- cooccurrence(x = subset(ud.mon, upos %in% c("NOUN", "ADJ")), 
#                       term = "lemma", group = c("doc_id", "paragraph_id", "sentence_id"))
# 
# ## Co-occurrences: How frequent do words follow one another
# stats <- cooccurrence(x = ud.mon$lemma, 
#                       relevant = ud.mon$upos %in% c("NOUN", "ADJ"))
# 
# ## Co-occurrences: How frequent do words follow one another even if we would skip 2 words in between
# stats <- cooccurrence(x = ud.mon$lemma, 
#                       relevant = ud.mon$upos %in% c("NOUN", "ADJ"), skipgram = 3)
# head(stats)
# 
# 
# 
# 
# 
# library(igraph)
# library(ggraph)
# library(ggplot2)
# wordnetwork <- head(stats, 30)
# wordnetwork <- graph_from_data_frame(wordnetwork)
# ggraph(wordnetwork, layout = "fr") +
#   geom_edge_link(aes(width = cooc, edge_alpha = cooc), edge_colour = "pink") +
#   geom_node_text(aes(label = name), col = "darkgreen", size = 4) +
#   theme_graph(base_family = "Arial Narrow") +
#   theme(legend.position = "none") +
#   labs(title = "Cooccurrences within 3 words distance", subtitle = "Nouns & Adjective")
# 
# 
# 
# 
# stats <- textrank_keywords(ud.mon$lemma, 
#                            relevant = ud.mon$upos %in% c("NOUN", "ADJ"), 
#                            ngram_max = 4, sep = " ")
# stats <- subset(stats$keywords, ngram > 1 & freq >= 4)
# wordcloud(words = stats$keyword, freq = stats$freq)
# 
# 
# 
# 
# 
# ## Using a sequence of POS tags (noun phrases / verb phrases)
# ud.mon$phrase_tag <- as_phrasemachine(ud.mon$upos, type = "upos")
# stats <- keywords_phrases(x = ud.mon$phrase_tag, term = tolower(ud.mon$token), 
#                           pattern = "(A|N)*N(P+D*(A|N)*N)*", 
#                           is_regex = TRUE, detailed = FALSE)
# stats <- subset(stats, ngram > 1 & freq > 3)
# stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
# barchart(key ~ freq, data = head(stats, 20), col = "green", 
#          main = "Keywords - simple noun phrases", xlab = "Frequency")
# 
# 
# 
# 
# 
# 
# 
# ## Using RAKE
# stats <- keywords_rake(x = monese.ud, term = "lemma", group = "doc_id", 
#                        relevant = monese.ud$upos %in% c("NOUN", "ADJ"))
# stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
# barchart(key ~ rake, data = head(subset(stats, freq > 3), 20), col = "red", 
#          main = "Keywords identified by RAKE", 
#          xlab = "Rake")
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# #Topic modelling using noun
# df.mon <- subset(ud.mon,filter)
# df.mon <- document_term_frequencies(df.mon,term="lemma")
# 
# # Document term matrix for building a topic model
# dtm.mon <- document_term_matrix(x = df.mon)
# ## Remove words which do not occur that much
# 
# 
# 
# #Topic modeling
# m <- LDA(dtm.mon, k = 4, method = "Gibbs", 
#          control = list(nstart = 5, burnin = 2000, best = TRUE, seed = 1:5))
# 
# scores <- predict(m, newdata = dtm.mon, type = "topics", 
#                   labels = c("labela", "labelb", "labelc", "xyz"))
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# #Tokenizing, stemming and crating document term matrix
# dtm.mon <- quanteda::dfm(rev.mon,tolower=TRUE,stem=TRUE,remove=stopwords("english"))
# dtm.monese <- quanteda::dfm(rev.monese,tolower=TRUE,stem=TRUE,remove=stopwords("english"))
# 
# #Creating frequencies of words
# freq.mon <- docfreq(dtm.mon)
# freq.monese <- docfreq(dtm.monese)
# 
# #Only keeping words with freq > 1
# dtm.mon <- dtm.mon[,freq.mon>1]
# dtm.monese <- dtm.monese[,freq.monese>1]
# 
# #Converting the frequencies to propertions
# dtm.mon <- dfm_tfidf(dtm.mon,scheme_tf="prop")
# dtm.monese <- dfm_tfidf(dtm.monese,scheme_tf="prop")
# 
# 
# 
# 
# 
# "mobile, money, quick, service, alternative, bank, current, transfer, salary, fee
# card, mastecard, debit, credit, overdraft, account, cash, withdrawal, international, 
# foreign, currency, business, travel, customer, support, finance, invest, saving, pot, 
# pay, manage, instant, paypal, wallet, british, airway, executive, friend, send, value,
# immigrants, refugee, free, limit, price, interest, payment, login, personalize, safe, 
# inefficient, hidden, opaque, expectation"
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# # toks.mon <- quanteda::tokens(rev.mon)%>%quanteda::tokens_tolower()%>%quanteda::tokens_wordstem()
# # toks.monese <- quanteda::tokens(rev.monese)%>%quanteda::tokens_tolower()%>%quanteda::tokens_wordstem()
# # #Dictionary of stop words
# # stop_words <- quanteda::stopwords("english")
# # #Removing stop words
# # toks.mon <- quanteda::tokens_remove(toks.mon,stop_words)
# # toks.monese <- quanteda::tokens_remove(toks.monese,stop_words)
# # 
# 
# 
# 
# 
# 
# 
# 
# 
# myDict <0- dictionary(list(feature = c("mobile*","phone*","")))
# 
