#uploading packages
library(rvest)
library(tidyverse)
library(dplyr)
library(pdftools)
library(purrr)
library(readtext)
library(quanteda)
library(stringr)
library(knitr) 
library(kableExtra) 
library(tm)
library(topicmodels)
library(reshape2)
library(ggplot2)
library(wordcloud)
library(pals)
library(SnowballC)
library(lda)
library(ldatuning)
library(flextable)
library(seededlda)
library(stm)
library(ggplot2)
library(stringi)

#getting files 
COA_files <- list.files(pattern = ".pdf", getwd(), recursive = 'TRUE')

#coding function for getting text

get_text <- function(year){
  
  #subsetting COA
  COA_store <- COA_files[grepl(as.character(year), COA_files)]
  #name_store <- str_match(COA_store, "/(\\w+|\\w+-\\w+)_\\D+\\d+.pdf$")[,2]
  name_store <- str_match(COA_store, "/([^/]+\\w+|\\w+-\\w+).pdf$")[,2]
  #storing texts in vector
  out <- list()
  empty <- c()
  
  for (i in 1:length(COA_store)){
    out[[i]] <-  pdf_text(COA_store[i])}
  out <- lapply(out, function(z){z[z != ""]})
  empty <- which(sapply(out,length)==0)
  out <- out[lapply(out, length) > 0]
  names(out) <- name_store[-empty]
  #names(out) <- str_match(names(out), "/[a-zA-Z]*_.*pdf")
  return(out)
}

#running function for 2010 to 2020
COA_2010 <- get_text(2010)
COA_2011 <- get_text(2011)
COA_2012 <- get_text(2012)
COA_2013 <- get_text(2013)
COA_2014 <- get_text(2014)
COA_2015 <- get_text(2015)
COA_2016 <- get_text(2016)
COA_2017 <- get_text(2017)
COA_2018 <- get_text(2018)
COA_2019 <- get_text(2019)
COA_2020 <- get_text(2020)

#removing image-based ES's (for further processing with OCR in future versions)
COA_2010 <- COA_2010[-c(946, 961,1268, 1327, 1465, 1510, 1514)] #Limawasa, Leyte; Salvador, Lanao del Norte; Gigaquit, Surigao del Norte; Placer, Surigao del Norte

COA_2011 <- COA_2011[-c(724)] #Corcuera, Romblon

#removed in 2012: Sipocot, Camsur; Barontac Nuevo, Iloilo 



#removed in 2013: GMA, Cavite 

COA_2013 <- COA_2013[-c(623, 626, 837, 843)] #Liliw, Laguna; Lumban, Laguna; Daraga, Albay; Manito, Albay


COA_2014 <- COA_2014[-c(8, 840,841)] #Maluso, Basilan; Pasacao, Albay; Presentacion, Albay



#removed in 2016: Pangutaran, Sulu; Languyan, Tawi-Tawi; Panglima Sugala, Tawi-Tawi; Aguilaldo, Ifugao; Makati duplicate


#removed in 2017: Urdaneta City, Pangasinan; Santa Barbara, Pangasinan; Bangui, IN; Pagudpud, IN; Bantay, IS; Sinait, IS; Sugpon, IS; Luna, LU; Angeles City, Pampanga; Bustos, Bulacan; Obando, Bulacan; Mantayoc, Tarlac; Iba, Zambales; Talisay, CamN; Tigaon, CamS; San Enrique, Iloilo; Valencia, Bukidnon; Ozamiz, Bukidnon; El Salvador, Misamis Oriental; Damulog, Bukidnon; Panaon, Misamis Occidental; Maitum, Saranggani; Norala, South Cotabato; La Libertad, Negros Oriental       

COA_2017 <- COA_2017[-c(1249, 1271,1274)] #Cotabato City, Cotabato; Glan, Sarangani; Malapatan, Sarangani

#removed in 2018: Rosario, Lobo, and Cuenca, Batangas; Madridejos, Cebu; 

COA_2018 <- COA_2018[-c(577)] #Mataas na Kahoy, Batanaas

#removed in 2019: Santa Fe, Nueva Vizcaya; Imus, Cavite; Sariaya, Quezon; Looc, Romblon; Ozamiz City, Misamis Oriental; 

COA_2019 <- COA_2019[-c(568)] #Alitagtag, Batanaas

#removed in 2020: Echague in Isabela

COA_2020 <- COA_2020[-c(46, 1131, 1134, 1384)] #Pualas, Lanao del Sur; San Julian, Eastern Samar; Taft, Eastern Samar; Tacurong City, Sultan Kudarat

#subsetting COA datasets from Auditor's Opinion sections onwards

COA_list <- list(COA_2010, COA_2011, COA_2012, COA_2013, COA_2014, COA_2015, COA_2016, COA_2017, COA_2018, COA_2019, COA_2020)
COA_cleaned <- list()

for (k in 1:length(COA_list)){
  
  COA <- list()
  for (i in 1:length(COA_list[[k]])){
    COA[[i]] <- read_lines(COA_list[[k]][[i]])}
  
  COA_2 <- list()
  for (i in 1:length(COA)){
    COA_2[[i]] <- COA[[i]][grep("Findings|Observations|FINDINGS|OBSERVATIONS|Auditor[^a-z]|AUDITOR|Auditors|AUIDITOR|SCOPE OF THE AUDIT|findings and recommendations|Scope of Audit|SCOPE OF AUDIT|observations", COA[[i]]):length(COA[[i]])]}
  
  COA_cleaned[[k]] <- COA_2
}

for (k in 1:length(COA_cleaned)){
  names(COA_cleaned[[k]]) <- names(COA_list[[k]])}

#cleaning dataset for topic modeling

library(tidytext)
textdf10 <- tibble(line = 1:length(COA_cleaned[[1]]), text = COA_cleaned[[1]])
textdf11 <- tibble(line = 1:length(COA_cleaned[[2]]), text = COA_cleaned[[2]])
textdf12 <- tibble(line = 1:length(COA_cleaned[[3]]), text = COA_cleaned[[3]])
textdf13 <- tibble(line = 1:length(COA_cleaned[[4]]), text = COA_cleaned[[4]])
textdf14 <- tibble(line = 1:length(COA_cleaned[[5]]), text = COA_cleaned[[5]])
textdf15 <- tibble(line = 1:length(COA_cleaned[[6]]), text = COA_cleaned[[6]])
textdf16 <- tibble(line = 1:length(COA_cleaned[[7]]), text = COA_cleaned[[7]])
textdf17 <- tibble(line = 1:length(COA_cleaned[[8]]), text = COA_cleaned[[8]])
textdf18 <- tibble(line = 1:length(COA_cleaned[[9]]), text = COA_cleaned[[9]])
textdf19 <- tibble(line = 1:length(COA_cleaned[[10]]), text = COA_cleaned[[10]])
textdf20 <- tibble(line = 1:length(COA_cleaned[[11]]), text = COA_cleaned[[11]])

makedf <- function(tibble){
  df <- as.data.frame(tibble)
  df$text <- as.character(df$text)
  return(df)
}

textdf10 <- makedf(textdf10)
textdf11 <- makedf(textdf11)
textdf12 <- makedf(textdf12)
textdf13 <- makedf(textdf13)
textdf14 <- makedf(textdf14)
textdf15 <- makedf(textdf15)
textdf16 <- makedf(textdf16)
textdf17 <- makedf(textdf17)
textdf18 <- makedf(textdf18)
textdf19 <- makedf(textdf19)
textdf20 <- makedf(textdf20)
textdf <- rbind(textdf10,textdf11,textdf12,textdf13,
                textdf14,textdf15,textdf16,textdf17,
                textdf18,textdf19,textdf20)

rownames(textdf) <- c(row.names(COA_2010), row.names(COA_2011), 
                      row.names(COA_2012), row.names(COA_2013), 
                      row.names(COA_2014), row.names(COA_2015),
                      row.names(COA_2016), row.names(COA_2017), 
                      row.names(COA_2018), row.names(COA_2019), 
                      row.names(COA_2020))

#preparing text corpus and dfm
corpus <- corpus(textdf)
quant_dfm <- tokens(corpus, remove_punct = TRUE, remove_numbers = TRUE, stem = TRUE) %>%
  tokens_remove(stopwords("en")) %>%
  #tokens_keep(english_words, valuetype = "fixed") %>%
  dfm()
quant_dfm <- dfm_trim(quant_dfm, min_termfreq = 2)

#running topic model with k=25 (this may take a very long time)
quant_dfm <- dfm_trim(quant_dfm, min_termfreq = 2)
tmod_lda <- textmodel_lda(quant_dfm, k = 25)
terms(tmod_lda)

#constructing topic proportions plot
topics <- topics(tmod_lda)
terms <- terms(tmod_lda)
prop_list <- list()

for (i in 1:tmod_lda$k){
  prop_list[[i]] <- data.frame(Description = 'Name', 
                               Terms = toString(terms[,i]), 
                               Proportion = length(which(topics == paste0('topic', i)))/length(topics))
}

prop_list <- do.call(rbind.data.frame, prop_list)
prop_list$Description <- c('Citation of COA Circulars', 
                           'Financial accounting and bookkeeping', 
                           'Employee/Personnel Payment Protocols', 
                           'Need for timely submission of documents', 
                           'City Audit Recommendations', 
                           'Municipal Audit Recommendations', 
                           'Fuel, Inventory, and Supplies Anomalies', 
                           'Implementation of Audit Recommendations for Past Year', 
                           'COA Circular Violations',
                           'Remittance of Withheld Income Taxes', 
                           'Completion of Construction Projects/Contracts', 
                           'Public land and property management', 
                           'Utilization of Disaster Management Fund (LDRRMF)', 
                           'Solid Waste Management Issues', 
                           'Unliquidated Cash Advances Issues', 
                           'Reporting of Local Financial Statements', 
                           'General Recommendations and Observations', 
                           'Management of Local Development Fund (LDF)', 
                           'Procurement and Bidding Issues', 
                           'Balance of Disallowances, Supensions, and Charges', 
                           'Real Property Tax Management', 
                           'Property, Plant, and Equipment Records and Reporting Issues', 
                           'Recommendations for Municipal Treasurer and Accountant',
                           'Gender and Development Budgeting and Project Issues', 
                           'Municipal Financial Accounting and Management Issues')

#reordering proportion list
prop_list <- prop_list[order(prop_list$Description, decreasing = TRUE),]
prop_list <- prop_list[, -2]
prop_list <- prop_list[order(-prop_list$Proportion),]

#plotting proportions list
prop_plot <- ggplot(prop_list, aes(x = Proportion, 
                                   y = reorder(Description, Proportion), 
                                   color = 'black')) + 
  geom_bar(stat = 'identity', fill = 'white') + theme_bw() +
  ggtitle('Topic Prevalence across Audit Corpus, k = 25') + 
  theme(legend.position="none") + xlab("Topic Prevalence") + 
  ylab('Topic')