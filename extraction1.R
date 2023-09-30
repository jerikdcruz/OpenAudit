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
library(pals)
library(lda)
library(ldatuning)
library(flextable)
library(seededlda)
library(gridExtra)
library(haven)

#generating corpus of documents - make sure that all the executive summaries 
#are now unnested in a single folder (titled "Allfiles')

corpus.raw <- readtext("./Allfiles/*.pdf", cache = FALSE)
corpus <- corpus(corpus.raw)

#Viewing values of unliquidated advances/transactions in context
context <- kwic(corpus, pattern = 'unliquidated', window = 40)

#Using kwic dataframe and file names to generate locational/categorical variables
#creating new variables and processing
context$year <- as.numeric(str_extract(context$docname, '(\\d+)'))
context$lgu <- str_extract(context$docname, '^.+[_|-]')
context$lgu <- str_sub(context$lgu, 1, -2)
context$lgu <- str_remove(context$lgu, '-Executive-Summary|-executive-summary
                          |-executive|-Executive')
context$lgu <- str_remove(context$lgu, '_ES\\d+')
context$province <- str_extract(context$lgu, '_(\\D+)')
context$province <- str_remove(context$province, '_')
context$lgu <- str_remove(context$lgu, '_\\D+$')
context$province <- str_remove(context$province, '\\D+_')

#getting unliquidated money values from both sides and consolidating
context$valuepre <- str_extract(context$pre, 'P[\\d., ]+|₱[\\d., ]+|P [\\d., ]+|₱ [\\d., ]+$')
context$valuepost <- str_extract(context$post, 'P[\\d., ]+|₱[\\d., ]+|P [\\d., ]+|₱ [\\d., ]+^')
context$valuepre <- as.numeric(gsub("[^0-9.-]", "", context$valuepre))
context$valuepost <- as.numeric(gsub("[^0-9.-]", "", context$valuepost))
context$valuepre[is.na(context$valuepre)] <- 0
context$valuepost[is.na(context$valuepost)] <- 0
context$unliquidated <- context$valuepre + context$valuepost

#creating new dataframe for presenting the unliquidated transactions
unliquidata <- as.data.frame(context) %>%
  select(lgu, province, year, unliquidated)
unliquidata <- unliquidata %>% 
  group_by(lgu, province, year) %>%
  summarise(unliquidated = sum(unliquidated))

#cleaning dataset: cleaning and matching province names
unliquidata <- unliquidata %>%
  mutate(province = str_replace(province, 'NV', 'Nueva Vizcaya')) %>%
  mutate(province = str_replace(province, 'OccMIn|OccMdo|OccMin', 'Occidental Mindoro')) %>%
  mutate(province = str_replace(province, 'IN|IlocosNorte', 'Ilocos Norte')) %>%
  mutate(province = str_replace(province, 'LaUnion|LU', 'La Union')) %>%
  mutate(province = str_replace(province, 'SDN', 'Surigao del Norte')) %>%
  mutate(province = str_replace(province, 'NE', 'Nueva Ecija')) %>%
  mutate(province = str_replace(province, 'ZS', 'Zamboanga Sibugay')) %>%
  mutate(province = str_replace(province, 'NoSamar', 'Northern Samar')) %>%
  mutate(province = str_replace(province, 'MisOr|MisOR', 'Misamis Oriental')) %>%
  mutate(province = str_replace(province, 'NegOr|NegrOr|NegrosOr', 
                                'Negros Oriental')) %>%
  mutate(province = str_replace(province, 'SoLeyte', 'Southern Leyte')) %>%
  mutate(province = str_replace(province, 'EaSamar', 'Eastern Samar')) %>%
  mutate(province = str_replace(province, 'DDN', 'Davao del Norte')) %>%
  mutate(province = str_replace(province, 'ZDS', 'Zamboanga del Sur')) %>%
  mutate(province = str_replace(province, 'NoSamar', 'Northern Samar')) %>%
  mutate(province = str_replace(province, 'CamSur', 'Camarines Sur')) %>%
  mutate(province = str_replace(province, 'LDN', 'Lanao del Norte')) %>%
  mutate(province = str_replace(province, 'NegOcc|NeOcc', 'Negros Occidental')) %>%
  mutate(province = str_replace(province, 'DO|DavaoOr', 'Davao Oriental')) %>%
  mutate(province = str_replace(province, 'NoSamar', 'Northern Samar')) %>%
  mutate(province = str_replace(province, 'SK', 'Sultan Kudarat')) %>%
  mutate(province = str_replace(province, 'ESamar|EaSamar|aar', 'Eastern Samar')) %>%
  mutate(province = str_replace(province, 'ZDN', 'Zamboanga del Norte')) %>%
  mutate(province = str_replace(province, 'IS', 'Ilocos Sur')) %>%
  mutate(province = str_replace(province, 'OrMdo', 'Oriental Mindoro')) %>%
  mutate(province = str_replace(province, 'SDS', 'Surigao del Sur')) %>%
  mutate(province = str_replace(province, 'DI', 'Dinagat Islands')) %>%
  mutate(province = str_replace(province, 'ADS', 'Agusan del Sur')) %>%
  mutate(province = str_replace(province, 'MtProv|Mountain Provinceince', 
                                'Mountain Province')) %>%
  mutate(province = str_replace(province, 'ADN', 'Agusan del Norte')) %>%
  mutate(province = str_replace(province, 'LDS', 'Lanao del Sur')) %>%
  mutate(province = str_replace(province, 'MisOcc', 'Misamis Occidental')) %>%
  mutate(province = str_replace(province, 'SoCot|South Cotabatoabato|SouthCot', 
                                'South Cotabato')) %>%
  mutate(province = str_replace(province, 'DDS', 'Davao del Sur')) %>%
  mutate(province = str_replace(province, 'CamNorte|CN', 'Camarines Norte')) %>%
  mutate(province = str_replace(province, 'DDS', 'Davao del Sur')) %>%
  mutate(province = str_replace(province, 'CV', 'Compostela Valley')) %>%
  mutate(province = str_replace(province, 'DavaoOcc', 'Davao Occidental')) %>%
  mutate(province = str_replace(province, 'Palalwan', 'Palawan')) %>%
  mutate(province = str_replace(province, 'TawiTawi', 'Tawi-Tawi')) %>%
  mutate(lgu = str_replace(lgu, 'New', 'New '))

#cleaning dataset: cleaning municipality values, general operations
unliquidata$lgu <- gsub("([[:lower:]])([[:upper:]])","\\1 \\2", unliquidata$lgu)
unliquidata$lgu <- gsub("-([[:upper:]])"," \\1", unliquidata$lgu)
unliquidata$lgu <- gsub("(San Jose)(de)","\\1 \\2", unliquidata$lgu)
unliquidata$lgu <- gsub("(Sto) ","\\1. ", unliquidata$lgu)
unliquidata$lgu <- gsub("(Sta) ","\\1. ", unliquidata$lgu)
unliquidata <- unliquidata %>%
  mutate(lgu = str_replace(lgu, 'City|-City|- City', ' City'))

#cleaning dataset: cleaning municipality values, idiosyncratic changes
unliquidata[c(3:8, 5438), 1] <- "Abra de Ilog" 
unliquidata[c(1, 105:112), 1] <- "Alfonso Castaneda" 
unliquidata[65:69, 1] <- "Alang Alang"
unliquidata[228:230, 1] <- "Anini-y" 
unliquidata[288, 1] <- "Braulio E. Dujali" 
unliquidata[312:320, 1] <- "Bacolod City"
unliquidata[327:329, 1] <- "City of Bacoor"
unliquidata[292, 1] <- "Licuan-Baay"
unliquidata[380, 1] <- "Bais City"
unliquidata[438:439, 1] <- "Baloi"
unliquidata[504, 1] <- "Banguingui (Tongkil)"
unliquidata[765:766, 1] <- "Boljo-on"
unliquidata[790, 1] <- "Borongan City"
unliquidata[c(918:919, 3419, 3770, 3793), 1] <- "Pres. Carlos P. Garcia"
unliquidata[971, 1] <- "Cabuyao City"
unliquidata[982:985, 1] <- "Cagayan de Oro City"
unliquidata[1097:1099, 1] <- "Can-avid"
unliquidata[1187, 1] <- "Carranglan"
unliquidata[1248, 1] <- "Cauayan City"
unliquidata[1266:1269, 1] <- "City of Manila"
unliquidata[1301:1302, 1] <- "Compostela"
unliquidata[1389, 1] <- "Cuyo"
unliquidata[1390, 1] <- "Doña Remedios Trinidad"
unliquidata[1391:1393, 1] <- "Don Salvador Benedicto"
unliquidata[1442:1447, 1] <- "City of Dasmariñas"
unliquidata[1514:1515, 1] <- "Dinagat"
unliquidata[1584:1585, 1] <- "Doña Remedios Trinidad"
unliquidata[1591, 1] <- "Rizal"
unliquidata[c(1659, 1661:1665, 1673:1674), 1] <- "Dupax del Norte"
unliquidata[c(1660, 1667:1672, 1675), 1] <- "Dupax del Sur"
unliquidata[c(1676:1682, 1693:1694), 1] <- "Enrique B. Magalona"
unliquidata[1684:1688, 1] <- "El Nido"
unliquidata[1696, 1] <- "Enrique Villanueva"
unliquidata[c(1730:1735, 1780, 1786:1787), 1] <- "General Mariano Alvarez"
unliquidata[1779, 1] <- "General Mamerto Natividad"
unliquidata[c(1774:1778, 1789), 1] <- "General Luna"
unliquidata[c(1781, 1790:1792), 1] <- "General Nakar"
unliquidata[1785, 1] <- "General MacArthur"
unliquidata[1783:1784, 1] <- "General Tinio"
unliquidata[c(1782, 1788, 1793:1796), 1] <- "General Santos City"
unliquidata[1843:1848, 1] <- "Governor Generoso"
unliquidata[1945, 1] <- "Hinobaan"
unliquidata[1956:1957, 1] <- "Island Garden City of Samal"
unliquidata[2018, 1] <- "Impasugong"
unliquidata[2052, 1] <- "Isabel"
unliquidata[2062, 1] <- "Isabela"
unliquidata[2062, 2] <- "Negros Occidental"
unliquidata[2331:2330, 1] <- "Laak/San Vicente"
unliquidata[2427:2434, 1] <- "Lapu-Lapu City"
unliquidata[2457, 1] <- 'Las Pinas City'
unliquidata[2481:2483, 1] <- 'Lawa-an'
unliquidata[2504:2511, 1] <- 'Leon B. Postigo'
unliquidata[2555:2560, 1] <- 'Licuan-Baay'
unliquidata[2639:2643, 1] <- 'Lope de Vega'
unliquidata[2706, 1] <- "M'lang" 
unliquidata[3038, 1] <- 'Mansalay'
unliquidata[3160:3162, 1] <- 'Miag-ao'
unliquidata[3471, 1] <- 'Palayan City'
unliquidata[3496, 1] <- 'Pamplona'
unliquidata[3621:3627, 1] <- 'Peñablanca'
unliquidata[3628:3632, 1] <- 'Peñaranda'
unliquidata[3633:3636, 1] <- 'Peñarrubia'
unliquidata[3690:3691, 1] <- 'Pinamungahan'
unliquidata[3692:3696, 1] <- 'Piñan'
unliquidata[3703:3707, 1] <- 'Pio V. Corpuz'
unliquidata[3760, 1] <- 'Poona-Piagapo'
unliquidata[c(3771:3772, 3786:3787, 3794), 1] <- 'Manuel Roxas'
unliquidata[3773:3777, 1] <- 'President Quirino'
unliquidata[c(3778:3785, 3796:3798), 1] <- 'President Roxas'
unliquidata[c(3782:3785, 3788), 1] <- 'Pres. Roxas'
unliquidata[3871:3876, 1] <- 'Remedios T. Romualdez'
unliquidata[3865:3870, 1] <- 'Roseller T. Lim'
unliquidata[3940, 1] <- 'Rosales'
unliquidata[4021:4022, 1] <- 'Sagñay'
unliquidata[4175, 1] <- 'San Fernando City'
unliquidata[4292:4293, 1] <- 'San Jose del Monte'
unliquidata[4571:4574, 1] <- 'Science City of Muñoz'
unliquidata[4575:4578, 1] <- 'Sen. Ninoy Aquino'
unliquidata[4579:4585, 1] <- 'Sergio Osmeña'
unliquidata[4719:4721, 1] <- 'Sofronio Española'
unliquidata[4752:4754, 1] <- 'Saint Bernard'
unliquidata[4841:4857, 1] <- 'Sto. Niño'
unliquidata[4937:4942, 1] <- 'Taganaan'
unliquidata[4949, 1] <- 'Tagapul-an'
unliquidata[4949, 1] <- 'Tagapul-an'
unliquidata[5175, 1] <- 'Tipo-Tipo'
unliquidata[5347, 1] <- 'Vincenzo Sagun'
unliquidata[5439, 1] <- 'Caraga'
unliquidata[4766:4773, 1] <- 'Santa Barbara'
unliquidata[4781:4782, 1] <- 'Santa Elena'
unliquidata[4825:4829, 1] <- 'Santa Rosa City'
unliquidata[4818:4820, 1] <- 'Santa Praxedes'

#writing out and saving
options(scipen=999)
unliquidata$unliquidated <- round(unliquidata$unliquidated, 2)
write.csv(unliquidata, "unliquidata.csv", row.names=FALSE)

#cleaning dataset: cleaning lgu na values, systemic changes
unliquidata$province[unliquidata$lgu == 'Abra de Ilog' & is.na(unliquidata$province)] <- 'Occidental Mindoro'
unliquidata$province[unliquidata$lgu == 'Agutaya' & is.na(unliquidata$province)] <- 'Palawan'
unliquidata$province[unliquidata$lgu == 'Ajuy' & is.na(unliquidata$province)] <- 'Iloilo'
unliquidata$province[unliquidata$lgu == 'Alabat' & is.na(unliquidata$province)] <- 'Quezon'
unliquidata$province[unliquidata$lgu == 'Alamada' & is.na(unliquidata$province)] <- 'Cotabato'
unliquidata$province[unliquidata$lgu == 'Aleosan' & is.na(unliquidata$province)] <- 'Cotabato'
unliquidata$province[unliquidata$lgu == 'Alfonso Castaneda' & is.na(unliquidata$province)] <- 'Nueva Vizcaya'
unliquidata$province[unliquidata$lgu == 'Aliaga' & is.na(unliquidata$province)] <- 'Nueva Ecija'
unliquidata$province[unliquidata$lgu == 'Alimodian' & is.na(unliquidata$province)] <- 'Iloilo'
unliquidata$province[unliquidata$lgu == 'Aloguinsan' & is.na(unliquidata$province)] <- 'Cebu'
unliquidata$province[unliquidata$lgu == 'Ambaguio' & is.na(unliquidata$province)] <- 'Nueva Vizcaya'
unliquidata$province[unliquidata$lgu == 'Anahawan' & is.na(unliquidata$province)] <- 'Southern Leyte'
unliquidata$province[unliquidata$lgu == 'Angeles  City' & is.na(unliquidata$province)] <- 'Pampanga'
unliquidata$province[unliquidata$lgu == 'Antipas' & is.na(unliquidata$province)] <- 'Cotabato'
unliquidata$province[unliquidata$lgu == 'Antipolo  City' & is.na(unliquidata$province)] <- 'Rizal'
unliquidata$province[unliquidata$lgu == 'Arakan' & is.na(unliquidata$province)] <- 'Cotabato'
unliquidata$province[unliquidata$lgu == 'Aritao' & is.na(unliquidata$province)] <- 'Nueva Vizcaya'
unliquidata$province[unliquidata$lgu == 'Bacacay' & is.na(unliquidata$province)] <- 'Albay'
unliquidata$province[unliquidata$lgu == 'Bacarra' & is.na(unliquidata$province)] <- 'Ilocos Norte'
unliquidata$province[unliquidata$lgu == 'Baclayon' & is.na(unliquidata$province)] <- 'Bohol'
unliquidata$province[unliquidata$lgu == 'Bacolod City' & is.na(unliquidata$province)] <- 'Negros Occidental'
unliquidata$province[unliquidata$lgu == 'Bacolor' & is.na(unliquidata$province)] <- 'Pampanga'
unliquidata$province[unliquidata$lgu == 'Bacuag' & is.na(unliquidata$province)] <- 'Surigao del Norte'
unliquidata$province[unliquidata$lgu == 'Badian' & is.na(unliquidata$province)] <- 'Cebu'
unliquidata$province[unliquidata$lgu == 'Badiangan' & is.na(unliquidata$province)] <- 'Iloilo'
unliquidata$province[unliquidata$lgu == 'Bagabag' & is.na(unliquidata$province)] <- 'Nueva Vizcaya'
unliquidata$province[unliquidata$lgu == 'Baganga' & is.na(unliquidata$province)] <- 'Davao Oriental'
unliquidata$province[unliquidata$lgu == 'Baguio  City' & is.na(unliquidata$province)] <- 'Benguet'
unliquidata$province[unliquidata$lgu == 'Bais  City' & is.na(unliquidata$province)] <- 'Negros Oriental'
unliquidata$province[unliquidata$lgu == 'Bais City' & is.na(unliquidata$province)] <- 'Negros Oriental'
unliquidata$province[unliquidata$lgu == 'Balamban' & is.na(unliquidata$province)] <- 'Cebu'
unliquidata$province[unliquidata$lgu == 'Balangkayan' & is.na(unliquidata$province)] <- 'Eastern Samar'
unliquidata$province[unliquidata$lgu == 'Baleno' & is.na(unliquidata$province)] <- 'Masbate'
unliquidata$province[unliquidata$lgu == 'Baliangao' & is.na(unliquidata$province)] <- 'Misamis Occidental'
unliquidata$province[unliquidata$lgu == 'Balingasag' & is.na(unliquidata$province)] <- 'Misamis Oriental'
unliquidata$province[unliquidata$lgu == 'Balud' & is.na(unliquidata$province)] <- 'Masbate'
unliquidata$province[unliquidata$lgu == 'Bambang' & is.na(unliquidata$province)] <- 'Nueva Vizcaya'
unliquidata$province[unliquidata$lgu == 'Banate' & is.na(unliquidata$province)] <- 'Iloilo'
unliquidata$province[unliquidata$lgu == 'Bangui' & is.na(unliquidata$province)] <- 'Ilocos Norte'
unliquidata$province[unliquidata$lgu == 'Bani' & is.na(unliquidata$province)] <- 'Pangasinan'
unliquidata$province[unliquidata$lgu == 'Bansud' & is.na(unliquidata$province)] <- 'Oriental Mindoro'
unliquidata$province[unliquidata$lgu == 'Barotac Nuevo' & is.na(unliquidata$province)] <- 'Iloilo'
unliquidata$province[unliquidata$lgu == 'Barotac Viejo' & is.na(unliquidata$province)] <- 'Iloilo'
unliquidata$province[unliquidata$lgu == 'Basilisa' & is.na(unliquidata$province)] <- 'Dinagat Islands'
unliquidata$province[unliquidata$lgu == 'Batan' & is.na(unliquidata$province)] <- 'Aklan'
unliquidata$province[unliquidata$lgu == 'Baungon' & is.na(unliquidata$province)] <- 'Bukidnon'
unliquidata$province[unliquidata$lgu == 'Bayambang' & is.na(unliquidata$province)] <- 'Pangasinan'
unliquidata$province[unliquidata$lgu == 'Bayang' & is.na(unliquidata$province)] <- 'Lanao del Sur'
unliquidata$province[unliquidata$lgu == 'Bayawan  City' & is.na(unliquidata$province)] <- 'Negros Oriental'
unliquidata$province[unliquidata$lgu == 'Bayog' & is.na(unliquidata$province)] <- 'Zamboanga del Sur'
unliquidata$province[unliquidata$lgu == 'Benito Soliven' & is.na(unliquidata$province)] <- 'Isabela'
unliquidata$province[unliquidata$lgu == 'Bien Unido' & is.na(unliquidata$province)] <- 'Bohol'
unliquidata$province[unliquidata$lgu == 'Bilar' & is.na(unliquidata$province)] <- 'Bohol'
unliquidata$province[unliquidata$lgu == 'Binalbagan' & is.na(unliquidata$province)] <- 'Negros Occidental'
unliquidata$province[unliquidata$lgu == 'Binangonan' & is.na(unliquidata$province)] <- 'Rizal'
unliquidata$province[unliquidata$lgu == 'Bindoy' & is.na(unliquidata$province)] <- 'Negros Oriental'
unliquidata$province[unliquidata$lgu == 'Binuangan' & is.na(unliquidata$province)] <- 'Misamis Oriental'
unliquidata$province[unliquidata$lgu == 'Bocaue' & is.na(unliquidata$province)] <- 'Bulacan'
unliquidata$province[unliquidata$lgu == 'Bombon' & is.na(unliquidata$province)] <- 'Camarines Sur'
unliquidata$province[unliquidata$lgu == 'Bongabong' & is.na(unliquidata$province)] <- 'Oriental Mindoro'
unliquidata$province[unliquidata$lgu == 'Bontoc' & is.na(unliquidata$province)] <- 'Southern Leyte'
unliquidata$province[unliquidata$lgu == 'Borongan  City' & is.na(unliquidata$province)] <- 'Eastern Samar'
unliquidata$province[unliquidata$lgu == 'Boston' & is.na(unliquidata$province)] <- 'Davao Oriental'
unliquidata$province[unliquidata$lgu == 'Bulan' & is.na(unliquidata$province)] <- 'Sorsogon'
unliquidata$province[unliquidata$lgu == 'Busuanga' & is.na(unliquidata$province)] <- 'Palawan'
unliquidata$province[unliquidata$lgu == 'Butuan  City' & is.na(unliquidata$province)] <- 'Agusan del Norte'
unliquidata$province[unliquidata$lgu == 'Buug' & is.na(unliquidata$province)] <- 'Zamboanga Sibugay'
unliquidata$province[unliquidata$lgu == 'Cabadbaran  City' & is.na(unliquidata$province)] <- 'Agusan del Norte'
unliquidata$province[unliquidata$lgu == 'Cabanatuan  City' & is.na(unliquidata$province)] <- 'Nueva Ecija'
unliquidata$province[unliquidata$lgu == 'Cabanglasan' & is.na(unliquidata$province)] <- 'Bukidnon'
unliquidata$province[unliquidata$lgu == 'Cabuyao  City' & is.na(unliquidata$province)] <- 'Laguna'
unliquidata$province[unliquidata$lgu == 'Cadiz  City' & is.na(unliquidata$province)] <- 'Negros Occidental'
unliquidata$province[unliquidata$lgu == 'Cagayan de Oro City' & is.na(unliquidata$province)] <- 'Misamis Oriental'
unliquidata$province[unliquidata$lgu == 'Cagayan de Oro  City' & is.na(unliquidata$province)] <- 'Misamis Oriental'
unliquidata$province[unliquidata$lgu == 'Cagayancillo' & is.na(unliquidata$province)] <- 'Palawan'
unliquidata$province[unliquidata$lgu == 'Cagdianao' & is.na(unliquidata$province)] <- 'Dinagat Islands'
unliquidata$province[unliquidata$lgu == 'Caibiran' & is.na(unliquidata$province)] <- 'Biliran'
unliquidata$province[unliquidata$lgu == 'Cainta' & is.na(unliquidata$province)] <- 'Rizal'
unliquidata$province[unliquidata$lgu == 'Calauan' & is.na(unliquidata$province)] <- 'Laguna'
unliquidata$province[unliquidata$lgu == 'Calbayog  City' & is.na(unliquidata$province)] <- 'Samar'
unliquidata$province[unliquidata$lgu == 'Calintaan' & is.na(unliquidata$province)] <- 'Occidental Mindoro'
unliquidata$province[unliquidata$lgu == 'Caloocan  City' & is.na(unliquidata$province)] <- 'Metro Manila'
unliquidata$province[unliquidata$lgu == 'Camiling' & is.na(unliquidata$province)] <- 'Tarlac'
unliquidata$province[unliquidata$lgu == 'Canaman' & is.na(unliquidata$province)] <- 'Camarines Sur'
unliquidata$province[unliquidata$lgu == 'Can-avid' & is.na(unliquidata$province)] <- 'Eastern Samar'
unliquidata$province[unliquidata$lgu == 'Candoni' & is.na(unliquidata$province)] <- 'Negros Occidental'
unliquidata$province[unliquidata$lgu == 'Canlaon  City' & is.na(unliquidata$province)] <- 'Negros Oriental'
unliquidata$province[unliquidata$lgu == 'Cantilan' & is.na(unliquidata$province)] <- 'Surigao del Sur'
unliquidata$province[unliquidata$lgu == 'Capoocan' & is.na(unliquidata$province)] <- 'Leyte'
unliquidata$province[unliquidata$lgu == 'Caraga' & is.na(unliquidata$province)] <- 'Davao Oriental'
unliquidata$province[unliquidata$lgu == 'Caraga' & is.na(unliquidata$province)] <- 'Davao Oriental'
unliquidata$province[unliquidata$lgu == 'Caramoan' & is.na(unliquidata$province)] <- 'Camarines Sur'
unliquidata$province[unliquidata$lgu == 'Carcar  City' & is.na(unliquidata$province)] <- 'Cebu'
unliquidata$province[unliquidata$lgu == 'Carles' & is.na(unliquidata$province)] <- 'Iloilo'
unliquidata$province[unliquidata$lgu == 'Carranglan' & is.na(unliquidata$province)] <- 'Nueva Ecija'
unliquidata$province[unliquidata$lgu == 'Carrascal' & is.na(unliquidata$province)] <- 'Surigao del Sur'
unliquidata$province[unliquidata$lgu == 'Castilla' & is.na(unliquidata$province)] <- 'Sorsogon'
unliquidata$province[unliquidata$lgu == 'Cataingan' & is.na(unliquidata$province)] <- 'Masbate'
unliquidata$province[unliquidata$lgu == 'Cateel' & is.na(unliquidata$province)] <- 'Davao Oriental'
unliquidata$province[unliquidata$lgu == 'Catmon' & is.na(unliquidata$province)] <- 'Cebu'
unliquidata$province[unliquidata$lgu == 'Cebu  City' & is.na(unliquidata$province)] <- 'Cebu'
unliquidata$province[unliquidata$lgu == 'City of DasmariÃ±as' & is.na(unliquidata$province)] <- 'Cavite'
unliquidata$province[unliquidata$lgu == 'Claver' & is.na(unliquidata$province)] <- 'Surigao del Norte'
unliquidata$province[unliquidata$lgu == 'Coron' & is.na(unliquidata$province)] <- 'Palawan'
unliquidata$province[unliquidata$lgu == 'Cotabato  City' & is.na(unliquidata$province)] <- 'North Cotabato'
unliquidata$province[unliquidata$lgu == 'Culion' & is.na(unliquidata$province)] <- 'Palawan'
unliquidata$province[unliquidata$lgu == 'Dagupan  City' & is.na(unliquidata$province)] <- 'Pangasinan'
unliquidata$province[unliquidata$lgu == 'Damulog' & is.na(unliquidata$province)] <- 'Bukidnon'
unliquidata$province[unliquidata$lgu == 'Dangcagan' & is.na(unliquidata$province)] <- 'Bukidnon'
unliquidata$province[unliquidata$lgu == 'Dapitan  City' & is.na(unliquidata$province)] <- 'Zamboanga del Norte'
unliquidata$province[unliquidata$lgu == 'Daraga' & is.na(unliquidata$province)] <- 'Albay'
unliquidata$province[unliquidata$lgu == 'Datu Saudi Ampatuan' & is.na(unliquidata$province)] <- 'Maguindanao'
unliquidata$province[unliquidata$lgu == 'Dauin' & is.na(unliquidata$province)] <- 'Negros Oriental'
unliquidata$province[unliquidata$lgu == 'Del Carmen' & is.na(unliquidata$province)] <- 'Surigao del Norte'
unliquidata$province[unliquidata$lgu == 'Del Gallego' & is.na(unliquidata$province)] <- 'Camarines Sur'
unliquidata$province[unliquidata$lgu == 'Dimasalang' & is.na(unliquidata$province)] <- 'Masbate'
unliquidata$province[unliquidata$lgu == 'Dimataling' & is.na(unliquidata$province)] <- 'Zamboanga del Sur'
unliquidata$province[unliquidata$lgu == 'Dimiao' & is.na(unliquidata$province)] <- 'Bohol'
unliquidata$province[unliquidata$lgu == 'Dinagat' & is.na(unliquidata$province)] <- 'Dinagat Islands'
unliquidata$province[unliquidata$lgu == 'Dinalupihan' & is.na(unliquidata$province)] <- 'Bataan'
unliquidata$province[unliquidata$lgu == 'Diplahan' & is.na(unliquidata$province)] <- 'Zamboanga Sibugay'
unliquidata$province[unliquidata$lgu == 'Dipolog  City' & is.na(unliquidata$province)] <- 'Zamboanga del Norte'
unliquidata$province[unliquidata$lgu == 'Don Victoriano' & is.na(unliquidata$province)] <- 'Misamis Occidental'
unliquidata$province[unliquidata$lgu == 'Donsol' & is.na(unliquidata$province)] <- 'Sorsogon'
unliquidata$province[unliquidata$lgu == 'Dumanjug' & is.na(unliquidata$province)] <- 'Cebu'
unliquidata$province[unliquidata$lgu == 'Dumaran' & is.na(unliquidata$province)] <- 'Palawan'
unliquidata$province[unliquidata$lgu == 'Dupax del Norte' & is.na(unliquidata$province)] <- 'Nueva Vizcaya'
unliquidata$province[unliquidata$lgu == 'Dupax del Sur' & is.na(unliquidata$province)] <- 'Nueva Vizcaya'
unliquidata$province[unliquidata$lgu == 'El Nido' & is.na(unliquidata$province)] <- 'Palawan'
unliquidata$province[unliquidata$lgu == 'Enrique B. Magalona' & is.na(unliquidata$province)] <- 'Negros Occidental'
unliquidata$province[unliquidata$lgu == 'Enrique Villanueva' & is.na(unliquidata$province)] <- 'Siquijor'
unliquidata$province[unliquidata$lgu == 'Famy' & is.na(unliquidata$province)] <- 'Laguna'
unliquidata$province[unliquidata$lgu == 'Gabaldon' & is.na(unliquidata$province)] <- 'Nueva Ecija'
unliquidata$province[unliquidata$lgu == 'Gamay' & is.na(unliquidata$province)] <- 'Northern Samar'
unliquidata$province[unliquidata$lgu == 'Gapan  City' & is.na(unliquidata$province)] <- 'Nueva Ecija'
unliquidata$province[unliquidata$lgu == 'General MacArthur' & is.na(unliquidata$province)] <- 'Eastern Samar'
unliquidata$province[unliquidata$lgu == 'General Mariano Alvarez' & is.na(unliquidata$province)] <- 'Cavite'
unliquidata$province[unliquidata$lgu == 'General Santos City' & is.na(unliquidata$province)] <- 'South Cotabato'
unliquidata$province[unliquidata$lgu == 'Gigaquit' & is.na(unliquidata$province)] <- 'Surigao del Norte'
unliquidata$province[unliquidata$lgu == 'Gingoog  City' & is.na(unliquidata$province)] <- 'Misamis Oriental'
unliquidata$province[unliquidata$lgu == 'Glan' & is.na(unliquidata$province)] <- 'Sarangani'
unliquidata$province[unliquidata$lgu == 'Goa' & is.na(unliquidata$province)] <- 'Camarines Sur'
unliquidata$province[unliquidata$lgu == 'Guinayangan' & is.na(unliquidata$province)] <- 'Quezon'
unliquidata$province[unliquidata$lgu == 'Guindulman' & is.na(unliquidata$province)] <- 'Bohol'
unliquidata$province[unliquidata$lgu == 'Guinsiliban' & is.na(unliquidata$province)] <- 'Camiguin'
unliquidata$province[unliquidata$lgu == 'Guiuan' & is.na(unliquidata$province)] <- 'Eastern Samar'
unliquidata$province[unliquidata$lgu == 'Gutalac' & is.na(unliquidata$province)] <- 'Zamboanga del Norte'
unliquidata$province[unliquidata$lgu == 'Hamtic' & is.na(unliquidata$province)] <- 'Antique'
unliquidata$province[unliquidata$lgu == 'Himamaylan  City' & is.na(unliquidata$province)] <- 'Negros Occidental'
unliquidata$province[unliquidata$lgu == 'Hinatuan' & is.na(unliquidata$province)] <- 'Surigao del Sur'
unliquidata$province[unliquidata$lgu == 'Hinigaran' & is.na(unliquidata$province)] <- 'Negros Occidental'
unliquidata$province[unliquidata$lgu == 'Hinunangan' & is.na(unliquidata$province)] <- 'Southern Leyte'
unliquidata$province[unliquidata$lgu == 'Hinundayan' & is.na(unliquidata$province)] <- 'Southern Leyte'
unliquidata$province[unliquidata$lgu == 'Iba' & is.na(unliquidata$province)] <- 'Zambales'
unliquidata$province[unliquidata$lgu == 'Ibajay' & is.na(unliquidata$province)] <- 'Aklan'
unliquidata$province[unliquidata$lgu == 'Igbaras' & is.na(unliquidata$province)] <- 'Iloilo'
unliquidata$province[unliquidata$lgu == 'Iligan  City' & is.na(unliquidata$province)] <- 'Lanao del Norte'
unliquidata$province[unliquidata$lgu == 'Iloilo  City' & is.na(unliquidata$province)] <- 'Iloilo'
unliquidata$province[unliquidata$lgu == 'Impasugong' & is.na(unliquidata$province)] <- 'Bukidnon'
unliquidata$province[unliquidata$lgu == 'Inabanga' & is.na(unliquidata$province)] <- 'Bohol'
unliquidata$province[unliquidata$lgu == 'Indanan' & is.na(unliquidata$province)] <- 'Sulu'
unliquidata$province[unliquidata$lgu == 'Isabel' & is.na(unliquidata$province)] <- 'Leyte'
unliquidata$province[unliquidata$lgu == 'Isabela' & is.na(unliquidata$province)] <- 'Negros Occidental'
unliquidata$province[unliquidata$lgu == 'Island Garden City of Samal' & is.na(unliquidata$province)] <- 'Davao del Norte'
unliquidata$province[unliquidata$lgu == 'Jaen' & is.na(unliquidata$province)] <- 'Nueva Ecija'
unliquidata$province[unliquidata$lgu == 'Jagna' & is.na(unliquidata$province)] <- 'Bohol'
unliquidata$province[unliquidata$lgu == 'Jaro' & is.na(unliquidata$province)] <- 'Leyte'
unliquidata$province[unliquidata$lgu == 'Jiabong' & is.na(unliquidata$province)] <- 'Samar'
unliquidata$province[unliquidata$lgu == 'Jomalig' & is.na(unliquidata$province)] <- 'Quezon'
unliquidata$province[unliquidata$lgu == 'Jones' & is.na(unliquidata$province)] <- 'Isabela'
unliquidata$province[unliquidata$lgu == 'Kabuntalan' & is.na(unliquidata$province)] <- 'Maguindanao'
unliquidata$province[unliquidata$lgu == 'Kapatagan' & is.na(unliquidata$province)] <- 'Lanao del Norte'
unliquidata$province[unliquidata$lgu == 'Katipunan' & is.na(unliquidata$province)] <- 'Zamboanga del Norte'
unliquidata$province[unliquidata$lgu == 'Kauswagan' & is.na(unliquidata$province)] <- 'Lanao del Norte'
unliquidata$province[unliquidata$lgu == 'Kawit' & is.na(unliquidata$province)] <- 'Cavite'
unliquidata$province[unliquidata$lgu == 'Kayapa' & is.na(unliquidata$province)] <- 'Nueva Vizcaya'
unliquidata$province[unliquidata$lgu == 'Kiamba' & is.na(unliquidata$province)] <- 'Sarangani'
unliquidata$province[unliquidata$lgu == 'Kidapawan  City' & is.na(unliquidata$province)] <- 'Cotabato'
unliquidata$province[unliquidata$lgu == 'Kitcharao' & is.na(unliquidata$province)] <- 'Agusan del Norte'
unliquidata$province[unliquidata$lgu == 'Kolambugan' & is.na(unliquidata$province)] <- 'Lanao del Norte'
unliquidata$province[unliquidata$lgu == 'Koronadal  City' & is.na(unliquidata$province)] <- 'South Cotabato'
unliquidata$province[unliquidata$lgu == 'La Carlota  City' & is.na(unliquidata$province)] <- 'Negros Occidental'
unliquidata$province[unliquidata$lgu == 'La Castellana' & is.na(unliquidata$province)] <- 'Negros Occidental'
unliquidata$province[unliquidata$lgu == 'Labangan' & is.na(unliquidata$province)] <- 'Zamboanga del Sur'
unliquidata$province[unliquidata$lgu == 'Labo' & is.na(unliquidata$province)] <- 'Camarines Norte'
unliquidata$province[unliquidata$lgu == 'Lagonglong' & is.na(unliquidata$province)] <- 'Misamis Oriental'
unliquidata$province[unliquidata$lgu == 'Lake Sebu' & is.na(unliquidata$province)] <- 'South Cotabato'
unliquidata$province[unliquidata$lgu == 'Lala' & is.na(unliquidata$province)] <- 'Lanao del Norte'
unliquidata$province[unliquidata$lgu == 'Lambunao' & is.na(unliquidata$province)] <- 'Iloilo'
unliquidata$province[unliquidata$lgu == 'Laoang' & is.na(unliquidata$province)] <- 'Northern Samar'
unliquidata$province[unliquidata$lgu == 'Lapinig' & is.na(unliquidata$province)] <- 'Northern Samar'
unliquidata$province[unliquidata$lgu == 'Lapu-Lapu City' & is.na(unliquidata$province)] <- 'Cebu'
unliquidata$province[unliquidata$lgu == 'Las Navas' & is.na(unliquidata$province)] <- 'Northern Samar'
unliquidata$province[unliquidata$lgu == 'Las Nieves' & is.na(unliquidata$province)] <- 'Agusan del Norte'
unliquidata$province[unliquidata$lgu == 'Las Pinas City' & is.na(unliquidata$province)] <- 'Metro Manila'
unliquidata$province[unliquidata$lgu == 'Laur' & is.na(unliquidata$province)] <- 'Nueva Ecija'
unliquidata$province[unliquidata$lgu == 'Lazi' & is.na(unliquidata$province)] <- 'Siquijor'
unliquidata$province[unliquidata$lgu == 'Leganes' & is.na(unliquidata$province)] <- 'Iloilo'
unliquidata$province[unliquidata$lgu == 'Legazpi  City' & is.na(unliquidata$province)] <- 'Albay'
unliquidata$province[unliquidata$lgu == 'Leon' & is.na(unliquidata$province)] <- 'Iloilo'
unliquidata$province[unliquidata$lgu == 'Leyte' & is.na(unliquidata$province)] <- 'Leyte'
unliquidata$province[unliquidata$lgu == 'Lezo' & is.na(unliquidata$province)] <- 'Aklan'
unliquidata$province[unliquidata$lgu == 'Libacao' & is.na(unliquidata$province)] <- 'Aklan'
unliquidata$province[unliquidata$lgu == 'Libjo' & is.na(unliquidata$province)] <- 'Dinagat Islands'
unliquidata$province[unliquidata$lgu == 'Ligao  City' & is.na(unliquidata$province)] <- 'Albay'
unliquidata$province[unliquidata$lgu == 'Limasawa' & is.na(unliquidata$province)] <- 'Southern Leyte'
unliquidata$province[unliquidata$lgu == 'Limay' & is.na(unliquidata$province)] <- 'Bataan'
unliquidata$province[unliquidata$lgu == 'Linamon' & is.na(unliquidata$province)] <- 'Lanao del Norte'
unliquidata$province[unliquidata$lgu == 'Linapacan' & is.na(unliquidata$province)] <- 'Palawan'
unliquidata$province[unliquidata$lgu == 'Llorente' & is.na(unliquidata$province)] <- 'Eastern Samar'
unliquidata$province[unliquidata$lgu == 'Loay Bohol' & is.na(unliquidata$province)] <- 'Bohol'
unliquidata$province[unliquidata$lgu == 'Lobo' & is.na(unliquidata$province)] <- 'Batangas'
unliquidata$province[unliquidata$lgu == 'Loboc' & is.na(unliquidata$province)] <- 'Bohol'
unliquidata$province[unliquidata$lgu == 'Lope de Vega' & is.na(unliquidata$province)] <- 'Northern Samar'
unliquidata$province[unliquidata$lgu == 'Maasim' & is.na(unliquidata$province)] <- 'Sarangani'
unliquidata$province[unliquidata$lgu == 'Maasin' & is.na(unliquidata$province)] <- 'Iloilo'
unliquidata$province[unliquidata$lgu == 'Maasin  City' & is.na(unliquidata$province)] <- 'Southern Leyte'
unliquidata$province[unliquidata$lgu == 'Mabinay' & is.na(unliquidata$province)] <- 'Negros Oriental'
unliquidata$province[unliquidata$lgu == 'Mabuhay' & is.na(unliquidata$province)] <- 'Zamboanga Sibugay'
unliquidata$province[unliquidata$lgu == 'Madalag' & is.na(unliquidata$province)] <- 'Aklan'
unliquidata$province[unliquidata$lgu == 'Maddela' & is.na(unliquidata$province)] <- 'Quirino'
unliquidata$province[unliquidata$lgu == 'Magpet' & is.na(unliquidata$province)] <- 'Cotabato'
unliquidata$province[unliquidata$lgu == 'Mahayag' & is.na(unliquidata$province)] <- 'Zamboanga del Sur'
unliquidata$province[unliquidata$lgu == 'Maigo' & is.na(unliquidata$province)] <- 'Lanao del Norte'
unliquidata$province[unliquidata$lgu == 'Mainit' & is.na(unliquidata$province)] <- 'Surigao del Norte'
unliquidata$province[unliquidata$lgu == 'Maitum' & is.na(unliquidata$province)] <- 'Sarangani'
unliquidata$province[unliquidata$lgu == 'Majayjay' & is.na(unliquidata$province)] <- 'Laguna'
unliquidata$province[unliquidata$lgu == 'Makati  City' & is.na(unliquidata$province)] <- 'Metro Manila'
unliquidata$province[unliquidata$lgu == 'Malabon  City' & is.na(unliquidata$province)] <- 'Metro Manila'
unliquidata$province[unliquidata$lgu == 'Malangas' & is.na(unliquidata$province)] <- 'Zamboanga Sibugay'
unliquidata$province[unliquidata$lgu == 'Malapatan' & is.na(unliquidata$province)] <- 'Sarangani'
unliquidata$province[unliquidata$lgu == 'Malilipot' & is.na(unliquidata$province)] <- 'Albay'
unliquidata$province[unliquidata$lgu == 'Malimono' & is.na(unliquidata$province)] <- 'Surigao del Norte'
unliquidata$province[unliquidata$lgu == 'Mallig' & is.na(unliquidata$province)] <- 'Isabela'
unliquidata$province[unliquidata$lgu == 'Mamasapano' & is.na(unliquidata$province)] <- 'Maguindanao'
unliquidata$province[unliquidata$lgu == 'Mambajao' & is.na(unliquidata$province)] <- 'Camiguin'
unliquidata$province[unliquidata$lgu == 'Mamburao' & is.na(unliquidata$province)] <- 'Occidental Mindoro'
unliquidata$province[unliquidata$lgu == 'Manapla' & is.na(unliquidata$province)] <- 'Negros Occidental'
unliquidata$province[unliquidata$lgu == 'Manay' & is.na(unliquidata$province)] <- 'Davao Oriental'
unliquidata$province[unliquidata$lgu == 'Mandaluyong  City' & is.na(unliquidata$province)] <- 'Metro Manila'
unliquidata$province[unliquidata$lgu == 'Mandaon' & is.na(unliquidata$province)] <- 'Masbate'
unliquidata$province[unliquidata$lgu == 'Mandaue  City' & is.na(unliquidata$province)] <- 'Cebu'
unliquidata$province[unliquidata$lgu == 'Mangaldan' & is.na(unliquidata$province)] <- 'Pangasinan'
unliquidata$province[unliquidata$lgu == 'Manila  City' & is.na(unliquidata$province)] <- 'Metro Manila'
unliquidata$province[unliquidata$lgu == 'Manjuyod' & is.na(unliquidata$province)] <- 'Negros Oriental'
unliquidata$province[unliquidata$lgu == 'Mansalay' & is.na(unliquidata$province)] <- 'Oriental Mindoro'
unliquidata$province[unliquidata$lgu == 'Manuel Roxas' & is.na(unliquidata$province)] <- 'Zamboanga del Norte'
unliquidata$province[unliquidata$lgu == 'Marabut' & is.na(unliquidata$province)] <- 'Samar'
unliquidata$province[unliquidata$lgu == 'Margosatubig' & is.na(unliquidata$province)] <- 'Zamboanga del Sur'
unliquidata$province[unliquidata$lgu == 'Maria' & is.na(unliquidata$province)] <- 'Siquijor'
unliquidata$province[unliquidata$lgu == 'Marikina  City' & is.na(unliquidata$province)] <- 'Metro Manila'
unliquidata$province[unliquidata$lgu == 'Masbate  City' & is.na(unliquidata$province)] <- 'Masbate'
unliquidata$province[unliquidata$lgu == 'Mati  City' & is.na(unliquidata$province)] <- 'Davao Oriental'
unliquidata$province[unliquidata$lgu == 'Matnog' & is.na(unliquidata$province)] <- 'Sorsogon'
unliquidata$province[unliquidata$lgu == 'Maydolong' & is.na(unliquidata$province)] <- 'Eastern Samar'
unliquidata$province[unliquidata$lgu == 'Mayoyao' & is.na(unliquidata$province)] <- 'Ifugao'
unliquidata$province[unliquidata$lgu == 'Merida' & is.na(unliquidata$province)] <- 'Leyte'
unliquidata$province[unliquidata$lgu == 'Milagros' & is.na(unliquidata$province)] <- 'Masbate'
unliquidata$province[unliquidata$lgu == 'Moises Padilla' & is.na(unliquidata$province)] <- 'Negros Occidental'
unliquidata$province[unliquidata$lgu == 'Mondragon' & is.na(unliquidata$province)] <- 'Northern Samar'
unliquidata$province[unliquidata$lgu == 'Montevista' & is.na(unliquidata$province)] <- 'Compostela Valley'
unliquidata$province[unliquidata$lgu == 'Muntinlupa  City' & is.na(unliquidata$province)] <- 'Metro Manila'
unliquidata$province[unliquidata$lgu == 'Murcia' & is.na(unliquidata$province)] <- 'Negros Occidental'
unliquidata$province[unliquidata$lgu == 'Mutia' & is.na(unliquidata$province)] <- 'Zamboanga del Norte'
unliquidata$province[unliquidata$lgu == 'Naawan' & is.na(unliquidata$province)] <- 'Misamis Oriental'
unliquidata$province[unliquidata$lgu == 'Nabas' & is.na(unliquidata$province)] <- 'Aklan'
unliquidata$province[unliquidata$lgu == 'Narra' & is.na(unliquidata$province)] <- 'Palawan'
unliquidata$province[unliquidata$lgu == 'Nasipit' & is.na(unliquidata$province)] <- 'Agusan del Norte'
unliquidata$province[unliquidata$lgu == 'Naval' & is.na(unliquidata$province)] <- 'Biliran'
unliquidata$province[unliquidata$lgu == 'New  Lucena' & is.na(unliquidata$province)] <- 'Iloilo'
unliquidata$province[unliquidata$lgu == 'Norala' & is.na(unliquidata$province)] <- 'South Cotabato'
unliquidata$province[unliquidata$lgu == 'Norzagaray' & is.na(unliquidata$province)] <- 'Bulacan'
unliquidata$province[unliquidata$lgu == 'Noveleta' & is.na(unliquidata$province)] <- 'Cavite'
unliquidata$province[unliquidata$lgu == 'Numancia' & is.na(unliquidata$province)] <- 'Aklan'
unliquidata$province[unliquidata$lgu == 'Obando' & is.na(unliquidata$province)] <- 'Bulacan'
unliquidata$province[unliquidata$lgu == 'Odiongan' & is.na(unliquidata$province)] <- 'Romblon'
unliquidata$province[unliquidata$lgu == 'Olongapo  City' & is.na(unliquidata$province)] <- 'Zambales'
unliquidata$province[unliquidata$lgu == 'Oras' & is.na(unliquidata$province)] <- 'Eastern Samar'
unliquidata$province[unliquidata$lgu == 'Oroquieta  City' & is.na(unliquidata$province)] <- 'Misamis Occidental'
unliquidata$province[unliquidata$lgu == 'Oton' & is.na(unliquidata$province)] <- 'Iloilo'
unliquidata$province[unliquidata$lgu == 'Ozamiz  City' & is.na(unliquidata$province)] <- 'Misamis Occidental'
unliquidata$province[unliquidata$lgu == 'Pagadian  City' & is.na(unliquidata$province)] <- 'Zamboanga del Sur'
unliquidata$province[unliquidata$lgu == 'Pagsanghan' & is.na(unliquidata$province)] <- 'Samar'
unliquidata$province[unliquidata$lgu == 'Palayan  City' & is.na(unliquidata$province)] <- 'Nueva Ecija'
unliquidata$province[unliquidata$lgu == 'Palimbang' & is.na(unliquidata$province)] <- 'Sultan Kudarat'
unliquidata$province[unliquidata$lgu == 'Panaon' & is.na(unliquidata$province)] <- 'Misamis Occidental'
unliquidata$province[unliquidata$lgu == 'Pandi' & is.na(unliquidata$province)] <- 'Bulacan'
unliquidata$province[unliquidata$lgu == 'Pangil' & is.na(unliquidata$province)] <- 'Laguna'
unliquidata$province[unliquidata$lgu == 'Paranaque  City' & is.na(unliquidata$province)] <- 'Metro Manila'
unliquidata$province[unliquidata$lgu == 'Pasacao' & is.na(unliquidata$province)] <- 'Camarines Sur'
unliquidata$province[unliquidata$lgu == 'Pasay  City' & is.na(unliquidata$province)] <- 'Metro Manila'
unliquidata$province[unliquidata$lgu == 'Pasig  City' & is.na(unliquidata$province)] <- 'Metro Manila'
unliquidata$province[unliquidata$lgu == 'Passi  City' & is.na(unliquidata$province)] <- 'Iloilo'
unliquidata$province[unliquidata$lgu == 'Pateros' & is.na(unliquidata$province)] <- 'Metro Manila'
unliquidata$province[unliquidata$lgu == 'Patnanungan' & is.na(unliquidata$province)] <- 'Quezon'
unliquidata$province[unliquidata$lgu == 'Payao' & is.na(unliquidata$province)] <- 'Zamboanga Sibugay'
unliquidata$province[unliquidata$lgu == 'Pikit' & is.na(unliquidata$province)] <- 'Cotabato'
unliquidata$province[unliquidata$lgu == 'Pintuyan' & is.na(unliquidata$province)] <- 'Southern Leyte'
unliquidata$province[unliquidata$lgu == 'Polanco' & is.na(unliquidata$province)] <- 'Zamboanga del Norte'
unliquidata$province[unliquidata$lgu == 'Polangui' & is.na(unliquidata$province)] <- 'Albay'
unliquidata$province[unliquidata$lgu == 'Pototan' & is.na(unliquidata$province)] <- 'Iloilo'
unliquidata$province[unliquidata$lgu == 'Pres. Carlos P. Garcia' & is.na(unliquidata$province)] <- 'Bohol'
unliquidata$province[unliquidata$lgu == 'Puerto Galera' & is.na(unliquidata$province)] <- 'Oriental Mindoro'
unliquidata$province[unliquidata$lgu == 'Puerto Princesa  City' & is.na(unliquidata$province)] <- 'Palawan'
unliquidata$province[unliquidata$lgu == 'Pulupandan' & is.na(unliquidata$province)] <- 'Negros Occidental'
unliquidata$province[unliquidata$lgu == 'Quezon  City' & is.na(unliquidata$province)] <- 'Metro Manila'
unliquidata$province[unliquidata$lgu == 'Quinapondan' & is.na(unliquidata$province)] <- 'Eastern Samar'
unliquidata$province[unliquidata$lgu == 'Ragay' & is.na(unliquidata$province)] <- 'Camarines Sur'
unliquidata$province[unliquidata$lgu == 'Real' & is.na(unliquidata$province)] <- 'Quezon'
unliquidata$province[unliquidata$lgu == 'Reina Mercedes' & is.na(unliquidata$province)] <- 'Isabela'
unliquidata$province[unliquidata$lgu == 'Remedios T. Romualdez' & is.na(unliquidata$province)] <- 'Agusan del Norte'
unliquidata$province[unliquidata$lgu == 'Rodriguez' & is.na(unliquidata$province)] <- 'Rizal'
unliquidata$province[unliquidata$lgu == 'Romblon' & is.na(unliquidata$province)] <- 'Romblon'
unliquidata$province[unliquidata$lgu == 'Rosales' & is.na(unliquidata$province)] <- 'Pangasinan'
unliquidata$province[unliquidata$lgu == 'Roseller T. Lim' & is.na(unliquidata$province)] <- 'Zamboanga Sibugay'
unliquidata$province[unliquidata$lgu == 'Roxas  City' & is.na(unliquidata$province)] <- 'Capiz'
unliquidata$province[unliquidata$lgu == 'Sabangan' & is.na(unliquidata$province)] <- 'Mountain Province'
unliquidata$province[unliquidata$lgu == 'Sablayan' & is.na(unliquidata$province)] <- 'Occidental Mindoro'
unliquidata$province[unliquidata$lgu == 'Sabtang' & is.na(unliquidata$province)] <- 'Batanes'
unliquidata$province[unliquidata$lgu == 'Saint Bernard' & is.na(unliquidata$province)] <- 'Southern Leyte'
unliquidata$province[unliquidata$lgu == 'Salay' & is.na(unliquidata$province)] <- 'Misamis Oriental'
unliquidata$province[unliquidata$lgu == 'Sampaloc' & is.na(unliquidata$province)] <- 'Quezon'
unliquidata$province[unliquidata$lgu == 'San Benito' & is.na(unliquidata$province)] <- 'Surigao del Norte'
unliquidata$province[unliquidata$lgu == 'San Dionisio' & is.na(unliquidata$province)] <- 'Iloilo'
unliquidata$province[unliquidata$lgu == 'San Jorge' & is.na(unliquidata$province)] <- 'Samar'
unliquidata$province[unliquidata$lgu == 'San Jose del Monte' & is.na(unliquidata$province)] <- 'Bulacan'
unliquidata$province[unliquidata$lgu == 'San Juan  City' & is.na(unliquidata$province)] <- 'Metro Manila'
unliquidata$province[unliquidata$lgu == 'San Leonardo' & is.na(unliquidata$province)] <- 'Nueva Ecija'
unliquidata$province[unliquidata$lgu == 'San Pedro  City' & is.na(unliquidata$province)] <- 'Laguna'
unliquidata$province[unliquidata$lgu == 'San Ricardo' & is.na(unliquidata$province)] <- 'Southern Leyte'
unliquidata$province[unliquidata$lgu == 'Santa Rosa  City' & is.na(unliquidata$province)] <- 'Laguna'
unliquidata$province[unliquidata$lgu == 'Sapang Dalaga' & is.na(unliquidata$province)] <- 'Misamis Occidental'
unliquidata$province[unliquidata$lgu == 'SCM NE' & is.na(unliquidata$province)] <- 'Nueva Ecija'
unliquidata$province[unliquidata$lgu == 'Sen. Ninoy Aquino' & is.na(unliquidata$province)] <- 'Sultan Kudarat'
unliquidata$province[unliquidata$lgu == 'Sergio OsmeÃ±a' & is.na(unliquidata$province)] <- 'Zamboanga del Norte'
unliquidata$province[unliquidata$lgu == 'Sibalom' & is.na(unliquidata$province)] <- 'Antique'
unliquidata$province[unliquidata$lgu == 'Sibunag' & is.na(unliquidata$province)] <- 'Guimaras'
unliquidata$province[unliquidata$lgu == 'Sibutad' & is.na(unliquidata$province)] <- 'Zamboanga del Norte'
unliquidata$province[unliquidata$lgu == 'Sierra Bullones' & is.na(unliquidata$province)] <- 'Bohol'
unliquidata$province[unliquidata$lgu == 'Sigma' & is.na(unliquidata$province)] <- 'Capiz'
unliquidata$province[unliquidata$lgu == 'Silago' & is.na(unliquidata$province)] <- 'Southern Leyte'
unliquidata$province[unliquidata$lgu == 'Silay  City' & is.na(unliquidata$province)] <- 'Negros Occidental'
unliquidata$province[unliquidata$lgu == 'Sinacaban' & is.na(unliquidata$province)] <- 'Misamis Occidental'
unliquidata$province[unliquidata$lgu == 'Siniloan' & is.na(unliquidata$province)] <- 'Laguna'
unliquidata$province[unliquidata$lgu == 'Sipocot' & is.na(unliquidata$province)] <- 'Camarines Sur'
unliquidata$province[unliquidata$lgu == 'Sirawai' & is.na(unliquidata$province)] <- 'Zamboanga del Norte'
unliquidata$province[unliquidata$lgu == 'Siruma' & is.na(unliquidata$province)] <- 'Camarines Sur'
unliquidata$province[unliquidata$lgu == 'Sofronio EspaÃ±ola' & is.na(unliquidata$province)] <- 'Palawan'
unliquidata$province[unliquidata$lgu == 'Solana' & is.na(unliquidata$province)] <- 'Cagayan'
unliquidata$province[unliquidata$lgu == 'Sorsogon  City' & is.na(unliquidata$province)] <- 'Sorsogon'
unliquidata$province[unliquidata$lgu == 'Sta. Josefa' & is.na(unliquidata$province)] <- 'Agusan del Sur'
unliquidata$province[unliquidata$lgu == 'Sta. Magdalena' & is.na(unliquidata$province)] <- 'Sorsogon'
unliquidata$province[unliquidata$lgu == 'Sta. Monica' & is.na(unliquidata$province)] <- 'Surigao del Norte'
unliquidata$province[unliquidata$lgu == 'Sta. Rita' & is.na(unliquidata$province)] <- 'Samar'
unliquidata$province[unliquidata$lgu == 'Subic' & is.na(unliquidata$province)] <- 'Zambales'
unliquidata$province[unliquidata$lgu == 'Sulop' & is.na(unliquidata$province)] <- 'Davao del Sur'
unliquidata$province[unliquidata$lgu == 'Sultan Dumalondong' & is.na(unliquidata$province)] <- 'Lanao del Sur'
unliquidata$province[unliquidata$lgu == 'Surigao  City' & is.na(unliquidata$province)] <- 'Surigao del Norte'
unliquidata$province[unliquidata$lgu == 'Taal' & is.na(unliquidata$province)] <- 'Batangas'
unliquidata$province[unliquidata$lgu == 'Tabaco Cty' & is.na(unliquidata$province)] <- 'Albay'
unliquidata$province[unliquidata$lgu == 'Tabina' & is.na(unliquidata$province)] <- 'Zamboanga del Sur'
unliquidata$province[unliquidata$lgu == 'Tacloban  City' & is.na(unliquidata$province)] <- 'Leyte'
unliquidata$province[unliquidata$lgu == 'Taganaan' & is.na(unliquidata$province)] <- 'Surigao del Norte'
unliquidata$province[unliquidata$lgu == 'Tagapul-an' & is.na(unliquidata$province)] <- 'Samar'
unliquidata$province[unliquidata$lgu == 'Tagaytay  City' & is.na(unliquidata$province)] <- 'Cavite'
unliquidata$province[unliquidata$lgu == 'Tagbilaran  City' & is.na(unliquidata$province)] <- 'Bohol'
unliquidata$province[unliquidata$lgu == 'Tagkawayan' & is.na(unliquidata$province)] <- 'Quezon'
unliquidata$province[unliquidata$lgu == 'Taguig  City' & is.na(unliquidata$province)] <- 'Metro Manila'
unliquidata$province[unliquidata$lgu == 'Tagum  City' & is.na(unliquidata$province)] <- 'Davao del Norte'
unliquidata$province[unliquidata$lgu == 'Talisayan' & is.na(unliquidata$province)] <- 'Misamis Oriental'
unliquidata$province[unliquidata$lgu == 'Talitay' & is.na(unliquidata$province)] <- 'Maguindanao'
unliquidata$province[unliquidata$lgu == 'Talusan' & is.na(unliquidata$province)] <- 'Zamboanga Sibugay'
unliquidata$province[unliquidata$lgu == 'Tampakan' & is.na(unliquidata$province)] <- 'South Cotabato'
unliquidata$province[unliquidata$lgu == 'Tanauan' & is.na(unliquidata$province)] <- 'Leyte'
unliquidata$province[unliquidata$lgu == 'Tandag  City' & is.na(unliquidata$province)] <- 'Surigao del Sur'
unliquidata$province[unliquidata$lgu == 'Tangalan' & is.na(unliquidata$province)] <- 'Aklan'
unliquidata$province[unliquidata$lgu == 'Tapaz' & is.na(unliquidata$province)] <- 'Capiz'
unliquidata$province[unliquidata$lgu == 'Tarlac  City' & is.na(unliquidata$province)] <- 'Tarlac'
unliquidata$province[unliquidata$lgu == 'Tarragona' & is.na(unliquidata$province)] <- 'Davao Oriental'
unliquidata$province[unliquidata$lgu == 'Teresa' & is.na(unliquidata$province)] <- 'Rizal'
unliquidata$province[unliquidata$lgu == 'Ternate' & is.na(unliquidata$province)] <- 'Cavite'
unliquidata$province[unliquidata$lgu == 'Tigbauan' & is.na(unliquidata$province)] <- 'Iloilo'
unliquidata$province[unliquidata$lgu == 'Toboso' & is.na(unliquidata$province)] <- 'Negros Occidental'
unliquidata$province[unliquidata$lgu == 'Toledo  City' & is.na(unliquidata$province)] <- 'Cebu'
unliquidata$province[unliquidata$lgu == 'Tolosa' & is.na(unliquidata$province)] <- 'Leyte'
unliquidata$province[unliquidata$lgu == 'Tomas Oppus' & is.na(unliquidata$province)] <- 'Southern Leyte'
unliquidata$province[unliquidata$lgu == 'Trece Martires  City' & is.na(unliquidata$province)] <- 'Cavite'
unliquidata$province[unliquidata$lgu == 'Trento' & is.na(unliquidata$province)] <- 'Agusan del Sur'
unliquidata$province[unliquidata$lgu == 'Trinidad' & is.na(unliquidata$province)] <- 'Bohol'
unliquidata$province[unliquidata$lgu == 'Tubajon' & is.na(unliquidata$province)] <- 'Dinagat Islands'
unliquidata$province[unliquidata$lgu == 'Tubay' & is.na(unliquidata$province)] <- 'Agusan del Norte'
unliquidata$province[unliquidata$lgu == 'Tuburan' & is.na(unliquidata$province)] <- 'Cebu'
unliquidata$province[unliquidata$lgu == 'Tuguegarao  City' & is.na(unliquidata$province)] <- 'Cagayan'
unliquidata$province[unliquidata$lgu == 'Tukuran' & is.na(unliquidata$province)] <- 'Zamboanga del Sur'
unliquidata$province[unliquidata$lgu == 'Tulunan' & is.na(unliquidata$province)] <- 'Cotabato'
unliquidata$province[unliquidata$lgu == 'Tunga' & is.na(unliquidata$province)] <- 'Leyte'
unliquidata$province[unliquidata$lgu == 'Tungawan' & is.na(unliquidata$province)] <- 'Zamboanga Sibugay'
unliquidata$province[unliquidata$lgu == 'Ubay' & is.na(unliquidata$province)] <- 'Bohol'
unliquidata$province[unliquidata$lgu == 'Unisan' & is.na(unliquidata$province)] <- 'Quezon'
unliquidata$province[unliquidata$lgu == 'Urdaneta  City' & is.na(unliquidata$province)] <- 'Pangasinan'
unliquidata$province[unliquidata$lgu == 'Uson' & is.na(unliquidata$province)] <- 'Masbate'
unliquidata$province[unliquidata$lgu == 'Uyugan' & is.na(unliquidata$province)] <- 'Batanes'
unliquidata$province[unliquidata$lgu == 'Valencia  City' & is.na(unliquidata$province)] <- 'Bukidnon'
unliquidata$province[unliquidata$lgu == 'Valenzuela  City' & is.na(unliquidata$province)] <- 'Metro Manila'
unliquidata$province[unliquidata$lgu == 'Valladolid' & is.na(unliquidata$province)] <- 'Negros Occidental'
unliquidata$province[unliquidata$lgu == 'Victorias  City' & is.na(unliquidata$province)] <- 'Negros Occidental'
unliquidata$province[unliquidata$lgu == 'Villaverde' & is.na(unliquidata$province)] <- 'Nueva Vizcaya'
unliquidata$province[unliquidata$lgu == 'Virac' & is.na(unliquidata$province)] <- 'Catanduanes'
unliquidata$province[unliquidata$lgu == 'Wao' & is.na(unliquidata$province)] <- 'Lanao del Sur'
unliquidata$province[unliquidata$lgu == 'Zamboanga  City' & is.na(unliquidata$province)] <- 'Zamboanga del Sur'
unliquidata$province[unliquidata$lgu == 'Zamboanguita' & is.na(unliquidata$province)] <- 'Negros Oriental'
unliquidata$province[unliquidata$lgu == 'Isabela City'] <- 'Basilan'

#cleaning dataset: cleaning province _ values, systemic changes
unliquidata$province[unliquidata$lgu == 'Isabel' & unliquidata$province == '_'] <- 'Leyte'

#cleaning dataset: cleaning province blank values, systemic changes
unliquidata$province[unliquidata$lgu == 'Guinayangan' & unliquidata$province == ''] <- 'Quezon'
unliquidata$province[unliquidata$lgu == 'Uyugan' & unliquidata$province == ''] <- 'Batanes'

#cleaning dataset: cleaning province wrong spell values, idiosyncratic changes
unliquidata$province[unliquidata$province == 'occmdo'] <- 'Occidental Mindoro'
unliquidata$province[unliquidata$province == 'do'] <- 'Davao Oriental'
unliquidata$province[unliquidata$province == 'South Cotabatoabato'] <- 'South Cotabato'
unliquidata$province[unliquidata$province == 'IlocosSur'] <- 'Ilocos Sur'
unliquidata$province[unliquidata$province == 'Mountain Provinceince'] <- 'Mountain Province'
unliquidata$province[unliquidata$province == 'Rombon'] <- 'Romblon'
unliquidata$province[unliquidata$province == 'NegsOcc'] <- 'Negros Occidental'
unliquidata$province[unliquidata$province == 'Catandunaes'] <- 'Catanduanes'
unliquidata$province[unliquidata$province == 'C'] <- 'Cagayan'

#cleaning dataset: cleaning lgu wrong spell values, idiosyncratic changes
unliquidata$lgu[unliquidata$lgu == 'Cagayande Oro  City'] <- 'Cagayan de Oro  City'
unliquidata$lgu[unliquidata$lgu == 'Baybay'] <- 'Baybay  City'
unliquidata$lgu[unliquidata$lgu == 'Loay Bohol'] <- 'Loay'
unliquidata$lgu[unliquidata$lgu == 'Tabaco Cty'] <- 'Tabaco City'
unliquidata$lgu[unliquidata$lgu == 'Sofronio EspaÃ±ola'] <- 'Sofronio Española'
unliquidata$lgu[unliquidata$lgu == 'Sofronio'] <- 'Sofronio Española'
unliquidata$lgu[unliquidata$lgu == 'City of DasmariÃ±as'] <- 'City of Dasmariñas'
unliquidata$lgu[unliquidata$lgu == 'DoÃ±a Remedios Trinidad'] <- 'Doña Remedios Trinidad'
unliquidata$lgu[unliquidata$lgu == 'PeÃ±ablanca'] <- 'Peñablanca'
unliquidata$lgu[unliquidata$lgu == 'PeÃ±aranda'] <- 'Peñaranda'
unliquidata$lgu[unliquidata$lgu == 'PeÃ±arrubia'] <- 'Peñarrubia'
unliquidata$lgu[unliquidata$lgu == 'PiÃ±an'] <- 'Piñan'
unliquidata$lgu[unliquidata$lgu == 'SagÃ±ay'] <- 'Sagñay'
unliquidata$lgu[unliquidata$lgu == 'Science City of MuÃ±oz'] <- 'Science City of Muñoz'
unliquidata$lgu[unliquidata$lgu == 'Sergio OsmeÃ±a'] <- 'Sergio Osmeña'
unliquidata$lgu[unliquidata$lgu == 'Sto. NiÃ±o'] <- 'Sto. Niño'
unliquidata$lgu[unliquidata$lgu == 'Tabaco'] <- 'Tabaco City'
unliquidata$lgu[unliquidata$lgu == 'Silvino Lubos'] <- 'Silvino Lobos'
unliquidata$lgu[unliquidata$lgu == 'SCM NE'] <- 'Science City of Muñoz'
unliquidata$lgu[unliquidata$lgu == 'Lilo-an'] <- 'Liloan'

#cleaning dataset: cleaning lgu na values, systemic changes after special character correction
unliquidata$province[unliquidata$lgu == 'Sofronio Española' & is.na(unliquidata$province)] <- 'Palawan'
unliquidata$province[unliquidata$lgu == 'City of Dasmariñas' & is.na(unliquidata$province)] <- 'Cavite'
unliquidata$province[unliquidata$lgu == 'Sergio Osmeña' & is.na(unliquidata$province)] <- 'Zamboanga del Norte'

#cleaning dataset: removing double spaces
unliquidata <- unliquidata %>%
  mutate(lgu = str_replace(lgu, '  ', ' '))

#cleaning dataset: cleaning province values, idiosyncratic changes
unliquidata[69, 2] <- 'Leyte'
unliquidata[495, 2] <- 'Aklan'
unliquidata[496, 2] <- 'Aklan'
unliquidata[841, 2] <- 'Guimaras'
unliquidata[899, 2] <- 'Surigao del Norte'
unliquidata[1034, 2] <- 'Negros Occidental'
unliquidata[1321, 2] <- 'Iloilo'
unliquidata[1716, 2] <- 'Masbate'
unliquidata[2287, 2] <- 'Leyte'
unliquidata[2667, 2] <- 'Quezon'
unliquidata[2718, 2] <- 'Southern Leyte'
unliquidata[2742, 2] <- 'Bohol'
unliquidata[2792, 2] <- 'Agusan del Norte'
unliquidata[3278, 2] <- 'Cebu'
unliquidata[3496, 2] <- 'Negros Oriental'
unliquidata[3727, 2] <- 'Masbate'
unliquidata[4053, 2] <- 'Quezon'
unliquidata[4059, 2] <- 'Iloilo'
unliquidata[4060, 2] <- 'Romblon'
unliquidata[4061, 2] <- 'Southern Leyte'
unliquidata[4062, 2] <- 'Quezon'
unliquidata[4065, 2] <- 'Southern Leyte'
unliquidata[4068, 2] <- 'Agusan del Sur'
unliquidata[4069, 2] <- 'Zamboanga del Sur'
unliquidata[4175, 2] <- 'Pampanga'
unliquidata[4506, 2] <- 'Isabela'
unliquidata[4755, 2] <- 'Leyte'
unliquidata[4830, 2] <- 'Sto. Niño'

#cleaning dataset: cleaning lgu values, idiosyncratic changes
unliquidata[71, 1] <- 'Albuquerque'
unliquidata[72, 1] <- 'Alcala'
unliquidata[73, 1] <- 'Alcala'
unliquidata[74, 1] <- 'Alcala'
unliquidata[77, 1] <- 'Alcala'
unliquidata[79, 1] <- 'Alcantara'
unliquidata[2653, 1] <- 'Loreto'
unliquidata[2667, 1] <- 'Lucena City'
unliquidata[2668, 1] <- 'Lugait'
unliquidata[2670, 1] <- 'Luisiana'
unliquidata[2671, 1] <- 'Luisiana'
unliquidata[2673, 1] <- 'Lumban'
unliquidata[2678, 1] <- 'Lumbatan'
unliquidata[2679, 1] <- 'Lumbayanague'
unliquidata[2681, 1] <- 'Luna'
unliquidata[2682, 1] <- 'Luna'
unliquidata[2685, 1] <- 'Luna'
unliquidata[2686, 1] <- 'Lupao'
unliquidata[2687, 1] <- 'Lupao'
unliquidata[2689, 1] <- 'Lupao'
unliquidata[2691, 1] <- 'Lupao'
unliquidata[2695, 1] <- 'Lupi'
unliquidata[2705, 1] <- 'Lutayan'
unliquidata[4830, 1] <- 'Cagayan'

#cleaning dataset: cleaning year values, idiosyncratic changes
unliquidata[311, 3] <- 2013
unliquidata[1389, 3] <- 2017
unliquidata[1959, 3] <- 2015
unliquidata[3621, 3] <- 2011
unliquidata[4141, 3] <- 2013
unliquidata[5343, 3] <- 2017

#final pass of cleaning
unliquidata[124, 2] <- 'Zamboanga Sibugay'
unliquidata[124, 3] <- 2019
unliquidata[633, 2] <- 'Masbate'
unliquidata[1577, 2] <- 'Eastern Samar'
unliquidata[2286, 2] <- 'Negros Oriental'
unliquidata[2500, 2] <- 'Iloilo'
unliquidata[2574, 2] <- 'Southern Leyte'
unliquidata[2635, 2] <- 'Romblon'
unliquidata[2654, 2] <- 'Agusan del Sur'
unliquidata[2680, 2] <- 'Lanao del Sur'
unliquidata[2706, 2] <- 'Cotabato'
unliquidata[2793, 2] <- 'Sorsogon'
unliquidata[3279, 2] <- 'Camarines Sur'
unliquidata[3280, 2] <- 'Cebu'
unliquidata[3280, 2] <- 'Cebu'
unliquidata[3715, 2] <- 'Zamboanga del Sur'
unliquidata[3795, 2] <- 'Capiz'
unliquidata[3852, 2] <- 'Bukidnon'
unliquidata[3853, 2] <- 'Isabela'
unliquidata[3979, 2] <- 'Isabela'
unliquidata[4052, 2] <- 'Surigao del Sur'
unliquidata[4056, 2] <- 'Negros Occidental'
unliquidata[4058, 2] <- 'Negros Occidental'
unliquidata[4063, 2] <- 'Masbate'
unliquidata[4174, 2] <- 'Pampanga'
unliquidata[4504:4505, 2] <- 'Iloilo'
unliquidata[4535, 2] <- 'Agusan del Norte'
unliquidata[4535, 3] <- 2019
unliquidata[4981:4982, 2] <- 'Lanao del Norte'
unliquidata[996, 2] <- 'Misamis Oriental'
unliquidata[2060:2061, 2] <- 'Negros Occidental'
unliquidata[431, 3] <- 2013
unliquidata[1049, 3] <- 2013
unliquidata[1248, 3] <- 2015
unliquidata[2052, 3] <- 2017
unliquidata[2706, 3] <- 2020
unliquidata[3092, 3] <- 2010
unliquidata <- unliquidata[-c(3420, 3831, 5440), ]

#writing out and saving: cleaning on september 23
options(scipen=999)
write.csv(unliquidata, "unliquidata.csv", row.names=FALSE)