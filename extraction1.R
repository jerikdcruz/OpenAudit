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
unliquidata[65:79, 1] <- "Alang Alang"
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
unliquidata[2706:2643, 1] <- 'Maelang' #review
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