###Disciplina de Ciencia Colaborativa###

#### ATIVIDADE FINAL - FRANCA AUSTRAL ####

###CARREGAR PACOTES###
library(tidyverse)
##Instalando pacote rgbif##
library(rgbif)

##GBIF##
# baixar ocorrencias
franca_gbif <- occ_data(scientificName = "Eubalena australis", 
                       hasCoordinate = TRUE,
                       hasGeospatialIssue=FALSE)

                   
library(dplyr)

## PLotar ocorrencias
library(dplyr)
library(ggmap)
library(maps)
library(mapdata)

### extrair dados do OBIS
library(robis)

# baixar ocorrências
franca_obis <- robis::occurrence("Eubalaena australis")
#Retrieved 17131 records of approximately 17131 (100%)

# checar dados
names(franca_obis)
[1] "country"                       "date_year"                    
[3] "scientificNameID"              "scientificName"               
[5] "superfamilyid"                 "individualCount"              
[7] "dropped"                       "aphiaID"                      
[9] "decimalLatitude"               "subclassid"                   
[11] "phylumid"                      "familyid"                     
[13] "catalogNumber"                 "occurrenceStatus"             
[15] "basisOfRecord"                 "terrestrial"                  
[17] "taxonConceptID"                "superclass"                   
[19] "modified"                      "id"                           
[21] "order"                         "superclassid"                 
[23] "infraorderid"                  "dataset_id"                   
[25] "decimalLongitude"              "date_end"                     
[27] "speciesid"                     "occurrenceID"                 
[29] "superfamily"                   "suborderid"                   
[31] "date_start"                    "genus"                        
[33] "eventDate"                     "brackish"                     
[35] "absence"                       "taxonRank"                    
[37] "genusid"                       "originalScientificName"       
[39] "marine"                        "subphylumid"                  
[41] "vernacularName"                "date_mid"                     
[43] "class"                         "suborder"                     
[45] "infraorder"                    "orderid"                      
[47] "footprintWKT"                  "geodeticDatum"                
[49] "kingdom"                       "specificEpithet"              
[51] "recordedBy"                    "classid"                      
[53] "phylum"                        "species"                      
[55] "coordinatePrecision"           "subphylum"                    
[57] "subclass"                      "family"                       
[59] "kingdomid"                     "node_id"                      
[61] "flags"                         "sss"                          
[63] "shoredistance"                 "sst"                          
[65] "bathymetry"                    "associatedReferences"         
[67] "type"                          "taxonRemarks"                 
[69] "recordNumber"                  "georeferencedDate"            
[71] "verbatimEventDate"             "collectionCode"               
[73] "license"                       "dateIdentified"               
[75] "ownerInstitutionCode"          "bibliographicCitation"        
[77] "scientificNameAuthorship"      "coordinateUncertaintyInMeters"
[79] "institutionCode"               "identificationRemarks"        
[81] "nomenclaturalCode"             "datasetName"                  
[83] "taxonomicStatus"               "waterBody"                    
[85] "datasetID"                     "occurrenceRemarks"            
[87] "organismQuantity"              "locality"                     
[89] "organismQuantityType"          "year"                         
[91] "maximumDepthInMeters"          "verbatimLatitude"             
[93] "organismID"                    "samplingProtocol"             
[95] "minimumDepthInMeters"          "verbatimLongitude"            
[97] "depth"                         "stateProvince"                
[99] "sex"                           "references"                   
[101] "fieldNotes"                    "day"                          
[103] "month"                         "rightsHolder"                 
[105] "eventTime"                     "associatedMedia"              
[107] "taxonID"                       "countryCode"                  
[109] "lifeStage"                     "locationID"                   
[111] "fieldNumber"                   "identifiedBy"                 
[113] "otherCatalogNumbers"           "organismRemarks"              
[115] "verbatimLocality"              "identificationID"             
[117] "informationWithheld"           "accessRights"                 
[119] "eventRemarks"                  "dynamicProperties"            
[121] "georeferenceRemarks"           "minimumElevationInMeters"     
[123] "maximumElevationInMeters"      "language"                     
[125] "eventID"                       "startDayOfYear"               
[127] "parentEventID"


# check problemas reportados (flags)
franca_obis1 %>% 
  distinct(flags)
# A tibble: 13 x 1
flags                     
<chr>                     
1 no_depth                  
2 NO_DEPTH                  
3 DEPTH_EXCEEDS_BATH        
4 NO_DEPTH,ON_LAND          
5 NA                        
6 no_depth,on_land          
7 ON_LAND                   
8 on_land                   
9 ON_LAND,DEPTH_EXCEEDS_BATH
10 on_land,no_depth          
11 depth_exceeds_bath,on_land
12 on_land,depth_exceeds_bath
13 ON_LAND,NO_DEPTH 

#check NA em datasetName
franca_obis1 %>% 
  filter(!flags %in% c("ON_LAND,NO_DEPTH", "no_depth,on_land", "depth_exceeds_bath,on_land", "on_land,no_depth", "on_land", "on_land,depth_exceeds_bath", "ON_LAND", "ON_LAND,DEPTH_EXCEEDS_BATH", "NA"),
         is.na(datasetName)) %>% 
  distinct(waterBody)
# A tibble: 12 x 1
waterBody           
<chr>               
1 NA                  
2 South Atlantic Ocean
3 Southern Ocean      
4 Indian              
5 Atlantic            
6 Sub-Antarctic waters
7 Antartic waters     
8 Patagonian shelf    
9 South Pacific Ocean 
10 Atlantic Ocean      
11 Indian Ocean        
12 San Jorge, Gulf


##Selecionar variáveis
franca_obis1 <- franca_obis %>% 
  dplyr::select(scientificName, decimalLatitude, decimalLongitude, bathymetry,
                flags, waterBody, basisOfRecord, occurrenceStatus, rightsHolder,
                datasetName, recordedBy, depth, locality, date_year, individualCount, year, sex) %>%
  distinct()

#depth ok
franca_obis1 %>% 
  filter(!flags %in% c("ON_LAND,NO_DEPTH", "no_depth,on_land", "depth_exceeds_bath,on_land", "on_land,no_depth", "on_land", "on_land,depth_exceeds_bath", "ON_LAND", "ON_LAND,DEPTH_EXCEEDS_BATH", "NA"),
         !is.na(datasetName),
         !waterBody %in% c()) %>%
  ggplot(aes(x = depth, fill = waterBody)) +
  geom_histogram() 

# checar niveis
franca_obis1 %>% 
  filter(!flags %in% c("ON_LAND,NO_DEPTH","no_depth,on_land","depth_exceeds_bath,on_land","on_land,no_depth", "on_land", "on_land,depth_exceeds_bath", "ON_LAND", "ON_LAND,DEPTH_EXCEEDS_BATH"),
         !is.na(datasetName),
         !waterBody %in% c("world",NA)) %>% 
  lapply(., unique)

# 
franca_obis_ok <- franca_obis1 %>% 
  filter(!flags %in% c("ON_LAND,NO_DEPTH","no_depth,on_land","depth_exceeds_bath,on_land","on_land,no_depth", "on_land", "on_land,depth_exceeds_bath", "ON_LAND", "ON_LAND,DEPTH_EXCEEDS_BATH"),
         !is.na(datasetName),
         !waterBody %in% c("world",NA))


world <- map_data('world')

# plotar mapa franca obis 1
ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group)) +
  coord_fixed() +
  theme_classic() +
  geom_point(data = franca_obis1, aes(x = decimalLongitude, y = decimalLatitude, color = waterBody)) +
  #theme(legend.title = element_blank()) +
  labs(x = "longitude", y = "latitude", title = expression(italic("Eubalaena australis")))

# plotar mapa franca obis ok
ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group)) +
  coord_fixed() +
  theme_classic() +
  geom_point(data = franca_obis_ok, aes(x = decimalLongitude, y = decimalLatitude, color = waterBody)) +
  #theme(legend.title = element_blank()) +
  labs(x = "longitude", y = "latitude", title = expression(italic("Eubalaena australis")))



### FERRAMENTAS EXTRAS - GRAFICOS INTERATIVOS ###
library(tidyverse)
library(rgbif)

# ocorrencias
corvus_gbif <- occ_data(scientificName = "Corvus", 
                        hasCoordinate = TRUE,
                        hasGeospatialIssue = FALSE)
# checar issues
issues_gbif <- corvus_gbif$data$issues %>% 
  unique() %>% 
  strsplit(., "[,]") %>% 
  unlist()

gbif_issues() %>% 
  data.frame() %>% 
  filter(code %in% issues_gbif)

# selecionar variaveis
corvus <- corvus_gbif$data %>%
  dplyr::select(scientificName, decimalLatitude, decimalLongitude) %>% 
  distinct()

library(leaflet)

# conferir no mapa
corvus %>% 
  leaflet() %>% 
  addTiles() %>% 
  addMarkers(~decimalLongitude,
             ~decimalLatitude)

pal <- colorFactor(palette = "viridis", domain = unique(corvus$scientificName))

corvus %>% 
  leaflet() %>% 
  addTiles() %>% 
  addCircleMarkers(~decimalLongitude,
                   ~decimalLatitude,
                   radius = 5,
                   label = ~as.character(scientificName),
                   color = ~pal(corvus$scientificName),
                   stroke = FALSE, fillOpacity = 0.5) %>% 
  addLegend('bottomright', 
            colors = unique(pal(corvus$scientificName)), 
            labels = unique(corvus$scientificName),
            title = 'Espécie',
            opacity = 0.5)

## PLOTLY ##

corvus %>% 
  mutate(lat = round(decimalLatitude)) %>% 
  group_by(lat, scientificName) %>%
  summarise(occ = length(scientificName)) %>%
  ggplot(aes(y = occ, x = lat, color = scientificName)) +
  geom_point() +
  geom_smooth() +
  theme_classic() +
  labs(x = "latitude", y = 'ocorrências')

library("plotly")
install.packages("plotly")
cc <- corvus %>% 
  mutate(lat = round(decimalLatitude)) %>% 
  group_by(lat, scientificName) %>%
  summarise(occ = length(scientificName)) %>%
  ggplot(aes(y = occ, x = lat, color = scientificName)) +
  geom_point() +
  geom_smooth() +
  theme_classic() +
  labs(x = "latitude", y = 'ocorrências')

ggplotly(cc)


## Salvar dados da franca obtidos no OBIS

write.csv(franca_obis,"D:/Doutorado_PPGERN/Dados_obis_geral.csv", row.names = FALSE)
write.csv(franca_obis1,"D:/Doutorado_PPGERN/Dados_obis_big.csv", row.names = FALSE)
write.csv(franca_obis2,"D:/Doutorado_PPGERN/Dados_obis.csv", row.names = FALSE)
teste <- read.csv("D:/Doutorado_PPGERN/Dados_obis.csv", header=TRUE)
Dados Salvos com sucesso

?rm (função para excluir planilhas e dados do ambiente trabalhado)
rm(teste)
?qlcMatrix

## classificação automática de pontos; pacote obistools


install.packages("devtools")
library("devtools")
devtools::install_github("iobis/obistools")

install.packages(obistools)
library(obistools)

# funcao para classificar ocorrencias suspeitas
 flag_outlier <- function(df, species){
  
  franca_obis1 <- df %>% 
    dplyr::filter(scientificName == species); 
  
  franca_obis2 <- geosphere::distVincentyEllipsoid(
    franca_obis1 %>%
      summarise(centr_lon = median(decimalLongitude),
                centr_lat = median(decimalLatitude)),
    franca_obis1 %>% 
      dplyr::select(decimalLongitude, decimalLatitude)
  ) %>% 
    bind_cols(franca_obis1) %>% 
    rename(dist_centroid = '...1') %>% 
    mutate(flag = ifelse(dist_centroid < quantile(dist_centroid, probs = 0.9), "OK",
                         ifelse(dist_centroid >= quantile(dist_centroid, probs = 0.90) & dist_centroid < quantile(dist_centroid, probs = 0.95), "check > Q90",
                                ifelse(dist_centroid >= quantile(dist_centroid, probs = 0.95), "check > Q95", "OK"))))
  print(franca_obis2)
 }
#não consegui aplicar esta função
 
 
## grafico interativo mais bonito e com os numeros referentes a cada dado

 pal <- colorFactor(palette = "viridis", domain = unique(individualCount))
 
 plot_map_leaflet(franca_obis2) %>%
   addCircleMarkers(~decimalLongitude,
                   ~decimalLatitude,
                   radius = 4,
                   label = ~as.character(date_year),
                   stroke = FALSE, fillOpacity = 0.4)
   addLegend('bottomright', 
           colors = unique(date_year), 
           labels = unique(date_year),
           title = 'Espécie',
           opacity = 0.5)
   
   
   ??addLegend
  
## Funcionou a junção de duas formas de aplicação de dados. Utilizei a função "plot_map_leaflet" e alterei algumas caracteristicas. Agora posso definir qual variável pode ficar visivel no mapa.
 




## Optei por NÃO fazer essas classificações automaticas para os meus dados. 


## Criar gráficos com plotly

franca_obis2 %>% 
  mutate(lat = round(decimalLatitude)) %>% 
  group_by(lat, scientificName) %>%
  summarise(occ = length(scientificName)) %>%
  ggplot(aes(y = occ, x = lat, color = scientificName)) +
  geom_point() +
  geom_smooth() +
  theme_classic() +
  labs(x = "latitude", y = 'ocorrências')








