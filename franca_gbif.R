## DADOS DE FRANCA NO GBIF ##
###Disciplina de Ciencia Colaborativa###

#### ATIVIDADE FINAL - FRANCA AUSTRAL ####

###CARREGAR PACOTES###
library(tidyverse)
##Instalando pacote rgbif##
library(rgbif)

##GBIF##
# baixar ocorrencias
franca_gbif <- occ_data(scientificName = "Eubalaena australis", 
                        hasCoordinate = TRUE,
                        hasGeospatialIssue=FALSE)
## dimensoes

dim(franca_gbif)

dim(franca_gbif$data)

# checar campos
franca_gbif$data %>% names

# Checar issus

gbif_issues()

# checar os issues individuais da espécie
issues_gbif <- franca_gbif$data$issues %>%
  # unique() %>%
  strsplit(.,"[,]") %>%
  unlist() %>%
  unique()

issues_gbif

# selecionar campos de interesse


franca_gbif1 <- franca_gbif$data %>%
  dplyr::select(scientificName, decimalLatitude, decimalLongitude,issues,
                basisOfRecord, sex, rightsHolder,continent, year, month, day, 
                datasetName, recordedBy, waterBody, country, iucnRedListCategory, countryCode, depth)


# ocorrencias únicas
franca_gbif1 <- franca_gbif1 %>% 
  distinct()

# checar niveis dos fatores
lapply(franca_gbif1, unique)

# investigar niveis suspeitos
franca_gbif1 %>% 
  distinct(country) %>% 
  pull()

# country
franca_gbif1 %>%
  group_by(country) %>% 
  summarise(occ = length(scientificName)) %>% 
  ggplot(aes(occ, y=country)) +
  geom_bar(stat = 'identity')

## PLotar ocorrencias

library(ggmap)
library(maps)
library(mapdata)

# checar pontos
ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group)) +
  coord_fixed() +
  theme_classic() +
  geom_point(data = franca_gbif1,aes(x = decimalLongitude, y = decimalLatitude), color = "red") +
  labs(x = "longitude", y = "latitude", title = expression(italic("Eubalaena australis")))

## Checar niveis suspeitos de recordBy
# recordedBy
franca_gbif1 %>%
  group_by(datasetName) %>% 
  summarise(occ = length(scientificName)) %>% 
  ggplot(aes(occ, y=datasetName)) +
  geom_bar(stat = 'identity') 

# fonte das regioes erradas
franca_gbif1 %>% 
  filter(recordedBy %in% c("covegirl", "possumpete")) %>% 
  distinct(datasetName)

franca_gbif1 %>% 
  filter(datasetName %in% c("iNaturalist research-grade observations"))
## As observações deste dataset representam mais da metade dos dados. Não irei excluir nada.

# checar profundidade
franca_gbif1 %>% 
  ggplot(aes(x = year, fill = country)) +
  geom_histogram()
## grafico interativo mais bonito e com os numeros referentes a cada dado

pal <- colorFactor(palette = "viridis", domain = unique(franca_gbif1$year))

plot_map_leaflet(franca_gbif1) %>%
  addCircleMarkers(~decimalLongitude,
                   ~decimalLatitude,
                   radius = 4,
                   label = ~as.character(waterBody),
                   color = ~pal(franca_gbif1$year),
                   stroke = FALSE, fillOpacity = 0.4)%>%
  addLegend('bottomright', 
            colors = unique(pal(franca_gbif1$year)), 
            labels = unique(franca_gbif1$year),
            title = 'Ocorrências',
            opacity = 0.5)
## Seria interessante unificar as duas ocorrencias agora. 

# unir GBIF e OBIS

# ver diferencas
setdiff(names(franca_gbif1), names(franca_obis2))

setdiff(names(franca_obis2), names(franca_gbif1))

??separete
# juntar os dados
franca_big <- bind_rows(franca_gbif1 %>% 
                        mutate(repo = paste0("gbif", row.names(.))), 
                      franca_obis2 %>% 
                        mutate(repo = paste0("obis", row.names(.)))) %>%
  column_to_rownames("repo") %>% 
  dplyr::select(decimalLongitude, decimalLatitude, depth) %>% 
  distinct() %>% 
  rownames_to_column("occ") %>% 
  separate(col = "occ", into = c("datasetName", "rn"), sep = 4) %>%
  mutate(scientificName = "Eubalaena australis") %>% 
  dplyr::select(-rn)

# mapear ocorrencias
ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group)) +
  coord_fixed() +
  theme_classic() +
  geom_point(data = franca_big, aes(x = decimalLongitude, y = decimalLatitude, color = datasetName)) +
  #theme(legend.title = element_black()) +
  labs(x = "longitude", y = "latitude", title = expression(italic("Eubalaena australis")))
