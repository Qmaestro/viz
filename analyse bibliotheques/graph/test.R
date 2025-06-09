############ Analyse des bibliothËques ############

rm(list = ls()) 
cat("\014")  #Effacer la console

package <- c("tidyverse", "broom","chron", "readxl", "rvest", "lubridate", "sf","jsonlite", "santoku", "scales", "esquisse", "httr", "openxlsx", "janitor")
lapply(package, require, character.only = TRUE)
rm(package)

options( "digits"=2, "scipen"=100) 




############################## INTRODUCTIION ################################### 


### L'article du Monde clame l'idÈe d'un rÈseau de bibliothËque unique au monde, et un accËs ‡ la lecture gratuit.
### https://www.lemonde.fr/idees/article/2025/05/31/le-livre-d-occasion-est-en-train-de-cannibaliser-en-silence-toute-la-chaine-du-livre_6609480_3232.html
### Point de dÈpart idÈal pour regarder l'Ètat des lieux des bibliothËques en France




############################ CHARGEMENT DES DONNEES ############################


# Il existe un dataset de 2023, basÈ sur un sondage
# Les salariÈs sont indiquÈs en ETP et elles semblent contenir des infos sur les populations des communes concernÈes
# L'amplitude horaire est exprimÈe en nombre d'ouverture d'heures hebdo

# DonnÈes des bibliothËques : https://www.data.gouv.fr/fr/datasets/adresses-des-bibliotheques-publiques-2/
# Lien du tÈlÈchargement :  https://www.data.gouv.fr/fr/datasets/r/e3588487-4732-4b6c-ab12-72d75d7f522f



biblio <- 
  fromJSON("analyse bibliotheques/data/adresses-des-bibliotheques-publiques.json") %>% 
  clean_names() %>% 
  tibble() %>% 
  print()



# On charge aussi les donnÈes de population par dÈpartement et on uniformise les noms
pop_dep <- 
  read_csv2("analyse bibliotheques/data/donnees_departements.csv") %>% 
  clean_names() %>% 
  mutate(code_region = as.numeric(reg)) %>% 
  mutate(code_dep = as.numeric(dep)) %>% 
  select(code_region, region, code_dep, departement, ptot) %>% 
  filter(!is.na(code_dep) & code_dep <= 95 ) %>% 
  print()



# On charge la pop des communes 
# Lien Data.gov : https://www.data.gouv.fr/fr/datasets/r/630e7917-02db-4838-8856-09235719551c

pop_com <- 
  read.xlsx("https://www.data.gouv.fr/fr/datasets/r/630e7917-02db-4838-8856-09235719551c") %>% 
  clean_names() %>% 
  tibble() %>% 
  select(code_region = reg, code_dep = dep, insee = codgeo,pop_21_com = p21_pop, nom_com = libgeo) %>% 
  print()





# On charge les donnÈes gÈographiques des communes issues de datagov
# lien : https://www.data.gouv.fr/fr/datasets/decoupage-administratif-communal-francais-issu-d-openstreetmap/
# Et on rajoute la population 2021 des communes

geo_com <- 
  read_sf("analyse bibliotheques/data/communes-20220101-shp/communes-20220101.shp") %>% 
  mutate(dep = str_extract(insee, "^..")) %>% 
  filter(!str_detect(dep, "[:alpha:]")) %>% 
  filter(dep < 96) %>% 
  select(-wikipedia) %>% 
  rename(code_dep = dep) %>% 
  left_join(pop_com %>% select(insee, pop_21_com, code_region), join_by(insee)) %>% 
  relocate(geometry, .after = last_col()) %>% 
  print()





# Et on charge les donnÈes spaciales des dÈpartements
# lien ---- > "https://www.data.gouv.fr/fr/datasets/r/90b9341a-e1f7-4d75-a73c-bbc010c7feeb"
# On y ajoute aussi les rÈgions et la population des dÈpartements

geo_dep <- 
  read_sf("analyse bibliotheques/data/contour-des-departements.geojson") %>% 
  mutate(code_dep = as.numeric(code)) %>% 
  filter(!is.na(code_dep)) %>% 
  select(-code) %>% 
  left_join(pop_dep %>% select(code_dep, code_region, ptot), join_by(code_dep)) %>% 
  relocate(geometry , .after = last_col()) %>% 
  print()

############################ FIN DE CHARGEMENT DES DONNEES ############################










############################ NETTOYAGE DES DONNEES ############################


# on explore un peu les donneÈes 

biblio %>% 
  glimpse()


biblio %>% 
  count(type_adresse)

biblio %>% 
  summary()

biblio %>% 
  colnames()




# Nous allons nous concentrer sur la France mÈtropolitaine et uniquement
# avec les bibliothËques "ouvertes", avec des horaires renseingÈes et des salariÈs

biblio_v1 <- 
  biblio %>% 
  filter(type_adresse == "Bâtiment ouvert") %>% 
  filter(!is.na(surface)) %>% 
  mutate(code_departement = as.numeric(code_departement)) %>% 
  filter(!is.na(code_departement)) %>% 
  filter(code_departement < 100) %>% 
  print()




# Nous cherchons maintenant ‡ savoir si la population communales exprimÈe est trËs
# diffÈrente de notre dataset de l'insee 

biblio_v1 %>% 
  select(population_commune) %>% 
  summary()


biblio_v1 %>% 
  select(code_insee_commune, population_commune, ville) %>% 
  left_join(pop_com, join_by(code_insee_commune == insee)) %>% 
  mutate(ecart = population_commune - pop_21_com) %>% 
  select(ecart) %>% 
  summary()




# MÍme si la mÈdiane est de 20, nous avons quelques valeurs extrËmes
#  trop importantes pour conserver les donnÈes du dataset. Nous les remplaÁons
#  par celles de l'insee, sauf si elles sont en NA

# Et comme nous disposons des latitudes et longitudes, nous les transformons afin de pouvoir les localiser

biblio_def <- 
  biblio_v1 %>% 
  left_join(pop_com %>% select(insee, pop_21_com, nom_com), join_by(code_insee_commune == insee)) %>% 
  mutate(pop_commune_2021 = if_else(is.na(pop_21_com), population_commune, pop_21_com)) %>% 
  mutate(longitude = as.numeric(longitude),
         latitude = as.numeric(latitude)) %>%   
  filter(!is.na(latitude)) %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs=4326) %>% 
  select(-c(pop_21_com, population_commune)) %>% 
  print()


######################## FIN DE NETTOYAGE DES DONNEES #########################




########################## ANALYSE DES DONNEES #################################


biblio_def %>% colnames()

biblio_def %>% 
  select(surface) %>% 
  summary()


hist(biblio_def$surface)


biblio_def %>% 
  ggplot(aes(x = amplitude_horaire)) +
  geom_histogram(bins = 20, fill = "skyblue", color = "white") +
  labs(title = "Distribution des amplitudes horaires d'ouverture des bibliothËques",
       x = "Nombre d'heures d'ouverture moyenne, par semaine", y = "Nombre de restaurants") +
  theme_minimal()

biblio_def %>% 
  mutate(amplitude_horaire = as.numeric(amplitude_horaire)) %>% 
  ggplot(aes(x = amplitude_horaire)) +
  geom_boxplot(fill = "orange") +
  labs(title = "RÈsumÈ statistique des tailles de restaurants", y = "Taille (m≤)") +
  scale_x_continuous(breaks = 1) %>% 
  theme_minimal()



data <- biblio_def %>% 
  mutate(
    intervalle_taille = cut(amplitude_horaire,
                            breaks = quantile(amplitude_horaire, probs = c(0, 0.33, 0.66, 1), na.rm = TRUE),
                            include.lowest = TRUE)
  ) %>% 
  mutate(
    categorie_taille = cut(amplitude_horaire,
                           breaks = quantile(amplitude_horaire, probs = c(0, 0.33, 0.66, 1), na.rm = TRUE),
                           labels = c("Petit", "Moyen", "Grand"),
                           include.lowest = TRUE)
  ) %>% 
  print()


ggplot(data, aes(x = intervalle_taille)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Restaurants par tranche de taille (m≤)",
       x = "Taille en m≤", y = "Nombre de restaurants") +
  theme_minimal()


ggplot(data, aes(x = categorie_taille)) +
  geom_bar(fill = "purple") +
  labs(title = "RÈpartition des tailles de restaurants",
       x = "CatÈgorie de taille", y = "Nombre de restaurants") +
  theme_minimal()







######################## CREATION DE GRAPHIQUES#### #########################





ggplot() +
  geom_sf() + # Ajouter la carte du monde
  geom_sf(data = data_sf, aes(color = code_departement), size = 1, show.legend = F) + # Ajouter les points +
  geom_sf(data = france_map, fill = NA, color = "black") + 
  ggtitle("RÈpartition des bibliothËques, par rÈgion") +
  theme_void()






ggplot() +
  geom_sf(data = france_map, aes(color = region), size = 1, linewidth = 1,show.legend = F) +
  geom_sf(data = data_sf, aes(color = nom), size = .4, show.legend = F) + # Ajouter la carte du monde
  ggtitle("RÈpartition des bibliothËques, par rÈgion") +
  theme_void()





ggplot() +
  geom_sf(data = geo_com %>% filter(code_dep == "75"), size = 1, linewidth = 1.5, show.legend = F, color = "black", fill = "white") +
  geom_sf(data = biblio_def %>% filter(code_departement == "75"), aes(size = amplitude_horaire,color = cp), alpha = .5, show.legend = F) +
  scale_size_continuous(range = c(4, 20)) +
  theme_void()

test %>% 
  filter(code_departement == "75") %>% 
  view()
