############ Analyse des bibliothèques ############

rm(list = ls()) 
cat("\014")  #Effacer la console

package <- c("tidyverse", "broom","chron", "readxl", "rvest", "lubridate", "sf","jsonlite", "santoku", "scales", "esquisse", "httr", "openxlsx", "janitor")
lapply(package, require, character.only = TRUE)
rm(package)

options( "digits"=2, "scipen"=100) 




############################## INTRODUCTIION ################################### 


### L'article du Monde clame l'idée d'un réseau de bibliothèque unique au monde, et un accés è la lecture gratuit.
### https://www.lemonde.fr/idees/article/2025/05/31/le-livre-d-occasion-est-en-train-de-cannibaliser-en-silence-toute-la-chaine-du-livre_6609480_3232.html
### Point de départ idéal pour regarder l'état des lieux des bibliothéques en France




############################ CHARGEMENT DES DONNEES ############################


# Il existe un dataset de 2023, basé sur un sondage
# Les salariés sont indiqués en ETP et elles semblent contenir des infos sur les populations des communes concernées
# L'amplitude horaire est exprimée en nombre d'ouverture d'heures hebdo

# Données des bibliothèques : https://www.data.gouv.fr/fr/datasets/adresses-des-bibliotheques-publiques-2/
# Lien du téléchargement :  https://www.data.gouv.fr/fr/datasets/r/e3588487-4732-4b6c-ab12-72d75d7f522f



biblio <- 
  fromJSON("analyse bibliotheques/data/adresses-des-bibliotheques-publiques.json") %>% 
  clean_names() %>% 
  tibble() %>% 
  print()



# On charge aussi les données de population par département et on uniformise les noms
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





# On charge les données géographiques des communes issues de datagov
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





# Et on charge les données spaciales des départements
# lien ---- > "https://www.data.gouv.fr/fr/datasets/r/90b9341a-e1f7-4d75-a73c-bbc010c7feeb"
# On y ajoute aussi les régions et la population des départements

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


# on explore un peu les donneées 

biblio %>% 
  glimpse()


biblio %>% 
  count(type_adresse)

biblio %>% 
  summary()

biblio %>% 
  colnames()




# Nous allons nous concentrer sur la France métropolitaine et uniquement
# avec les bibliothèques "ouvertes", avec des horaires renseingées et des salariés

biblio_v1 <- 
  biblio %>% 
  filter(type_adresse == "Bâtiment ouvert") %>% 
  filter(!is.na(surface)) %>% 
  mutate(code_departement = as.numeric(code_departement)) %>% 
  filter(!is.na(code_departement)) %>% 
  filter(code_departement < 100) %>% 
  print()




# Nous cherchons maintenant à savoir si la population communales exprimée est très
# différente de notre dataset de l'insee 

biblio_v1 %>% 
  select(population_commune) %>% 
  summary()


biblio_v1 %>% 
  select(code_insee_commune, population_commune, ville) %>% 
  left_join(pop_com, join_by(code_insee_commune == insee)) %>% 
  mutate(ecart = population_commune - pop_21_com) %>% 
  select(ecart) %>% 
  summary()




# Même si la médiane est de 20, nous avons quelques valeurs extrèmes
#  trop importantes pour conserver les données du dataset. Nous les remplaçons
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
  labs(title = "Distribution des amplitudes horaires d'ouverture des bibliothèques",
       x = "Nombre d'heures d'ouverture moyenne, par semaine", y = "Nombre de restaurants") +
  theme_minimal()


biblio_def %>% 
  mutate(amplitude_horaire = as.numeric(amplitude_horaire)) %>% 
  ggplot(aes(x = amplitude_horaire)) +
  geom_boxplot(fill = "#0072B2", color = "#003366", width = 1, outlier.color = "red", outlier.size = 2) +
  labs(title = "Résumé statistique des amplitudes horaires des bibliotèques") +
  scale_x_continuous(n.breaks = 10) +
  
  labs(
    title = "Amplitude horaire des bibliothèques",
    subtitle = "Distribution des heures d'ouverture hebdomadaires",
    x = "Amplitude horaire (en heures)",
    y = ""
  ) +
  
  
  theme_minimal(base_size = 14) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_blank(),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(color = "gray30"))  +
  
  geom_vline(xintercept = median(data$amplitude_horaire, na.rm = TRUE),
               linetype = "dashed", color = "darkred")





font_add_google("Lato", "lato")
showtext_auto()

library(ggimage)
library(showtext)



# Exemple image : une horloge (icône PNG ou SVG local ou en ligne)
# Tu peux remplacer le lien par un fichier local ou autre image
icon_url <- "https://img.icons8.com/?size=100&id=l6iocFkbmCrh&format=png&color=000000"  # petite horloge noire

# Préparer la position de l’image (facultatif)
image_df <- data.frame(
  x = 50,
  y = 1.1,
  image = icon_url
)

ggplot(data, aes(x = amplitude_horaire, y = "")) +
  geom_boxplot(fill = "#009E73", color = "#004D40", width = 0.25, outlier.color = "firebrick", outlier.size = 2) +
  
  # Ligne médiane
  geom_vline(xintercept = median(data$amplitude_horaire, na.rm = TRUE),
             linetype = "dashed", color = "black", linewidth = 0.8) +
  
  # Ajouter une image d’icône
  geom_image(data = image_df, aes(x = x, y = y, image = image), size = 0.05, inherit.aes = FALSE) +
  
  # Titres avec style
  labs(
    title = "Amplitudes horaires des bibliothèques",
    subtitle = "Boxplot des heures d'ouverture hebdomadaires observées",
    x = "Amplitude horaire (heures)", y = NULL
  ) +
  
  # Thème minimal mais élégant
  theme_minimal(base_family = "lato") +
  theme(
    plot.title = element_text(size = 18, face = "bold", color = "#333333"),
    plot.subtitle = element_text(size = 13, color = "gray40"),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_blank()
  )  
  















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
  labs(title = "Restaurants par tranche de taille (mâ‰¤)",
       x = "Taille en mâ‰¤", y = "Nombre de restaurants") +
  theme_minimal()


ggplot(data, aes(x = categorie_taille)) +
  geom_bar(fill = "purple") +
  labs(title = "Répartition des tailles de restaurants",
       x = "Catégorie de taille", y = "Nombre de restaurants") +
  theme_minimal()







######################## CREATION DE GRAPHIQUES#### #########################





ggplot() +
  geom_sf() + # Ajouter la carte du monde
  geom_sf(data = data_sf, aes(color = code_departement), size = 1, show.legend = F) + # Ajouter les points +
  geom_sf(data = france_map, fill = NA, color = "black") + 
  ggtitle("Répartition des bibliothèques, par région") +
  theme_void()






ggplot() +
  geom_sf(data = france_map, aes(color = region), size = 1, linewidth = 1,show.legend = F) +
  geom_sf(data = data_sf, aes(color = nom), size = .4, show.legend = F) + # Ajouter la carte du monde
  ggtitle("Répartition des bibliothèques, par région") +
  theme_void()





ggplot() +
  geom_sf(data = geo_com %>% filter(code_dep == "75"), size = 1, linewidth = 1.5, show.legend = F, color = "black", fill = "white") +
  geom_sf(data = biblio_def %>% filter(code_departement == "75"), aes(size = amplitude_horaire,color = cp), alpha = .5, show.legend = F) +
  scale_size_continuous(range = c(4, 20)) +
  theme_void()

test %>% 
  filter(code_departement == "75") %>% 
  view()
