############ Analyse des bibliothèques ############

rm(list = ls()) 
cat("\014")  #Effacer la console

package <- c("tidyverse", "broom","chron", "readxl", "rvest", "ggrepl","lubridate", "jsonlite", "santoku", "scales", "esquisse", "httr", "openxlsx", "janitor")
lapply(package, require, character.only = TRUE)
rm(package)

options( "digits"=2, "scipen"=100) 




############################## INTRODUCTIION ################################### 


### L'article du Monde clame l'idée d'un réseau de bibliothèque unique au monde, et un accès à la lecture gratuit.
### https://www.lemonde.fr/idees/article/2025/05/31/le-livre-d-occasion-est-en-train-de-cannibaliser-en-silence-toute-la-chaine-du-livre_6609480_3232.html
### Point de départ idéal pour regarder l'état des lieux des bibliothèques en France




############################ CHARGEMENT DES DONNEES ############################

# Les données semblent limitées, certains départements donnent des informations exaustives 
# https://www.data.gouv.fr/fr/datasets/enquete-sur-les-bibliotheques-municipales-des-cotes-darmor-1/


# Données des bibliothèques : https://www.data.gouv.fr/fr/datasets/adresses-des-bibliotheques-publiques-2/
# Lien du téléchargement :  https://www.data.gouv.fr/fr/datasets/r/e3588487-4732-4b6c-ab12-72d75d7f522f

# TEST 


biblio <- 
fromJSON("analyse bibliotheques/data/adresses-des-bibliotheques-publiques.json") %>% 
  clean_names() %>% 
  tibble() %>% 
  print()



# On charge aussi les données de population par département
pop <- 
  read_csv2("analyse bibliotheques/data/donnees_departements.csv") %>% 
  clean_names() %>% 
  print()




############################ FIN DE CHARGEMENT DES DONNEES ############################







############################ NETTOYAGE DES DONNEES ############################


# on explore un peu les donneées 

biblio %>% 
  glimpse()

biblio %>% 
  count(statut)

biblio %>% 
  count(type_adresse)



biblio %>% 
  colnames()



# Nous allons nous concentrer sur la France métropolitaine et uniquement
# les bibliothèques ouvertes, avec des horaires renseingées et des salariés


biblio %>% 
  filter(type_adresse == "Bâtiment ouvert") %>% 
  filter(!is.na(surface)) %>% 
  mutate(code_departement = as.numeric(code_departement)) %>% 
  filter(!is.na(code_departement)) %>% 
  filter(code_departement < 100) %>% 
  view()
  
  


######################## FIN DE NETTOYAGE DES DONNEES #########################



