############ Vote législative 2024 VS Ecart moyenne revenu commune ############

rm(list = ls()) 
cat("\014")  #Effacer la console

package <- c("tidyverse", "broom","chron", "readxl", "rvest", "ggrepl","lubridate", "clipr", "santoku", "scales", "esquisse", "ggpubr", "openxlsx", "janitor")
lapply(package, require, character.only = TRUE)
rm(package)

options( "digits"=2, "scipen"=100) 




#### Nous cherchons à regarder le nombre de communes qui avaient mis la Majorité Présidentielle 
#### ou le RN en tête lors du 1er Tour des législatives de 2022 et qui ont basculé vers l'autre parti
#### lors des législatives 2024. 
#### En parallèle nous regardons quel est le niveau de vie relatif de la commune 
#### par rapport à son département.



############################ CHARGEMENT DES DONNEES ############################



# Niveau de vie des communes en 2013 par rapport au département : https://www.data.gouv.fr/fr/datasets/revenus-des-francais-a-la-commune/#/resources/d3ce0107-416f-42cf-a335-d71f89b00b21
# Lien du téléchargement :  https://www.data.gouv.fr/fr/datasets/r/d3ce0107-416f-42cf-a335-d71f89b00b21

revenus <- 
  read_excel("data/Niveau_de_vie_2013_a_la_commune-Global_Map_Solution.xlsx") %>% 
  clean_names() %>% 
  print()



# Élections législatives des 30 juin et 7 juillet 2024 - Résultats définitifs du 1er tour par commune
# https://www.data.gouv.fr/fr/datasets/elections-legislatives-des-30-juin-et-7-juillet-2024-resultats-definitifs-du-1er-tour/
# Lien de téléchargement :  https://www.data.gouv.fr/fr/datasets/r/ab337c6f-e7e8-4981-843c-45052b71096b


vote_2024 <- 
  read_excel("data/2024 legislative resultats-definitifs-par-communes.xlsx") %>% 
  clean_names() %>% 
  print()






# Elections législatives des 12 et 19 juin 2022 - Résultats du 1er tour par commune
# https://www.data.gouv.fr/fr/datasets/elections-legislatives-des-12-et-19-juin-2022-resultats-du-1er-tour/
# 
# Lien de téléchargement :https://www.data.gouv.fr/fr/datasets/r/847591e4-dc12-4431-9fe1-5fac57bfe5b9


vote_2022 <- 
  read_excel("data/resultats-par-niveau-subcom-t1-france-entiere 2022 legislative.xlsx") %>% 
  clean_names() %>% 
  print()



# Il nous manque le lexique des nuances politiques afin d'identifier correctement le RN ou ENS 
# Lexique nuance 2024 : https://www.archives-resultats-elections.interieur.gouv.fr/resultats/legislatives2024/referentiel.php
# Lexique nuance 2022 : https://www.archives-resultats-elections.interieur.gouv.fr/resultats/legislatives-2022/nuances.php
# Un simple fichier texte est créé pour chaque lexique. 


nuance_2024 <- 
  read_delim("data/lexique nuance 2024.txt", ) %>% 
  clean_names() %>% 
  print()



nuance_2022 <- 
  read_delim("data/lexique nuance 2022.txt") %>% 
  clean_names() %>% 
  print()


############################ FIN CHARGEMENT DES DONNEES ############################







############################ NETTOYAGE DONNEES  ############################


# On commence par uniformiser et nettoyer les données pour rentre les datasets 
# utilisables entre eux. 


### On calcule pour les 36 000 communes, le % d'écart de niveau de vie par rapport à son département
revenus_def <- 
  revenus %>% 
  mutate(ecart_commune_dep = ((niveau_de_vie_commune - niveau_de_vie_departement ) / niveau_de_vie_departement),
         dept = str_sub(code_commune, 0, 2)) %>% # Permet de créer un écart en pourcentage par rapport au département
  relocate(dept) %>% 
  mutate(code_commune = as.numeric(code_commune)) %>% 
  filter(!is.na(code_commune)) %>% 
  print()





### On nettoie le dataset des élections législatives de 2024. 
### Nous souhaitons uniquement conservé la première personne arrivée en tête pour chaque commune
vote_def_2024 <-
  vote_2024 %>% 
  left_join(revenus_def %>% select(code_commune, ecart_commune_dep), join_by(code_commune)) %>% 
  relocate (ecart_commune_dep, .after = code_commune) %>% # On rajouter le % d'écart du niveau de vie dans le dataset des votes de 2024 pour chaque commune
  
  #Afin d'identifier facilement qui est arrivé en tête de chaque commune, on réalise un pivot_longer sur la partie des votes 
  pivot_longer(
    cols = -c(1:19),  # garde "departement" fixe
    names_to = c(".value", "groupe"),
    names_pattern = "(.*)_(\\d+)") %>% 
  
  # On rajoute explicitement la nuance correspondante de chaque liste.
  left_join(nuance_2024, join_by("nuance_candidat" == "code")) %>% 
  relocate (nuance, .after = nuance_candidat) %>% 
  
  # On retire les nuances qui sont en NA
  filter(!is.na(nuance_candidat))  %>%
  
  #On converti numériquement la variable "voix"
  mutate(voix = as.numeric(voix)) %>% 
  
  #On sous trie par commune et par nombre de voix de chaque candidat
  group_by(code_commune) %>% 
  arrange(desc(voix), .by_group = T) %>% 
  
  #On ne conserve que le candidat arrivé en tête, en nombre de voix
  slice(1) %>% 
  ungroup() %>% 
  print()







### Pour le fichier de vote de 2022, il y a un nettoyage plus important
### car une partie des variables n'est pas nommée dans le fichier. 
### On doit commencer par nommer correctement chaque variable, puis comment le nettayage des communes 
### pour retenir le candidat arrivé en tête. 


# Pour le fichier de 2022, il faut compléter les nom de colonnes manquants.
# On doit répéter le nom des colonnes de 22 à 29 qui sont relatives à chaque candidat.
nom_col_2022 <- 
  vote_2022 %>% select(22:29) %>%  colnames() %>% 
  print()



# On ajoute un chiffre qui augmente de 1, toutes les 8 colonnes
nombre_de_cycle <- 
  vote_2022 %>% select(22:length(vote_2022)) %>% 
  ncol() %>% # Il y a 176 variables à nommer 
  {./8} %>% # Chaque candidat à 8 variables qui lui sont propres
  print()


# On créé une routine pour pré nommé correctement chaque variable sans nom
nom_col_2022_def <-
  paste0(rep(nom_col_2022, times = nombre_de_cycle),"_", rep(c(1:nombre_de_cycle), each = 8))


# On prépare le nom des variables qui serviront de manière générique dans le pivot_longer
new_col_name_2022 <- 
  vote_2022 %>% colnames() %>% str_subset(pattern = "^x", negate = T) %>% setdiff(nom_col_2022) %>% c(., nom_col_2022_def) 

# On renomme l'ensemble des colonnes
vote_2022_v2 <- 
  vote_2022 %>% 
  set_names(new_col_name_2022) %>% 
  print()




# On peut maintenant travailler sur le dataset
vote_2022_vdef <- 
  
  #Lors de l'import, certaines variables sont catégorisées comme logique. 
  #Nous les supprimons, elles ne contiennent pas de données utilisables
  vote_2022_v2 %>% 
  select(!where(is.logical)) %>% 
  rename(code_commune = code_de_la_commune) %>% 
  
  # On supprime les départements d'outre mer et la Corse pour des raisons de lecture
  # C'est à dire les codes de départements qui contiennent une lettre 
  mutate(code_du_departement = as.numeric(code_du_departement)) %>% 
  filter(!is.na(code_du_departement)) %>% 
  
  # On doit reconstituer le code de la commune avec 5 chiffres, basée sur le département
  # Le code à 5 chiffres désigne seulement la commune. 
  # Il a le format DDCCC où DD est le numéro du département et CCC le numéro de la commune
  mutate(code_commune = paste0(code_du_departement, code_commune)) %>% 
  mutate(code_commune = as.numeric(code_commune)) %>% 
  
  # On rajoute le niveau de revenus de chaque commune 
  left_join(revenus_def %>% select(code_commune, ecart_commune_dep), join_by(code_commune)) %>% 
  relocate (ecart_commune_dep, .after = code_commune) %>% 
  
  # Comme précédemment, on fait un pivot longer pour identifier correctement 
  # qui est arrivé en tête de chaque commune
  pivot_longer(
    cols = -c(1:22),  
    names_to = c(".value", "groupe"),
    names_pattern = "(.*)_(\\d+)")  %>% 
  rename(nuance_candidat = nuance) %>% 
  
  # On rajoute la nuance des candidats de 2022 
  left_join(nuance_2022, join_by("nuance_candidat" == "code")) %>% 
  filter(!is.na(nuance_candidat))  %>%
  relocate (nuance, .after = nuance_candidat) %>% 
  mutate(voix = as.numeric(voix)) %>% 
  
  # On sous trie par commune et par nombre de voix
  group_by(code_commune) %>% 
  arrange(desc(voix), .by_group = T) %>% 
  slice(1) %>% 
  ungroup() %>% 
  print()  




### On peut maintenant ne garder que les communes où le RN ou MP ont été en tête
### en 2022 et en 2024


nuance_candidat_2022 <-
  vote_2022_vdef %>% 
  filter(str_detect(nuance_candidat,"ENS|RN")) %>% 
  
  # On ne garde que le numéro de commune qui sert d'identifiant unique, puis l'écart de niveau de vie, le nombre de la commune 
  # et la nuance du candidat arrivée en tête (RN ou ENS)
  select(code_commune, ecart_commune_dep, libelle_de_la_commune, nuance_candidat) %>% 
  rename(nuance_candidat_2022 = nuance_candidat) %>% 
  print()


# Même logique en 2024
nuance_candidat_2024 <- 
  vote_def_2024 %>% 
  filter(str_detect(nuance_candidat,"ENS|RN")) %>% 
  select(code_commune, inscrits, nuance_candidat)   %>% 
  rename(nuance_candidat_2024 = nuance_candidat) %>% 
  print()



### Nous préparons maintenant le dataset qui va servir à créer le graphique
### Nous réunissons les datasets de 2022 et 2024 
graph <- 
  nuance_candidat_2022 %>% 
  left_join(nuance_candidat_2024, by = "code_commune") %>% 
  relocate(inscrits_2024 = inscrits, .after = code_commune) %>% 
  
  #On retire les nuances en NA car toutes les communes ayant fait arriver le RN ou ENS en 2022 ou 2024
  # ne l'ont pas nécessairement fait lors de l'autre élection
  filter((if_all(starts_with("nuance"), ~!is.na(.)))) %>% 
  filter(!is.na(ecart_commune_dep)) %>% 
  
  # On créer une variable pour indiquer vers qui le changement s'est fait
  mutate(switch = case_when(nuance_candidat_2022 == nuance_candidat_2024 ~NA,
                            nuance_candidat_2022 == "RN" & nuance_candidat_2024 == "ENS"~ "ENS",
                            nuance_candidat_2022 == "ENS" & nuance_candidat_2024 == "RN"~ "RN")) %>% 
  filter(!is.na(switch)) %>% 
  mutate(ecart_commune_dep = round(ecart_commune_dep*100, 1)) %>% 
  print()





### On termine maintenant le dataset pour créer le graphique
graph_def <- 
  
  # Comme nous allons avoir besoin de mettre en avant des points sur le graph, 
  # nous ne pouvons pas utiliser geom_jitter() . Dès lors, nous forçons les points à se répartir 
  # autour de chaque axe. 
  graph %>% 
  mutate(switch= factor(switch, levels = c("ENS", "RN")),
         x_numeric = as.numeric(switch),
         x_jit = x_numeric + runif(n(), -0.3, 0.3),
         ecart_commune_dep = ecart_commune_dep + runif(n(), -0.2, 0.2)) %>% 
  print()



# On identifie un point que nous mettrons en avant plus tard
point_a_annoter <- 
  graph_def %>% 
  filter(switch == 'RN') %>% 
  arrange(desc(ecart_commune_dep)) %>% 
  slice(1) %>% 
  print()


######################### FIN DU NETTOYAGE DES DONNEES ########################






###########################CREATION DU GRAPHIQUE ###############################




ggplot(graph_def,
       aes(x = x_jit,
           y = ecart_commune_dep,
           size = inscrits_2024,
           color = switch)) +
  
  geom_point(alpha = .1)  +
  geom_point(data = graph_def %>% filter(switch == "ENS")) +
  
  
  scale_x_discrete(limits = c("ENS", "RN"), expand = expansion(add = 0.5))  + # Ajuste l'écart entre les 2 positions
  
  
  scale_y_continuous(
    labels = scales::percent_format(scale = 1),
    limits = c(-100, 100),
    breaks = seq(-100, 100, by = 50)) +
  
  scale_color_manual(values = c("RN" = "mediumblue", "ENS" = "gold1")) +
  
  scale_size(range = c(3, 15)) +
  
  
  
  
  annotate("text",
           x = 1, y = -65,
           label = "Bascule vers ENS",
           hjust = 0.5, size = 5,
           fontface="bold",
           colour = "gold1") +
  
  annotate("text",
           x = 1, y = -80,
           label = "Seules 27 communes ayant voté RN \nau 1er Tour des législatives en 2022,\n ont mis ENS en tête, en juillet 2024",
           hjust = 0.5, vjust = 0.5, size = 4)  +
  
  
  annotate("text",
           x = 2, y = -65,
           label = "Bascule vers le RN",
           hjust = 0.5, size = 5,
           fontface="bold",
           colour = "mediumblue") +
  
  annotate("text",
           x = 2, y = - 80,
           label = "Les communes ayant basculé vers le RN \n ont en moyenne un niveau de vie supérieures \nà celle de leur département",
           hjust = 0.5, vjust = 0.5, size = 4) +
  
  
  
  
  # On met en avant les communes de St Raphaël et Toulon
  # Ajout du point de St Raphaël ainsi que la flèche et le texte 
  geom_point(data = graph_def %>% filter(libelle_de_la_commune %in% c("Saint-Raphaël","Toulon")),
             aes(x = x_jit, y = ecart_commune_dep),
             shape = 21,
             fill = "blue",
             color = "black",
             alpha = .3,
             stroke = 1.1) +

  
  geom_curve(
    data =  graph_def %>% filter(libelle_de_la_commune == "Saint-Raphaël"),
    aes(x = x_jit, y = ecart_commune_dep, xend = x_jit-0.15, yend = ecart_commune_dep+15),
    colour = "black", curvature = -.3, size = .5,
    arrow = arrow(length = unit(0.03, "npc"))) +
  
  geom_label(data =  graph_def %>% filter(libelle_de_la_commune == "Saint-Raphaël"),
             aes(x = x_jit-0.14, y =  ecart_commune_dep+17, label = "Saint-Raphaël"),
             hjust = 0.5, vjust = 0, colour = "black", label.size = NA, size = 4) +
  
  
  
  
  # Ajout du point de Toulon ainsi que la flèche et le texte 
  geom_curve(
    data =  graph_def %>% filter(libelle_de_la_commune == "Toulon"),
    aes(x = x_jit, y = ecart_commune_dep, xend = x_jit+0.1, yend = ecart_commune_dep-30),
    colour = "black", curvature = .5, size = .5,
    arrow = arrow(length = unit(0.03, "npc"))) +
  
  geom_label(data =  graph_def %>% filter(libelle_de_la_commune == "Toulon"),
             aes(x = x_jit+0.11, y =  ecart_commune_dep-33, label = "Toulon"),
             hjust = 0, vjust = 0, colour = "black", label.size = NA, size = 4) +
  
  
  
  
  
  # Créer une annotation d'une donnée
  
  geom_point(
    data =  graph_def %>% 
      filter(switch == 'ENS') %>% 
      arrange(desc(ecart_commune_dep)) %>% 
      slice(1), 
    
    aes(x = x_jit, y = ecart_commune_dep),
    colour = "gold4", size = 4) +
  
  
  
  
  geom_segment(
    data =  graph_def %>% 
      filter(switch == 'ENS') %>% 
      arrange(desc(ecart_commune_dep)) %>% 
      slice(1), 
    
    aes(x = x_jit, y = ecart_commune_dep+4, xend = x_jit, yend = ecart_commune_dep+22),
    colour = "orange", linewidth = .6,
    arrow = arrow(length = unit(0.018, "npc"), type = "closed"),
    linetype = "twodash") +
  
  
  
  geom_label(data =  graph_def %>% 
               filter(switch == 'ENS') %>% 
               arrange(desc(ecart_commune_dep)) %>% 
               slice(1), 
             color = "orange",
             fill = "white", 
             
             label.padding = unit(0.55, "lines"),
             
             aes(x = x_jit, y =  ecart_commune_dep+25,
                 label = paste0("La commune de ",
                                libelle_de_la_commune, 
                                "\n à un niveau de vie de ",
                                round(ecart_commune_dep,1),
                                " % \nplus élevé que celui de son département")),
             hjust = 0.5, vjust = 0, 
             label.size = 1.2, 
             size = 3.5) +
  
  
  geom_label(data =  graph_def %>% 
               filter(switch == 'ENS') %>% 
               arrange(desc(ecart_commune_dep)) %>% 
               slice(1), 
             color = "black",
             label.padding = unit(0.55, "lines"),
             
             
             aes(x = x_jit, y =  ecart_commune_dep+25,
                 label = paste0("La commune de ",
                                libelle_de_la_commune, 
                                "\n à un niveau de vie de ",
                                round(ecart_commune_dep,1),
                                " % \nplus élevé que celui de son département")),
             hjust = 0.5, vjust = 0, 
             label.size = 0, size = 3.5) +
  
  
  theme_minimal(base_family = "Helvetica") +
  
  labs(
    title = "Evolution des votes vers le RN ou vers Ensemble, \nen fonction du niveau de vie de la commune, en France métropolitaine.",
    subtitle = "Bascule entre le RN et Ensemble, lors du 1er tour des législative de 2022 et de 2024",
    caption = "Sources: Data.gouv, Ministère de l'intérieur, INSEE.
    La taille des point est fonction du nombre d'inscrits sur les listes électorales de la commune en 2024
    @quentin_DataViz",
    y = "Ecart du niveau de la commune par rapport à son département") +

  
    theme(
    legend.position = "none",
    panel.grid = element_line(color = "black",
                              size = 0.2,
                              linetype = 3),
    axis.title.x = element_blank(),
    #axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 10),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5, margin = margin(b = 7)),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    plot.caption = element_text(size = 8, color = "gray50", face = "italic"))

 

