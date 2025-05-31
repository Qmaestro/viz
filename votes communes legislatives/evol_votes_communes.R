############ Vote législative 2024 VS Ecart moyenne revenu commune ############

rm(list = ls()) 
cat("\014")  #Effacer la console

package <- c("tidyverse", "broom","chron", "readxl", "rvest", "ggrepl","lubridate", "clipr", "santoku", "scales", "esquisse", "ggpubr", "openxlsx", "janitor")
lapply(package, require, character.only = TRUE)
rm(package)

options( "digits"=2, "scipen"=100) 



############################ CHARGEMENT DES DONNEES ############################





# Ecart niveau de vie : https://www.data.gouv.fr/fr/datasets/revenus-des-francais-a-la-commune/#/resources/d3ce0107-416f-42cf-a335-d71f89b00b21


revenus <- 
  read_excel("ecart moyenne dep/data/Niveau_de_vie_2013_a_la_commune-Global_Map_Solution.xlsx") %>% 
  clean_names() %>% 
  print()


# Vote législative 1er Tour 2024
# https://www.data.gouv.fr/fr/datasets/elections-legislatives-des-30-juin-et-7-juillet-2024-resultats-definitifs-du-1er-tour/

vote_2024 <- 
  read_excel("ecart moyenne dep/data/2024 legislative resultats-definitifs-par-communes.xlsx") %>% 
  clean_names() %>% 
  print()




# Vote législative 1er Tour 2022
# https://www.data.gouv.fr/fr/datasets/elections-legislatives-des-12-et-19-juin-2022-resultats-du-1er-tour/


vote_2022 <- 
  read_excel("ecart moyenne dep/data/resultats-par-niveau-subcom-t1-france-entiere 2022 legislative.xlsx") %>% 
  clean_names() %>% 
  print()



# Lexique nuance 2024 : https://www.archives-resultats-elections.interieur.gouv.fr/resultats/legislatives2024/referentiel.php
# Lexique nuance 2022 : https://www.archives-resultats-elections.interieur.gouv.fr/resultats/legislatives-2022/nuances.php



nuance_2024 <- 
  read_delim("ecart moyenne dep/data/lexique nuance 2024.txt", ) %>% 
  clean_names() %>% 
  print()



nuance_2022 <- 
  read_delim("ecart moyenne dep/data/lexique nuance 2022.txt") %>% 
  clean_names() %>% 
  print()


############################ FIN CHARGEMENT DES DONNEES ############################





############################ NETTOYAGE DONNEES  ############################

revenus_def <- 
  revenus %>% 
  mutate(ecart_commune_dep = ((niveau_de_vie_commune - niveau_de_vie_departement ) / niveau_de_vie_departement),
         dept = str_sub(code_commune, 0, 2)) %>% 
  relocate(dept) %>% 
  mutate(code_commune = as.numeric(code_commune)) %>% 
  filter(!is.na(code_commune)) %>% 
  print()





vote_def_2024 <-
  vote_2024 %>% 
  left_join(revenus_def %>% select(code_commune, ecart_commune_dep), join_by(code_commune)) %>% 
  relocate (ecart_commune_dep, .after = code_commune) %>% 
  pivot_longer(
    cols = -c(1:19),  # garde "departement" fixe
    names_to = c(".value", "groupe"),
    names_pattern = "(.*)_(\\d+)") %>% 
  left_join(nuance_2024, join_by("nuance_candidat" == "code")) %>% 
  filter(!is.na(nuance_candidat))  %>%
  relocate (nuance, .after = nuance_candidat) %>% 
  mutate(voix = as.numeric(voix)) %>% 
  #filter(str_detect(libelle_commune, "Vaulry|Paris|Romainville")) %>% 
  group_by(code_commune) %>% 
  arrange(desc(voix), .by_group = T) %>% 
  slice(1) %>% 
  ungroup() %>% print()




# Pour le fichier de 2022, il faut compléter les nom de colonnes manquants.
# On doit répéter le nom des colonnes de 22 à 29.

nom_col_2022 <- 
  vote_2022 %>% select(22:29) %>%  colnames() %>% 
  print()



# On ajoute un chiffre qui augmente de 1, toutes les 8 colonnes
nombre_de_cycle <- 
  vote_2022 %>% select(22:length(vote_2022)) %>% ncol() %>%  {./8} %>% print()



nom_col_2022_def <-
  paste0(rep(nom_col_2022, times = nombre_de_cycle),"_", rep(c(1:nombre_de_cycle), each = 8))

new_col_name_2022 <- 
  vote_2022 %>% colnames() %>% str_subset(pattern = "^x", negate = T) %>% setdiff(nom_col_2022) %>% c(., nom_col_2022_def) 


vote_2022_v2 <- 
  vote_2022 %>% set_names(new_col_name_2022)


vote_2022_vdef <- 
  vote_2022_v2 %>% 
  select(!where(is.logical)) %>% 
  rename(code_commune = code_de_la_commune) %>% 
  mutate(code_du_departement = as.numeric(code_du_departement)) %>% 
  filter(!is.na(code_du_departement)) %>% 
  mutate(code_commune = paste0(code_du_departement, code_commune)) %>% 
  mutate(code_commune = as.numeric(code_commune)) %>% 
  left_join(revenus_def %>% select(code_commune, ecart_commune_dep), join_by(code_commune)) %>% 
  mutate(across(where(is.logical), ~ as.character(.))) %>% 
  relocate (ecart_commune_dep, .after = code_commune) %>% 
  pivot_longer(
    cols = -c(1:22),  # garde "departement" fixe
    names_to = c(".value", "groupe"),
    names_pattern = "(.*)_(\\d+)")  %>% 
  rename(nuance_candidat = nuance) %>% 
  left_join(nuance_2022, join_by("nuance_candidat" == "code")) %>% 
  filter(!is.na(nuance_candidat))  %>%
  relocate (nuance, .after = nuance_candidat) %>% 
  mutate(voix = as.numeric(voix)) %>% 
  #filter(str_detect(libelle_commune, "Vaulry|Paris|Romainville")) %>% 
  group_by(code_commune) %>% 
  arrange(desc(voix), .by_group = T) %>% 
  slice(1) %>% 
  ungroup() %>% print()  





# On extrait maintenant que ceux qui ont eu le RN ou MP en tête


nuance_2022 

nuance_candidat_2022 <-
  vote_2022_vdef %>% 
  filter(str_detect(nuance_candidat,"ENS|RN")) %>% 
  select(code_commune, ecart_commune_dep, libelle_de_la_commune, nuance_candidat) %>% 
  rename(nuance_candidat_2022 = nuance_candidat) %>% 
  print()

nuance_candidat_2024 <- 
  vote_def_2024 %>% 
  filter(str_detect(nuance_candidat,"ENS|RN")) %>% 
  select(code_commune, inscrits, nuance_candidat)   %>% 
  rename(nuance_candidat_2024 = nuance_candidat) %>% 
  print()


graph <- 
  nuance_candidat_2022 %>% 
  left_join(nuance_candidat_2024, by = "code_commune") %>% 
  relocate(inscrits_2024 = inscrits, .after = code_commune) %>% 
  filter((if_all(starts_with("nuance"), ~!is.na(.)))) %>% 
  filter(!is.na(ecart_commune_dep)) %>% 
  mutate(switch = case_when(nuance_candidat_2022 == nuance_candidat_2024 ~NA,
                            nuance_candidat_2022 == "RN" & nuance_candidat_2024 == "ENS"~ "ENS",
                            nuance_candidat_2022 == "ENS" & nuance_candidat_2024 == "RN"~ "RN")) %>% 
  filter(!is.na(switch)) %>% 
  mutate(ecart_commune_dep = round(ecart_commune_dep*100, 1)) %>% 
  print()




graph_def <- 
  graph %>% 
  mutate(switch= factor(switch, levels = c("ENS", "RN")),
         x_numeric = as.numeric(switch),
         x_jit = x_numeric + runif(n(), -0.3, 0.3),
         ecart_commune_dep = ecart_commune_dep + runif(n(), -0.2, 0.2)) %>% 
  print()


point_a_annoter <- 
  graph_def %>% 
  filter(switch == 'RN') %>% 
  arrange(desc(ecart_commune_dep)) %>% 
  slice(1) %>% 
  print()







ggplot(graph_def,
       aes(x = x_jit,
           y = ecart_commune_dep,
           size = inscrits_2024,
           color = switch)) +
  
  geom_point(alpha = .1)  +
  geom_point(data = graph_def %>% filter(switch == "ENS")) +
  
  
  scale_x_discrete(limits = c("ENS", "RN"), expand = expansion(add = 0.5))  + # Ajustez l'expansion
  
  
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
  
  
  
  
  
  
  geom_point(data = graph_def %>% filter(libelle_de_la_commune %in% c("Saint-Raphaël","Toulon")),
             aes(x = x_jit, y = ecart_commune_dep),
             shape = 21,
             fill = "blue",
             color = "black",
             alpha = .2,
             stroke = 1.1) +
  
  
  geom_curve(
    data =  graph_def %>% filter(libelle_de_la_commune == "Saint-Raphaël"),
    aes(x = x_jit, y = ecart_commune_dep, xend = x_jit-0.15, yend = ecart_commune_dep+15),
    colour = "black", curvature = -.3, size = .5,
    arrow = arrow(length = unit(0.03, "npc"))) +
  
  geom_label(data =  graph_def %>% filter(libelle_de_la_commune == "Saint-Raphaël"),
             aes(x = x_jit-0.14, y =  ecart_commune_dep+17, label = "Saint-Raphaël"),
             hjust = 0.5, vjust = 0, colour = "black", label.size = NA, size = 4) +
  
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
    colour = "orange", size = .6,
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
             label.size = 1.2, size = 3.5) +
  
  
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
    title = "Evolution des votes vers le RN ou vers Ensemble, \nen fonction du niveau de vie de la commune, par rapport à son département",
    subtitle = "Bascule entre le RN et Ensemble, lors du 1er tour des législative de 2022 et de 2024",
    caption = "Sources: Data.gouv, Ministère de l'intérieur, INSEE.
    La taille des point est fonction du nombre d'inscrits sur les listes électorales de la commune en 2024
    @quentin_DataViz") +
  
  theme(
    legend.position = "none",
    panel.grid = element_line(color = "black",
                              size = 0.2,
                              linetype = 3),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5, margin = margin(b = 7)),
    plot.subtitle = element_text(size = 12, , hjust = 0.5),
    plot.caption = element_text(size = 8, color = "gray50", face = "italic"))









