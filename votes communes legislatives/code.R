library(readr)
library(stringi)

convert_to_utf8 <- function(input_file, output_file = NULL) {
  if (!file.exists(input_file)) {
    stop("Le fichier n'existe pas : ", input_file)
  }
  
  # Lire le fichier brut
  raw_content <- read_file_raw(input_file)
  
  # Détecter l'encodage
  encoding_guess <- stri_enc_detect(raw_content)[[1]]
  best_guess <- encoding_guess$Encoding[1]
  confidence <- round(encoding_guess$Confidence[1] * 100, 1)
  
  cat("Encodage détecté :", best_guess, "(", confidence, "% de confiance)\n")
  
  # Lecture avec l'encodage détecté
  text_lines <- readLines(input_file, encoding = best_guess, warn = FALSE)
  
  # Nom de sortie (si non spécifié)
  if (is.null(output_file)) {
    output_file <- sub("\\.([Rr])$", "_utf8.\\1", input_file)
  }
  
  # Écriture en UTF-8
  writeLines(text_lines, output_file, useBytes = FALSE)
  cat("??? Fichier converti et enregistré sous :", output_file, "\n")
}


convert_to_utf8("evol_votes_communes.R")


