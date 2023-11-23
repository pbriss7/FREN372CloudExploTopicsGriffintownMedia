######### Modélisations thématiques (topic modelling): visualisation de trois modèles #########

# Fonction d'installation et d'activation des modules
inst_ext_fun <- function(extension) {
  if(!extension %in% rownames(installed.packages())) {
    install.packages(extension, dependencies = TRUE)
  }
  require(extension, character.only = TRUE)
}

extensions <- c("LDAvis",
                "topicmodels",
                "data.table",
                "text2vec", 
                "stringr",
                "udpipe",
                "dplyr",
                "viridis",
                "ggplot2")

sapply(extensions, inst_ext_fun)

# Nettoyage de l'environnement
rm(list = ls())

# Importation des structures de données
lda.model.14.fr <- readRDS("data/output/lda_model_14_fr.RDS")
lda.model.22.fr <- readRDS("data/output/lda_model_22_fr.RDS")
lda.model.47.fr <- readRDS("data/output/lda_model_47_fr.RDS")


doc.topic.distr.14<- readRDS("data/output/doc_topic_distrib_14.RDS")
doc.topic.distr.22 <- readRDS("data/output/doc_topic_distrib_22.RDS")
doc.topic.distr.47 <- readRDS("data/output/doc_topic_distrib_47.RDS")

# Autres données (à noter que les textes bruts ont été retirés de la structure `df.fr` pour respecter la license d'utilisation d'Eureka)
df.fr <- readRDS("data/output/df_fr_lemma.RDS")
dgTMatrix.fr <- readRDS("data/output/dgtMatrix_fr.RDS")
dtm.tm.fr <- readRDS("data/output/dtm_tm_fr.RDS")
annotation.fr <- readRDS("data/output/POS_fr_net.RDS")



# Visualiser, pour le premier document seulement, les proportions des thèmes
barplot(doc.topic.distr.14[1, ], xlab = "topic",
        ylab = "proportion", ylim = c(0, 0.4),
        names.arg = 1:ncol(doc.topic.distr.14))

# Obtenir les mots les plus importants (chance qu'on les trouve dans un topic particulier, lambda = 1)
lda.model.14.fr$get_top_words(n = 6, topic_number = c(14L, 1L, 7L, 4L), lambda = 0.5)


# On peut retourner directement au texte, pour valider que les mots clés des trois thèmes résonnent avec le texte
mon_doc_id <- names(doc.topic.distr.14[1,3])
df.fr[doc_id == mon_doc_id, .(Title, `Publication Date`, lemma)]


#### Documents les plus représentatifs de chaque topic du modèle 45 ----
most.rep.docs.indices <- apply(doc.topic.distr.14, 2, which.max)

# Extraction des documents selon leur index
most.rep.docs <- df.fr[most.rep.docs.indices, ]

# Exrtraction des 8 mots les plus représentatifs de chaque topic (lambda 0.4)
top.words.matrix <- lda.model.14.fr$get_top_words(n = 8, topic_number = 1:dim(doc.topic.distr.14)[2], lambda = 0.4)

# Transformation de la matrice en liste
top.words.list <- lapply(1:ncol(top.words.matrix), function(i) top.words.matrix[, i])

doc.mots.rep.dt <- data.table(topic=1:dim(doc.topic.distr.14)[2],
                              top_doc = most.rep.docs$doc_id,
                              titre_doc = most.rep.docs$Title,
                              source = most.rep.docs$`Publication Name`,
                              date = most.rep.docs$`Publication Date`,
                              couverture = most.rep.docs$couverture,
                              most_rep_doc = most.rep.docs$lemma,
                              topic_top_words = top.words.list)


#### Visualisation des thèmes des trois modèles avec LDAVis ----

# Modèle avec 14 thèmes
lda.model.14.fr$plot()

# Modèle avec 14 thèmes
lda.model.22.fr$plot()

# Modèle avec 14 thèmes
lda.model.47.fr$plot()



#### Variance captée par les composantes dites principales (avancé) ----
# L'outil LDAvis distribue les thèmes sur un plan en 2 dimensions.
# Ces dimensions sont traversées par deux axes, PC1 et PC2. 
# PC1 et PC2 sont les deux composantes principales, celles qui captent le maximum de variance dans le jeu de données.
# On peut visualiser la proportion de cette variance captée par ces deux dimensions et les suivantes.

# Modèle à 14 thèmes

# Application de la fonction prcomp() à la matrice thèmes/mots
pca_result = stats::prcomp(t(doc.topic.distr.14), scale. = TRUE)

# Calcul de la valeur dite "eigenvalue" à partir de l'écart-type (sd)
eigenvalues = pca_result$sdev^2

# Calcul de la captation de la variance par les composantes
composantes_variance <- sapply(eigenvalues, function(x) round((x/(sum(eigenvalues))*100)))

# Visualisation de base (les valeurs en y sont des pourcentages)
plot(composantes_variance)

# Interprétation des résultats
# Les deux axes principaux captent environ 35% de la variance





#### Distribution chronologique des collocations ----
# Première étape: Trouver les collocations
# Step 1: Séparer les documents par groupe de 2 années
setDT(df.fr)
df.fr.lemma <- df.fr[, annee:=as.integer(str_sub(`Publication Date`, 1, 4))][
  , .(doc_id, annee)
]

setDT(annotation.fr)

# Vérification de l'identité des identifiants
identical(annotation.fr[, .(doc_id), by="doc_id"][, .(doc_id)]$doc_id, df.fr.lemma$doc_id)

# Ajout des années à la table udpipe
annotation.fr <- data.table::merge.data.table(annotation.fr, df.fr.lemma, by="doc_id")



data_by_year <- split(annotation.fr, annotation.fr$annee)

# Étape 3: Calculer la cooccurrence pour chaque année
cooccurrence_by_year <- lapply(data_by_year, function(x) {
  temp <- cooccurrence(x = x$lemma, 
                       relevant = x$upos %in% c("NOUN", "ADJ"),
                       skipgram = 2)
  temp$annee <- unique(x$annee)
  return(temp)
})

# Assembler tous les groupes en une table unique
all_cooc <- rbindlist(cooccurrence_by_year)

all_cooc[,`:=`(cooc_terms = paste(term1, term2),
               term1=NULL,
               term2=NULL,
               N=cooc,
               cooc=NULL)]

# Élimination de "arrondissement sud ouest"
all_cooc <- all_cooc[!cooc_terms %in% c("arrondissement sud ouest",
                                        "pied carré",
                                        "comité exécutif",
                                        "client création",
                                        "client direction",
                                        "direction création", 
                                        "octobre heure",
                                        "heure heure",
                                        "sud ouest année")]


top_cooc_per_year <- all_cooc[order(annee, -N)]

top_n <- 3
top_cooc_per_year <- top_cooc_per_year[, head(.SD, top_n), by = "annee"]

#### Visualisation des cooccurrences en fonction d'années données ----
# Example years to focus on
selected_years <- c(2004, 2006, 2011, 2022)

# Filter the data for the selected years and top 3 collocations per year
filtered_data <- top_cooc_per_year %>%
  filter(annee %in% selected_years) %>%
  arrange(annee, -N) %>%
  group_by(annee) %>%
  slice_head(n = 3)

# Plotting
colors <- viridis(12)
ggplot(filtered_data, aes(x = as.factor(annee), y = N, fill = cooc_terms)) +
  geom_bar(stat = "identity", position = position_stack()) +
  labs(x = "Année", y = "Fréquence", fill = "Collocation") +
  theme_minimal() +
  scale_fill_viridis_d(begin = 0.4)+
  geom_text(aes(label = cooc_terms), position = position_stack(vjust = 0.5), size = 4) +
  theme(text = element_text(size = 13))


