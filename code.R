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
                "text2vec")

sapply(extensions, inst_ext_fun)

rm(list = ls())

# Construction de deux modèles

lda.model.14.fr <- readRDS("data/output/lda_model_14_fr.RDS")
lda.model.22.fr <- readRDS("data/output/lda_model_22_fr.RDS")
lda.model.47.fr <- readRDS("data/output/lda_model_47_fr.RDS")


doc.topic.distr.14<- readRDS("data/output/doc_topic_distrib_14.RDS")
doc.topic.distr.22 <- readRDS("data/output/doc_topic_distrib_22.RDS")
doc.topic.distr.47 <- readRDS("data/output/doc_topic_distrib_47.RDS")

df.fr <- readRDS("data/output/df_fr_lemma.RDS")
dgTMatrix.fr <- readRDS("data/output/dgtMatrix_fr.RDS")
dtm.tm.fr <- readRDS("data/output/dtm_tm_fr.RDS")



# Avec un modèle à 59 topics, le premier document est fortement associé aux thèmes 1, 17 et 53
barplot(doc.topic.distr.14[1, ], xlab = "topic",
        ylab = "proportion", ylim = c(0, 0.4),
        names.arg = 1:ncol(doc.topic.distr.14))

# Obtenir les mots les plus importants (chance qu'on les trouve dans un topic particulier, lambda = 1)
lda.model.14.fr$get_top_words(n = 6, topic_number = c(8L, 2L, 13L, 11L), lambda = 0.5)


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

# fwrite(doc.mots.rep.dt, "data/output/tableau_synthese_docs_topics.csv")
# doc.mots.rep.dt <- fread("data/output/tableau_synthese_docs_topics.csv")
