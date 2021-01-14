# Daten importieren
load(url("https://github.com/dgromer/projws20/blob/main/projws20_fb_test.RData?raw=true"))

# PSWQ -------------------------------------------------------------------------

# PSWQ-Items auswählen
pswq_items <- fb[, paste("pswq", 1:16, sep = "_")]

# Items 1, 3, 8, 10, 11 invertieren
pswq_items[, c(1, 3, 8, 10, 11)] <- 6 - pswq_items[, c(1, 3, 8, 10, 11)]

# Summe berechnen
pswq <- rowSums(pswq_items)

# ASI-3 ------------------------------------------------------------------------

# ASI-3 Items auswählen
asi3_items <- fb[, paste("asi3", 1:18, sep = "_")]

# Skalierung ändern von 1-5 nach 0-4
asi3_items <- asi3_items - 1

# Summenscores berechnen
asi3_social <- rowSums(asi3_items[c(1, 6, 9, 11, 13, 17)])
asi3_cognitive <- rowSums(asi3_items[c(2, 5, 10, 14, 16, 18)])
asi3_physical <- rowSums(asi3_items[c(3, 4, 7, 8, 12, 15)])
asi3_total <- rowSums(asi3_items)

# PANAS pre --------------------------------------------------------------------

# Items extrahieren
panas_pre_items <- fb[, paste("panas", 1:20, "pre", sep = "_")]

# Scores berechnen
panas_positive_pre <- rowMeans(panas_pre_items[c(1, 3, 4, 6, 10, 11, 13, 15, 17,
                                                 18)])
panas_negative_pre <- rowMeans(panas_pre_items[c(2, 5, 7, 8, 9, 12, 14, 16, 19,
                                                 20)])

# PANAS post -------------------------------------------------------------------

# Items extrahieren
panas_post_items <- fb[, paste("panas", 1:20, "post", sep = "_")]

# Scores berechnen
panas_positive_post <- rowMeans(panas_post_items[c(1, 3, 4, 6, 10, 11, 13, 15,
                                                   17, 18)])
panas_negative_post <- rowMeans(panas_post_items[c(2, 5, 7, 8, 9, 12, 14, 16, 19,
                                                   20)])

# Data-Frame zusammenbauen -----------------------------------------------------

proj <- data.frame(fb$session, fb$group, fb$sex, fb$age, pswq, asi3_social,
                   asi3_cognitive, asi3_physical, asi3_total,
                   panas_positive_pre, panas_negative_pre, panas_positive_post,
                   panas_negative_post)
