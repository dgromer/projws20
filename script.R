# Daten importieren
load(url("https://github.com/dgromer/projws20/blob/main/projws20_fb_test.RData?raw=true"))

# PSWQ-Items auswählen
pswq_items <- fb[, paste("pswq", 1:16, sep = "_")]

# Items 1, 3, 8, 10, 11 invertieren
pswq_items[, c(1, 3, 8, 10, 11)] <- 6 - pswq_items[, c(1, 3, 8, 10, 11)]

# Summe berechnen
pswq <- rowSums(pswq_items)

# ASI-3 Items auswählen
asi3_items <- fb[, paste("asi3", 1:18, sep = "_")]

# Umskalieren: 1-5 -> 0-4
asi3_items <- asi3_items - 1

# Subskala "Sozial" berechnen
asi3_social <- rowSums(asi3_items[,c(1, 6, 9, 11, 13, 17)])
