# Télécharge et installe sur ton Mac les 4 boîtes à outils dont on aura besoin ce soir.
install.packages(c("here", "tidyverse", "seasonal", "tseries"))


# Ouvre le package qui construit les chemins à partir de la racine du projet.
library(here)
# Ouvre la boîte à outils pour manipuler des tableaux de données.
library(tidyverse)
# Exécute R/models.R pour charger en mémoire toutes les fonctions de l'équipe.
source(here("R", "models.R"))
# Charge les données nettoyées dès 2000Q1 (log x 100, avec les poids) dans la boîte "q".
q <- load_quarterly(as.Date("2000-01-01"))


# Affiche la première et la dernière date du tableau, pour vérifier la période (2000Q1 → 2026Q3).
range(q$date)
# Compte le nombre de lignes, donc de trimestres ; on attend 107.
nrow(q)
# Affiche le nom de toutes les colonnes, y compris celles créées par load_quarterly.
names(q)
# Dessine l'indice des loyers (en log x 100) au fil du temps ; type = "l" relie les points par une ligne.
plot(q$date, q$y_rent, type = "l")

#######X-13----
# Désaisonnalise l'indice des loyers avec X-13 (départ = première date du tableau, horizon = 12 trimestres).
adj_rent <- x13_adjust(q$cpi_rent, min(q$date), 12)
# Vérifie si X-13 a trouvé une saisonnalité : TRUE = oui, FALSE = non (pour les loyers, on attend FALSE).
any(adj_rent$sf_fc != 0)
# Estime un ARIMA(3,1,0) avec drift sur la série désaisonnalisée et range le modèle dans "fit_rent".
fit_rent <- fit_arima(adj_rent$sa, list(order = c(3, 1, 0)), q$date)
# Affiche le modèle : les coefficients ar1, ar2, ar3, drift, et leurs erreurs standard (s.e.).
fit_rent
###test des résidus ----
# Récupère les résidus du modèle, sans le premier (pas un vrai résidu à cause de la différenciation).
res_rent <- residuals(fit_rent)[-1]

# Ljung-Box sur 8 trimestres, corrigé des 3 coefficients AR : H0 = pas d'autocorrélation (on veut p > 0,05).
Box.test(res_rent, lag = 8, type = "Ljung-Box", fitdf = 3)

# Jarque-Bera : H0 = les résidus suivent une loi normale (on veut p > 0,05).
tseries::jarque.bera.test(res_rent)
# Dessine l'histogramme des résidus pour voir leur forme à l'œil (onglet Plots).
hist(res_rent, breaks = 20)# Jarque-Bera : H0 = les résidus suivent une loi normale (on veut p > 0,05).
tseries::jarque.bera.test(res_rent)
# Dessine l'histogramme des résidus pour voir leur forme à l'œil (onglet Plots).
hist(res_rent, breaks = 20)

##trouver xtrm
# Crée un petit tableau qui associe chaque résidu à sa date (sans la première date, comme pour les résidus).
res_tab <- tibble(date = q$date[-1], residu = res_rent)
# Trie du plus grand au plus petit résidu en valeur absolue (abs) et affiche les 5 premiers.
head(arrange(res_tab, desc(abs(residu))), 5)

##ajout dummy pour 2000 Q4 
# Ré-estime l'ARIMA(3,1,0) avec une dummy valant 1 uniquement en 2000Q4 (le saut détecté par X-13), 0 ailleurs.
fit_rent2 <- fit_arima(adj_rent$sa, list(order = c(3, 1, 0), dummy = c(from = "2000-10-01", to = "2000-10-01")), q$date)
# Récupère les résidus du nouveau modèle, sans le premier.
res_rent2 <- residuals(fit_rent2)[-1]
# Refait Ljung-Box (toujours 3 coefficients ARMA) pour vérifier que l'absence d'autocorrélation tient toujours.
Box.test(res_rent2, lag = 8, type = "Ljung-Box", fitdf = 3)
# Refait Jarque-Bera : la normalité est-elle encore rejetée sans le choc de 2000Q4 ?
tseries::jarque.bera.test(res_rent2)


###meme test sur les autres blocs

# Range dans "specs" les spécifications de l'équipe pour les 3 blocs ARIMA (loyers sans dummy pour l'instant, comme dans leur code).
specs <- list(rent = list(order = c(3, 1, 0)), imp_xoil = list(order = c(0, 1, 2), dummy = c(from = "2009-01-01", to = "2016-10-01")), dom_xrent = list(order = c(1, 1, 1)))
# Estime d'un coup les 4 modèles (X-13 sur chaque bloc, ECM pour le pétrole, ARIMA pour les 3 autres) et les range dans "fits".
fits <- fit_blocks(q, 12, specs)
# Pour chacun des 3 blocs ARIMA, calcule la p-value de Jarque-Bera sur ses résidus (sans le premier).
sapply(fits$arima, function(fit) tseries::jarque.bera.test(residuals(fit)[-1])$p.value)
# Même test pour l'ECM du pétrole, sur les résidus de l'équation de court terme.
tseries::jarque.bera.test(residuals(fits$ecm$sr))$p.value

##controle résidu import
# Tableau dates + résidus standardisés (résidu / σ) du bloc importé hors pétrole, sans le premier résidu.
z_imp <- tibble(date = q$date[-1], z = residuals(fits$arima$imp_xoil)[-1] / sqrt(fits$arima$imp_xoil$sigma2))
# Affiche les 5 chocs les plus extrêmes (en valeur absolue) du bloc importé.
head(arrange(z_imp, desc(abs(z))), 5)

##controle résidu domestic 
# Même tableau pour le bloc domestique hors loyers.
z_dom <- tibble(date = q$date[-1], z = residuals(fits$arima$dom_xrent)[-1] / sqrt(fits$arima$dom_xrent$sigma2))
# Affiche les 5 chocs les plus extrêmes du bloc domestique.
head(arrange(z_dom, desc(abs(z))), 5)

##dummy pour les période xtrm 
# Ré-estime le domestique hors loyers, ARIMA(1,1,1) avec drift, sur sa série désaisonnalisée, avec une dummy valant 1 seulement en 2023Q1.
fit_dom2 <- fit_arima(fits$adj$dom_xrent$sa, list(order = c(1, 1, 1), dummy = c(from = "2023-01-01", to = "2023-01-01")), q$date)
# Récupère les résidus du nouveau modèle, sans le premier.
res_dom2 <- residuals(fit_dom2)[-1]

##test again
# Ljung-Box sur 8 retards, corrigé des 2 coefficients ARMA (1 AR + 1 MA) : on veut toujours p > 0,05.
Box.test(res_dom2, lag = 8, type = "Ljung-Box", fitdf = 2)
# Jarque-Bera sur les nouveaux résidus : la normalité tient-elle sans le choc de 2023Q1 ?
tseries::jarque.bera.test(res_dom2)

# Dummy "franc fort" : 1 pour chaque trimestre entre 2009Q1 et 2016Q4, 0 sinon (la même que celle de l'équipe).
D_franc <- as.numeric(q$date >= as.Date("2009-01-01") & q$date <= as.Date("2016-10-01"))

# Dummy d'impulsion : 1 seulement en 2011Q4, 0 sinon.
D_2011 <- as.numeric(q$date == as.Date("2011-10-01"))

# Estime l'ARIMA(0,1,2) sur l'importé désaisonnalisé, avec drift + les deux dummies cumulées comme régresseurs (xreg).
fit_imp2 <- arima(fits$adj$imp_xoil$sa, order = c(0, 1, 2), xreg = cbind(drift = seq_along(q$date), franc = cumsum(D_franc), imp2011 = cumsum(D_2011)))

# Récupère les résidus du nouveau modèle, sans le premier.
res_imp2 <- residuals(fit_imp2)[-1]

# Affiche le modèle : ma1, ma2, drift, franc et imp2011 avec leurs erreurs standard.
fit_imp2
# Ljung-Box sur 8 retards, corrigé des 2 coefficients MA.
Box.test(res_imp2, lag = 8, type = "Ljung-Box", fitdf = 2)
# Jarque-Bera sur les nouveaux résidus.
tseries::jarque.bera.test(res_imp2)

# Densité gaussienne – bloc loyers ----

# Prévision du niveau des loyers (log x 100) sur 12 trimestres avec la boucle de l'équipe (chocs futurs = 0, comme dans le cours).
y_fc <- drop(arima_paths(fit_rent2, adj_rent$sa, 12))
# Met l'historique et la prévision bout à bout dans une seule série.
y_all <- c(adj_rent$sa, y_fc)
# Retient la position du dernier trimestre observé (2026Q3).
n <- length(adj_rent$sa)
# Inflation des loyers sur un an (%) : niveau du trimestre moins niveau 4 trimestres plus tôt, pour chaque horizon h = 1 à 12.
pi_fc <- y_all[n + 1:12] - y_all[n + 1:12 - 4]

##les poids
# Poids d'un choc sur l'inflation annuelle : somme des 4 derniers psi (une inflation annuelle = 4 croissances trimestrielles).
c_w <- sapply(1:12, function(j) sum(psi[max(1, j - 3):j]))
# Variance de l'erreur de prévision pour h = 1 à 12 : sigma² × somme cumulée des poids au carré (formule du cours).
var_pi <- fit_rent2$sigma2 * cumsum(c_w^2)
# Borne basse de l'intervalle à 90 % : prévision − 1,645 × écart-type (1,645 = quantile 95 % d'une loi normale).
low90 <- pi_fc - qnorm(0.95) * sqrt(var_pi)
# Borne haute de l'intervalle à 90 % : prévision + 1,645 × écart-type.
up90 <- pi_fc + qnorm(0.95) * sqrt(var_pi)

##fanchart
# Crée les dates des 12 trimestres prévus : on part du dernier trimestre observé et on avance d'un trimestre 12 fois.
fc_dates <- seq(max(q$date), by = "quarter", length.out = 13)[-1]
# Inflation des loyers observée sur un an pour les 12 derniers trimestres (niveau moins niveau 4 trimestres plus tôt).
pi_hist <- y_all[(n - 11):n] - y_all[(n - 15):(n - 4)]
# Trace l'historique puis la prévision en une seule ligne ; ylim fixe l'échelle verticale, ylab le titre de l'axe.
plot(c(q$date[(n - 11):n], fc_dates), c(pi_hist, pi_fc), type = "l", lwd = 2, ylim = c(-0.5, 3.5), xlab = "", ylab = "Inflation des loyers sur un an (%)")
# Ajoute la zone de l'intervalle à 90 % en bleu transparent.
polygon(c(fc_dates, rev(fc_dates)), c(low90, rev(up90)), col = rgb(0, 0, 1, 0.2), border = NA)