library(dplyr)
library(openxlsx)
library(MASS)
library(ggplot2)
library(car)
library(FactoMineR)
library(factoextra)
library(missMDA)
library(GPArotation)
library(explor)
library(ggrepel)

setwd("C:\\Users\\leozw\\Desktop\\these_Isis")
list.files()

data <- read.xlsx("export_spss Isis 14.10.26.xlsx")
head(data)
glimpse(data)
summary(data)

# Combien agés de plus de 6 ans et IPPIsisDents
count(data, Age_An >= 6 & I_IPPIsisDents == 1)

# Filtrer les enfants de plus de 6 ans et IPPIsisDents
data6 <- data %>% filter(Age_An >= 6, I_IPPIsisDents == 1)
nrow(data6)

# Triage variables factorielles et numériques
fac <- c("I_Centre", "gender", "E_A_0CarnetdeSante", "E_B_0SuiviMed",
         "E_ToutesSecu150721", "E_D_Descol012019", "E_TousLogements150721",
         "E_ToutesASE150721", "E_H_ParentIsole", "E_O_InsecuAliment",
         "E_I_0Entourage", "E_M_Allophonie", "M_cl6.26param84",
         "FDep15IRISquintile", "C_IOTF3normal0")
num <- c("Age_An", "M_param84", "FDep15IRIS", "APL17CC", "C_NbPassSaup365derniersJ")


# Respecter les mêmes modalités de référence que dans le doc SPSS
# Référence = plus petite valeur
data6 <- data6 %>% mutate(across(all_of(fac), factor)) 

# Prendre les bonnes classes de référence
data6 <- data6 %>% mutate(
  E_ToutesSecu150721    = relevel(E_ToutesSecu150721, ref = "PUMA+Mutuelle"),
  E_TousLogements150721 = relevel(E_TousLogements150721, ref = "Logement personnel et salubre"),
  M_cl6.26param84       = relevel(M_cl6.26param84, ref = "No deprivation"),
  E_ToutesASE150721     = relevel(E_ToutesASE150721, ref = "pas de besoin de suivi social"))

# On va effectuer des tests de ce type: 
m <- glm(C_Nbcaries ~ E_ToutesSecu150721, family = negative.binomial(theta = 1), data = data6)
m


# Mais on automatise pour produire un tableau:
# Modèles univariés: test de chaque facteur pour les deux outcomes
# Compilation dans un tableau

# Fonction permettant de boucler chaque couple de variables avec reformulate
uni_nb <- function(y, x, data, theta = 1) {
  m  <- glm(reformulate(x, y), family = negative.binomial(theta), data = data)
  b  <- coef(m)
  se <- sqrt(diag(vcov(m, dispersion = 1)))  
  
  # on retire le nom de la variable au début du terme
  terme <- substring(names(b), nchar(x) + 1)
  terme[terme == ""] <- "(continue)"     # variables numériques ou binaires 0/1 sans suffixe
  
  data.frame(outcome = y, variable = x, terme = terme, n = nobs(m),
             IRR = exp(b), IC_bas = exp(b - 1.96 * se), IC_haut = exp(b + 1.96 * se),
             p = 2 * pnorm(-abs(b / se)), row.names = NULL)[-1, ]
}


outcomes <- c("C_Nbcaries", "C_IndiceCO")

# Application de la fonction
res_uni <- do.call(rbind, lapply(outcomes, function(y)
  do.call(rbind, lapply(c(fac, num), function(x)
    tryCatch(uni_nb(y, x, data6), error = function(e) NULL)))))

res_uni[, c("IRR", "IC_bas", "IC_haut")] <- round(res_uni[, c("IRR", "IC_bas", "IC_haut")], 2)
res_uni$p <- signif(res_uni$p, 3)
res_uni
write.xlsx(res_uni, "glm_univaries.xlsx")

glimpse(data6)


################################### ACM ########################################

# ---- 1. Jeu de données (noms de variables d'origine) ----
vars_act <- c("E_A_0CarnetdeSante", "E_B_0SuiviMed", "E_ToutesSecu150721",
              "E_D_Descol012019", "E_TousLogements150721", "E_ToutesASE150721",
              "E_H_ParentIsole", "E_O_InsecuAliment", "E_I_0Entourage",
              "C_IOTF3normal0", "C_Nbcaries", "C_IndiceCO", "C_NbPassSaup365derniersJ")


# Regrouper les indices caries et CO en classes
acm_df <- data6 %>%
  dplyr::select(all_of(c(vars_act))) %>%
  mutate(
    C_Nbcaries = cut(C_Nbcaries, c(-Inf, 0, 1, 2, Inf),
                     labels = c("0", "1", "2", ">=3")),
    C_IndiceCO = cut(C_IndiceCO, c(-Inf, 0, 1, 2, 4, Inf),
                     labels = c("0", "1", "2", "3-4", ">=5")),
    C_NbPassSaup365derniersJ = cut(C_NbPassSaup365derniersJ, c(-Inf, 0, 1, 2, 3, Inf),
                                   labels = c("0", "1", "2", "3", ">=4"))
  )

# Tout en facteur, modalités préfixées par le nom de la variable (NA conservés)
for (v in names(acm_df)) {
  f <- factor(acm_df[[v]])
  levels(f) <- paste0(v, "_", levels(f))
  acm_df[[v]] <- f
}
summary(acm_df)   # vérifier les effectifs de chaque modalité


# ---- 2. ACM ----
res <- MCA(acm_df, ncp = 50, graph = FALSE)
#explor(res)

# ---- 3. Valeurs propres ----
res$eig # Regarder la distribution des eigenvalue et leur pourcentage
fviz_screeplot(res, addlabels = TRUE, ncp = 5)

# ---- 4. Plans 1/2, 2/3, 1/3 (non tournés) ----
# fviz_mca_var trace un nuage où chaque pointe est une modalité
# on prend les 15 les plus contributives
# plus un point est éloigné, plus la modalité est atypique par rapport à l’ensemble des enfants.
# couleur: contribution à la construction des deux axes affichés
for (ax in list(c(1,2), c(2,3), c(1,3)))
  print(fviz_mca_var(res, axes = ax, repel = TRUE, col.var = "contrib",
                     gradient.cols = c("grey60", "orange", "red"),
                     select.var = list(contrib = 15)))

# ---- 5. Rotation oblimin sur k axes ----
k   <- 3
A   <- res$var$coord[, 1:k]
rot <- oblimin(A)
A_rot <- as.matrix(unclass(rot$loadings))
Phi   <- rot$Phi            # corrélations entre facteurs obliques
Phi

w <- res$call$marge.col     # masses des modalités
var_rot <- colSums(w * A_rot^2)
data.frame(facteur = paste0("F", 1:k),
           valeur_propre_avant = res$eig[1:k, 1],
           variance_apres = var_rot,
           pct_inertie_apres = 100 * var_rot / sum(res$eig[, 1]))

# Scores individuels (convention GPArotation : L = A %*% solve(t(Th)))
ind_rot <- res$ind$coord[, 1:k] %*% solve(t(rot$Th))
colnames(ind_rot) <- paste0("F", 1:k)

# Modalités dans l'espace tourné
mod <- as.data.frame(A_rot)
colnames(mod) <- paste0("F", 1:k)
mod$modalite <- rownames(A)

# ---- 6. Plans obliminés ----
plot_pair <- function(x, y)
  ggplot(mod, aes(.data[[x]], .data[[y]], label = modalite)) +
  geom_hline(yintercept = 0, lty = 2, col = "grey60") +
  geom_vline(xintercept = 0, lty = 2, col = "grey60") +
  geom_point(colour = "firebrick") + geom_text_repel(size = 3) +
  labs(x = x, y = y, title = paste("ACM, rotation oblimin :", x, "/", y)) +
  theme_minimal()
plot_pair("F1", "F2"); plot_pair("F2", "F3"); plot_pair("F1", "F3")














