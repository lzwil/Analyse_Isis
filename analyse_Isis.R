library(dplyr)
library(openxlsx)
library(MASS)
library(ggplot2)
library(car)

setwd("C:\\Users\\A159692.APHM\\Desktop\\analyse_Isis")
list.files()

data <- read.xlsx("export_spss Isis 14.10.26.xlsx")
head(data)
glimpse(data)
summary(data)

# Combien agés de plus de 6 ans et IPPIsisDents
count(data, Age_An >= 6 & I_IPPIsisDents == 1)

# Filtrer les enfants de plus de 6 ans et IPPIsisDents
data6 <- data %>% filter(Age_An < 6, I_IPPIsisDents == 1)
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
write.xlsx(res_uni, "glm_univariesMoinsde6ans.xlsx")

