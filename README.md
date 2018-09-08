# Presentation of easieR (English/French below)

easieR is R metapckage. The aim is to provide a GUI able to perform classical statistical analyses. However, easieR is more flexible than most R GUI given that it can also be used directly in command line : each individual function can be used either with a GUI or with command line. Moreover, there are a lot of possibilities with R, but each author print their results differently, which might cause discomfort to beginners. easieR avoid this problem by providing a nice (Rmarkdown) standard presentation for all the analyses. easieR can progress quite quickly since the functions is based on the functions developed in other R packages.

Concerning the presentation, easieR systematically prints : descriptive statistics, assumptions, the main analysis, and effect sizes. For each analysis, it is possible to obtain less common analyses (e.g., Wilcox test instead of t test) and identify and remove these outliers automatically. This approach aims to increase robustness of analyses and improve replication of results.

# Functions already present in easieR

easieR has many capabilities for the moment : import/export data, sort, select, scale, ranking, multiple imputation for missing data, mathematical operations, descriptive statistics, t test, correlations, anova, regressions, logistic regressions, exploratory factor analyses, confirmatory factor analyses, principal component analyses, reliability analyses.

It is possible to split analyses by group (regressions, correlations) for some analyses. Plots are also automatically generated, most often using ggplot2.



# Evolutions and correction
The English version is under development. 

# The most important
Most functions works quite well, and many new functions will be developed soon but if there bugs, just let me know by posting in the issues part of this github repository.


# Présentation générale d'easieR

easieR() est un ensemble de fonction R qui permettent de réaliser les analyses statistiques classiques en utilisant des boîtes de dialogues. 
Contrairement à une interface graphique complète, easieR permet plus de possibilités car chaque fonction peut-être utilisée dans la console indépendamment de l'arborescence des menus. 
easieR s'appuie sur les fonctions qui ont déjà été développées dans R et les intégère en un tout cohérent avec une présentation des résultats homogène. L'intérêt de cette approche est que easieR peut se développer rapidement en proposant les fonctions qui sont parmi les plus flexibles/intéressantes pour chaque type d'analyse. 

easieR renvoie systématiquement les statistiques descriptives, la vérification des conditions d'application, l'analyse principale, et les tailles d'effets.
Pour chaque analyse, il y a la possibilité d'obtenir les résultats pour des alternatives plus robustes lorsque les conditions d'application ne sont pas respectées. Par ailleurs, un algorithme adapté à chaque type d'analyse permet d'identifier les valeurs influentes et de les supprimer. Cette manière de procéder permet à l'utilisateur de s'assurer que ses résultats sont robustes et réplicables, et ne dépendent pas de la présence de quelques observations qui pourraient modifier les résultats. 

# Les fonctionnalités de easieR
A l'heure actuelle, easieR permet de réaliser :
- des opérations d'importation (y compris des fichiers Excel)/exportation de données et de résultats, 
- de sélection d'observations et de variables, 
- de réaliser des opérations mathématiques basiques,
- de centrer/centrer réduire des variables

D'obtenir le répertoire de travail, de modifier le répertoire de travail, d'obtenir la liste des objets en mémoire de R, d'obtenir les fonctions qui permettent de réaliser les analyses qui ne sont pas intégrées à easieR

Au niveau des analyses, easieR permet de réaliser :
- les différentes formes de chi carré (ajustement, indépendance, McNemar), 
- les corrélations (Bravais-Pearson, Sperman, Kendall, tetrachorique, polychorique, mixte, comparaison de corrélations dépendantes et indépendantes
- les différentes formes de t de Student (comparaison à la norme, échantillons appariés, échantillons indépendants) et leurs équivalents non paramétriques et robustes
- les analyses de variances et de covariance, avec leurs équivalents non paramétriques et robustes lorsque cela est possible, 
- les régressions simples, multiples, avec possibilité de tester les interactions, ainsi que les médiations simples ou distantes,
- les regressions logistiques, 
- les analyses factorielles exploratoires, 
- les analyses en composantes principales,
- les indices de fiabilité (alpha de Cronbach, coefficient d'accord de Kendall, corrélation intra-classe)
 

# Evolutions et correction
Une version anglophone de easieR va être développée.



## Fonctions qui vont être développées 
- fonctions de manipulation de données
1) fonctions logiques (si, et, ou) 
2) les fonctions d'appariement de base de données
3) des fonctionnalités de manipulation de données textuelles
- fonctions graphiques
- les analyses suivantes :
1) SEM
2) analyse en cluster
3) manova
4) modèle linéaire généralisé
5) analyse en série temporelle
6) analyse de puissance
7) théorie de réponse à l'item
8) analyse du cas unique
...



## Utiliser easieR en ligne de commande
A l'heure actuelle, easieR n'est pas totalement compatible avec une utilisation en ligne de commande. Une des évolutions de easieR sera de pouvoir l'utiliser soit par des boîtes de dialogues, soit en ligne de commande. Cette fonctions est en cours de développement, avec pour avantage, lorsque l'utilisateur se trompe d'avoir la boîte de dialogue qui prend le relais. 

# Pour finir
toutes suggestions d'amélioration du code ou de développement de nouvelles fonctionnalités sont bienvenues.
vous pouvez les déposer en français ou en anglais dans l'onglet issues
