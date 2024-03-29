# Presentation of easieR (English/French below)

easieR is R metapackage. The aim is to provide a GUI able to perform classical statistical analyses. However, easieR is more flexible than most R GUI given that it can also be used directly in command line : each individual function can be used either with a GUI or with command line. Moreover, there are a lot of possibilities with R, but each author print their results differently, which might cause discomfort to beginners. easieR avoid this problem by providing a nice (Rmarkdown) standard presentation for all the analyses. easieR can progress quite quickly since the functions is based on the functions developed in other R packages.

Concerning the presentation, easieR systematically prints : descriptive statistics, assumptions, the main analysis, and effect sizes. For each analysis, it is possible to obtain less common analyses (e.g., Wilcox test instead of t test) and identify and remove these outliers automatically. This approach aims to increase robustness of analyses and improve replication of results.

## Functions already present in easieR

easieR has many capabilities for the moment : import/export data, sort, select, scale, ranking, multiple imputation for missing data, mathematical operations, descriptive statistics, t test, correlations, anova, regressions, logistic regressions, exploratory factor analyses, confirmatory factor analyses, principal component analyses, reliability analyses.

It is possible to split analyses by group (regressions, correlations) for some analyses. Plots are also automatically generated, most often using ggplot2.


## Evolutions and correction
The English version is under development. 
easieR already allow switching language (easieR() > Interface > Choose language). 
File `./R/lang_en_EN.R` contain english messages to be displayed, but need proper review.

## The most important
Most functions works quite well, and many new functions will be developed soon but if there are bugs, just let me know by posting in the issues part of this github repository.

easieR can be used either by the GUI and in command lines. For now, easieR is not a package available on CRAN. Thus, it requires  to follow several steps for the installation. 

**Step 0** : (Linux only) prerequisites

For Linux users, ensure to have `tcl`, `tk`, `glpk` and `yad` (optional but recommended for dialog boxes) installed on your system. Refer to your distribution packages repositories to install them. 

**Step 1** : open R and install *devtools*. 

```{r eval=FALSE}
install.packages("devtools",dependencies = TRUE, repos = "https://mirror.ibcp.fr/pub/CRAN")
```

For Linux users, `stringi` could require to be installed before `devtools`.

```{r eval=FALSE}
# If installing devtools fails because of stringi:
install.packages("stringi", repos = "https://mirror.ibcp.fr/pub/CRAN")
```

Now, the package « devtools » is installed. You must load it with the function  <code>library</code>. Once the package loaded, you can use it during all the session without have to load it anew. To load "devtools", you must use the command:

```{r}
library(devtools)
```

If an error message occurs in the console, the package is not correctly installed. Try to close R, open R, and the following command line:

```{r echo=F}
# If loading devtools failed:
install.packages("devtools",dependencies = TRUE, type="binary", repos = "https://mirror.ibcp.fr/pub/CRAN")
```

If the package is correctly installed, a message should be printed in the console:

```{r echo=F}
print("##Le chargement a nécessité le package : devtools")
```

It is also possible that nothing is printed. Do not worry.

**Step 2**: the installation of easieR

You can install easieR with the following command line:

```{r ,eval=FALSE}
install_github("NicolasStefaniak/easieR", type="binary")
```

You know that easieR is correctly installed by loading the package with the following command line:

```{r eval=FALSE}
library(easieR)
```

The last update is printed. Check that it is the last time that I have update easieR on github. 


**Step 3**: Check that all the packages have been correctly installed 

In order to ensure that all the functions works, it is required to check that all the packages are correctly installed. This check is made with the following command line:

```{r eval=FALSE }
ez.install()
```

If for "package.mal.installes", the result is character(0), everything has been correctly done.



**Step 4**: Install Pandoc

The last step requires to install pandoc. This supplementary package allows to get html ouput.
All the instructions for pandoc is available here: https://github.com/jgm/pandoc/releases/

Once R is closed, the packages are unloaed. Each time you open R, if you want to use 'easieR', you need to use the following command line before you can use it:

```{r eval=FALSE }
library(easieR)
```

## Update of easieR 

I try to correct bug as quickly as possible. Moreover, I wan to add some new functions in the future. If you want to update easieR, you can use the following command line:

```{r eval=FALSE}
devtools::install_github("nicolasstefaniak/easier", type="binary", dependencies=F)
```

# Version français


easieR est un package qui peut être utilisé en boîte de dialogue (GUI) ou en ligne de commande. Ce document expliquera comment l'utiliser d'une manière ou d'une autre. Cependant, pour le moment, easieR n'est pas un package disponible sur le CRAN. Il est donc nécessaire de passer par quelques étapes intermédiaires. Pour s'assurer d'une installation correcte de easieR, il faut suivre **les étapes suivantes**.  

**Étape 0** : (Linux uniquement) installer les pré-requis

Pour les utilisateurs Linux, assurez-vous que les paquets suivants soient installés sur votre système : `tcl`, `tk`, `glpk` et `yad` (optionnel mais recommandé pour un bon affichage des boîtes de dialogue). Référez-vous au gestionnaire de paquets de votre distribution.

**Etape 1** : tout d'abord, il faut  ouvrir R et installer le package *devtools* de la manière suivante. 

```{r eval=FALSE}
install.packages("devtools",dependencies = TRUE, repos = "https://mirror.ibcp.fr/pub/CRAN")
```

Pour les utilisateurs Linux, si le paquet `stringi` pose problème lors de l'installation de `devtools`, installez-le indépendemment puis relancer l'installation de `devtools` comme suit:

```{r eval=FALSE}
# Si l'installation de devtools échoue en raison d'un problème avec stringi:
install.packages("stringi", repos = "https://mirror.ibcp.fr/pub/CRAN")
```

Normalement, le package « devtools » est à présent installé. Il faut le charger. Pour charger un package, on utilise la fonction <code>library</code>. Une fois que le package est chargé, vous pouvez l’utiliser pendant toute la durée de la session sans devoir le charger à nouveau. Donc, pour charger le package *devtools*, il faut taper :

```{r}
library(devtools)
```


Si un message d’erreur survient ici (« erreur » apparaît dans la console), c’est que vous n’avez pas réalisé correctement les opérations qui précèdent. Si le package est installé correctement, R va indiquer :

```{r echo=F}
print("##Le chargement a nécessité le package : devtools")
```

Il se peut aussi que rien ne soit affiché. Ce n’est pas grave.

**Etape 2**: l'installation de easieR 

Vous pouvez installer easieR à présent grâce à la ligne de commande :

```{r ,eval=FALSE}
install_github("NicolasStefaniak/easieR", type="binary")
```

Vous savez que easieR est installé en chargeant le package à l'aide de la ligne de commande suivante :

```{r eval=FALSE}
library(easieR)
```



La date de la dernière mise à jour est également indiquée (Last update). Il est bon de vérifier sur github dans l'onglet "nouvelles fonctionnalités et bugs" si des versions plus récentes sont proposées. 

**Etape 3**: Vérifier l'installation des packages

Pour pouvoir profiter de l’ensemble des fonctionnalités de easieR, il est nécessaire de vérifier que tous les packages soient bien installés. Cela peut être réalisé grâce à la fonction :

```{r eval=FALSE }
ez.install()
```

A cette étape, vous saurez que tous les packages sont correctement installés si dans la sortie de résultats, vous avez "packages.mal.installes : character(0)". 



**Etape 4**: l'installation de Pandoc

La dernière étape consiste à installer pandoc. Ce logiciel complémentaire vous permet d'obtenir les sorties html pour les résultats. 

Pour pandoc, il faut se rendre sur le site de pandoc (https://github.com/jgm/pandoc/releases/)  et de l’installer manuellement. Vous devez d'abord avoir fermé R avant de l'installer. Vous choisissez la version qui correspond à votre ordinateur (le fichier pkg pour les utilisateurs **mac** et le fichier msi pour les utilisateurs **windows**). Après l'avoir téléchargé, vous double-cliquez et vous suivez les consignes d’installation.


Chaque fois que vous fermez R, les packages sont déchargés. Pour pouvoir utiliser easieR, il faudra donc utiliser la fonction <code>library</code> et ensuite, vous pourrez utiliser la fonction easieR :

```{r eval=FALSE }
library(easieR)
```

## 2.3. Mise à jour de easieR 

Dès qu’un bug est identifié, des corrections du package sont réalisées. Par ailleurs, de nouvelles fonctionnalités sont régulièrement ajoutées à easieR. Dès lors, pour pouvoir en profiter, il est utile de pouvoir faire une mise à jour. En d’autres termes, la procédure décrite ci-dessous est utile pour les personnes qui ont déjà installé easieR et qui veulent le mettre à jour. La procédure la plus sûre pour atteindre cet objectif est :

1) d’ouvrir R (si R est déjà ouvert, fermez R au préalable pour vous assurer que des packages ne sont pas chargés)

2) copier coller les lignes de commandes ci-dessous : 


```{r eval=FALSE}
devtools::install_github("nicolasstefaniak/easier", type="binary", dependencies=F)
```

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
easieR peut d'ores et déjà changer la langue utilisée (easieR() > Interface > Choisir la langue). 
Le fichier `./R/lang_en_EN.R` doit être mis à jour pour contenir des messages en anglais correct.



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
