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

## Compatibilité avec les OS
easieR est compatible avec Windows 10 64 bits. 
Les tests de compatibilité avec les Mac OS et les autres versions de Windows sont en cours et certains bugs sont en cours de résolution. 
Les tests de compatibilité Linux suivront.


## Bugs identifiés et qui doivent être corrigés
- les corrélations tétrachoriques polychoriques ne sont pas compatibles avec Mac
- les fonctionnalités pour centrer centrer/réduire ne sont pas compatible avec Mac
- l'annulation de certaines boîtes de dialogue dans l'analyse de variance fait planter la fonction
- la sauvegarde des résultats intégrées à easieR n'est pas totalement compatible avec Mac

## Fonctions qui vont être développées 
- fonctions de manipulation de données
1) fonctions logiques (si, et, ou) 
2) les fonctions d'appariement de base de données
3) des fonctionnalités de manipulation de données textuelles
- fonctions graphiques
- les analyses suivantes :
1) analyse factorielle confirmatoire
2) SEM
3) analyse en cluster
4) manova
5) modèle linéaire généralisé
6) analyse en série temporelle
7) analyse de puissance
8) théorie de réponse à l'item
9) analyse du cas unique
...

## Amélioration des fonctions existantes 
- permettre au code d'être sourcé
- intégrer directement à easieR l'installation des packages requis pour sa bonne utilisation
- fournir les statistiques descriptives par effet dans l'analyse de variance
- ajouter des fonctionnalités pour les contrastes d'interactions (effets simples/post hoc sur les interactions)
- ajouter les conditions d'application de l'analyse de covariance


## Utiliser easieR en ligne de commande
A l'heure actuelle, easieR n'est pas totalement compatible avec une utilisation en ligne de commande. Une des évolutions de easieR sera de pouvoir l'utiliser soit par des boîtes de dialogues, soit en ligne de commande.

# Pour finir
toutes suggestions d'amélioration du code ou de développement de nouvelles fonctionnalités sont bienvenues.
vous pouvez me contacter à l'adresse suivante : nicolas.stefaniak(at)univ-reims.fr
