# Fonctions_R_Calcul_pauvrete-et-simuler_Interval
simuler par rééchantillonnage un intervalle de confiance d’un paramètre calculé sur un ensemble de données. 

Voici les descriptions pour les deux fonctions :

## Fonction `povAF`

**Description :**  
La fonction `povAF` calcule la mesure de pauvreté multidimensionnelle suivant la méthode AF (Alkire & Foster), prenant en compte les privations multiples et leur intensité dans une population.

**Arguments :**  
- `X` : Un dataframe ou une matrice contenant les réalisations des individus.
- `z` : Un vecteur de seuils définissant les privations.
- `w` : Un vecteur de poids pour les différents indicateurs.
- `k` : Un réel représentant le seuil d'intensité de privation.

**Sortie :**  
Un vecteur de trois éléments contenant :
1. La mesure de pauvreté multidimensionnelle `M0`.
2. L'incidence de la pauvreté `H`.
3. L'intensité de la pauvreté `A`.

**Méthodologie :**  
1. Calcul des privations pondérées pour chaque individu.
2. Détermination des individus pauvres basés sur le seuil `k`.
3. Calcul de l'incidence de la pauvreté `H` et de l'intensité `A`.
4. Agrégation des mesures pour obtenir `M0`.

## Fonction `simulerIC`

**Description :**  
La fonction `simulerIC` permet de simuler un intervalle de confiance pour un paramètre calculé sur un ensemble de données en utilisant des méthodes de rééchantillonnage comme le bootstrap et le jackknife.

**Arguments :**  
- `database` : Un dataframe ou un vecteur contenant les données.
- `typeSondage` : Un caractère indiquant le type de sondage utilisé (`SAS` pour sondage aléatoire simple, `SAT` pour sondage aléatoire stratifié).
- `strate` : Un entier indiquant le numéro de la variable de stratification (0 si non applicable).
- `FUN` : Une fonction utilisée pour calculer le paramètre cible.
- `methoSim` : Un caractère indiquant la méthode de rééchantillonnage à utiliser (`jackknife` ou `bootstrap`).
- `times` : Le nombre de simulations à effectuer (valeur par défaut : 1000, utilisé pour le bootstrap).
- `er` : Le niveau de confiance pour l’intervalle (valeur par défaut : 0.05 pour un intervalle de confiance de 95%).

**Sortie :**  
Un objet de classe List contenant :
1. Les données utilisées.
2. La méthode de simulation utilisée.
3. La fonction utilisée pour calculer le paramètre.
4. La valeur du paramètre calculée.
5. Un vecteur avec les bornes de l’intervalle de confiance.
6. Le niveau de confiance de l’intervalle.
7. Un vecteur avec les valeurs du paramètre calculées à chaque rééchantillonnage.

**Méthodologie :**  
- **Bootstrap** :
  1. Tirage avec remise pour former des sous-échantillons.
  2. Application de la fonction `FUN` sur chaque sous-échantillon.
  3. Répétition des étapes 1 et 2 plusieurs fois.
  4. Calcul des quantiles pour obtenir les bornes de l'intervalle de confiance.

- **Jackknife** :
  1. Élimination successive de chaque individu de l'échantillon.
  2. Application de la fonction `FUN` sur chaque échantillon réduit.
  3. Calcul des quantiles pour obtenir les bornes de l'intervalle de confiance.
