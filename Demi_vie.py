print("La demi vie d'un medicament correspond au temps necessaire afin que la concentration plasmatique diminue de moitie. Cette demi vie peut etre d une poignee d'heures a des dizaines d'heures, ainsi celle-ci est tres variable d'un medicament a un autre et ce meme au sein d'une famille de medicaments ;
      par exemple l'Alprazolam et le Bromazepam sont deux benzodiazepines, le premier a une duree de vie moyenne de 11 heures tandis que le second de 17 heures et peut monter a 20 heures. Certains medicaments ayant des effets secondaires pouvant etre dangereux, 
comme la somnolence au volant notamment, il peut etre utile de connaitre la demi vie d'un medicament ou la concentration plasmatique du medicament absorbe.")

print("Il faut 5 demi-vie afin qu'un médicament soit totalement éliminé") 

#On met en place une fonction nous permettant de calculer le pourcentage restant du medicament dans l'organisme de l utilisateur
def pourcentage(demi_vie, heure_depuis_prise):
      pourcentage=100*(0.5**(heure_depuis_prise/(demi_vie))
                       return pourcentage

#On demande a l utilisateur depuis combien de temps le medicament a ete absorbe ainsi que la demi vie du medicament concerne
