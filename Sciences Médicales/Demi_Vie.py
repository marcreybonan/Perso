#On présente le principe de demie-vie d'une substance médicamenteuse en abandonnant les précédentes explications basées sur une classe thérapeutique précise
print("La demi-vie correspond au temps nécessaire pour que, après l’administration d’un médicament, sa concentration plasmatique diminue de moitié. \n
  La demi-vie est exprimée en unité de temps et peut varier de quelques minutes à plusieurs semaines selon les médicaments \n
  La fraction de médicament éliminée en fonction du temps dépend donc de sa demi-vie et l’on considère que la quasi-totalité du médicament est éliminée au bout de 5 demi-vie")

#On met en place une fonction nous permettant de calculer la concentration restante selon plusieurs critères (la demi-vie, la dose absorbée, le temps écoulé)
def drestante(demi_vie, heure_depuis_prise, dose_initiale):

      #On calcule le nombre de demi-vies écoulées
      demie_vie_passees = heure_depuis_prise/demi_vie

      #On s'intéresse à la concentration restante apres le temps passé
      crestante = dose_initiale*(0.5**demi_vie)
      
      return crestante

#On demande maintenant à l'utilisateur la dose administrée, la demi_vie du medicament et le temps ecoule et on stock ces informations dans une variable flottante 
def main(): 
   dose_initiale = float(input("Quelle est la dose consommée en mg ? :"))
   demi_vie = float(input("Quelle est la demi-vie de la molecule consommée en heures ? :"))
   heure_depuis_prise = float(input("Quand avez vous consommé la molecule en heure ? :"))

   #On calcule la concentration restante dans l'organisme de l'utilisateur
   
   
