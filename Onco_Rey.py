#Ce programme traite un sujet médical lié à l'oncologie, pour des raisons évidentes, cela a été vulgarisé et très simplifié
#Dans ce programme nous nous interesserons à un patient nommé Rey.

import random 

protocole = "ABVD" 
details_protocole = ["Vinblastine", "Bléomycine", "Doxorubicine", "Dacarbazine"]
taille = 1.65
poids_debut_traitement = 67
imc_debut_traitement = poids_debut_traitement/(taille*taille)
#On affiche les données que l'on possède sur le patient

print(imc_debut_traitement)
print(f"Rey mesure {taille}, pèse {poids_debut_traitement} et a un IMC de {imc_debut_traitement}, il suit le protocole {protocole}")

#Après plusieurs mois de traitement il a été décidé de changer le protocole
details_protocole.remove("Dacarbazine")
details_protocole.append("Cyclophosphamide")
print(details_protocole)

#Après quelques mois de traitement on décide de faire un bilan afin de voir si les cellules cancéreuses sont en régression
scanner_bon = False 
if scanner_bon:
   print("Le patient est sur le chemin de la guérison")
else: 
   print("Le protocole ne fonctionne pas comme prévu")
   
#Après plusieurs semaines de traitement on décide à nouveau de faire un bilan 
scanner_bon = True
bilan_sanguin = False 

if scanner_bon and bilan_sanguin :
   print("Le patient est en rémission")
 elif scanner_bon and not bilan_sanguin:
    print("La chimiothérapie a fonctionnée mais il faut traiter le patient pour qu'il retrouve une formule leucocytaire normale")
 else:
    print("Revoir le traitement")
    
#Rey doit réaliser des bilans à intervalle régulier
bilan_complet=["Scan", "bilan sanguin", "echographie", "radiographie"]

#On commence la surveillance, au début tous les 3 mois puis tous les 6 mois et enfin une fois par an, les durees sont exprimees en mois 
mois_remission= 0
frequence_bilan = 3
duree_bilan = 24

while mois_remission < duree_bilan:
   if mois< 12:
      frequence_bilan = 3
   elif mois < 24:
      frequence_bilan = 6
   else:
      frequence_bilan = 12

#Je vous demande si vous trouvez le traitement du patient Rey long
#Repondre avec un adjectif
duree_traitement = (input("Trouvez vous la durée du traitement élevée :"))
print(f"Selon l'utilisateur, la durée du traitement est {duree_traitement}")

#Dans chaque cancer, il y a un risque plus ou moins eleve de rechute, dans le cas de Rey, le taux de rechute est de 10%
#Nous souhaitons savoir si il va rechuter


def rechute(probabilite=0.1):
   return random.random() < probabilite
if rechute(0.1):
   print("Rey aura une rechute")
else:
   print("Rey n'aura pas de rechute")
