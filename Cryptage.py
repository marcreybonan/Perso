#Le but ici est de crypter un message avec l'aide de deux clés, le decryptage avec une seule clé ne sera pas possible
#Possibilite d utiliser des caracteres d autres langues tels l arabe ou des caracteres chinois...
#En premier lieu, le programme ne sera réalisé qu'avec une clé 

import random
import string

#Générer un dictionnaire basé sur les règles désirées de substitution (chaque lettre d'un message = triplet alphanumérique)

def dic(clé):
