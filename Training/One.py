#Part 1 - Les bases
# Les variables

#Definir une variable
auteur = "Boris Vian"
livre = "Ecume des jours"

#Saisie de variable
pourquoi = input("Pourquoi ce livre est il votre prefere ? :")

#Afficher les variables
print(f"Son auteur prefere est {auteur}, son livre {livre} car {pourquoi}") 

#Les differents types de variable 
int = 2
float = 2.5
str = "Mercredi"
bool = True

#Possibilité de vérifier le type d'une variable
type(nom_variable)

#Récapitulatif
#Demander à l'utilisateur de donner sa taille, son age, son poids, si il est un homme, vérifier le type de chaque variable et afficher le tout
#On utilise .lower() afin de convertir la saisie en minuscules et éviter tout problème de casse
taille = input("Quelle est votre taille ? : ")
poids = input("Quel est votre poids ? :") 
age = input("Quel age avez vous ? :")
homme = input("Etes vous un homme ? :").lower()=="vrai"

type(taille)
type(poids)
type(age)
type(homme)

print(f"L utilisateur s appelle {nom}, il fait {taille}cm et pese {poids}kg et est il un homme ? {homme}")

#Operations entre variable
a = input("Valeur de a ? :")
b = input("Valeur de b ? :") 

c = a+b 
d = c-a
e = a/d
f = a%c 

