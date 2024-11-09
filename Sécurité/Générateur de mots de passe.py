#Import des librairies nécessaires

import random
import string

#On choisit les caractéristique des mots de passe qu'on souhaite générer
def generate_mdp(num_special_chars, num_uppercase, num_digits, num_lowercase):
    #Ajout d'ensemble de caractère désiré
    lowercase_chars = string.ascii_lowercase  
    uppercase_chars = string.ascii_uppercase  
    digit_chars = string.digits  
    special_chars = string.punctuation

    #On vérifie les valeurs, si il n'y en a pas on retourne une erreur 
    if num_lowercase < 0 or num_uppercase < 0 or num_digits < 0 or num_special_chars < 0:
        raise ValueError("Erreur")

    # Générer des parties du mot de passe qu'on mélangera ensuite
    password_chars = (
        random.choices(lowercase_chars, k=num_lowercase) + 
        random.choices(uppercase_chars, k=num_uppercase) + 
        random.choices(digit_chars, k=num_digits) + 
        random.choices(special_chars, k=num_special_chars)
    )

   #Mélange des caractères 
