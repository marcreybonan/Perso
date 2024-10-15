#Jouer des sons issus de meme en appuyant sur des boutons sur une fenetre tkinter

import tkinter as tk 
from tkinter import*
import winsound

#Def fonction 

def pbouton():
def dbouton()
def tbouton():
def qbouton():
def cbouton():
def sbouton():
def sebouton():
def hbouton():
def nbouton():
def dibouton():
#Pour chaque son une fonction

fenetre=Tk()
fenetre.title("Jukebox")

#Boutons à gauche de l'écran, verticalement

bouton1 = tk.Button(fenetre, text="Meme 1", command=pbouton)
bouton1.pack(pady=10)  

bouton2 = tk.Button(fenetre, text="Meme 2", command=dbouton)
bouton2.pack(pady=10)

bouton3 = tk.Button(fenetre, text="Meme 3", command=tbouton)
bouton3.pack(pady=10)

bouton4 = tk.Button(fenetre, text="Meme 4", command=qbouton)
bouton4.pack(pady=10)  

bouton5 = tk.Button(fenetre, text="Meme 5", command=cbouton)
bouton5.pack(pady=10)

bouton6 = tk.Button(fenetre, text="Meme 6", command=sbouton)
bouton6.pack(pady=10)

#Les autres seront à cote, cad à droite (7 a 12)
#fin

fenetre.mainloop()



#Differentes etapes
#Creer fonction
#Creer fenetre principale ok
#Creer boutons moitie
#Charger fichiers audio
#Creer une boucle ok 
