#Ici on souhaite creer un bot medical simple qui posera des questions simples a l utilisateur et qui adaptera en fonction ses questions
#Au debut il ne sera pas question de questions compliquees dans l examen clinique, le programme devra etre capable a terme de lire une prise de sang etc....

def bot_medgenerale():
   print("Enchante, je suis le Dr bot")

input(" :")

#Recolte des donnees 

taille=float(input("Quelle taille faites vous ? :"))
poids=float(input("Combien pesez vous ? :"))
imc=poids/(taille*taille)
tension=(input("Quelle est votre tension de reference ? :"))

#boleen pour ce qui suit
fumeur=input("Est ce que vous fumez" ? :"))
bruit_ausculation_pulmonaire
bruit_auscultation_cardiaque
hepatomegalie
splenomegalie
anemie


if imc<19:
   print("Vous etes en situation de sous poids")
elif imc>20 and imc<25:
   print("Vous avez un imc normal")
elif imc>25 and imc<30:
   print("Vous etes en situation d obesite")
else:
   print("Vous etes en situation d obesite severe")



#tension, bruit a l auscultation, fumeur, fc au repos, atcd....
