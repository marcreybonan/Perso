#Un etudiant en pharmacie n est pas encocre a l aise avec la denomination commune internationale des medicaments, un de ses amis lui a ecrit un programme en python, qui, a partir de la DCI du medicament indique la famille de medicaments a laquelle il appartient 

#On commence par creer une fonction reposant sur un dictionnaire 
def classe_medicament(molecule):
familles_medicaments = {
"zepam" = "benzodiazepine"
"olol" = "betabloquants" 
"dine" = "antihistaminique" 
"vir" = "anti viraux"
"one" = "corticoides"
"prazole" = "inhibiteurs de la pompe a protons"
"cilline" = "betalactalines"
"mycine" = "macrolides" 
"fibrate" = "normolipemiants" 
"conazole" = "antifongiques"
"tropium" = "antagonistes muscariniques"
"statine" = "inhibiteurs de hmg_coa reductase"
"navir" = "inhibiteurs de la prothease du vih"
"amide" = "antagonistes des canaux potassiques"
"caine" = "antagonistes des canaux sodiques voltage_dependants"}

#Liste bientôt terminée 

def (x,y):
   return chainex.lower == chainey.lower 
#A compléter plus tard, éviter tout problème de casse

#On pourrait éventuellement expliquer quelle classe pharmaceutique fait quoi ainsi que les posologies de base
