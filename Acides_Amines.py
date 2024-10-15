#Le present programme renverra pour chaque codon (triplet de nucleotides parmi une chaine d'ARN messager dit ARNm) l'acide amine correspondant


#On crée notre dictionnaire base sur le code genetique, celui ci renvoie à un codon l'acide amine correspondant ainsi que son code a trois et une lettre et sa famille
#Plus tard on pourra indiquer les acides amines essentiels 
#Il faudra faire attention a la casse 

code = {
    'GGU': 'Glycine (Gly) (G) AA le plus simple et unique',
    'GGC': 'Glycine (Gly) (G) AA le plus simple et unique',
    'GGA': 'Glycine (Gly) (G) AA le plus simple et unique',
    'GGG': 'Glycine (Gly) (G) AA le plus simple et unique ',
    'CCU': 'Proline (Pro) (P) AA unique',
    'CCC': 'Proline (Pro) (P) AA unique',
    'CCA': 'Proline (Pro) (P) AA unique',
    'CCG': 'Proline (Pro) (P) AA unique',
    'GUU': 'Valine (Val) (V) AA hydrophobe aliphatique',
    'GUC': 'Valine (Val) (V) AA hydrophobe aliphatique',
    'GUA': 'Valine (Val) (V) AA hydrophobe aliphatique',
    'GUG': 'Valine (Val) (V) AA hydrophobe aliphatique',
    'UUU': 'Phénylalanine (Phe) (F) AA hydrophobe aromatique',
    'UUC': 'Phénylalanine (Phe) (F) AA hydrophobe aromatique',
    'UGG': 'Tryptophane (Trp) (W) AA hydrophobe aromatique',
    'UAU': 'Tyrosine (Tyr) (Y) AA hydrophobe aromatique ',
    'UAC': 'Tyrosine (Tyr) (Y) AA hydrophobe aromatique',
    'AUG': 'Méthionine (Met) (M) AA soufré hydrophobe aliphatique',
    'UGU': 'Cystéine (Cys) (C) AA soufré neutre',
    'UGC': 'Cystéine (Cys) (C) AA soufré neutre',
    'GAU': 'Acide aspartique (Asp) (D) AA acide ',
    'GAC': 'Acide aspartique (Asp) (D) AA acide',
    'GAA': 'Acide glutamique (Glu) (E) AA acide',
    'GAG': 'Acide glutamique (Glu) (E) AA acide',
    'CAA': 'Glutamine (Gln) (Q) AA amide neutre ',
    'CAG': 'Glutamine (Gln) (Q) AA amide neutre ',
    'AAU': 'Asparagine (Asn) (N) AA amide neutre',
    'AAC': 'Asparagine (Asn) (N) AA amide neutre',
    'UCU': 'Sérine (Ser) (S) AA neutre',
    'UCC': 'Sérine (Ser) (S) AA neutre',
    'UCA': 'Sérine (Ser) (S) AA neutre',
    'UCG': 'Sérine (Ser) (S) AA neutre',
    'AGU': 'Sérine (Ser) (S) AA neutre',
    'AGC': 'Sérine (Ser) (S) AA neutre',
    'ACU': 'Thréonine (Thr) (T) AA neutre',
    'ACC': 'Thréonine (Thr) (T) AA neutre',
    'ACA': 'Thréonine (Thr) (T) AA neutre',
    'ACG': 'Thréonine (Thr) (T) AA neutre'
    'CGU': 'Arginine (Arg) (R) AA basique',
    'CGC': 'Arginine (Arg) (R) AA basique',
    'CGA': 'Arginine (Arg) (R) AA basique',
    'CGG': 'Arginine (Arg) (R) AA basique',
    'AGA': 'Arginine (Arg) (R) AA basique',
    'AGG': 'Arginine (Arg) (R) AA basique',
    'CAU': 'Histidine (His) (H) AA basique',
    'CAC': 'Histidine (His) (H) AA basique',
    'AAA': 'Lysine (Lys) (K) AA basique',
    'AAG': 'Lysine (Lys) (K) AA basique',
    'UUA': 'Leucine (Leu) (L) AA hydrophobe aliphatique',
    'UUG': 'Leucine (Leu) (L) AA hydrophobe aliphatique',
    'CUU': 'Leucine (Leu) (L) AA hydrophobe aliphatique',
    'CUC': 'Leucine (Leu) (L) AA hydrophobe aliphatique',
    'CUA': 'Leucine (Leu) (L) AA hydrophobe aliphatique',
    'CUG': 'Leucine (Leu) (L) AA hydrophobe aliphatique',
    'AUU': 'Isoleucine (Ile) (I) hydrophobe aliphatique',
    'AUC': 'Isoleucine (Ile) (I) hydrophobe aliphatique',
    'AUA': 'Isoleucine (Ile) (I) hydrophobe aliphatique',
    'GCU': 'Alanine (Ala) (A) hydrophobe aliphatique',
    'GCC': 'Alanine (Ala) (A) hydrophobe aliphatique',
    'GCA': 'Alanine (Ala) (A) hydrophobe aliphatique',
    'GCG': 'Alanine (Ala) (A) hydrophobe aliphatique',
    'UAA': 'Codon stop mettant fin à la traduction',   
    'UAG': 'Codon stop mettant fin à la traduction',  
    'UGA': 'Codon stop mettant fin à la traduction',  
}

#Eviter la casse

def acide_amine(triplet):
   triplet = triplet.upper()
   return code.get(triplet, "Erreur")

#On demande à l'utilisateur pour quel triplet il souhaite avoir l'acide amine ainsi que ses proprietes 

if__nom__ =="__main__":
   codon=input("Entrer un triplet de nucleotides (triplet): ")
   acide_amine = acide_amine(codon)
   print(f"L acide amine correspondant a votre codon '{codon}' est {acide_amine}")
