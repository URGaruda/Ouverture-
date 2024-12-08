import numpy as np
import matplotlib.pyplot as plt
import matplotlib.cm as cm


""" Courbe pour les résultats de la 2.14 """
iterations = []
durations = []
file = open('exemples_fichiers_export/exemple_fichier_214_v2.txt', "r")
lines = file.readlines()
for line in lines:
    res=line.strip().split(":")
    n=res[0]
    tps=res[1]
    iterations.append(int(n))
    durations.append([float(v) for v in tps.split(";")])

""" Transposée de matrice afin d'avoir la liste des durées pour une fonction somme comme ça on peut tracée directement la liste des tps d'une fonction"""
tps_array = np.array(durations) 
fct_tps = tps_array.T.tolist()
couleur=cm.tab20(np.linspace(0, 1, len(fct_tps))) 
for i,(ls_tps,c) in enumerate(zip(fct_tps,couleur)):
    plt.plot(iterations,ls_tps,label=f"exp_somme{i+1}",color=c)
    
plt.legend()
plt.xlabel("Itérations")
plt.ylabel("Durée")
plt.show()