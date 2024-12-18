import random
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.cm as cm


# Courbes pour les résultats de la 2.13 

iterations = []
durations = []
file= open("exper_16_17_18_19/exper_gen_abr_15_5.txt","r")
lines = file.readlines()
for line in lines:
    res=line.strip().split(":")
    n=res[0]
    tps=res[1]
    iterations.append(int(n))
    durations.append(float(tps))

couleur=cm.tab20(np.linspace(0, 1, 20)) 
plt.plot(durations,iterations,label=f"etiquetage",color=couleur[random.randint(0,19)])
    
plt.legend()
plt.ylabel("Puissance de 2")
plt.xlabel("Durée")
plt.show()
file.close()
# Courbes pour les résultats de la 2.14 

"""
iterations = []
durations = []
taille_liste = []
file = open('exper_13_14_15/exper_somme.txt', "r")
lines = file.readlines()
for line in lines:
    res=line.strip().split(":")
    n=res[0]
    data=res[1]
    tmp_tps=[]
    tmp_ls=[]
    for sep in data.split(";"):
        val=sep.split(",")
        tps=val[0]
        len_1=val[1]
        tmp_ls.append(int(len_1))
        tmp_tps.append(float(tps))
    iterations.append(int(n))
    durations.append(tmp_tps)
    taille_liste.append(tmp_ls)

tps_array = np.array(durations) 
fct_tps = tps_array.T.tolist()
len_array=np.array(taille_liste)
fct_len = len_array.T.tolist()

couleur=cm.tab20(np.linspace(0, 1, len(fct_len))) 

for i,(ls_tps,ls_len,c) in enumerate(zip(fct_tps,fct_len,couleur)):
    plt.plot(ls_tps,iterations,label=f"exp_somme{i+1}",color=c)
    #plt.scatter(iterations,ls_len,label=f"exp_somme{i+1}",color=c)
    #plt.scatter(ls_tps,iterations,label=f"exp_somme{i+1}",color=c)
plt.legend()
plt.ylabel("Itérations")
plt.xlabel("Durée")
plt.show()

#for i,(ls_len,c) in enumerate(zip(fct_len,couleur)):
    #plt.plot(iterations,ls_tps,label=f"exp_somme{i+1}",color=c)
    #plt.scatter(iterations,ls_len,label=f"taille du polynôme pour exp_somme{i+1}",color=c)

#plt.legend()
#plt.xlabel("Itérations")
#plt.ylabel("taille du polynome")
#plt.show()
file.close()
"""

#  Courbes pour les résultats de la 2.15
"""
iterations = []
durations = []
taille_liste = []
file = open('exper_16_17_18_19/exper_produit_15_2.txt', "r")
lines = file.readlines()
for line in lines:
    res=line.strip().split(":")
    n=res[0]
    data=res[1]
    tmp_tps=[]
    tmp_ls=[]
    for sep in data.split(";"):
        val=sep.split(",")
        tps=val[0]
        len_1=val[1]
        tmp_ls.append(int(len_1))
        tmp_tps.append(float(tps))
    iterations.append(int(n))
    durations.append(tmp_tps)
    taille_liste.append(tmp_ls)

tps_array = np.array(durations) 
fct_tps = tps_array.T.tolist()
len_array=np.array(taille_liste)
fct_len = len_array.T.tolist()

couleur=cm.tab20(np.linspace(0, 1, len(fct_len))) 

for i,(ls_tps,ls_len,c) in enumerate(zip(fct_tps,fct_len,couleur)):
    #plt.plot(ls_tps,iterations,label=f"exp_produit{i+1}",color=c)
    #plt.scatter(iterations,ls_len,label=f"taille du polynôme pour exp_somme{i+1}",color=c)
    plt.scatter(ls_tps,iterations,label=f"exp_produit{i+1}",color=c)

plt.legend()
plt.xlabel("Durée")
plt.ylabel("Itérations")
plt.show()

for i,(ls_len,c) in enumerate(zip(fct_len,couleur)):
    #plt.plot(iterations,ls_tps,label=f"exp_somme{i+1}",color=c)
    plt.scatter(iterations,ls_len,label=f"taille du polynôme pour exp_somme{i+1}",color=c)

plt.legend()
plt.xlabel("Itérations")
plt.ylabel("taille du polynome")
plt.show()
file.close()"""