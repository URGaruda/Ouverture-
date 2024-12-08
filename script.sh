#! /bin/bash 

make all 



./exp 

python3 courbes.py 

rm exp_*
rm exper_gen_abrs_20.txt
rm gen_permutation.txt
make clean 