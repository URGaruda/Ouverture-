<?php

function traitement_fichier_1 (string $fichier) : void {
    global $table;
    $contenu = file_get_contents($fichier);
    $contenu = explode("\n", $contenu);
    foreach ($contenu as $ligne) {
        $ligne = explode(":", $ligne);
        if (count($ligne) < 2)
            continue;
        $n = $ligne[0];
        $val = $ligne[1];
        if (is_numeric($n)) {
            $n = (int) $n;
            $table[$n][] = $val;
        }
    }
}

function traitement_fichier_2 (string $fichier) : void {
    global $table;    
    $contenu = file_get_contents($fichier);
    $contenu = explode("\n", $contenu);
    foreach ($contenu as $ligne) {
        $ligne = explode(":", $ligne);
        if (count($ligne) < 2)
            continue;
        $n = $ligne[0];
        if (!is_numeric($n))
            continue;
        $couples = $ligne[1];
        $couples = explode(";", $couples);
        for ($strat = 0; $strat < count($couples); $strat++) {
            $couple = explode(",", $couples[$strat]);
            if (count($couple) < 2)
                continue;
            $t = $couple[0];
            $len = $couple[1];
            if (is_numeric($t) && is_numeric($len)) {
                $n = (int) $n;
                $t = (float) $t;
                $len = (int) $len;
                $table[$n][$strat][] = ["t" => $t, "len" => $len];
            }
        }
    }

}

function ecrire_fichier_1 (string $fichier) : void {
    global $table;
    $contenu = "";
    foreach ($table as $n => $valeurs) {
        if (count($table) < 0)
            continue;
        $moyenne = array_sum($valeurs) / count($valeurs);
        $contenu .= "$n:".number_format($moyenne, 6, '.', '')."\n";
    }
    file_put_contents($fichier, $contenu);
}

function ecrire_fichier_2 (string $fichier) : void {
    global $table;
    $contenu = "";
    foreach ($table as $n => $valeurs) {
        if (count($valeurs) < 0)
            continue;
        $contenu .= "$n:";
        foreach ($valeurs as $strat => $couple) {
            $t = [];
            $len = [];
            foreach ($couple as $c) {
                $t[] = $c["t"];
                $len[] = $c["len"];
            }
            $moyenne_t = array_sum($t) / count($t);
            $moyenne_len = intval(array_sum($len) / count($len));
            $contenu .= number_format($moyenne_t, 6, '.', '').",$moyenne_len;";
        }
        $contenu = substr($contenu, 0, -1)."\n";
    }
    file_put_contents($fichier, $contenu);
}

function traitement (string $fichier, int $type, int $n) {
    global $table;
    $table = [];
    $f1 = "traitement_fichier_{$type}";
    $f2 = "ecrire_fichier_{$type}";
    for ($i = 1; $i <= $n; $i++) {
        if (function_exists($f1))
            $f1(str_replace(".txt", "_$i.txt", $fichier));
    }
    if (function_exists($f2))
        $f2($fichier);
}


$common = "exper_13_14_15/";
traitement($common."exper_gen_abrs_20.txt", 1, 5);
traitement($common."exper_somme.txt", 2, 5);

$common = "exper_16_17_18_19/";
traitement($common."exper_gen_abr_15.txt", 1, 5);
traitement($common."exper_somme_15.txt", 2, 5);
traitement($common."exper_produit_15.txt", 2, 5);
