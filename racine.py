#! /usr/bin/python3
""" calcul de racine carrée de nombre entier en python. Le 20-04-2015.
Revue le 20-07-2020."""


import sys


def moyenne(a, b):
    """Retourne la moyenne de deux entiers"""
    return (a + b)//2


def sqrt_floor(nb):
    """Calcul de la partie entière de la racine carrée d'un nombre entier"""
    if nb <= 1:
        return nb
    inf = 1
    sup = nb
    while inf != sup - 1:
        med = moyenne(inf, sup)
        if (med * med) > nb:
            sup = med
        else:
            inf = med
    return inf


def newton(u, v):
    """Calcul le paquet de décimales suivant"""
    kplus = 1 + v // u
    while True:
        k = kplus
        kplus = (k * k + v) // (2 * k + u)
        if k == kplus:
            break
    return k


def iterations(p, exp_base):
    """Calcul le nombre d'itérations"""
    if p % exp_base == 0:
        return (p // exp_base)
    else:
        return (1 + p // exp_base)


def rac(nb, p0, exp_base):
    """Fonction qui réalise vraiment le calcul"""
    ent = sqrt_floor(nb)
    p = iterations(p0, exp_base)
    if p == 0 or ent * ent == nb:
        return [ent]
    r = [ent]
    base_racine = 10**exp_base
    carre_base = 10**(2*exp_base)
    u = 2 * ent * base_racine
    v = (nb - ent * ent) * carre_base
    while True:
        k = newton(u, v)
        r.append(k)
        p -= 1
        if p <= 0:
            break
        v = (v - (u + k) * k) * carre_base
        u = (u + 2 * k) * base_racine
    return r


def racine(nb, p, exp_base=10):
    """racine(nb, p, exp_base=10)
La fonction racine caclul la racine carrée du nombre entier nb
avec p décimales. exp_base contrôle la granularité du calcul, c'est
à dire le nombre de décimales calculées par itérations.
Retourne la racine carrée du nombre sous forme de chaîne."""
    floor, *fracs = rac(nb, p, exp_base)
    s = "{0}".format(floor)
    if len(fracs) == 0:
        return s
    else:
        fs = ["{0:0{width:d}}".format(frac, width=exp_base) for frac in fracs]
        return ''.join([s, '.'] + fs)


def usage():
    """Affiche l'usage et termine le programme"""
    print("Usage: %s <nombre> <précision> <granularité>" % sys.argv[0])
    s = "où <nombre>, <précision> et <granularité> sont des entiers positifs"
    print(s)
    sys.exit(1)


def main():
    """Fonction principale qui gère les arguments de la ligne de commande,
puis lance le caclul et affiche le résultat."""
    narg = len(sys.argv)
    if narg < 2 or narg > 4:
        usage()
    try:
        nombre = int(sys.argv[1])
        if nombre < 0:
            print("Racine des nombres négatifs non gérés")
            usage()
        if narg >= 3:
            precision = int(sys.argv[2])
            if precision < 0:
                print("La précision ne peut pas être négative")
                usage()
        else:
            precision = 30
        if narg == 4:
            exp_base = int(sys.argv[3])
            if exp_base <= 0:
                print("La granularité doit être stritement positive")
                usage()
        else:
            exp_base = 10

        print(racine(nombre, precision, exp_base))
    except ValueError:
        usage()


if __name__ == "__main__":
    main()
