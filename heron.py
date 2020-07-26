#! /usr/bin/python3
# -*- coding: UTF-8 -*-

def dichotomie(nb):
    """Calcule la partie entière de la racine carrée de l'entier positif nb."""
    def moyenne(a, b):
        return (a + b)//2
    if nb < 1:
        return nb
    inf=1
    sup=nb
    med = moyenne(inf, sup)
    while inf < med:
        if med * med > nb:
            sup=med
        else:
            inf=med
        med = moyenne(inf, sup)
    return med

def heron(nb, x):
    """Calcule le terme suivant x dans la méthode de Héron""" 
    return (x + nb/x)/2

import decimal

def heron_by_iteration(nb, ent, iteration):
    """Caclule de la racine carrée du nombre nb, dont la partie
    entière est ent par la méthode de héron, en fixant le nombre
    d'itération à iteration"""
    n=decimal.Decimal(nb)
    e=decimal.Decimal(ent)
    u=n/e
    for i in range(iteration):
        u=heron(n, u)
    return u

def heron_to_precision(nb, ent, precision):
    """caclule de la racine carrée du nombre 'nb', dont la partie 
    entière est 'ent' par la méthode de héron, pour au moins
    'precision' decimales (en fait jusqu'à ce que la différence
    entre deux termes successifs soit inférieure à 10 ** (-precision)"""
    
    precision=precision+len(str(ent))
    decimal.getcontext().prec=precision
    n=decimal.Decimal(nb)
    e=decimal.Decimal(ent)
    dix=decimal.Decimal(10)
    prec=decimal.Decimal(precision)
    p=10 ** -prec
    v=n/e
    u=heron(n, v)
    while v - u > p:
        v=u
        u=heron(n, v)
    return u



def racine(nb, precision=0, iteration=10):
    """Calcule la racine carrée du nombre entier nb par la méthde de Héron.
    """
    if nb < 0:
        raise ValueError
    if precision < 0:
        raise ValueError
    if iteration < 0:
        raise ValueError
    ent=dichotomie(nb)
    if ent * ent == nb:
        return nb
    if precision == 0:
        r=heron_by_iteration(nb, ent, iteration)
    else:
        r=heron_to_precision(nb, ent, precision)
    return r

import argparse

if __name__ == "__main__" :
    parser=argparse.ArgumentParser(
            description="Calcule la racine carrée d'un entier par la méthode de Héron",
            epilog="""L'argument PRECISION n'a pas le même sens lorsque ITERATION est fixé ou pas.
Si ITERATION n'est pas fixée c'est le nombre de décimales juste qui seront
calculées. 
Si ITERATION est fixée, c'est le nombre de décimales qui seront affichées
au bout de ITERATION itérations. Le nombre de décimales affichées par défaut dans ce cas est 28.""")
    parser.add_argument('number', type=int, help="Le nombre dont on calcule la racine carrée")
    parser.add_argument('-p', '--precision', type=int, nargs=1, help="le nombre de décimales calculées, par défaut 1000")
    parser.add_argument('-i', '--iteration', type=int, nargs=1, help="Fixe le nombre d'itérations")
    ns=parser.parse_args()
    nb=ns.number
    precision=None
    iteration=None
    if ns.precision:
        precision=ns.precision.pop()
    if ns.iteration:
        iteration=ns.iteration.pop()
    if iteration:
        if precision:
            decimal.getcontext().prec=precision
        precision=0
    elif not precision:
        precision=1000
    if not iteration:
        iteration = 0
    print(racine(nb, precision,  iteration))
