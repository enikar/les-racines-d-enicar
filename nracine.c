#include <stdlib.h>
#include <stdio.h>
#include <limits.h>
#include <unistd.h>
#include <time.h>

/* FIXME Revoir les fonctions pour utiliser des entiers plutôt
 * que des pointeurs pour tout ce qui n'est pas un tableau dans les
 * structures ci-dessous.
 * Dans un second temps, plutôt que d'utiliser des pointeurs, il
 * faudrait utiliser des tableaux pour permettre à gcc d'optimiser
 * d'avantage.
 *
 * Revoir aussi la façon dont je calcul la quantité de mémoire à 
 * allouer pour le calcul. Pour les grands nombres, ça ne marche
 * pas comme ça devrait. On calcule moins de décimales que demandées.
 * En plus le calcul est faux ! Exemple :
 * nracine -r18446744073709551615 =>        4294967294.999999999999999999999999999999
 * alors que le bon résultat devrait être : 4294967295.999999999883584678173065185545
 * La partie entière étant fausse, ça fausse tout le calcul.
 * Résolu : le problème provient du fait que pour éviter un débordement dans le
 * calcul, j'initialise à UINT_MAX la borne supérieure dans la dichotomie.
 * Or pour les nombre qui sont supérieur ou égale à UINT_MAX * UINT_MAX ma dichotomie
 * va retourner UINT_MAX-1 (car pour qu'elle fonctionne il faut que la borne supérieure
 * soit strictement supérieur au nombre cherché, car mon algo garantie que inf < sup
 * et que la dichotomie s'arrête quand med <= inf et retourne med). 
 */
typedef struct {
                size_t *ind;
                unsigned short *chiffre;
                } ENTIER;

typedef struct {
                short *ind;
                char *chiffre;
                } BCD;

/* FAIT */
typedef struct {
                size_t ind;
                unsigned long ent;
                char *chiffre;
                } RACINE;

typedef struct {
                unsigned short *ptr_t;
                unsigned short *ptr_v;
                unsigned short *ptr_u;
                size_t size;
                } POOL;

typedef enum {FALSE, TRUE} BOOLEAN;

#define MAX_PRECISION UINT_MAX
#define DEFAUT_PRECISION 30U
#define BASE_ENTIER 65536L
#define EXP_BASE 16

static void 
bcd(unsigned long m, BCD result)

/* convertie l'entier m en un nombre decimal code binaire BCD */
{
short indice;

    *result.chiffre=0;
    if (m == 0) {
        *result.ind=1;
        *(++result.chiffre)=0;
        return;
    }
    for (indice=1; m>0; indice++) {
        *(++result.chiffre)=m % 10;
        m /= 10;
    }
    *result.ind=--indice;
} /* bcd */


static BOOLEAN 
inf_ou_egal( BCD m, BCD n ) {
/* si m<=n inf_ou_egal=TRUE, si m>n inf_ou_egal=FALSE */

short indice;

    if (*m.ind > *n.ind)
        return FALSE;
    if (*m.ind < *n.ind)
        return TRUE;

    for (indice=*m.ind, m.chiffre += indice, n.chiffre += indice ;
            indice > 0;
            indice--, m.chiffre--, n.chiffre-- ) {
        if (*m.chiffre > *n.chiffre)
            return FALSE;
        if (*m.chiffre < *n.chiffre)
            return TRUE;
    } /* for ... */

    return TRUE;

} /* inf_ou_egal */


static void 
code_entier(unsigned long m, ENTIER result) {
size_t indice;

    *result.chiffre=0;
    if( m==0) {
        *(result.chiffre++)=0;
        *result.ind=1;
        return;
    }

    for (indice=1; m>0; indice++) {
        *(result.chiffre++)=(unsigned short)m;
        m >>=EXP_BASE;
    }
    *result.ind=--indice;

} /* code_entier */


static BOOLEAN 
strict_sup(ENTIER m, ENTIER n) {

/* si m>n strict_sup=TRUE, si m<=n strict_sup=FALSE */

size_t indice;

    if ( *m.ind < *n.ind )
        return FALSE;
    if ( *m.ind > *n.ind )
        return TRUE;

    for (indice=*m.ind, m.chiffre += indice-1, n.chiffre += indice - 1 ;
        indice >0;
        indice--, m.chiffre--, n.chiffre--) {
        if ( *m.chiffre < *n.chiffre )
            return  FALSE;
        if ( *m.chiffre > *n.chiffre )
            return  TRUE;

    } /* for ... */

    return FALSE;

} /* strict_sup */


static void 
ajoute( unsigned short m, ENTIER source, ENTIER result ) {

/* result=source + m
 * 0=<m<=99
 */

size_t indice;
size_t ind;
unsigned long retenue;

    retenue=  (unsigned long)m;
    ind=*source.ind;

    for (indice=1; indice <= ind; indice++) {
        unsigned long intermed=(unsigned long) *(source.chiffre++) + retenue;
        retenue= intermed >> EXP_BASE;
        *(result.chiffre++) = (unsigned short) intermed ;
    }/* for .... */

    if (retenue == 0)
        *result.ind=indice - 1;
    else {
        *result.ind = indice;
        *result.chiffre=(unsigned short)retenue;
    }

} /* ajoute */

static
void soustraire(ENTIER source, ENTIER result) {

/* result = result - source
 * source <= result
 */

unsigned short *ptr;
unsigned short retenue=0;
size_t indice;

    ptr=result.chiffre;
    for (indice=1; indice <= *source.ind; indice++, ptr++, source.chiffre++ ) {
        if ((unsigned long)*ptr >= (unsigned long)*source.chiffre + retenue) {
            *ptr -= *source.chiffre + retenue;
            retenue=0;
        } else {
            *ptr -= *source.chiffre + retenue - BASE_ENTIER;
            retenue=1;
        }
    } /* for .... */

    for (; indice <= *result.ind && retenue != 0; indice++, ptr++ ) {
        if (*ptr >= retenue) {
            *ptr -= retenue;
            retenue=0;
        } else {
            *ptr -= retenue - BASE_ENTIER ;
            retenue=1;
        }
    } /* for ... */


    for (ptr=result.chiffre + *result.ind - 1;  *ptr == 0 ;  ptr-- )
        --*result.ind;

} /* soustraire */

static
void multiplie( char m, ENTIER source, ENTIER result ) {
/* ( 0 <= m <= 127 )calcul dans le tableau result.chiffre le produit de m
 * par source.chiffre
 */

size_t indice;
unsigned long retenue=0;

    if (m==0) {
         *(result.chiffre)=0;
         *result.ind=1;
         return;
    }

    for(indice=1; indice<=*(source.ind);
        indice++, source.chiffre++, result.chiffre++) {
         unsigned long intermed=(unsigned long)m * *source.chiffre + retenue;
         retenue= intermed >> EXP_BASE;
         *result.chiffre = (unsigned short) intermed ;
    }

    if ( retenue==0 )
         *(result.ind) = indice - 1;
    else {
         *result.ind = indice;
         *result.chiffre =(unsigned short) retenue;
    }

} /* multiplie */


static unsigned long 
dichotomie(unsigned long nb) {

unsigned long sup, inf, med;

/* initialisation: recherche du plus grand entier n tel que n*n<=nb 
 * par dichotomie 
 * */

    if (nb<2)
        return nb;

    /* Gestion des débordements */
    if (ULONG_MAX == (unsigned long) UINT_MAX) { // 32 bits sur linux 
        if (nb >= (unsigned long) USHRT_MAX * (unsigned long) USHRT_MAX)
            return (unsigned long) USHRT_MAX;
        sup = (unsigned long) USHRT_MAX;
    }
    else { // 64 bits sur linux.
        if ( nb >= (unsigned long) UINT_MAX * (unsigned long) UINT_MAX)
            return (unsigned long) UINT_MAX;
        sup = (unsigned long) UINT_MAX;
    }
   
    /* calcul de la dichotomie */
    inf = 1;
    sup = nb < sup ? nb : sup;
    while (inf != sup - 1) {
      med = (sup + inf) >>1 ;
      if ( (med * med) > nb ) {
        sup=med;
      } else {
        inf=med;
      }
    } /*  while */

    return inf;
} /* dichotimie */


static unsigned long nb_int_utilise=0;

static void 
racine(unsigned long nb, size_t precision, RACINE *r, POOL *pool) {

/* les ENTIERs t,u et v sont codés en base '65536', chaque unsigned int
 * des tableaux U et V représente un chiffre entre 0 et 65535
 * u.ind et v.ind représente leur longueur respective i.e. le nombres
 * d'éléments de chaque tableaux (en fait ce sont des pointeurs car c'est
 * 'TURBO tableaux') . e.g. 123 sera codé de la facon suivante:
 * V[0]=3, V[1]=2, V[2]=1 v.ind=2, v.chiffre=V .
 * Le nombre 0 est codé avec un indice=1 et X[0]=0
 * cf la structure du type (pas content !) ENTIER
 */
char *chiffre;
size_t ind_t=0;
size_t ind_u=0;
size_t ind_v=0;
unsigned long ent, k;
ENTIER t;
ENTIER u;
ENTIER v;
//char c;

    r->ind=0;
    ent=dichotomie(nb);
    r->ent=ent;           /* partie entiere */
    if (ent * ent == nb)
        return;

    t.ind=&ind_t;
    t.chiffre=pool->ptr_t;
    u.ind=&ind_u;
    u.chiffre=pool->ptr_v;
    v.ind=&ind_v;
    v.chiffre=pool->ptr_u;

    k=ent;
    k *= 20;
    code_entier(k, u);
    k=ent * ent;
    k=100 * (nb-k);
    code_entier(k, v);
    chiffre=r->chiffre;

    for (;precision>0;precision--) {
        if ( (ind_v>=pool->size) || (ind_u>=pool->size) ) {
            nb_int_utilise=ind_v > ind_u  ? ind_v : ind_u;
            return;
        }
        char c=9;
        ajoute( 9, u, t );
        multiplie( 9, t, t );
        while( strict_sup(t,v) ) {
            c--;
            ajoute( c, u, t );
            multiplie( c, t, t );
        }

        ++r->ind;
        *(++chiffre)=c;
        soustraire(t, v);                  /* v=v-t  */
        multiplie(100, v, v);
        c <<= 1;
        ajoute(c, u, u);
        multiplie(10, u, u);

    } /* for(;precision>0;…) */

    nb_int_utilise=ind_v > ind_u ? ind_v : ind_u;
    return;

} /* racine() */


static BOOLEAN 
verifie(char *saisie, unsigned long max,unsigned long *nb) {

/* renvoie vraie si le nombre contenu dans saisie <= max */
char *TEMP; //26
char *MAXBCD; //24
short long_max;
unsigned long t;
short ind_t=0;
short ind_m=0;
BCD temp; //{&ind_t,TEMP};
BCD maxbcd; //{&ind_m,MAXBCD};
short indice=0;
short i;

    t=max;
    long_max=0;
    while (t != 0 ) {
        long_max++;
        t /= 10;
    }
    
    if ((TEMP=malloc(long_max + 2)) == NULL) {
        fprintf(stderr, "Pas assez de mémoire dans verifie\n");
        exit(1);
    }
    
    if((MAXBCD=malloc(long_max + 1)) == NULL) {
        fprintf(stderr, "Pas assez de mémoire dans verifie\n");
        exit(1);
    }

    TEMP[1]=0;
    TEMP[0]=0;
    temp.ind= &ind_t;
    temp.chiffre = TEMP;
    maxbcd.ind = &ind_m;
    maxbcd.chiffre = MAXBCD;

    while (*saisie != '\0' && indice < long_max + 1) {
        if (*saisie >= '0' && *saisie <= '9' ) {
            TEMP[++indice]=*saisie - '0';
        }
        saisie++;
    } /* while */

    if (indice>long_max)
        return FALSE;

    *temp.ind=indice;
    for (i=1; indice>i; indice--,i++) {
        char c=TEMP[indice];
        TEMP[indice]=TEMP[i];
        TEMP[i]=c;
    }

    bcd(max,maxbcd);
    if (inf_ou_egal(temp, maxbcd)) {
        *nb=0;
        for ( indice=*temp.ind; indice>0; indice-- ) {
            *nb *=10;
            *nb +=TEMP[indice] ;
        }
        return TRUE;
    }
    return FALSE;

} /* verifie */

static void 
usage(void) {
    fprintf(stderr,"usage:racine -h -p<précision> -r<nombre>\n\n");
    fprintf(stderr,"calcule la racine carrée d'un nombre entier\n\n");
    fprintf(stderr,"-h             imprime cette page et quitte\n");
    fprintf(stderr,"-p<n>          calcule  n décimales\n");
    fprintf(stderr,"               par défaut précision = %u\n",DEFAUT_PRECISION);
    fprintf(stderr,"-r<nombre>     calcule la racine carrée de nombre\n");
    fprintf(stderr,"               max. = %lu\n",ULONG_MAX);
} /* usage */


static unsigned long 
demande_nombre(void) {

char *buf;
int indice;
unsigned long nb;


        fprintf(stderr,"racine -h pour l'aide\n");
        buf = (char *)malloc(84 * sizeof(char));
        if ( buf==NULL ) {
            fprintf(stderr,"operation impossible !\n");
            exit(1);
        }
        setvbuf(stdin,buf,_IOLBF,40);
        do {
            for ( indice=0; indice<84; indice++ )
                *(buf+indice)='\0';
            fprintf(stderr,"\nOn calcule la racine de quoi au juste ? :");
            fgets(buf+40, 40, stdin );
            fprintf(stderr,"\n");
        } while (!verifie(buf + 40, ULONG_MAX, &nb));
        free((char *)buf);
        return nb;
} /* demande_nombre */


static unsigned long nb_int_alloue;

static void 
alloc_pool(unsigned long precision, RACINE *r, POOL *pool) {

/* FIXME à refaire */    
size_t tailalloue;
/* factor = log 10 / log 65536 */
double factor=0.20762050593046016;

    //tailalloue=precision + (precision % 2) + 2;
    tailalloue=precision + 1;
    r->chiffre=(char *)malloc( tailalloue *sizeof(char) );
    if (r->chiffre==NULL) {
        fprintf(stderr," Pas assez de mémoire pour le calcul de %lu décimales\n",precision);
        exit(1);
    }

    //tailalloue = precision/4 + 3; // FIXME
    tailalloue = (size_t) (factor * precision) + 12;
    pool->ptr_u=(unsigned short *)calloc(3*sizeof(unsigned short), tailalloue);
    if (pool->ptr_u== NULL ) {
        fprintf(stderr," Pas assez de mémoire !\n");
        exit(1);
    }

    pool->ptr_v=pool->ptr_u + tailalloue;
    pool->ptr_t=pool->ptr_v + tailalloue;
    pool->size=tailalloue-1;
    nb_int_alloue=tailalloue-1;

}


typedef struct {
    unsigned long nb;
    size_t precision;
    RACINE *racine;
    POOL *pool;
} INITIALISATION;


static void 
initialise_prog(int argc, char **argv, INITIALISATION *initialisation) {

extern char *optarg;
unsigned long t;
short rep;

char fl_sais=0;
    
    initialisation->precision = DEFAUT_PRECISION;

    while((rep = getopt(argc, argv,"hp:r:")) != EOF) {
        switch(rep) {
            case '?':
            case 'h':
                usage();
                exit(0);

            case 'p':
                if ( verifie(optarg,(unsigned long)MAX_PRECISION,&t) )
                    initialisation->precision = (size_t) t;
                break;
            case 'r':
                if ( verifie(optarg, ULONG_MAX, &t) ) {
                    fl_sais=1;
                    initialisation->nb=t;
                }
                break;
        } /* switch */
    } /* while */

    if (fl_sais == 0)
        initialisation->nb=demande_nombre();

    alloc_pool(initialisation->precision,
               initialisation->racine, 
               initialisation->pool);

} /* initialise_prog */


static void 
affiche(unsigned long nb, RACINE *r) {

unsigned long ind_r;

    ind_r = r->ind;
    if (ind_r > 0 ) {
        printf("racine de %lu est approximativement égal a:\n%lu.",nb,r->ent);
        for(size_t indice=1; indice <= ind_r; indice++) {
            r->chiffre++;
            printf("%d", *r->chiffre);
        }
        printf("\n\n");
        printf("nombres de décimales calculées :%lu\n",ind_r);
    }
    else
        printf("racine de %lu = %lu\n",nb,r->ent);


} /* affiche */


int 
main(int argc,char **argv) {

INITIALISATION init;
RACINE r;
POOL pool;

clock_t tictac;
float temps;
   
    init.racine=&r;
    init.pool=&pool;
    initialise_prog(argc, argv, &init);
    tictac = clock();
    racine(init.nb, init.precision, init.racine, init.pool);
    tictac = clock() - tictac ;
    temps =(float) tictac /  CLOCKS_PER_SEC;

    affiche(init.nb, init.racine);

    fprintf(stderr, "Nombres d'int alloués %lu, nombres d'int utilisés %lu\n",nb_int_alloue, nb_int_utilise);
    fprintf(stderr, "Temps de calcul :%.2f secondes\n",temps);

    return(0);

} /* main */

/*
 * Local Variables:
 * compile-command: "gcc -O2 -Wall -W -o nracine nracine.c"
 * End:
 */
