/* Pour compiler
   $ gcc -Wall -W -g racine.c -lm -lgmp -o racine
*/

#include <stddef.h>
#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>
#include <stdarg.h>
#include <time.h>
#include <errno.h>
#include <limits.h>
#include <math.h>
#include <gmp.h>

/* les bonnes idées du PERL */
static void die(int status, char *fmt, ... ) {

  va_list arg;

  va_start(arg, fmt);
  vfprintf(stderr, fmt, arg);
  va_end(arg);
  exit(status);
} /* die */

/* message(...) = fprintf(stderr, ... ) */
static void message(char *fmt, ...) {

  va_list arg;

  va_start(arg, fmt);
  vfprintf(stderr, fmt, arg);
  va_end(arg);
} /* message */

unsigned long dichotomie(unsigned long n) {

unsigned long inf, sup, med;

      if (n < 2)
        return n;

      /* Gestion des débordements */
      if (ULONG_MAX == (unsigned long) UINT_MAX) { // 32 bits sur linux
          if (n >= (unsigned long) USHRT_MAX * (unsigned long) USHRT_MAX)
              return (unsigned long) USHRT_MAX;
            sup = (unsigned long) USHRT_MAX;
      }
      else { // 64 bits sur linux
          if ( n >= (unsigned long) UINT_MAX * (unsigned long) UINT_MAX)
              return (unsigned long) UINT_MAX;
          sup = (unsigned long) UINT_MAX;
      }

      /* calcul par dichotomie */
      inf=1;
      sup = n < sup ? n : sup;

      while (inf != sup - 1) {
        med = (sup + inf) / 2;
        if (med * med > n ) {
          sup = med;
        }
        else {
          inf = med;
        }
      }
      return inf;
} /* dichotomie */

static size_t exp_base = (size_t) 10 ; /* granularité du calcul */

static char *decimales;    /* tampon de conversion pour l'affichage */

void affiche(mpz_t k) {
  char *ptr;

  mpz_get_str(decimales, 10, k);
  for (ptr = decimales + (ptrdiff_t) exp_base; *ptr != '\0' ; ptr --)
     putchar('0');

  printf("%s", decimales);
  *ptr = 'f' ;
} /* affiche */

/* fonction racine en quatres parties */
/* nb est le nombre dont on veut calculer la racine carrée */
/* p représente le nombre de décimales à calculer */
void racine(unsigned long nb, unsigned long p) {
  unsigned long ent;
  unsigned long niter;
  double fsp ;
  double esp;
  double entlg;
  mpz_t u, v, k, k1, s, v1, d, base_racine, carre_base;

  /* calcul de la partie entiére */
  ent = dichotomie(nb);
  if (ent * ent == nb || p == 0) {
    printf("%lu", ent);
    return;
  }
  printf("%lu.", ent);
  entlg = log10( (double) ent) + 1 ;
  /* calcul le nombre d'itération */
  niter = (p % exp_base) == 0 ? (p / exp_base) : (1 + p / exp_base);

  /* calcul le nombre maximum de limb occupé par exp_base décimales
     nota : limb est une cellule de tableau du type mpz_t comme l'est 1 bit
     pour un nombre binaire (limb à une taille de 32 bits)
  */
  fsp = (double) exp_base / log10((double) UINT_MAX) ;

  /* initialistion des nombres magiques du calcul */
  mpz_init(base_racine);
  mpz_ui_pow_ui (base_racine,10UL,exp_base);     /* 10 exposant exp_base */
  mpz_init(carre_base);
  mpz_mul(carre_base, base_racine, base_racine); /* base_racine au carré */

  esp = ceil (fsp * niter + entlg);
  mpz_init_set_ui(u, 2 * ent);
  _mpz_realloc (u, (mp_size_t) esp );
  mpz_mul(u ,u, base_racine);
  mpz_init(d);
  _mpz_realloc (d, (mp_size_t) esp);

  esp = ceil ( fsp * niter + 2 * fsp + entlg);
  mpz_init_set_ui(v, nb - ent * ent);
  _mpz_realloc (v, (mp_size_t) esp);
  mpz_mul(v, v, carre_base);
  mpz_init(k1);
  _mpz_realloc (k1, (mp_size_t) esp);

  mpz_init(k);
  _mpz_realloc (k, (mp_size_t) ceil(fsp));
  mpz_init(s);
  _mpz_realloc (s, (mp_size_t) ceil(fsp + entlg));
  mpz_init(v1);
  _mpz_realloc (v1, (mp_size_t) ceil (fsp * 2 + entlg));

  /* calcul en deux temps : plus rapide */
  mpz_fdiv_q(d, v, u);
  mpz_add_ui(d, d, 1);
  do {
    mpz_set(k, d);
    mpz_mul(k1, d, d);
    mpz_add(k1, k1, v);
    mpz_mul_ui(d, d, 2);
    mpz_add(d, d, u);
    mpz_fdiv_q(d, k1, d);
  } while (mpz_cmp(k, d) != 0 );
  affiche(d);

  if ( --niter == 0 )
    return;
  /* seconde partie : initialisation */
  mpz_add(s, d, u);
  mpz_mul(v1, s, d);
  mpz_sub(v, v, v1);
  mpz_mul(v, v, carre_base);
  mpz_add(u, s, d);
  mpz_mul(u, u, base_racine);

  mpz_clear(v1);
  mpz_clear(s);
  mpz_clear(k);
  /* boucle principale : sans autre imbrication de boucle */
  while ( 1 ) {
   mpz_fdiv_q(d, v, u);
   mpz_add(k1, d, u);
   mpz_mul(k1, k1, d);
   if ( mpz_cmp(k1, v) <= 0 ) {
     affiche(d);
     if ( --niter == 0 )
       return;
     mpz_sub(v, v, k1);
     mpz_mul(v, v, carre_base);
     mpz_mul_ui(d, d, 2UL);
     mpz_add(u, u, d);
     mpz_mul(u, u, base_racine);
   }
   else {
     mpz_sub_ui(d, d, 1UL);
     affiche(d);
     if( --niter == 0 )
       return;
     mpz_add(k1, u, d);
     mpz_mul(u, k1, d);
     mpz_sub(v, v, u);
     mpz_mul(v, v, carre_base);
     mpz_add(u, k1, d);
     mpz_mul(u, u, base_racine);
   }
  } /* while ( 1 ) */
} /* racine */

int main(int argc, char **argv) {

  unsigned long nb[]={0,30,10}; /* {nombre, precision, granularite } */
  int i;
  unsigned long j;
  clock_t tictac;

  /* traitements des arguemnts de la ligne de commande */
  if ( argc == 1 )
    die(1, "usage : %s <nombre> [ <précision> [ <granularité> ] ]\n", *argv);
  if ( argc > 4 ) {
    message("%s les arguments en trop seront ignorés\n", argv[0]);
    argc=4;
  }
  for(i = 1; i < argc; i++ ) {

    if( (isdigit(*argv[i]) == 0 ) && (*argv[i] != '+') )
       die(1, "%s : %s n'est pas un nombre postif\n", argv[0], argv[i]);
    nb[i-1] = strtoul(argv[i], (char **) NULL, 10);
    if (errno == ERANGE)
      die(1, "%s : dépassement de capacité : %s \n", argv[0], argv[i]);
  }
  if ( nb[2] == 0 )
    message("%s : la granularité ne peut être nulle\n", argv[0]);
  else
    exp_base = nb[2];
  /* initialisations de la chaine de conversion et d'affichage */
  if ( (decimales = malloc((size_t) exp_base + 1)) == NULL )
    die(1, "%s : pas assez de mémoire\n", argv[0]);
  for ( j=0; j <= exp_base; j++ )
    decimales[j] = 'f';
  /* lancement du calcul */
  tictac = clock();
  racine(nb[0], nb[1]);
  printf("\n");
  message(" Temps de calcul : %.2f secondes\n",
         (float) (clock() - tictac) / CLOCKS_PER_SEC);
  return 0;
} /* main */

/* 
 * Local Variables:
 * compile-command: "gcc -Wall -W racine.c -lm -lgmp -o racine"
 * End:
*/
