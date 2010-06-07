/**
  stringpair.c
  Kevin Lynx
  6.6.2010
*/
#include "stringpair.h"
#include <stdlib.h>
#include <string.h>

static StringPair *newspair (const char *s, void *u) {
    StringPair *sp = (StringPair*) malloc(sizeof(*sp));
    sp->s = (char*) malloc(strlen(s)+1);
    strcpy(sp->s, s);
    sp->u = u;
    return sp;
}

static void freespair (StringPair *sp) {
    free(sp->s);
    free(sp);
}

StringPairList *sp_new () {
    StringPairList *l = (StringPairList*) malloc(sizeof(*l));
    l->head = 0;
    l->tail = 0;
    return l;
}

void sp_free (StringPairList *sl) {
    while (sl) {
        StringPairList *t = sl->tail;
        freespair(sl->head);
        free(sl);
        sl = t;
    }
    free(sl);
}

void sp_add (StringPairList **sl, const char *s, void *u) {
    if (*sl != 0 && sp_get(*sl, s) != 0 ) return ; /* exist */
    else {
        StringPairList *spl = (StringPairList*) malloc(sizeof(*spl));
        spl->head = newspair(s, u);
        spl->tail = *sl;
        *sl = spl;
    }
}

void *sp_get (StringPairList *sl, const char *s) {
    for (; sl; sl = sl->tail) {
        if (strcmp(s, sl->head->s) == 0) return sl->head->u;
    }
    return 0;
}

void sp_remove (StringPairList **sl, const char *s) {
    StringPairList **prev = sl, *spl = *sl;
    for (; spl; prev = &spl->tail, spl = spl->tail) {
        if (strcmp(s, spl->head->s) == 0) {
            *prev = spl->tail;
            freespair(spl->head);
            free(spl);
            return;
        }
    }
}

void sp_tranvers (StringPairList *sl, tranverseFunc f, void *u) {
    while (sl) {
        StringPairList *t = sl->tail;
        f(sl->head, u);
        sl = t;
    }
}
