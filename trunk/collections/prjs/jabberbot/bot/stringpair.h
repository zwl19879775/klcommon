/**
  stringpair.h
  Kevin Lynx
  6.6.2010
*/
#ifndef ___STRING_PAIR_H_
#define ___STRING_PAIR_H_

typedef struct _StringPair {
    char *s; void *u;
} StringPair;

typedef void (*tranverseFunc) (StringPair*);

typedef struct _StringPairList {
    StringPair *head; struct _StringPairList *tail;
} StringPairList;

StringPairList *sp_new ();
void sp_free (StringPairList *sl);
void sp_add (StringPairList **sl, const char *s, void *u);
void *sp_get (StringPairList *sl, const char *s);
void sp_remove (StringPairList **sl, const char *s);
void sp_tranvers (StringPairList *sl, tranverseFunc f);

#endif
