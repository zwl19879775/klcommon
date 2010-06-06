/**
 command parser
 Kevin Lynx
 6.5.2010
*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "cparse.h"

#define curchar(ps) (ps->str[ps->pos])

static int isidstr (char c) {
    return c == '_' || isalpha(c);
}

static Token numtoken (NumType num) {
    Token t = { T_NUM };
    t.u.num = num;
    return t;
}

static Token strtoken (char *str) {
    Token t = { T_STRING };
    t.u.str = str;
    return t;
}

static Token idtoken (char *str) {
    Token t = { T_ID };
    t.u.str = str;
    return t;
}

static Token deftoken (int type)  {
    Token t = { type };
    return t;
}

static NumType readnum (ParseState *ps) {
    char m[128], c;
    int p = 0;
    ps->pos--;
    while (isdigit(c = curchar(ps))) {
        m[p++] = c; 
        ps->pos++;
    }
    m[p] = 0;
    return castnum(m);
}

static char *readstr (ParseState *ps) {
    char s[MAX_STR_LEN], c;
    char *r;
    int p = 0;
    while ((c = curchar(ps)) != '"') {
        s[p++] = c;
        ps->pos++;
    }
    ps->pos++; /* eat '"' */
    s[p] = 0;
    r = (char*) malloc(p+1);
    strcpy(r, s);
    return r;
}

static char *readid (ParseState *ps) {
    char s[MAX_ID_LEN], c;
    char *r;
    int p = 0;
    ps->pos--;
    while (isidstr(c = curchar(ps))) {
        s[p++] = c;
        ps->pos++;
    }
    s[p] = 0;
    r = (char*) malloc(p+1);
    strcpy(r, s);
    return r;
}

Token cp_token (ParseState *ps) {
    for (; curchar(ps); ) {
        char c = ps->str[ps->pos++];
        if (c == ',') return deftoken(T_COMMA);
        if (c == '.') return deftoken(T_DOT);
        if (isdigit(c)) {
            NumType num = readnum(ps);
            return numtoken(num);
        }
        if (c == '"') {
            char *s = readstr(ps);
            return strtoken(s);
        }
        if (isidstr(c)) {
            char *s = readid(ps);
            return idtoken(s);
        }
    }
    return deftoken(T_EOF);
}

void cp_freetoken (Token *t) {
    if (t->t == T_STRING) {
        free(t->u.str);
    }
}

void cp_init (ParseState *ps, const char *s) {
    ps->str = (char*) malloc(strlen(s)+1);
    strcpy(ps->str, s);
    ps->pos = 0;
}

void cp_deinit (ParseState *ps) {
    free(ps->str);
    ps->pos = 0;
}
