/**
 command parser
 Kevin Lynx
 6.5.2010
*/
#ifndef ___CPARSE_H_
#define ___CPARSE_H_

enum TokenType {
    T_EOF, T_ID, T_NUM, T_STRING, T_COMMA, T_DOT
};

typedef int NumType;
#define castnum(v) (atoi(v))
#define MAX_STR_LEN (256)
#define MAX_ID_LEN (256)

typedef struct _Token {
    int t;
    union {
        NumType num;
        char *str;
    } u;
} Token;

typedef struct _ParseState {
    char *str;
    int pos;
} ParseState;

Token cp_token (ParseState *ps);
void cp_freetoken (Token *t);
void cp_init (ParseState *ps, const char *s);
void cp_deinit (ParseState *ps);

#endif
