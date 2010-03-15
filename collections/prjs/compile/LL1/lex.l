%{
#define NUM (256)
%}
DIGIT [0-9]
%%
{DIGIT}+ { return NUM; }
{DIGIT}+"."{DIGIT}* { return NUM; }
[ \t]+ ;
. { return yytext[0]; }
%%

int yywrap()
{
	return 0;
}
