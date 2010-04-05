#
LEX = flex
YACC = byacc
TARGET = tinyc

all: $(TARGET)

y.tab.c: tinyc.y
	$(YACC) -d $<
lex.yy.c: tinyc.l 
	$(LEX) $<
%.o: %.c
	gcc -g -c -Wall $<
$(TARGET): y.tab.o lex.yy.o symtab.o code.o
	gcc -o $@ $^ -lfl

clean:
	-rm *.o y.tab.c y.tab.h lex.yy.c $(TARGET)

