#
LEX = flex
YACC = byacc
TARGET = type_check

all: $(TARGET)

y.tab.c: type_check.y
	$(YACC) -d $<
lex.yy.c: type_check.l 
	$(LEX) $<
%.o: %.c
	gcc -g -c -Wall $<
$(TARGET): y.tab.o lex.yy.o sym_table.o
	gcc -o $@ $^ -lfl

clean:
	-rm *.o y.tab.c y.tab.h lex.yy.c $(TARGET)

