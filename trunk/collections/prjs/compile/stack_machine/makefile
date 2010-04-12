CC = gcc
LINK = gcc
override CFLAG += -c -Wall -o
LINKFLAG = -o
RM = rm

all: sm sasm

sm: sm.o
	$(LINK) $(LINKFLAG) $@ $^

sasm: sasm.o
	$(LINK) $(LINKFLAG) $@ $^

sm.o: sm.c sm.h
	$(CC) $(CFLAG) $@ $<

sasm.o: sasm.c sm.h
	$(CC) $(CFLAG) $@ $<

clean:
	-$(RM) sm.o sasm.o sm sasm

rebuild: clean all

