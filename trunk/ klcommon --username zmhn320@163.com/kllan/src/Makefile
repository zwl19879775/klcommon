CC = gcc
LINK = gcc
AR = ar
CFLAG = -c -Wall -g -o
LINKFLAG = -o
MMFLAG = -MM
ARFLAG = crv
RM = del
SRCDIR = ./
TMPDIR = ../tmp
LIBDIR = ../lib
TARGET = $(LIBDIR)/libkl.a
SRCLIST = $(wildcard *.c)
OBJLIST = $(SRCLIST:%.c=%.o)

all: Makefile.obj $(TARGET)

Makefile.obj: $(SRCLIST)
	$(CC) $(MMFLAG) $(SRCLIST) > $@

include Makefile.obj

%.o: $(SRCDIR)/%.c
	$(CC) $(CFLAG) $@ $<

$(TARGET): $(OBJLIST)
	$(AR) $(ARFLAG) $@ $^ 

clean:
	-$(RM) *.o

rebuild: clean all

.PHONY: clean all rebuild

