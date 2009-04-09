# makefile for kl script base plugin and libloader plugin
CC = gcc
LINK = gcc
AR = ar
override CFLAG += -c -Wall -I../src -o
LINKFLAG = -o
MMFLAG = -MM -I../src
ARFLAG = crv
RM = rm
SRCDIR = ./
LIBDIR = ../lib
TARGET = $(LIBDIR)/libklplugin.a
SRCLIST = $(wildcard *.c)
OBJLIST = $(SRCLIST:%.c=%.o)

ifeq ($(OS), Windows)
	MACRO = -D_WIN32 
	LIBDIR = ..\lib
	TARGET = $(LIBDIR)\libklplugin.a
	RM = del
else
	MACRO = -D_UNIX
endif

all: Makefile.obj $(TARGET)

Makefile.obj: $(SRCLIST)
	$(CC) $(MMFLAG) $(SRCLIST) > $@

include Makefile.obj

%.o: $(SRCDIR)/%.c
	$(CC) $(MACRO) $(CFLAG) $@ $<

$(TARGET): $(OBJLIST)
	$(AR) $(ARFLAG) $@ $^ 

clean:
	-$(RM) *.o

distclean: clean
	-$(RM) $(TARGET)

rebuild: distclean all

.PHONY: clean all rebuild distclean

