#
# Makefile to make 2dim dynamical plugin
#  
#  Kevin Lynx
#  3.23.2009
#
#
CC = gcc
LINK = gcc
override CFLAG += -c -Wall -I../../src -o
LINKFLAG = -shared -o
MMFLAG = -MM -I../../src 
RM = rm
SRCDIR = ./
TARGET = ../../bin/2dim_plugin.dll
SRCLIST = $(wildcard *.c)
OBJLIST = $(SRCLIST:%.c=%.o)
DEPLIB = ../../lib/libkl.a

ifeq ($(OS),Windows)
	TARGET = ..\..\bin\2dim_plugin.dll
	RM = del
endif

all: Makefile.obj $(TARGET)

Makefile.obj: $(SRCLIST)
	$(CC) $(MMFLAG) $(SRCLIST) > $@

include Makefile.obj

%.o: $(SRCDIR)/%.c
	$(CC) $(CFLAG) $@ $<

$(TARGET): $(OBJLIST) $(DEPLIB)
	$(LINK) $(LINKFLAG) $@ $^ 

clean:
	-$(RM) *.o

distclean: clean
	-$(RM) $(TARGET)

rebuild: distclean all

.PHONY: clean all rebuild distclean




