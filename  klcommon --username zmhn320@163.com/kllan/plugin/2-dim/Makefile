#
# Makefile to make 2dim dynamical plugin
#  
#  Kevin Lynx
#  3.23.2009
#
# compiled on MingW
#
CC = gcc
LINK = gcc
CFLAG = -c -Wall -I../../src -o
LINKFLAG = -shared -o
MMFLAG = -MM -I../../src 
RM = del
SRCDIR = ./
TARGET = ../../bin/2dim_plugin.dll
SRCLIST = $(wildcard *.c)
OBJLIST = $(SRCLIST:%.c=%.o)
DEPLIB = ../../lib/libkl.a

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

rebuild: clean all

.PHONY: clean all rebuild



