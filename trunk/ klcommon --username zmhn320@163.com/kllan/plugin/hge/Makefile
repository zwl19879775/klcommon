#
# Makefile to make hge dynamical plugin
#  
#  Kevin Lynx
#  3.4.2009
#
#
# Makefile for hgeHelper by Kevin Lynx 3.4.2009
#
# compiled on MingW
#
CPP = g++
LINK = g++
CXXFLAG = -c -Wall -I../../src -IE:/libs/hge181/include -o
LINKFLAG = -shared -o
MMFLAG = -MM -I../../src -IE:/libs/hge181/include
RM = del
SRCDIR = ./
TARGET = ../../bin/hge_plugin.dll
SRCLIST = $(wildcard *.cpp)
OBJLIST = $(SRCLIST:%.cpp=%.o)
DEPLIB = E:\libs\hge181\lib\gcc\libhgehelper.a E:\libs\hge181\lib\gcc\libhge.a ../../lib/libkl.a

all: Makefile.obj $(TARGET)

Makefile.obj: $(SRCLIST)
	$(CXX) $(MMFLAG) $(SRCLIST) > $@

include Makefile.obj

%.o: $(SRCDIR)/%.cpp
	$(CPP) $(CXXFLAG) $@ $<

$(TARGET): $(OBJLIST) $(DEPLIB)
	$(LINK) $(LINKFLAG) $@ $^ 

clean:
	-$(RM) *.o

rebuild: clean all

.PHONY: clean all rebuild



