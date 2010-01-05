
KLWINDIR = ../../../ klcommon --username zmhn320@163.com/kl_win
KLWINDIRI = "$(KLWINDIR)"
KLWINLIB = "$(KLWINDIR)/libklwin.a"
SRCLIST = $(wildcard *.cpp)
OBJLIST = $(SRCLIST:%.cpp=%.o)
TARGET = autooff.exe
CXX = g++
LINK = g++

all: Makefile.obj $(TARGET) 
	
Makefile.obj: $(SRCLIST)
	$(CXX) -MM $(SRCLIST) > $@

$(KLWINLIB):
	cd $(KLWINDIR) && $(MAKE)

include Makefile.obj

%.o: %.cpp
	$(CXX) -Wall -c -I $(KLWINDIRI) $< 

$(TARGET): $(OBJLIST) $(KLWINLIB)
	$(LINK) -o $@ -lwinmm -mwindows $^

