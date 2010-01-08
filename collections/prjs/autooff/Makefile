#
# makefile for autooff tool, used in mingw
#
SRCLIST = $(wildcard *.cpp)
OBJLIST = $(SRCLIST:%.cpp=%.o)
RESSRC = autooff.rc app.ico
RESOBJ = autooff.res 
TARGET = autooff.exe
override CXXFLAG += -c -Wall
CXX = g++
LINK = g++

all: Makefile.obj $(TARGET) 
	
Makefile.obj: $(SRCLIST)
	$(CXX) -MM $(SRCLIST) > $@

include Makefile.obj

%.o: %.cpp
	$(CXX) $(CXXFLAG) $< 

$(RESOBJ): $(RESSRC)
	windres $< -o $@ -O coff

$(TARGET): $(OBJLIST) $(RESOBJ)
	$(LINK) -o $@ -mwindows $^ -lws2_32

clean:
	del $(RESOBJ) $(OBJLIST)

rebuild: clean all

