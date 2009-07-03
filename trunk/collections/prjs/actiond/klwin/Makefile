
SRCLIST = $(wildcard *.cpp)
OBJLIST = $(SRCLIST:%.cpp=%.o)
TARGET = libklwin.a
CXX = g++

all: Makefile.obj $(TARGET)
	
Makefile.obj: $(SRCLIST)
	$(CXX) -MM $(SRCLIST) > $@

include Makefile.obj

%.o: %.cpp
	$(CXX) -Wall -c $< 

$(TARGET): $(OBJLIST)
	ar crv $@ $^

