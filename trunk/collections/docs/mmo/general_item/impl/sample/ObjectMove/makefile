CPP = g++
CFLAG = -c -Wall -D"GI_DEPTYPES=\"sample/TypesDef.h\""
MMFLAG = -MM
SRCLIST = $(wildcard *.cpp)
OBJLIST = $(SRCLIST:%.cpp=%.o)
TARGET = .build

all: makefile.obj $(TARGET)

makefile.obj: $(SRCLIST)
	$(CPP) $(MMFLAG) $(SRCLIST) > $@

include makefile.obj

%.o: %.cpp
	$(CPP) $(CFLAG) $<

$(TARGET): $(OBJLIST)
	-date > $@
	-echo $(OBJLIST) >> $@

clean: 
	-rm *.o
	-rm $(TARGET)

rebuild: clean all
	
