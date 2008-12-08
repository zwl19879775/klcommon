# makefile for xml parser
CC = gcc
BINDIR = ./bin
TMPDIR = ./tmp
COPTION = -c 

all: test_scanner.exe test_parser.exe test_helper.exe
xml_helper.o: xml_helper.c xml_helper.h xml_parser.h xml_scan.h
	${CC} ${COPTION} xml_helper.c
xml_parser.o: xml_parser.c xml_parser.h xml_scan.h
	${CC} ${COPTION} xml_parser.c
xml_scan.c: xml.l
	flex -oxml_scan.c xml.l
xml_scan.o: xml_scan.c xml_scan.h
	${CC} ${COPTION} xml_scan.c
tests_scanner.o: xml_scan.h tests_scanner.c
	${CC} ${COPTION} tests_scanner.c
test_parser.o: test_parser.c xml_parser.h xml_scan.h
	${CC} ${COPTION} test_parser.c
test_xmlhelper.o: test_xmlhelper.c xml_helper.h xml_parser.h xml_scan.h
	${CC} ${COPTION} test_xmlhelper.c
test_parser.exe: xml_scan.o xml_parser.o test_parser.o
	${CC} -o test_parser.exe xml_scan.o xml_parser.o test_parser.o
test_scanner.exe: xml_scan.o tests_scanner.o
	${CC} -o test_scanner.exe xml_scan.o tests_scanner.o 
test_helper.exe: test_xmlhelper.o xml_helper.o xml_helper.o xml_scan.o
	${CC} -o test_helper.exe test_xmlhelper.o xml_helper.o xml_parser.o xml_scan.o
clean:
	del *.o xml_scan.c test_scanner.exe test_parser.exe test_helper.exe


