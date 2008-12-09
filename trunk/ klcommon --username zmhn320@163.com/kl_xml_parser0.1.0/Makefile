# makefile for xml_parser
# author : Kevin Lynx
SRCDIR = .\src
TMPDIR = .\tmp
BINDIR = .\bin
CC = gcc
LINK = gcc
CFLAG = -c -o
LINKFLAG = -o

all: ${TMPDIR} ${BINDIR}/test_scan.exe ${BINDIR}/test_parser.exe ${BINDIR}/test_helper.exe 

${TMPDIR}:
	mkdir tmp
${TMPDIR}/xml_scan.o: ${SRCDIR}/xml_scan.c ${SRCDIR}/xml_scan.h
	${CC} ${CFLAG} $@ $<
${TMPDIR}/xml_parser.o: ${SRCDIR}/xml_parser.c ${SRCDIR}/xml_parser.h ${SRCDIR}/xml_scan.h
	${CC} ${CFLAG} $@ $<
${TMPDIR}/xml_helper.o: ${SRCDIR}/xml_helper.c ${SRCDIR}/xml_parser.h ${SRCDIR}/xml_scan.h
	${CC} ${CFLAG} $@ $<
${TMPDIR}/test_scan.o: ${SRCDIR}/tests_scanner.c ${SRCDIR}/xml_scan.h
	${CC} ${CFLAG} $@ $< 
${BINDIR}/test_scan.exe: ${TMPDIR}/test_scan.o ${TMPDIR}/xml_scan.o
	${LINK} ${LINKFLAG} $@ $^
${TMPDIR}/test_parser.o: ${SRCDIR}/test_parser.c ${SRCDIR}/xml_parser.h ${SRCDIR}/xml_scan.h
	${CC} ${CFLAG} $@ $< 
${BINDIR}/test_parser.exe: ${TMPDIR}/test_parser.o ${TMPDIR}/xml_parser.o ${TMPDIR}/xml_scan.o
	${LINK} ${LINKFLAG} $@ $^
${TMPDIR}/test_helper.o: ${SRCDIR}/test_xmlhelper.c ${SRCDIR}/xml_parser.h ${SRCDIR}/xml_scan.h
	${CC} ${CFLAG} $@ $<
${BINDIR}/test_helper.exe: ${TMPDIR}/test_helper.o ${TMPDIR}/xml_helper.o ${TMPDIR}/xml_parser.o ${TMPDIR}/xml_scan.o
	${LINK} ${LINKFLAG} $@ $^

clean:
	-rmdir /Q /S ${TMPDIR}
	-del ${BINDIR}\*.exe
rebuild: clean all
