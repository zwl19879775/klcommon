#makefile for kl interpred language
CC = gcc
LINK = gcc
CFLAG = -c -g -o
LINKFLAG = -o
SRCDIR = .\src
TMPDIR = .\tmp
BINDIR = .\bin

all: ${TMPDIR} ${BINDIR}/testlex.exe ${BINDIR}/testsyntax.exe

${TMPDIR}:
	mkdir tmp

${TMPDIR}/kllex.o: ${SRCDIR}/kllex.c ${SRCDIR}/kllex.h
	${CC} ${CFLAG} $@ $< 

${TMPDIR}/klparser.o: ${SRCDIR}/klparser.c ${SRCDIR}/klparser.h ${SRCDIR}/kllex.h
	${CC} ${CFLAG} $@ $<

${TMPDIR}/testlex.o: ${SRCDIR}/testlex.c ${SRCDIR}/kllex.h
	${CC} ${CFLAG} $@ $<

${TMPDIR}/testsyntax.o: ${SRCDIR}/testsyntax.c ${SRCDIR}/klparser.h ${SRCDIR}/kllex.h
	${CC} ${CFLAG} $@ $<

${BINDIR}/testlex.exe: ${TMPDIR}/kllex.o ${TMPDIR}/testlex.o
	${LINK} ${LINKFLAG} $@ $^

${BINDIR}/testsyntax.exe: ${TMPDIR}/testsyntax.o ${TMPDIR}/kllex.o ${TMPDIR}/klparser.o
	${LINK} ${LINKFLAG} $@ $^

clean: 
	-rmdir /Q /S ${TMPDIR}
	-del ${BINDIR}\*.exe

rebuild: clean all

