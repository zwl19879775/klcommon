#makefile for kl interpred language
CC = gcc
LINK = gcc
CFLAG = -c -g -o
LINKFLAG = -o
SRCDIR = .\src
TMPDIR = .\tmp
BINDIR = .\bin

all: ${TMPDIR} ${BINDIR}/testlex.exe

${TMPDIR}:
	mkdir tmp

${TMPDIR}/kllex.o: ${SRCDIR}/kllex.c ${SRCDIR}/kllex.h
	${CC} ${CFLAG} $@ $< 

${TMPDIR}/testlex.o: ${SRCDIR}/testlex.c ${SRCDIR}/kllex.h
	${CC} ${CFLAG} $@ $<

${BINDIR}/testlex.exe: ${TMPDIR}/kllex.o ${TMPDIR}/testlex.o
	${LINK} ${LINKFLAG} $@ $^

clean: 
	-rmdir /Q /S ${TMPDIR}
	-del ${BINDIR}\*.exe

rebuild: clean all

