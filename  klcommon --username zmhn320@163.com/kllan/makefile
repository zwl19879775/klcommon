#
# Makefile for kl script.
#

all:
	cd src && $(MAKE)
	cd plugin && $(MAKE)
	cd test && $(MAKE)

clean:
	cd src && $(MAKE) clean
	cd plugin && $(MAKE) clean
	cd test && $(MAKE) clean

rebuild: clean all

.PHONY: all clean rebuild

