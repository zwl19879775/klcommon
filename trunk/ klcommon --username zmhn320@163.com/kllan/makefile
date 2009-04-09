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

distclean: 
	cd src && $(MAKE) distclean
	cd plugin && $(MAKE) distclean
	cd test && $(MAKE) distclean

rebuild: distclean all

.PHONY: all clean rebuild distclean

