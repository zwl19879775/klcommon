g++ -O2 -Wall -c actiond.cpp
g++ -O2 -Wall -c cmdline.cpp
g++ -o actiond.exe actiond.o cmdline.o klwin/libklwin.a -lwinmm -mwindows