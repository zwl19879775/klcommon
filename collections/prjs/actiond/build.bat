g++ -Wall -c actiond.cpp
g++ -Wall -c cmdline.cpp
g++ -o actiond.exe actiond.o cmdline.o klwin/libklwin.a -lwinmm -mwindows