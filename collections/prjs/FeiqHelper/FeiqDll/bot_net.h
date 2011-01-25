#ifndef __BOT_NET_H
#define __BOT_NET_H


#ifdef _WIN32

#define _USE_32BIT_TIME_T
#include <winsock2.h>
#include <windows.h>
#pragma comment(lib, "ws2_32.lib")

typedef char 			int8_t;
typedef unsigned char 	uint8_t;
typedef unsigned short 	uint16_t;
typedef unsigned long 	uint32_t;
//typedef unsigned int 	size_t;

#else

#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <stdint.h>
#include <unistd.h>

typedef int SOCKET;
#endif

#include <string.h>
#include <stdlib.h>
#include <time.h>
#include <stdio.h>

#define VERSION "1"
#define USERNAME "BOT"
#define HOSTNAME "BOT"

#define IPMSG_SENDMSG			0x00000020UL
#define IPMSG_RECVMSG			0x00000021UL

SOCKET create_socket();

int send_to(SOCKET s, const char *ip, uint16_t port, const void *msg, size_t size);

void close_socket(SOCKET s);

#endif
