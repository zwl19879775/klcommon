#include "bot_net.h"
#include "Utils.h"

SOCKET create_socket() {
#ifdef _WIN32
	WSADATA wsa;
	WSAStartup(MAKEWORD(2, 0), &wsa);
#endif
	SOCKET s = socket(AF_INET, SOCK_DGRAM, 0);
	return s;
}

int send_to(SOCKET s, const char *ip, uint16_t port, const void *msg, size_t size) {

	struct sockaddr_in clt;

	memset(&clt, 0, sizeof(clt));

	clt.sin_family = AF_INET;
	clt.sin_addr.s_addr = inet_addr(ip);
	clt.sin_port = htons(port);

	char buf[65535];

	memset(buf, 0, sizeof(buf));

	sprintf(buf, "%s:%lu:%s:%s:%lu:", VERSION, time(NULL), USERNAME, HOSTNAME, IPMSG_SENDMSG);

	int len = strlen(buf);

	memcpy((void*)(buf + len), msg, size);

	len += size;

	return sendto(s, buf, len, 0, (struct sockaddr*)&clt, sizeof(struct sockaddr));
}


void close_socket(SOCKET s) {
#ifdef _WIN32
	closesocket(s);
	WSACleanup();
#else
	close(s);
#endif
}
