/**
 Kevin Lynx
 2.23.2011
 Test feiqgroup on windows.
*/
#include <stdio.h>
#include <winsock2.h>
#include <windows.h>
#include "feiq_group.h"

#define BROADCAST_ADDR "226.81.9.8"
#define PORT (2425)
#define BUFLEN (4096)
#define SELF_MAC "002622CB9F79"

#pragma comment(lib, "ws2_32.lib")

SOCKET startup()
{
    WSADATA wd;
    WSAStartup(MAKEWORD(2, 0), &wd);
    SOCKET s = WSASocket(AF_INET, SOCK_DGRAM, 0, NULL, 0, 
        WSA_FLAG_MULTIPOINT_C_LEAF | WSA_FLAG_MULTIPOINT_D_LEAF | WSA_FLAG_OVERLAPPED);
    sockaddr_in local, remote;
    memset(&local, 0, sizeof(local));
    local.sin_family = AF_INET;
    local.sin_addr.s_addr =  INADDR_ANY;
    local.sin_port = htons(PORT);
    if(bind(s, (sockaddr*) &local, sizeof(local)) < 0)
    {
        fprintf(stderr, "bind error %d.\n", WSAGetLastError());
        return 0;
    }
    memset(&remote, 0, sizeof(remote));
    remote.sin_family = AF_INET;
    remote.sin_port = htons(PORT);
    remote.sin_addr.s_addr = inet_addr(BROADCAST_ADDR);
    if(WSAJoinLeaf(s, (sockaddr*) &remote, sizeof(remote), NULL, NULL, NULL, NULL, JL_BOTH) <= 0)
    {   
        fprintf(stderr, "Join leaf failed %d.\n", WSAGetLastError());
        return 0;
    }   

    printf("Startup ok, bind on port %d.\n", PORT);
    return s;
}

void shutdown(SOCKET s)
{
    printf("Shutdown...\n");
    closesocket(s);
    WSACleanup();
}

#ifdef _DEBUG
void save_raw_message(const char *buf, int size)
{
    static int index = 1;
    char name[256];
    sprintf(name, "raw_%02d.dat", index++);
    FILE *fp = fopen(name, "wb");
    if(!fp) return;
    fwrite(buf, size, 1, fp);
    fclose(fp);
}

void save_message(const Message *msg)
{
    static int index = 1;
    char name[256];
    sprintf(name, "plain_%02d.dat", index++);
    FILE *fp = fopen(name, "wb");
    if(!fp) return;
    fwrite(msg->body, strlen(msg->body), 1, fp);
    fclose(fp);
}

#else
#define save_raw_message(a, b)
#define save_message(a)
#endif

void save_group_message(const Message *msg, const char *group, const char *user, const char *pc)
{
    char file[512];
    sprintf(file, "msg_%s_.txt", group);
    FILE *fp = fopen(file, "a+");
    if(!fp) return;
    fprintf(fp, "%s:%s\n%s\n", user, pc, msg->body);
    fclose(fp);
}

void test_response_msg(SOCKET s, const sockaddr_in *clientaddr, const Message *src_msg)
{
    char text[512];
    char body[1024];
    char number[32];
    char encrypt[1024];
    char feiqheader[128];
    char fullheader[128];
    char raw[2048];
    int raw_size;
    int encrypt_size;
    int ret;
    Message msg;
    /* broadcast address, the sender will receive this also.*/
    sockaddr_in remote;
    memset(&remote, 0, sizeof(remote));
    remote.sin_family = AF_INET;
    remote.sin_port = htons(PORT);
    remote.sin_addr.s_addr = inet_addr(BROADCAST_ADDR);

    create_body(message_text(src_msg, text), message_groupnumber(src_msg, number), body);
    encrypt_size = encrypt_body(body, (int) strlen(body), SELF_MAC, encrypt);
    create_feiqheader(SELF_MAC, feiqheader, encrypt_size);
    create_fullheader(feiqheader, "kevin", "PC-kevin", TEXTCMD, fullheader);
    create_message(fullheader, encrypt, encrypt_size, &msg);
    raw_size = message_toraw(&msg, raw);
    //ret = sendto(s, raw, raw_size, 0, (const sockaddr*) clientaddr, sizeof(*clientaddr));
    ret = sendto(s, raw, raw_size, 0, (const sockaddr*) &remote, sizeof(remote));
    if(ret < 0)
    {
        fprintf(stderr, "Send response failed.\n");
        return;
    }
    printf("Send response success.\n");
}

void handle_message(const char *buf, int size, Message *msg)
{
    char num[32];
    char user[64];
    char pc[64];
    save_raw_message(buf, size);
    message_create(buf, size, msg);
    message_decrypt(msg);
    save_message(msg);
    unsigned long c = message_cmdno(msg);
    if(c != TEXTCMD)
    {
        logd("Non text command(%x), ignore it.\n", c);
        return;
    }
    printf("================================================================================\n");
    printf("GroupNo(%s)\tUser(%s)\tPC(%s)\n", message_groupnumber(msg, num), message_username(msg, user),
        message_pcname(msg, pc));
    printf("%s\n", msg->body);
    save_group_message(msg, num, user, pc);
    printf("================================================================================\n");
}

void loop(SOCKET s)
{
    printf("Start loop.\n");
    char buf[BUFLEN];
    sockaddr_in client_addr;
    Message msg;
    int addr_len = sizeof(client_addr);
    while(1)
    {
        memset(buf, 0, sizeof(buf));
        int ret = recvfrom(s, buf, BUFLEN, 0, (sockaddr*) &client_addr, &addr_len);
        if(ret < 0)
        {
            fprintf(stderr, "Recvfromm failed.\n");
            Sleep(1000);
            continue;
        }
        logd("Recv data from (%s-%d)(size=%d).\n", inet_ntoa(client_addr.sin_addr), 
            ntohs(client_addr.sin_port), ret);
        handle_message(buf, ret, &msg);
        /* send response */
       /*test_response_msg(s, &client_addr, &msg);*/
    }
}

int main()
{
    SOCKET s = startup();
    if(s <= 0) exit(-1);
    loop(s);
    shutdown(s);

    return 0;
}
