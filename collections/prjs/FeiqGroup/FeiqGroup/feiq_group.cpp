/**
 Kevin Lynx
 1.23.2011
*/
#include "feiq_group.h"
#define _USE_32BIT_TIME_T
#include <time.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "blowfish.h"

static char *strncpy_e(char *dest, const char *src, size_t cnt)
{
    strncpy(dest, src, cnt);
    dest[cnt] = '\0';
    return dest;
}

static int read_message_header(const char *buf, int size, char *header)
{
    int i = 0;
    int cnt = 0;
    for( ; cnt < 5 && i < size; ++ cnt, ++ i )
    {
        i = skipto(buf, i, ':');
    }
    strncpy_e(header, buf, i);
    return i;
}

static int read_feiq_header(const char *header, char *feiq)
{
    int i = skipto(header, 0, ':');
    strncpy_e(feiq, header, i);
    return i;
}

int message_create(const char *buf, int size, Message *msg)
{
    char s[32];
    int offset;
    offset = read_message_header(buf, size, msg->full_header);
    logd("full header : %s\n", msg->full_header);
    read_feiq_header(msg->full_header, msg->feiq_header);
    logd("feiq header : %s\n", msg->feiq_header);
    readin(msg->feiq_header, s, '#', 5);
    msg->body_s = atoi(s);
    logd("body from : %d, size : %d\n", offset, msg->body_s);
    memcpy(msg->body, buf + offset, msg->body_s);
    return 1;
}

void message_decrypt(Message *msg)
{
    char mac[32];
    message_mac(msg, mac);
    CBlowFish fish((const unsigned char*) mac, MAC_SIZE);
    fish.Decrypt((const unsigned char*) msg->body, (unsigned char*) msg->body, msg->body_s);
}

int skipto(const char *buf, int pos, char split)
{
    for( ; buf[pos] != '\0' && buf[pos] != split; ++ pos ) ;
    return pos;
}

int readin(const char *buf, char *ret, char split, int cnt)
{
    int s = 0, e = 0;
    for( ; cnt > 0 && buf[s] != '\0'; -- cnt )
    {
        s = skipto(buf, s, split);
        if(buf[s] == '\0') return -1;
        ++ s;
    }
    e = skipto(buf, s, split);
    if(buf[e] == '\0')
    {
        strcpy(ret, &buf[s]);
    }
    else
    {
        strncpy_e(ret, &buf[s], e - s);
    }
    return s;
}

char *message_mac(const Message *msg, char *mac)
{
    readin(msg->feiq_header, mac, '#', 2);
    return mac;
}

char *message_username(const Message *msg, char *user)
{
    readin(msg->full_header, user, ':', 2);
    return user;
}

char *message_pcname(const Message *msg, char *pc)
{
    readin(msg->full_header, pc, ':', 3);
    return pc;
}

unsigned long message_cmdno(const Message *msg)
{
    char v[32];
    readin(msg->full_header, v, ':', 4);
    return atol(v);
}

char *message_groupnumber(const Message *msg, char *number)
{
    readin(msg->body, number, '#', 1);
    return number;
}

char *message_text(const Message *msg, char *text)
{
    readin(msg->body, text, '#', 2);
    return text;
}

char *create_body(const char *text, const char *groupnum, char *body)
{
    sprintf(body, "QUNMSGMARK#%s#%s", groupnum, text);
    return body;
}

int encrypt_body(const char *body, int size, const char *mac, void *encrypt)
{
    CBlowFish fish((const unsigned char*) mac, MAC_SIZE);
    int ret = 0;
    ret = (int) fish.Encrypt((const unsigned char*) body, (unsigned char*) encrypt, size);
    logd("encrypted data size : %d\n", ret);
    return ret;
}

char *create_feiqheader(const char *mac, char *feiqheader, int body_s)
{
    char s[32];
    itoa(body_s, s, 10);
    strcpy(feiqheader, "1_lbt4_0#");
    strcat(feiqheader, "128#"); /*level*/
    strcat(feiqheader, mac);
    strcat(feiqheader, "#0#0#");
    strcat(feiqheader, s);
    logd("create feiqheader : %s\n", feiqheader);
    return feiqheader;
}

char *create_fullheader(const char *feiqheader, const char *user, const char *pc, 
                        int cmd, char *fullheader)
{
    /*feiqheader:packetNo(timeStamp):username:pcname:cmdNo;*/
    sprintf(fullheader, "%s:%lu:%s:%s:%lu:", feiqheader, time(NULL), user, pc,
        cmd);
    logd("create full header : %s\n", fullheader);
    return fullheader;
}

Message *create_message(const char *fullheader, const void *encrypt, int size, Message *msg)
{
    read_feiq_header(fullheader, msg->feiq_header);
    strcpy(msg->full_header, fullheader);
    msg->body_s = size;
    memcpy(msg->body, encrypt, size);
    return msg;
}

int message_toraw(const Message *msg, void *raw)
{
    int hsize = (int) strlen(msg->full_header);
    memcpy(raw, msg->full_header, hsize);
    memcpy((char*)raw + hsize, msg->body, msg->body_s);
    return hsize + msg->body_s;
}