/**
 Kevin Lynx
 1.23.2011
*/
#ifndef __FEIQGROUP_H_
#define __FEIQGROUP_H_

#define MAC_SIZE 12
#define HSIZE 256
#define FSIZE 512
#define BSIZE 2048

#define TEXTCMD (0x400023)
#define ONLINECMD (0x2000C9)
#define ONLINECMD2 (0xCA)

#ifdef _DEBUG
#define logd printf
#else
#define logd(fmt, ...)
#endif

struct Message
{
    char feiq_header[HSIZE];
    char full_header[FSIZE];
    char body[BSIZE];
    int body_s;
};

/*
Create a message from UDP raw buffer. If success return non-zero.
*/
int message_create(const char *buf, int size, Message *msg);

/*
Decrypt the message and the decrypted body is in body filed with '\0' end.
*/
void message_decrypt(Message *msg);

/*
Helper functions to retreieve data memeber in a message.
*/
/*
Skip to the specified character 'split' position, return the 
position.
*/
int skipto(const char *buf, int pos, char split);

/*
Read the string between 'cnt'th and 'cnt'+1th 'split' and return position
at 'cnt' 'split', i.e: readto(buf, ret, '#', 5) .
*/
int readin(const char *buf, char *ret, char split, int cnt);

char *message_mac(const Message *msg, char *mac);

char *message_username(const Message *msg, char *user);

char *message_pcname(const Message *msg, char *pc);

/* must be called after decrypt */
char *message_groupnumber(const Message *msg, char *number);

/* must be called fater decrypt */
char *message_text(const Message *msg, char *text);

/*
Functions below are used to send group messages.
*/
/* create a message body */
char *create_body(const char *text, const char *groupnum, char *body);

int encrypt_body(const char *body, int size, const char *mac, void *encrypt);

char *create_feiqheader(const char *mac, char *feiqheader, int body_s);

char *create_fullheader(const char *feiqheader, const char *user, const char *pc, 
                        int cmd, char *fullheader);

Message *create_message(const char *fullheader, const void *encrypt, int size, Message *msg);

int message_toraw(const Message *msg, void *raw);

#endif
