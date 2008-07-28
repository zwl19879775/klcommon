/**
  @brief A chat server sample based on libevent.
  @file chat_server.h
  @author Kevin Lynx
*/
#ifndef ___CHAT_SERVER_H_
#define ___CHAT_SERVER_H_

#include <event.h>
#include <compat/sys/queue.h>

#ifdef _MSC_VER
#if _MSC_VER >= 1400
#pragma warning( disable:4996 )
#endif
#endif

/**
  user info
*/
struct user_info
{
	char _id[256];
	int _fd;
	struct event _ev;
	/** read buffer */
	struct evbuffer *_readbuf;
	/** write buffer */
	struct evbuffer *_writebuf;
	/** the server */
	struct chat_server *_server;

	LIST_ENTRY( user_info ) _next;
};

LIST_HEAD( user_info_head, user_info );

#define MAX_USER 256
#define NEW( type ) (type*) malloc( sizeof( type ) )

/**
  chat server.
*/
struct chat_server
{
	/** listen socket */
	int _fd; 
	/** bind address */
	struct sockaddr_in _addr;
	/** accept event */
	struct event _ev;
	/** user list */
	struct user_info_head _user_head;
};

/**
  create a new user.
*/
int new_conn( struct chat_server *server );

/**
  delete a user.
*/
void del_user( struct user_info *user );

/**
  broadcast the message talked by talker.
*/
void broadcast( struct chat_server *server, const struct user_info *except, const char *msg, const char *header );

/**
  user_broadcast, usually broacast some messages.
*/
#define user_broadcast( server, talker, msg ) \
	do { \
			char header[280]; \
			sprintf( header, "[%s] said : ", talker->_id ); \
			broadcast( server, talker, msg, header ); \
	} while( 0 ) 

/**
  sys_broadcast, usually broadcast some system notify texts.
*/
#define sys_broadcast( server, except, msg ) \
	broadcast( server, except, msg, "[SERVER] : " )

/**
  send message to a user.
*/
void send_msg( struct user_info *user, const char *header, const char *msg );


#endif 