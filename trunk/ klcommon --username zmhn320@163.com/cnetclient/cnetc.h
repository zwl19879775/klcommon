/**
 @file cnetc.h
 @author Kevin Lynx
 @brief A simple c version wrapper for select,only for client application.
*/
#ifndef ___CNET_C_H__
#define ___CNET_C_H__

#include <stdarg.h>
#include "evbuffer.h"

#ifdef __cplusplus
extern "C"
{
#endif

/**
  notify event type:
  CC_RECV notifies that you can get data from READ_BUF,
  CC_SEND notifies some data you added to the SEND_BUF has been sent.
*/
#define CC_NONE 0x00
#define CC_RECV 0x01
#define CC_SEND 0x02

struct cnetc
{
	int fd;
	struct evbuffer *write_buf;
	struct evbuffer *read_buf;
	void (*err_log)( const char *fmt, ... );
	void (*notify_fn)( int event, struct cnetc *cc ); 
	unsigned long sip;
	unsigned short sport;
	unsigned char connect_flag;
};

#define READ_BUF( cc ) (cc->read_buf)
#define WRITE_BUF( cc ) (cc->write_buf)
#define ISCONNECT( cc ) (cc->connect_flag!=0)

/**
  Init the socket file descriptor and connect to the server.If success returns a 
  new cnetc structure, otherwise null.
*/
struct cnetc *cnetc_connect( const char *sip, unsigned short sport, 
		void (*err_log)( const char *fmt, ... ), 
	    void (*notify_fn)( int, struct cnetc * ) );

/**
  Reconnect to another server using the exist file descriptior.
*/
int cnetc_reconnect( struct cnetc *cc, const char *sip, unsigned short sport );

/**
  Poll the file descriptior, and send data if it can, receive data if it can.

  @param sec timeout value in seconds 
*/
int cnetc_poll( struct cnetc *cc, unsigned long sec );

/**
  Disconnect from the server and destroy file descriptor resource.
*/
void cnetc_disconnect( struct cnetc *cc );

#ifdef __cplusplus
}
#endif
#endif

