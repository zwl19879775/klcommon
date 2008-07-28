/**
  @brief A chat server sample based on libevent.
  @file chat_server.c
  @author Kevin Lynx

*/
#ifdef WIN32
#include <winsock2.h>
#endif
#include <stdio.h>
#include "chat_server.h"
#include "user_cmd.h"

/**
  send welcome message.
*/
void send_welcome_msg( struct chat_server *server, struct user_info *user )
{
	char *msg = 
"********************************************************************************"
"             Welcome to this CHAT_SERVER based on libevent\r\n"
"\r\n"
"Type any texts anytime and press ENTER to send messages.\r\n"
"\r\n"
"Command : \r\n"
"    /quit : to quit the chat server.\r\n"
"    /name new_name : to change your nick name.\r\n"
"\r\n"
"                                                    Programmed by Kevin Lynx\r\n"
"********************************************************************************";
	evbuffer_add( user->_writebuf, msg, strlen( msg ) );
	evbuffer_add( user->_writebuf, "\r\n", 2 );
}

/**
  read/write data.
*/
void on_read_write( int fd, short ev, void *arg )
{
	struct user_info *user = (struct user_info *) arg;
	int ret ;

	if( ev & EV_READ )
	{
		char *msg;
		ret = evbuffer_read( user->_readbuf, fd, 4096 );
		if( ret <= 0 )
		{
			/* disconnect */
			del_user( user );			
			return ;
		}

		/* read some message, broadcast it. */
		/*msg = evbuffer_readline( user->_readbuf );*/
		while( ( msg = evbuffer_readline( user->_readbuf ) ) != 0 )
		{
			/* parse command */
			if( is_command( msg ) )
			{
				int user_exist = parse_cmd( user, user->_server, msg );
				if( user_exist != 0 ) 
				{
					free( msg );
					return ;
				}
			}
			else
			{
				user_broadcast( user->_server, user, msg );
			}

			free( msg );
		}

	}

	if( ev & EV_WRITE )
	{
		evbuffer_write( user->_writebuf, fd );
	}

	event_add( &user->_ev, 0 );
}

/**
  accept event.
*/
void on_accept( int fd, short ev, void *arg )
{
	struct chat_server *server = (struct chat_server*) arg;
	if( new_conn( server ) != 0 )
	{
		fprintf( stderr, "new_conn failed\n" );
	}
}

/**
  create a new user.
*/
struct user_info *new_user( int fd, struct sockaddr_in addr, struct chat_server *server )
{
	struct user_info *user = NEW( struct user_info ) ;
	user->_server = server;
	user->_fd = fd;
	sprintf( user->_id, "%s:%u", inet_ntoa( addr.sin_addr ), ntohs( addr.sin_port ) );
	/* init the event */
	event_set( &user->_ev, user->_fd, EV_READ | EV_WRITE, on_read_write, user );
	event_add( &user->_ev, 0 );
	/* init the buffer */
	user->_readbuf = evbuffer_new();
	user->_writebuf = evbuffer_new();
	
	/* add to the list */
	LIST_INSERT_HEAD( &server->_user_head, user, _next );
	return user;
}

/**
  delete a user.
*/
void del_user( struct user_info *user )
{
	char notify[280];
	sprintf( notify, "user [%s] offline.", user->_id );
	printf( "%s\n", notify );
	sys_broadcast( user->_server, user, notify );

	event_del( &user->_ev );
	evbuffer_free( user->_readbuf );
	evbuffer_free( user->_writebuf );
	closesocket( user->_fd );

	LIST_REMOVE( user, _next );
	free( user );
}

/**
  send message to a user.
*/
void send_msg( struct user_info *user, const char *header, const char *msg )
{
	evbuffer_add( user->_writebuf, header, strlen( header ) );
	evbuffer_add( user->_writebuf, msg, strlen( msg ) );
	evbuffer_add( user->_writebuf, "\r\n", 2 );
}

/**
  broadcast the message talked by talker.
*/
void broadcast( struct chat_server *server, const struct user_info *except, const char *msg, const char *header )
{
	struct user_info_head *head = &server->_user_head ;
	struct user_info *user;
	LIST_FOREACH( user, head, _next )
	{
		if( user != except )
		{
			send_msg( user, header, msg );
		}
	}
}

/**
  accept a new connection.
*/
int new_conn( struct chat_server *server )
{
	struct sockaddr_in addr;
	int len = sizeof( addr );
	int fd = (int)accept( server->_fd, (struct sockaddr*)&addr, &len );
	if( fd == -1 )
	{
		fprintf( stderr, "accept failed\n" );
		return -1;
	}
	
	{
		struct user_info *user = new_user( fd, addr, server );
		char notify[280];
		sprintf( notify, "user [%s] online.", user->_id );
		printf( "%s\n", notify );
		send_welcome_msg( server, user );
		sys_broadcast( server, user, notify );
	}

	return 0;
}

/**
  startup the chat server.
*/
int server_startup( struct chat_server *server, const char *ip, unsigned short port )
{
	int ret = 0;

	server->_fd = (int) socket( AF_INET, SOCK_STREAM, 0 );
	/* init the user list head */
	LIST_INIT( &server->_user_head );
	/* bind address */
	memset( &server->_addr, 0, sizeof( server->_addr ) );
	server->_addr.sin_family = AF_INET;
	server->_addr.sin_port = htons( port );
	server->_addr.sin_addr.s_addr = inet_addr( ip );
	ret = bind( server->_fd, (struct sockaddr*) &server->_addr, sizeof( server->_addr ) );
	if( ret < 0 )
	{
		fprintf( stderr, "bind failed\n" );
		closesocket( server->_fd );
		return -1;
	}

	/* listen */
	ret = listen( server->_fd, MAX_USER );
	if( ret < 0 )
	{
		fprintf( stderr, "listen failed\n" );
		closesocket( server->_fd );
		return -1;
	}

	/* setup the event */
	event_set( &server->_ev, server->_fd, EV_READ | EV_PERSIST, on_accept, server );
	event_add( &server->_ev, 0 );

	return 0;
}

/**
  shutdown the chat server.
*/
void server_shutdown( struct chat_server *server )
{
	/* del all events */
	{
		struct user_info *user;
		event_del( &server->_ev );
		LIST_FOREACH( user, &server->_user_head, _next )
		{
			event_del( &user->_ev );
			evbuffer_free( user->_readbuf );
			evbuffer_free( user->_writebuf );
			closesocket( user->_fd );
			free( user );
		}

		LIST_INIT( &server->_user_head );
	}

	closesocket( server->_fd );
}

/**
  the chat server instance.
*/
struct chat_server server;

/**
  handle the ctrl+c event.
*/
#ifdef WIN32
BOOL WINAPI Handler( DWORD ctrl_event )
{
	switch( ctrl_event )
	{
	case CTRL_CLOSE_EVENT:
	case CTRL_C_EVENT:
		{
			/* shutdown the server */
			server_shutdown( &server );
		}

		return TRUE;
	}

	return FALSE;
}
#endif

int main( int argc, char **argv )
{
	/* init the event base */
	struct event_base *base = event_init();

#ifdef WIN32
	if( !SetConsoleCtrlHandler( Handler, TRUE ) )
	{
		fprintf( stderr, "error setting event handler.\n" );
		return -1;
	}
#endif

	/* startup the server */
	{
		char ip[32];
		unsigned short port;
		int ret;

		if( argc < 3 )
		{
			fprintf( stderr, "no argument, set to default.\n" );
			strcpy( ip, "0.0.0.0" );
			port = 11011;
		}
		else
		{
			strcpy( ip, argv[1] );
			port = (unsigned short) atoi( argv[2] );
		}

		ret = server_startup( &server, ip, port );
		if( ret == 0 )
		{
			printf( "server startup ok.\n" );
		}
	}
	
	event_dispatch();
	event_base_free( base );
	printf( "server shutdown.\n" );

	return 0;
}