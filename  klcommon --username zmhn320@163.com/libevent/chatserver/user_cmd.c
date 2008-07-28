/**
  @file user_cmd.c
  @author Kevin Lynx
  @brief to parse the user's commands.
*/
#ifdef WIN32
#include <winsock2.h>
#endif
#include <stdio.h>
#include "chat_server.h"
#include "user_cmd.h"

#define CMD_TYPE( str, cmd ) strcmp( str, cmd ) == 0

int parse_cmd( struct user_info *user, struct chat_server *server, const char *str )
{
	char *space;
	char cmd[32];
	strcpy( cmd, str );

	/* spilit the command */
	{
		space = strstr( cmd, " " );
		if( space != 0 )
		{
			*space = 0;
		}
	}

	/* quit */
	if( CMD_TYPE( cmd, "/quit" ) )
	{
		del_user( user );
		return -1;
	}
	
	/* change name : /name name */
	if( CMD_TYPE( cmd, "/name" ) )
	{
		char new_name[256];
		char notify[512];
		strcpy( new_name, str + strlen( cmd ) + 1 );
		sprintf( notify, "[%s] changed his/her name to [%s].",  user->_id, new_name );
		sys_broadcast( server, user, notify );
		printf( "%s\n", notify );

		sprintf( notify, "changed name to [%s] ok.", new_name );
		send_msg( user, "[SERVER] : ", notify );

		strcpy( user->_id, new_name );
	}

	return 0;
}