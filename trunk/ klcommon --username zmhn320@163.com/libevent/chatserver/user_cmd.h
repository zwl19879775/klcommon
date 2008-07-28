/**
  @file user_cmd.h
  @author Kevin Lynx
  @brief to parse the user's commands.
*/
#ifndef ___USER_CMD_H_
#define ___USER_CMD_H_

/**
  check whether it's a command.
*/
#define is_command( str ) str[0] == '/'

/**
  parse the command.
  This function maybe delete the user, when that happened, it returns -1.

*/
int parse_cmd( struct user_info *user, struct chat_server *server, const char *str );

#endif 