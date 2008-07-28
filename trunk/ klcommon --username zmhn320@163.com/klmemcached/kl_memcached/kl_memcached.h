/**
  @file kl_memcached.h
  @author Kevin Lynx
  @brief a simple memcached client module, provides the basic ability to
  store/retrive/delete items on the memcached server.
  @date 7.17.2008
*/
#ifndef ___KL_MEMCACHED_H_
#define ___KL_MEMCACHED_H_

#ifdef __cplusplus
extern "C" {
#endif

#include "evbuffer.h"

/**
  memcached command executed result.
  check out the memcached protocol document for details.
*/
enum
{
	/** these value below is memcached command execute result. */
	MEM_ERROR = -4,
	MEM_CLIENT_ERROR,
	MEM_SERVER_ERROR,
	MEM_SYSTEM_ERROR = -1,
	MEM_STORED,
	MEM_NOT_STORED,
	MEM_EXISTS,
	MEM_NOT_FOUND,
	MEM_DELETED,
	MEM_OK,
	MEM_NO_RESPONSE,
};

/**
  error callback function type.
*/
typedef void (*error_func_type)( int err_code, const char *desc );

/**
  set the error callback function, when some error occures, this function will be called.
*/
void mem_set_error_handler( error_func_type func );

/**
  memcached server.
*/
struct mem_server
{
	int _fd;
	struct sockaddr_in _addr;
};

/**
  connect to the memcached server.

  This function will create the socket and connect to the memcached server.

  @param server [out] implements the server information.
  @param addr the server ip address.
  @param port the server port.
  @return return 0 if succesful, or -1 when an error occures.
*/
int mem_connect( struct mem_server *server, const char *addr, unsigned short port );

/**
  disconnect to the memcached server.
*/
void mem_disconnect( const struct mem_server *server );

/**
  item data
*/
struct item_data
{
	/** item key */
	char _key[256];
	/** item data buffer */
	struct evbuffer *_buffer;
	/** flag, check out the memcached protocol doc */
	size_t _flag;
};

/** 
  store item data.
*/
struct store_item
{
	struct item_data _item;
	size_t _exp_time;
};

/**
  create an item_data.
*/
#define NEW_ITEM_DATA( item, key, flag ) \
	strcpy( item._key, key ); \
	item._buffer = evbuffer_new(); \
	item._flag = flag; 

/**
  delete an item_data.
*/
#define DEL_ITEM_DATA( item ) \
	evbuffer_free( item._buffer );

/**
  retrieve the store_item->_item->_buffer
*/
#define STORE_ITEM_BUF( item ) item._item._buffer

/**
  store an item to the memcached server.

  In this tiny memcached client library, i use the block socket function to send/recv data, so 
  be aware that some functions below will block if the IO operation blocks.
  You donot need to call this function manually, instead i suggest you call these macros below.
 
  Some old memcached version does not support 'noreply' option.

  @param server the memcached server, must be connected already.
  @param item the item which will be stored.must be valid.
  @param store_type a c-string specified the stored command type like : add, set, replace, etc.
  @param need_response 0 if you donot want to get the response.
  @return the command execute result.
*/
int mem_store( const struct mem_server *server, const struct store_item *item, int need_response, const char *store_type );

/**
  add an item to the memcached server.

  @see mem_store
*/
#define mem_add( server, item_ptr, need_response ) mem_store( server, item_ptr, need_response, "add" )

/**
  set an item to the memcached server.

  @see mem_store
*/
#define mem_set( server, item_ptr, need_response ) mem_store( server, item_ptr, need_response, "set" )

/**
  replace an item to the memcached server.

  @see mem_store
*/
#define mem_replace( server, item_ptr, need_response ) mem_store( server, item_ptr, need_response, "replace" )

/**
  append data to an item to the memcached server.

  The 'append' command is not supported until memcached 1.2.4.

  @see mem_store
*/
#define mem_append( server, item_ptr, need_response ) mem_store( server, item_ptr, need_response, "append" )

/**
  prepend data to an item to the memcached server.

  The 'prepend' command is not supported until memcached 1.2.4.

  @see mem_store
*/
#define mem_prepend( server, item_ptr, need_response ) mem_store( server, item_ptr, need_response, "prepend" )

/**
  get many items.

  @param items specified the item array.
  @param size the items array size.
  @return if successful, return the amount this function got.
*/
int mem_gets( const struct mem_server *server, struct item_data *items[], size_t size );

/**
  get an item.

  @param item you must set the key of the item, and this function will put the data in the item.
*/
int mem_get( const struct mem_server *server, struct item_data *item );

/**
  delete an item.

  Some old memcached version does not support 'noreply' option.
*/
int mem_delete( const struct mem_server *server, const char key[256], int need_response );

/**
  increase/decrease an item (treat the item as an unsigned int64 number) data.

  You donot need to call this function, instead you should call these macros below.
	
  @param new_value if this argument is not null, the function will ask memcached server's reply and write the new value of the item if successful.
*/
int mem_inc_dec( const struct mem_server *server, const char key[256], unsigned int value, unsigned int *new_value, const char *cmd_type );

/**
  increase an item.

  @see mem_inc_dec
*/
#define mem_inc( server, key, value, new_value ) mem_inc_dec( server, key, value, new_value, "incr" )

/**
  decrease an item.

  @see mem_inc_dec
*/
#define mem_dec( server, key, value, new_value ) mem_inc_dec( server, key, value, new_value, "decr" )

/**
  dump memcached server stats.
*/
int mem_stats( const struct mem_server *server, char *stat_desc, size_t size );

/**
  flush all items.
*/
int mem_flush_all( const struct mem_server *server );

#ifdef __cplusplus
}
#endif

#endif
