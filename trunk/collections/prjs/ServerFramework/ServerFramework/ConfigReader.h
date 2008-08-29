///
/// @file ConfigReader.h
/// @author Kevin Lynx
/// @date 3.11.2008
///
#ifndef ___CONFIG_READER_H_
#define ___CONFIG_READER_H_

///
/// ConfigReader class, manage reading config and maintain these config
///
struct ConfigReader
{
public:
	/// constructor, set default
	ConfigReader();

	/// trivial destructor
	~ConfigReader();

	/// load config from file
	bool load( const char *file_name );

	/// reset to default
	void reset();

public:
	/// server host port
	unsigned short _host_port;
	/// max client count
	int _max_client_count;
	/// send io number
	long _send_io_number;
	/// max client buffer size
	long _max_send_buf_size;
	/// send inter time, after these time there is still no data received, 
	/// the server will disconnect it
	long _send_inter_time;
};

#endif // end ___CONFIG_READER_H_