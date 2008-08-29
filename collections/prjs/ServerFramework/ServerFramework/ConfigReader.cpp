///
/// @file ConfigReader.cpp
/// @author Kevin Lynx
/// @date 3.11.2008
///
#include "ConfigReader.h"
#include <string>
#include <fstream>

ConfigReader::ConfigReader()
{
	reset();
}

ConfigReader::~ConfigReader()
{
}

bool ConfigReader::load( const char *file_name )
{
	std::ifstream file_stream( file_name );
	if( !file_stream.is_open() )
	{
		return false;
	}

	std::string str_temp;
	void *ret = file_stream >> str_temp >> _host_port
		>> str_temp >> _max_client_count 
		>> str_temp >> _send_io_number
		>> str_temp >> _max_send_buf_size
		>> str_temp >> _send_inter_time;

	file_stream.close();
	return ret != 0;
}

void ConfigReader::reset()
{
	_host_port = 7100;

	_max_client_count = 200;
	_send_io_number = 100;
	_max_send_buf_size = 167772160;
	_send_inter_time = 5000;
}

