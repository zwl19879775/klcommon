///
/// @file klnet_system.h
/// @author Kevin Lynx
/// @date 5.23.2008
///
#ifndef ___KLNET_SYSTEM_H_
#define ___KLNET_SYSTEM_H_

#include "klnet_config.h"

namespace kl_net
{
	namespace system
	{
		///
		/// startup the net system
		///
		inline bool startup()
		{
			WSADATA wd;
			return ::WSAStartup( MAKEWORD( 2, 2 ), &wd ) == 0;
		}

		/// 
		/// shutdown the net system
		///
		inline void shutdown()
		{
			WSACleanup();
		}

		///
		/// create an ID for some objects, this id can only be used in this process.
		/// As you can see here, if the id is overflow, some bugs will come out to bit you.
		///
		typedef unsigned long SystemIDType;
		inline SystemIDType create_id()
		{
			static SystemIDType _cur_id = 0;
			return _cur_id ++;
		}
	}
}

#endif // end ___KLNET_SYSTEM_H_