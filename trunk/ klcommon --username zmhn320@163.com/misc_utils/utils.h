#ifndef ___UTILS_H_
#define ___UTILS_H_

#include <string>

namespace Utils
{
	struct StringInSensCmp
	{
		StringInSensCmp( const std::string &l ) : _l( l )
		{
		}

		bool operator() ( const std::string &r )
		{
			return ::stricmp( _l.c_str(), r.c_str() ) == 0;
		}

		template <typename _Tp>
			bool operator() ( const _Tp &r )
			{
				return operator() ( r.name );
			}

		const std::string &_l;
	};
}

#endif

