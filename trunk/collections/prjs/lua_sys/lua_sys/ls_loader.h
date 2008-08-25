///
/// @file ls_loader.h
/// @author Kevin Lynx
/// @date 8.22.2008
/// @brief to provide some helper function like load scripts/encode scripts/decode scripts.
///
#ifndef ___LS_LOADER_H_
#define ___LS_LOADER_H_

#include <vector>
#include <string>

#define __FLOWER

#ifdef __FLOWER
#define __FLOWER_PUBLIC
#endif

namespace lua_sys
{
	class ScriptHolder;

	///
	/// ScriptLoader it can load/encode/decode script files 
	///
	class ScriptLoader
	{
	public:
		typedef std::vector<unsigned char> RawType;

		///
		/// Load script files recursivly in a directory.
		/// 
		/// @param path the directory to search
		/// @param suffix the suffix string of a file.
		/// @return the number of files loaded.
		///
		static std::size_t load( ScriptHolder &holder, const std::string &path, const std::string &suffix = "lua" );

		///
		/// Encode a script holder to raw data.
		///
		/// @param holder the script holder which holds script files.
		/// @param raw [out] to hold the encode data.
		/// @return the number of bytes added into raw.
		///
		static std::size_t encode( const ScriptHolder &holder, RawType &raw );

		///
		/// Decode a script holder from raw data.
		///
		/// @param raw the raw data
		/// @param holder to hold the script files.
		/// @param offset the offset from raw, default value is zero.
		/// @return the number of bytes it decoded.
		///
		static std::size_t decode( const RawType &raw, ScriptHolder &holder, std::size_t offset = 0 );
	};
}

#endif // ___LS_LOADER_H_