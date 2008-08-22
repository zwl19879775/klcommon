///
/// @file ls_script_holder.h
/// @author Kevin Lynx
/// @date 8.22.2008
/// @brief to hold script file data
///
#ifndef ___LS_SCRIPT_HOLDER_H_
#define ___LS_SCRIPT_HOLDER_H_

#include <map>
#include <string>

namespace lua_sys
{
	///
	/// ScriptHolder is to hold script file data.You can use it to hold many scripts.
	///
	class ScriptHolder
	{
	public:
		///
		/// Script file info description.
		///
		struct FileInfo
		{
			/// file size
			std::size_t _size;
			/// file content, the raw data.
			char *_content;
		};
		/// the script file list.
		typedef std::map<std::string, FileInfo> FileList;

	public:
		ScriptHolder()
		{
		}

		~ScriptHolder()
		{
			clear();
		}

		///
		/// Add script file to this holder.
		///
		/// @param name the file name to identify the file.
		/// @param size the file size
		/// @param content the file content pointer, you must keep this pointer valid, the holder will free
		/// the memory.
		void add_file( const std::string &name, std::size_t size, char *content );

		///
		/// Remove a script file in this holder.It will free the memory which content holds.
		/// @todo to implement this interface.
		///
		void remove_file( const std::string &name );

		/// clear the holder, it will free all memory.
		void clear();

		/// get the file list
		const FileList &get_file_list() const
		{
			return _files;
		}

		/// return the file count holded by this holder.
		std::size_t get_file_count() const
		{
			return _files.size();
		}

	private:
		/// the file list
		FileList _files;
	};
}

#endif // ___LS_SCRIPT_HOLDER_H_