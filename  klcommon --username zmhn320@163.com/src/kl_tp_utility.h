///
/// @file kl_tp_utility.h
/// @author Kevin Lynx
/// @brief To provide some utilify about template.
///
#ifndef ___KL_TP_UTILITY_H_
#define ___KL_TP_UTILITY_H_

namespace kl_common
{
	///
	/// The kl_cast is primary to remove the warning from static_cast<bool>( int )
	///
	template <typename _Tp>
	struct kl_cast
	{
		template <typename _Other>
		_Tp operator() ( const _Other &v )
		{
			return static_cast<_Tp>( v );
		}
	};

	template <>
	struct kl_cast<bool>
	{
		template <typename _Other>
		bool operator() ( const _Other &v )
		{
			return v ? true : false;
		}
	};

	///
	/// deletor is used to help std::for_each to delete stl container items.And it makes you
	/// can write these codes :
	/// std::for_each( my_list.begin(), my_list.end(), deletor<ObjType> ); if your list is:
	/// std::list<ObjType*> my_list; 
	///
	template <typename _Tp>
	void deletor( _Tp *obj )
	{
		delete obj;
	}

	///
	/// A tiny macro to make your map::find.It will expand some codes to find something in map.
	///
	/// usage :
	/// Obj *MyClass::GetObj( const long id )
	/// {
	///      MAP_FIND_FUNC( ObjMap, m_Objs, id, NULL );
	/// }
	///
#define MAP_FIND_FUNC( type, container, key, null ) \
	do { \
	type::iterator it = container.find( key ); \
	if( it != container.end() ) \
	{ \
		return it->second; \
	} \
	return null; \
	} while( 0 ) 

#define MAP_FIND_CONST_FUNC( type, container, key, null ) \
	do { \
	type::const_iterator it = container.find( key ); \
	if( it != container.end() ) \
	{ \
		return it->second; \
	} \
	return null; \
	} while( 0 ) \

	///
	/// map_func_first is such a component that can be used to for_each a map container.And visit
	/// the key of the map item.
	/// i.e :
	///
	///   void IDHandler( int id )
	///   {
	///   }
	///   std::map<int, std::string> IDString;
	///   for_each( IDString.begin(), IDString.end(), map_func_first( IDHandler ) );
	///
	template <typename _Func>
	struct _map_func_first
	{
		_map_func_first( _Func func ) : _func( func )
		{
		}
		template <typename _Elem>
		void operator() ( _Elem &item )
		{
			_func( item.first );
		}

		_Func _func;
	};

	template <typename _Func>
	_map_func_first<_Func> map_func_first( _Func func )
	{
		return _map_func_first<_Func>( func );
	}

	///
	/// It's almost the same as map_func_first, but it visit the value of the map item.
	///
	template <typename _Func>
	struct _map_func_second
	{
		_map_func_second( _Func func ) : _func( func )
		{
		}
		template <typename _Elem>
		void operator() ( _Elem &item )
		{
			_func( item.second );
		}

		_Func _func;
	};

	template <typename _Func>
	_map_func_second<_Func> map_func_second( _Func func )
	{
		return _map_func_second<_Func>( func );
	}

}

#endif // ___KL_TP_UTILITY_H_