///
/// @file kl_obj_manager.h
/// @author Kevin Lynx
/// @brief To implement a simple class to manage some objects.
///
#ifndef ___KL_OBJ_MANAGER_H_
#define ___KL_OBJ_MANAGER_H_

#include <map>
#include <algorithm>
#include "kl_tp_utility.h"

namespace kl_common
{
	///
	/// ObjManager, to manage some objects in a container such as std::map.
	/// ObjType can be a pointer as you wish.
	///
	template <typename IDType, typename ObjType, typename ContainerType = std::map<IDType, ObjType> >
	class ObjManager
	{
	public:
		typedef IDType id_type;
		typedef ObjType obj_type;
		typedef ContainerType container_type;	
	public:
		/// default ctor
		ObjManager()
		{
		}
		
		/// virtual dtor, so that you can inherit this class.
		virtual ~ObjManager()
		{
		}
		
		///  clear all objects in the container.
		template <typename Deletor>
		void clear( Deletor deletor )
		{
			std::for_each( _objs.begin(), _objs.end(), map_func_second( deletor ) );
			_objs.clear();
		}
		
		/// add an object into the container.
		void add( id_type id, obj_type &obj )
		{
			_objs[id] = obj;
		}
		
		/// get a write-able object.
		obj_type get( id_type id )
		{
			MAP_FIND_FUNC( container_type, _objs, id, obj_type() );
		}
		
		/// get a read-only object.
		const obj_type get( id_type id ) const
		{
			MAP_FIND_CONST_FUNC( container_type, _objs, id, obj_type() );
		} 
		
	protected:
		container_type _objs;
	};
}

#endif // end ___KL_OBJ_MANAGER_H_