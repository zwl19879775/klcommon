/// 
/// @file kl_var_list.h
/// @author Kevin Lynx
/// @brief To save some variables with different type in a list, and keep the list in the map.
///               I suppose the better way is to use 'tuple'.
///
#ifndef ___KL_VAR_LIST_H_
#define ___KL_VAR_LIST_H_

#include <vector>
#include <map>
#include <algorithm>
#include "kl_tp_utility.h"
#include "kl_obj_manager.h"

namespace kl_common
{	
	///
	/// Base type to make everything compatible.
	///
	struct base_type
	{
		virtual ~base_type() { }
	};
	
	///
	/// Wrap a variable with different type.Because var_wrapper inherit from base_type, so that i can 
	/// handle them .
	///
	template <typename _Tp>
	class var_wrapper : public base_type
	{
	public:
		typedef _Tp type;
	public:
		var_wrapper( const type &t ) : _t( t )
		{
		}
		
		operator type& ()
		{
			return _t;
		}
	
		operator const type& () const
		{
			return _t;
		}
	
	private:
		type _t;
	};
	
	///
	/// var_list maintain a list of variables.You can retrieve the variable by its index. In fact, it's an array container which
	/// contains variables with different types.
	///
	class var_list
	{
	public:
		typedef std::vector<base_type*> var_list_type;
	public:
		var_list()
		{
		}
		
		~var_list()
		{
			std::for_each( _var_list.begin(), _var_list.end(), deletor<base_type> );
		}
		
		/// 
		/// Add a variable to the back of the list.
		///
		template <typename _Tp>
		void add( const _Tp &t )
		{
			var_wrapper<_Tp> *var = new var_wrapper<_Tp>( t );
			_var_list.push_back( var );
		}
		
		///
		/// Retrieve the variable in the list, you must provide the right type for the variable.
		////
		template <typename _Tp>
		_Tp &get( unsigned int index )
		{
			base_type *base = _var_list.at( index );
			typedef var_wrapper<_Tp> var_type;
			var_type *var = static_cast<var_type*>( base );
			return *var;		
		}
		
		template <typename _Tp>
		const _Tp &get( unsigned int index ) const
		{
			base_type *base = _var_list.at( index );
			typedef var_wrapper<_Tp> var_type;
			var_type *var = static_cast<var_type*>( base );
			return *var;		
		}		
		
	private:
		var_list_type _var_list;
	};
	
	///
	/// Wrap the var list in a map. It's a helper class to help you manage the variable list.
	///
	template <typename _IDType, typename _ContainerType = std::map<_IDType, var_lsit*> >
	class var_list_keeper : public ObjManager<_IDType, var_list*>
	{
	};
}

#endif ___KL_VAR_LIST_H_