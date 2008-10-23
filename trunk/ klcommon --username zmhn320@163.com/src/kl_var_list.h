/// 
/// @file kl_var_list.h
/// @author Kevin Lynx
/// @brief To save some variables with different type in a list, and keep the list in the map.
///               I suppose the better way is to use 'tuple'.
///
#ifndef ___KL_VAR_LIST_H_
#define ___KL_VAR_LIST_H_

#include <vector>
#include <algorithm>
#include "kl_tp_utility.h"
#include "kl_any.h"

namespace kl_common
{		
	///
	/// var_list maintain a list of variables.You can retrieve the variable by its index. In fact, it's an array container which
	/// contains variables with different types.
	///
	class var_list
	{
	public:
		typedef std::vector<any*> var_list_type;
	public:
		var_list()
		{
		}
		
		~var_list()
		{
			std::for_each( _var_list.begin(), _var_list.end(), deletor<any> );
		}
		
		/// 
		/// Add a variable to the back of the list.
		///
		template <typename _Tp>
		void add( const _Tp &t )
		{
			any *var = new any( t );
			_var_list.push_back( var );
		}
		
		///
		/// Retrieve the variable in the list, you must provide the right type for the variable.
		///
		template <typename _Tp>
		_Tp &get( unsigned int index )
		{
			any *t = _var_list.at( index );
			return *any_cast<_Tp>( t );		
		}
		
		template <typename _Tp>
		const _Tp &get( unsigned int index ) const
		{
			const any *t = _var_list.at( index );
			return *any_cast<_Tp>( t );
		}		
		
	private:
		var_list_type _var_list;
	};
}

#endif ___KL_VAR_LIST_H_