///
/// @file kl_typelist.h
/// @author Kevin Lynx
/// @date 3.11.2008
///
#ifndef ___KL_TYPELIST_H_
#define ___KL_TYPELIST_H_

namespace kl_common
{
	/// 
	/// null type
	///
	struct null_type
	{
	};

	/// 
	/// type list
	///
	template <typename T, typename U>
	struct type_list
	{
		typedef T head_type;
		typedef U tail_type;
	};

	/// type list macro
#define TYPE_LIST1( T1 ) ::kl_common::type_list<T1, kl_common::null_type>
#define TYPE_LIST2( T1, T2 ) ::kl_common::type_list<T1, TYPE_LIST1( T2 ) >
#define TYPE_LIST3( T1, T2, T3 ) ::kl_common::type_list<T1, TYPE_LIST2( T2, T3 ) >
#define TYPE_LIST4( T1, T2, T3, T4 ) ::kl_common::type_list<T1, TYPE_LIST3( T2, T3, T4 ) >
#define TYPE_LIST5( T1, T2, T3, T4, T5 ) ::kl_common::type_list<T1, TYPE_LIST4( T2, T3, T4, T5 ) >
#define TYPE_LIST6( T1, T2, T3, T4, T5, T6 ) ::kl_common::type_list<T1, TYPE_LIST5( T2, T3, T4, T5, T6 ) >
#define TYPE_LIST7( T1, T2, T3, T4, T5, T6, T7 ) ::kl_common::type_list<T1, TYPE_LIST6( T2, T3, T4, T5, T6, T7 ) >
#define TYPE_LIST8( T1, T2, T3, T4, T5, T6, T7, T8 ) ::kl_common::type_list<T1, TYPE_LIST7( T2, T3, T4, T5, T6, T7, T8 ) >
#define TYPE_LIST9( T1, T2, T3, T4, T5, T6, T7, T8, T9 ) ::kl_common::type_list<T1, TYPE_LIST8( T2, T3, T4, T5, T6, T7, T8, T9 ) >
#define TYPE_LIST10( T1, T2, T3, T4, T5, T6, T7, T8, T9, T10 ) ::kl_common::type_list<T1, TYPE_LIST9( T2, T3, T4, T5, T6, T7, T8, T9, T10 ) >

}

#endif // end ___KL_TYPELIST_H_