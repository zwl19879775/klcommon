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
}

#endif // ___KL_TP_UTILITY_H_