///
/// @file kl_ref.h
/// @author Kevin Lynx
/// @date 10.8.2008
/// @brief This is a small library taken from boost or passing references to function templates (algorithms) that would usually take copies of their arguments. 
///
#ifndef ___KL_REF_H_
#define ___KL_REF_H_

namespace kl_common
{
	///
	/// This class contain a reference to the object.
	/// You donot need to use it directly.
	///
	template <typename _Tp>
	class ref_wrapper
	{
	public:
		typedef _Tp type;
	public:
		explicit ref_wrapper( type &t ) : _t( &t )
		{
		}
	
		operator type& () const
		{
			return *_t;
		}
	
		type &get() const
		{
			return *_t;
		}
	
	private:
		type *_t;
	};

	///
	/// This is the interface to wrap an object to reference.
	///
	template <typename _Tp>
	ref_wrapper<_Tp> ref( _Tp &t )
	{
		return ref_wrapper<_Tp>( t );
	}

	///
	/// This is the const version.
	///
	template <typename _Tp>
	ref_wrapper<const _Tp> cref( const _Tp &t )
	{
		return ref_wrapper<const _Tp>( t );
	}
	
	///
	/// To check whether a type is a ref_wrapper instance.
	///
	template <typename _Tp>
	struct is_ref_wrapper_t
	{
		static const bool value = false;
	};
	
	template <typename _Tp>
	struct is_ref_wrapper_t<ref_wrapper<_Tp> >
	{
		static const bool value = true;
	};
	
	template <typename _Tp>
	bool is_ref_wrapper( _Tp t )
	{
		return is_ref_wrapper_t<_Tp>::value;
	}
	
	///
	/// To trait the type wrapped by ref_wrapper. 
	///
	template <typename _Tp>
	struct unwrap_ref
	{
		typedef _Tp type;
	};
	
	template <typename _Tp>
	struct unwrap_ref<ref_wrapper<_Tp> >
	{
		typedef typename ref_wrapper<_Tp>::type type;
	};
}

#endif // end ___KL_REF_H_