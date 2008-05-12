///
/// @file kl_functor.h
/// @author Kevin Lynx
/// @date 3.11.2008
///
#ifndef ___KL_FUNCTOR_H_
#define ___KL_FUNCTOR_H_

#include "kl_typelist.h"

#ifndef DISABLE_FUNCTOR_ASSERT
#include <cassert>
#endif

namespace kl_common
{
	namespace kl_private
	{
		///
		/// handler_base_type, provide type information.
		///
		template <typename R>
		class handler_type
		{
		public:
			typedef R Result;
			typedef null_type Param1;
			typedef null_type Param2;
			typedef null_type Param3;
			typedef null_type Param4;
			typedef null_type Param5;
			typedef null_type Param6;
			typedef null_type Param7;
			typedef null_type Param8;
			typedef null_type Param9;
			typedef null_type Param10;
			/// TODO : more parameters

			virtual handler_type *do_clone() const = 0;

			template <typename U>
			static U *clone( U *obj )
			{
				if( obj == 0 ) return 0;

				return static_cast<U*>( obj->do_clone() );
			}
		};
	};

	/// 
	/// handler_base
	///
	template <typename R, typename ParamList>
	class handler_base;

	///
	/// partial speciallization for non-parameter type.
	///
	template <typename R>
	class handler_base<R, null_type> :  public kl_private::handler_type<R>
	{
	public:
		///
		virtual R operator() () = 0;
	};

	/// one parameter
	template <typename R, typename P1>
	class handler_base<R, TYPE_LIST1( P1 )> : public kl_private::handler_type<R>
	{
	public:
		typedef P1 Param1;
	public:
		///
		virtual R operator() ( P1 ) = 0;
	};

	/// two parameter
	template <typename R, typename P1, typename P2>
	class handler_base<R, TYPE_LIST2( P1, P2 )> : public kl_private::handler_type<R>
	{
	public:
		typedef P1 Param1;
		typedef P2 Param2;
	public:
		///
		virtual R operator() ( P1, P2 ) = 0;
	};

	/// three parameter
	template <typename R, typename P1, typename P2, typename P3>
	class handler_base<R, TYPE_LIST3( P1, P2, P3 )> : public kl_private::handler_type<R>
	{
	public:
		typedef P1 Param1;
		typedef P2 Param2;
		typedef P3 Param3;
	public:
		///
		virtual R operator() ( P1, P2, P3 ) = 0;
	};

	/// four parameter
	template <typename R, typename P1, typename P2, typename P3, typename P4>
	class handler_base<R, TYPE_LIST4( P1, P2, P3, P4)> : public kl_private::handler_type<R>
	{
	public:
		typedef P1 Param1;
		typedef P2 Param2;
		typedef P3 Param3;
		typedef P4 Param4;
	public:
		///
		virtual R operator() ( P1, P2, P3, P4 ) = 0;
	};

	/// five parameters
	template <typename R, typename P1, typename P2, typename P3, typename P4, typename P5>
	class handler_base<R, TYPE_LIST5( P1, P2, P3, P4, P5)> : public kl_private::handler_type<R>
	{
	public:
		typedef P1 Param1;
		typedef P2 Param2;
		typedef P3 Param3;
		typedef P4 Param4;
		typedef P5 Param5;
	public:
		///
		virtual R operator() ( P1, P2, P3, P4, P5 ) = 0;
	};

	/// six parameters
	template <typename R, typename P1, typename P2, typename P3, typename P4, typename P5,
	typename P6>
	class handler_base<R, TYPE_LIST6( P1, P2, P3, P4, P5, P6)> : public kl_private::handler_type<R>
	{
	public:
		typedef P1 Param1;
		typedef P2 Param2;
		typedef P3 Param3;
		typedef P4 Param4;
		typedef P5 Param5;
		typedef P6 Param6;
	public:
		///
		virtual R operator() ( P1, P2, P3, P4, P5, P6 ) = 0;
	};

	/// seven parameters
	template <typename R, typename P1, typename P2, typename P3, typename P4, typename P5,
	typename P6, typename P7>
	class handler_base<R, TYPE_LIST7( P1, P2, P3, P4, P5, P6, P7)> : public kl_private::handler_type<R>
	{
	public:
		typedef P1 Param1;
		typedef P2 Param2;
		typedef P3 Param3;
		typedef P4 Param4;
		typedef P5 Param5;
		typedef P6 Param6;
		typedef P7 Param7;
	public:
		///
		virtual R operator() ( P1, P2, P3, P4, P5, P6, P7 ) = 0;
	};

	/// eight parameters
	template <typename R, typename P1, typename P2, typename P3, typename P4, typename P5,
	typename P6, typename P7, typename P8>
	class handler_base<R, TYPE_LIST8( P1, P2, P3, P4, P5, P6, P7, P8)> : public kl_private::handler_type<R>
	{
	public:
		typedef P1 Param1;
		typedef P2 Param2;
		typedef P3 Param3;
		typedef P4 Param4;
		typedef P5 Param5;
		typedef P6 Param6;
		typedef P7 Param7;
		typedef P8 Param8;
	public:
		///
		virtual R operator() ( P1, P2, P3, P4, P5, P6, P7, P8 ) = 0;
	};

	/// nine parameters
	template <typename R, typename P1, typename P2, typename P3, typename P4, typename P5,
	typename P6, typename P7, typename P8, typename P9>
	class handler_base<R, TYPE_LIST9( P1, P2, P3, P4, P5, P6, P7, P8, P9)> : public kl_private::handler_type<R>
	{
	public:
		typedef P1 Param1;
		typedef P2 Param2;
		typedef P3 Param3;
		typedef P4 Param4;
		typedef P5 Param5;
		typedef P6 Param6;
		typedef P7 Param7;
		typedef P8 Param8;
		typedef P9 Param9;
	public:
		///
		virtual R operator() ( P1, P2, P3, P4, P5, P6, P7, P8, P9 ) = 0;
	};

	/// ten parameters
	template <typename R, typename P1, typename P2, typename P3, typename P4, typename P5,
	typename P6, typename P7, typename P8, typename P9, typename P10>
	class handler_base<R, TYPE_LIST10( P1, P2, P3, P4, P5, P6, P7, P8, P9, P10)> : public kl_private::handler_type<R>
	{
	public:
		typedef P1 Param1;
		typedef P2 Param2;
		typedef P3 Param3;
		typedef P4 Param4;
		typedef P5 Param5;
		typedef P6 Param6;
		typedef P7 Param7;
		typedef P8 Param8;
		typedef P9 Param9;
		typedef P10 Param10;
	public:
		///
		virtual R operator() ( P1, P2, P3, P4, P5, P6, P7, P8, P9, P10 ) = 0;
	};

	/// 
	/// handler class, It supports c-functions and functors.
	/// 
	template <typename _HandlerBase, typename _FuncType>
	class handler : public _HandlerBase
	{
	public:
		/// function type
		typedef _FuncType func_type;
		/// base class type
		typedef _HandlerBase base_type;
		typedef typename _HandlerBase::Result result_type;
		typedef typename _HandlerBase::Param1 param1_type;
		typedef typename _HandlerBase::Param2 param2_type;
		typedef typename _HandlerBase::Param3 param3_type;
		typedef typename _HandlerBase::Param4 param4_type;
		typedef typename _HandlerBase::Param5 param5_type;
		typedef typename _HandlerBase::Param6 param6_type;
		typedef typename _HandlerBase::Param7 param7_type;
		typedef typename _HandlerBase::Param8 param8_type;
		typedef typename _HandlerBase::Param9 param9_type;
		typedef typename _HandlerBase::Param10 param10_type;

		/// TODO : add more parameters
	public:
		/// constructor
		handler( const func_type &func ) :
		  _func( func )
		{
		}
		
		/// to copy this
		kl_private::handler_type<result_type> *do_clone() const
		{
			return new handler<base_type, func_type>( _func );
		}

		/// operator() functions
		result_type operator() ()
		{
			// even result_type is void, it's correct.
			return _func();
		}

		result_type operator() ( param1_type p1 )
		{
			return _func( p1 );
		}

		result_type operator() ( param1_type p1, param2_type p2 )
		{
			return _func( p1, p2 );
		}

		result_type operator() ( param1_type p1, param2_type p2, param3_type p3 )
		{
			return _func( p1, p2, p3 );
		}

		result_type operator() ( param1_type p1, param2_type p2, param3_type p3, param4_type p4 )
		{
			return _func( p1, p2, p3, p4 );
		}

		result_type operator() ( param1_type p1, param2_type p2, param3_type p3, param4_type p4, 
			param5_type p5 )
		{
			return _func( p1, p2, p3, p4, p5 );
		}

		result_type operator() ( param1_type p1, param2_type p2, param3_type p3, param4_type p4, 
			param5_type p5, param6_type p6 )
		{
			return _func( p1, p2, p3, p4, p5, p6 );
		}

		result_type operator() ( param1_type p1, param2_type p2, param3_type p3, param4_type p4, 
			param5_type p5, param6_type p6, param7_type p7 )
		{
			return _func( p1, p2, p3, p4, p5, p6, p7 );
		}

		result_type operator() ( param1_type p1, param2_type p2, param3_type p3, param4_type p4, 
			param5_type p5, param6_type p6, param7_type p7, param8_type p8 )
		{
			return _func( p1, p2, p3, p4, p5, p6, p7, p8 );
		}

		result_type operator() ( param1_type p1, param2_type p2, param3_type p3, param4_type p4, 
			param5_type p5, param6_type p6, param7_type p7, param8_type p8, param9_type p9 )
		{
			return _func( p1, p2, p3, p4, p5, p6, p7, p8, p9 );
		}

		result_type operator() ( param1_type p1, param2_type p2, param3_type p3, param4_type p4, 
			param5_type p5, param6_type p6, param7_type p7, param8_type p8, param9_type p9, param10_type p10 )
		{
			return _func( p1, p2, p3, p4, p5, p6, p7, p8, p9, p10 );
		}
	protected:
		/// function
		func_type _func;
	};

	///
	/// memfun_handler, wrap member functions.
	///
	/// @param _ObjPtrType a class pointer type
	template <typename _HandlerBase, typename _ObjType, typename _FuncType>
	class memfun_handler : public _HandlerBase
	{
	public:
		/// function type
		typedef _FuncType func_type;
		/// object type
		typedef _ObjType obj_type;
		typedef typename _HandlerBase::Result result_type;
		typedef typename _HandlerBase::Param1 param1_type;
		typedef typename _HandlerBase::Param2 param2_type;
		typedef typename _HandlerBase::Param3 param3_type;
		typedef typename _HandlerBase::Param4 param4_type;
		typedef typename _HandlerBase::Param5 param5_type;
		typedef typename _HandlerBase::Param6 param6_type;
		typedef typename _HandlerBase::Param7 param7_type;
		typedef typename _HandlerBase::Param8 param8_type;
		typedef typename _HandlerBase::Param9 param9_type;
		typedef typename _HandlerBase::Param10 param10_type;
		/// TODO : add more parameters
	public:
		/// constructor
		memfun_handler( obj_type &obj, const func_type &func ) :
		  _obj( obj ), _func( func )
		{

		}
	
		/// to copy this
		kl_private::handler_type<result_type> *do_clone() const
		{
			return new memfun_handler( _obj, _func );
		}

		/// operator() functions
		result_type operator() ()
		{
			// even result_type is void, it's correct.
			return (_obj.*_func)();
		}

		result_type operator() ( param1_type p1 )
		{
			return (_obj.*_func)( p1 );
		}

		result_type operator() ( param1_type p1, param2_type p2 )
		{
			return (_obj.*_func)( p1, p2 );
		}

		result_type operator() ( param1_type p1, param2_type p2, param3_type p3 )
		{
			return (_obj.*_func)( p1, p2, p3 );
		}

		result_type operator() ( param1_type p1, param2_type p2, param3_type p3, param4_type p4 )
		{
			return (_obj.*_func)( p1, p2, p3, p4 );
		}

		result_type operator() ( param1_type p1, param2_type p2, param3_type p3, param4_type p4, 
			param5_type p5 )
		{
			return (_obj.*_func)( p1, p2, p3, p4, p5 );
		}

		result_type operator() ( param1_type p1, param2_type p2, param3_type p3, param4_type p4, 
			param5_type p5, param6_type p6 )
		{
			return (_obj.*_func)( p1, p2, p3, p4, p5, p6 );
		}

		result_type operator() ( param1_type p1, param2_type p2, param3_type p3, param4_type p4, 
			param5_type p5, param6_type p6, param7_type p7 )
		{
			return (_obj.*_func)( p1, p2, p3, p4, p5, p6, p7 );
		}

		result_type operator() ( param1_type p1, param2_type p2, param3_type p3, param4_type p4, 
			param5_type p5, param6_type p6, param7_type p7, param8_type p8 )
		{
			return (_obj.*_func)( p1, p2, p3, p4, p5, p6, p7, p8 );
		}

		result_type operator() ( param1_type p1, param2_type p2, param3_type p3, param4_type p4, 
			param5_type p5, param6_type p6, param7_type p7, param8_type p8, param9_type p9 )
		{
			return (_obj.*_func)( p1, p2, p3, p4, p5, p6, p7, p8, p9 );
		}

		result_type operator() ( param1_type p1, param2_type p2, param3_type p3, param4_type p4, 
			param5_type p5, param6_type p6, param7_type p7, param8_type p8, param9_type p9, param10_type p10 )
		{
			return (_obj.*_func)( p1, p2, p3, p4, p5, p6, p7, p8, p9, p10 );
		}


	protected:
		/// object
		obj_type &_obj;
		/// member function
		func_type _func;
	};

	/// whether enable assert
#ifndef DISABLE_FUNCTOR_ASSERT
#define ASSERT_CHECK_HANDLER \
	assert( _handler != 0 && "functor : The functor is invalid!" );
#else
#define ASSERT_CHECK_HANDLER
#endif

	///
	/// functor class, can wrap functions, functors and member functions.
	/// And now it supports 10 parameters at most.
	/// @warning if the functor is invalid, it may cause an assertion. You can 
	/// define DISABLE_FUNCTOR_ASSERT to disable the assertion checking.
	/// @note this module is some like loki::functor.
	///
	template <typename R = void, typename ParamList = null_type>
	class functor
	{
	public:
		typedef R result_type;
		typedef ParamList param_list;
		/// handler base type
		typedef handler_base<R, ParamList> handler_base_type;
		/// parameter
		typedef typename handler_base_type::Param1 param1_type;
		typedef typename handler_base_type::Param2 param2_type;
		typedef typename handler_base_type::Param3 param3_type;
		typedef typename handler_base_type::Param4 param4_type;
		typedef typename handler_base_type::Param5 param5_type;
		typedef typename handler_base_type::Param6 param6_type;
		typedef typename handler_base_type::Param7 param7_type;
		typedef typename handler_base_type::Param8 param8_type;
		typedef typename handler_base_type::Param9 param9_type;
		typedef typename handler_base_type::Param10 param10_type;
	public:
		/// trivial constructor
		functor() : _handler( 0 )
		{
		}

		/// wrap c-function or functor
		template <typename _FuncType>
		functor( _FuncType func ) :
			_handler( new handler<handler_base_type, _FuncType>( func ) )
		{
		}
		
		/// wrap member functions
		template <typename _ObjType, typename _FuncType>
		functor( _ObjType &obj, _FuncType func ) :
			_handler( new memfun_handler<handler_base_type, _ObjType, _FuncType>( obj, func ) )
		{
		}

		/// copy constructor, will create a new handler
		functor( const functor &f ) :
			_handler( handler_base_type::clone( f._handler ) )
		{
			
		}

		~functor()
		{
			delete _handler;
		}

		/// copy assignment , will create a new handler
		functor &operator= ( const functor &f ) 
		{
			/// this 
			if( f._handler == this->_handler )
			{
				return *this;
			}

			if( _handler != 0 )
			{
				delete _handler;
				_handler = 0;
			}

			_handler = handler_base_type::clone( f._handler );
			return *this;
		}

		/// operator bool, so you can if( cmd ) to check whether the functor is valid.
		/// @return true if the functor is valid.
		operator bool () const
		{
			return valid();
		}

		/// whether the functor is invalid, if the functor has handler, return true.
		bool valid() const
		{
			return _handler != 0;
		}

		/// reset the functor to invalid
		void reset()
		{
			if( _handler != 0 )
			{
				delete _handler;
				_handler = 0;
			}
		}

		/// operator() 
		result_type operator() ()
		{
			ASSERT_CHECK_HANDLER
			return (*_handler)();
		}

		result_type operator() ( param1_type p1 )
		{
			ASSERT_CHECK_HANDLER
			return (*_handler)( p1 );
		}
		
		result_type operator() ( param1_type p1, param2_type p2 )
		{
			ASSERT_CHECK_HANDLER
			return (*_handler)( p1, p2 );
		}

		result_type operator() ( param1_type p1, param2_type p2, param3_type p3 )
		{
			ASSERT_CHECK_HANDLER
			return (*_handler)( p1, p2, p3 );
		}

		result_type operator() ( param1_type p1, param2_type p2, param3_type p3, param4_type p4 )
		{
			ASSERT_CHECK_HANDLER
			return (*_handler)( p1, p2, p3, p4 );
		}

		result_type operator() ( param1_type p1, param2_type p2, param3_type p3, param4_type p4, param5_type p5 )
		{
			ASSERT_CHECK_HANDLER
			return (*_handler)( p1, p2, p3, p4, p5 );
		}

		result_type operator() ( param1_type p1, param2_type p2, param3_type p3, param4_type p4, param5_type p5,
			param6_type p6 )
		{
			ASSERT_CHECK_HANDLER
			return (*_handler)( p1, p2, p3, p4, p5, p6 );
		}

		result_type operator() ( param1_type p1, param2_type p2, param3_type p3, param4_type p4, param5_type p5,
			param6_type p6, param7_type p7 )
		{
			ASSERT_CHECK_HANDLER
			return (*_handler)( p1, p2, p3, p4, p5, p6, p7 );
		}

		result_type operator() ( param1_type p1, param2_type p2, param3_type p3, param4_type p4, param5_type p5,
			param6_type p6, param7_type p7, param8_type p8 )
		{
			ASSERT_CHECK_HANDLER
			return (*_handler)( p1, p2, p3, p4, p5, p6, p7, p8 );
		}

		result_type operator() ( param1_type p1, param2_type p2, param3_type p3, param4_type p4, param5_type p5,
			param6_type p6, param7_type p7, param8_type p8, param9_type p9 )
		{
			ASSERT_CHECK_HANDLER
			return (*_handler)( p1, p2, p3, p4, p5, p6, p7, p8, p9 );
		}

		result_type operator() ( param1_type p1, param2_type p2, param3_type p3, param4_type p4, param5_type p5,
			param6_type p6, param7_type p7, param8_type p8, param9_type p9, param10_type p10 )
		{
			ASSERT_CHECK_HANDLER
			return (*_handler)( p1, p2, p3, p4, p5, p6, p7, p8, p9, p10 );
		}

	private:
		/// handler
		handler_base_type *_handler;
	};

}

#endif // end ___KL_FUNCTOR_H_