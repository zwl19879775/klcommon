///
/// @file kl_obj_counter.h
/// @author Kevin Lynx
/// @brief The utility to count the instance count for a class.
///
#ifndef ___KL_OBJ_COUNTER_H_
#define ___KL_OBJ_COUNTER_H_

namespace kl_common
{
	///
	///  obj_counter can count the instance count for a class.
	/// usage : 
	/// class YourClass : public obj_counter<YourClass>
	/// { 
	///      ...
	/// };
	/// and anytime you can YourClass::count() to retrieve the instance count.
	///
	template <typename _Tp>
	class obj_counter
	{
	public:
		/// ctor
		obj_counter()
		{
			++ _count;
		}
		
		/// copy ctor
		obj_counter( const obj_counter & )
		{
			++ _count;
		}
		
		/// assignment operator
		obj_counter &operator = ( const obj_counter & )
		{
			++ _count;
			return *this;
		}
		
		/// dtor
		~obj_counter()
		{
			-- _count;
		}
		
		/// retrieve the instance count.
		static unsigned long count()
		{
			return _count;
		}
		
	private:
		static unsigned long _count;
	};
	
	template <typename _Tp> unsigned long obj_counter<_Tp>::_count = 0;
}

#endif // ___KL_OBJ_COUNTER_H_