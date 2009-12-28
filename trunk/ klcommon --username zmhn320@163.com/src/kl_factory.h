///
/// @file kl_factory.h
/// @author Kevin Lynx
/// @brief Implement factory pattern.
///
#ifndef ___KL_FACTORY_H_
#define ___KL_FACTORY_H_

#include <map>
#include "kl_macro_params.h"

namespace kl_common
{
	/// 
	/// Wrap the implemention to create products.
	///
	template <typename IdType, typename Product, typename Creator, typename Destroyer>
	class factory
	{
	public:
		typedef IdType id_type;
		typedef Product product_type;
		typedef Creator creator_type;
		typedef Destroyer destroyer_type;

		struct life_policy
		{
			life_policy( creator_type c, destroyer_type d ) :
				creator( c ), destroyer( d )
			{
			}
			life_policy() : creator( creator_type() ), destroyer( destroyer_type() )
			{
			}
			creator_type creator;
			destroyer_type destroyer;
		};

		typedef std::map<id_type, life_policy> policy_table_type;
	public:
		factory()
		{
			clear();
		}

		~factory()
		{
			clear();
		}

		/// Clear all creators in the factory.
		void clear()
		{
			_policies.clear();
		}

		/// Add a creator in the factory.
		void add( id_type id, const life_policy &lp )
		{
			_policies[id] = lp;
		}

		void add( id_type id, creator_type c, destroyer_type d )
		{
			add( id, life_policy( c, d ) );
		}

		/// Remove a creator from the factory.
		void remove( id_type id )
		{
			_policies.erase( id );
		}

		/// Create a product, return the pointer. If there's no creator
		/// for this product, return NULL.
		product_type *create( id_type id ) const
		{
			policy_table_type::const_iterator it = _policies.find( id );
			if( it == _policies.end() )
			{
				return 0;
			}
			return it->second.creator();
		}

		/// To expand many 'create' functions to support multiple parameters creation.
	#define EXPAND_CREATE_FUNC( n ) \
		template <DEF_PARAM( n )> \
		product_type *create( id_type id, DEF_FUNC_PARAM_P( n ) ) \
		{ \
			policy_table_type::const_iterator it = _policies.find( id ); \
			if( it == _policies.end() ) \
			{ \
				return 0; \
			} \
			return it->second.creator( DEF_FUNC_ARG( n ) ); \
		}

		EXPAND_CREATE_FUNC( 1 );
		EXPAND_CREATE_FUNC( 2 );
		EXPAND_CREATE_FUNC( 3 );
		EXPAND_CREATE_FUNC( 4 );
		EXPAND_CREATE_FUNC( 5 );
		EXPAND_CREATE_FUNC( 6 );
		EXPAND_CREATE_FUNC( 7 );
		EXPAND_CREATE_FUNC( 8 );
		EXPAND_CREATE_FUNC( 9 );
		EXPAND_CREATE_FUNC( 10 );
		EXPAND_CREATE_FUNC( 11 );
		EXPAND_CREATE_FUNC( 12 );
		EXPAND_CREATE_FUNC( 13 );
		EXPAND_CREATE_FUNC( 14 );
		EXPAND_CREATE_FUNC( 15 );

		/// Delete a product. If there's no deletor for this product,
		/// return false.
		bool destroy( id_type id, product_type *p ) const
		{
			policy_table_type::const_iterator it = _policies.find( id );
			if( it == _policies.end() )
			{
				return false;
			}
			it->second.destroyer( p );
			return true;
		}

	private:
		policy_table_type _policies;
	};
}

#endif

