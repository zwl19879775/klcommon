///
/// @file kl_state.h
/// @author Kevin Lynx
/// @date 12.21.2009
/// @brief A simple FSM(finite state machine) pattern implemention.
///
#ifndef ___KL_STATE_H_
#define ___KL_STATE_H_

namespace kl_common
{
	///
	/// Base state class, you should inherite this class to implement your
	/// concrete state.
	///
	template <typename Tp>
	class state
	{
	public:
		typedef Tp entity_type;
		typedef state<entity_type> self_type;
	public:
		virtual ~state() { }

		/// Execute when enter this state.
		virtual void enter( entity_type* )  { }

		/// Execute when leave this state.
		virtual void leave( entity_type* ) { }

		/// Execute when in this state.
		virtual void execute( entity_type* ) = 0;

		/// Compare two states.
		virtual bool operator == ( const self_type &other ) const = 0;

		bool operator != ( const self_type &other ) const
		{
			return !( *this == other );
		}
	};

	///
	/// Usage: state_machine<Player> m_smachine;
	///
	template <typename Tp>
	class state_machine
	{
	public:
		typedef Tp entity_type;
		typedef state<entity_type> state_type;
	public:
		state_machine( entity_type *owner ) :
			_owner( owner )
		{
			clear();
		}

		~state_machine()
		{
		}

		/// Clear the current state.
		void clear()
		{
			_cur_state = 0;
		}

		/// Return true if the machine has a state.
		bool has_state() const
		{
			return _cur_state != 0;
		}

		/// Check whether is in the specified state.
		bool is_state( const state_type *other ) const
		{
			if( !has_state() )
			{
				return false;
			}
			return *other == *_cur_state;
		}

		/// Execute the current state.
		void execute() const
		{
			if( has_state() )
			{
				_cur_state->execute( _owner );
			}
		}

		/// Change state.
		void change( state_type *other )
		{
			if( has_state() )
			{
				_cur_state->leave();
			}
			other->enter();
			_cur_state = other;
		}

	private:
		state_type *_cur_state;
		entity_type *_owner;
	};
}

#endif // __KL_STATE_H_
