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
	/// Inherit this class to add more event data. Event is used to 
	/// transform states. It's not necessary to implement this class
	/// because this state machine only depends on your concrete class.
	///
	struct event
	{
	};	

	template <typename StateWrapper>
	class state;

	///
	/// Transition between two states. Also an optional util for you.
	///
	template <typename ConcreteType>
	struct simple_transition
	{
		typedef ConcreteType state_type;
		simple_transition( state_type *from, state_type *to ) :
			_from( from ), _to( to )
		{
		}

		state_type *_from, *_to;
	};
		
	template <typename StateWrapper>
	class state_machine;

	///
	/// Base state class, you should inherite this class to implement your
	/// concrete state.
	/// To use this state library, you'd better to define a wrapper class to 
	/// typedef many require typedefs. This class will be used in base state 
	/// class, to make these functions' parameter type safe.
	/// These typedefs should be defined:
	///  - concrete_type: concrete state type, usually derived from state<Type>.
	///  - event_type: event sent to states.
	///  - transition_type: transition between two states.
	///  - entity_type: state owner type.
	///
	template <typename StateWrapper>
	class state
	{
	public:
		typedef StateWrapper wrapper_type;
		typedef state_machine<wrapper_type> machine_type;
		typedef state<wrapper_type> self_type;
		typedef typename wrapper_type::concrete_type concrete_type;
		typedef typename wrapper_type::event_type event_type;
		typedef typename wrapper_type::transition_type transition_type;
		typedef typename wrapper_type::entity_type entity_type;
	public:
		state( machine_type *machine ) : _machine( machine )
		{
		}

		virtual ~state() { }

		/// Receive an event to transform to other states.
		virtual void receive_event( entity_type*, const event_type &ev ) { }

		/// Execute when enter this state.
		virtual void enter( entity_type*, const transition_type& ) { }

		/// Execute when leave this state.
		virtual void leave( entity_type*, const transition_type& ) { }

		/// Execute when in this state.
		virtual void execute( entity_type* ) = 0;

		/// Compare two states. It's not type safe actually.
		virtual bool operator == ( const concrete_type &other ) const = 0;

		bool operator != ( const concrete_type &other ) const
		{
			return !( *this == other );
		}

		/// Query the machine.
		machine_type *get_machine() const
		{
			return _machine;
		}
	protected:
		machine_type *_machine;
	};

	///
	/// Usage: state_machine<Player> m_smachine;
	///
	/// @see state
	///
	template <typename StateWrapper>
	class state_machine
	{
	public:
		typedef StateWrapper wrapper_type;
		typedef state<wrapper_type> state_type;
		typedef typename wrapper_type::concrete_type concrete_type;
		typedef typename wrapper_type::event_type event_type;
		typedef typename wrapper_type::transition_type transition_type;
		typedef typename wrapper_type::entity_type entity_type;
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
			_pre_state = 0;
		}

		/// Return true if the machine has a state.
		bool has_state() const
		{
			return _cur_state != 0;
		}

		/// Get the current state.
		concrete_type *cur_state()
		{
			return _cur_state;
		}

		/// Check whether is in the specified state.
		bool is_state( const concrete_type &other ) const
		{
			if( !has_state() )
			{
				return false;
			}
			return other == *_cur_state;
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
		void change( concrete_type *other, const transition_type &tran )
		{
			if( has_state() )
			{
				_cur_state->leave( _owner, tran );
			}
			other->enter( _owner, tran );
			_pre_state = _cur_state;
			_cur_state = other;
		}

		/// Revert the previous state.
		void revert( const transition_type &tran )
		{
			if( _pre_state != 0 )
			{
				change( _pre_state, tran );
			}
		}

		/// Get the entity object.
		entity_type *get_entity() const
		{
			return _owner;
		}
	private:
		concrete_type *_cur_state;
		concrete_type *_pre_state;
		entity_type *_owner;
	};
}

#endif // __KL_STATE_H_
