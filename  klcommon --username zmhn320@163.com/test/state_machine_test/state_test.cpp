///
/// state machine test sample.
///
#include <stdio.h>
#include "state_wrapper.h"
#include <assert.h>


enum StateType
{
	ST_PEACE = 0,
	ST_FIGHT,
	ST_CYCLE,
	ST_HANGUP,
	ST_INVALID
};

enum EventType
{
	ET_HURT = 0,
	ET_WAKEUP,
	ET_INVALID
};


struct HurtEvent : public AIEvent
{
	HurtEvent( long hurt, long targetId ) : AIEvent( ET_HURT )
	{
		this->hurt = hurt;
		this->targetId = targetId;
	}

	long hurt;
	long targetId;
};

struct WakeupEvent : public AIEvent
{
	WakeupEvent( long targetId ) : AIEvent( ET_WAKEUP )
	{
		this->targetId = targetId;
	}

	long targetId;
};

/// Base ai state, to add a type identifier mechanism.
class AIState : public kl_common::state<Wrapper>
{
public:
	typedef kl_common::state<Wrapper> base_type;
public:
	AIState( MachineType *machine, long type ) : m_Type( type ), base_type( machine )
	{
	}

	bool operator== ( const base_type::concrete_type &other ) const
	{
		return other.Type() == Type(); 
	}

	long Type() const
	{
		return m_Type;
	}
private:
	long m_Type;
};

/// Concrete state
class FightState : public AIState
{
public:
	FightState( MachineType *machine ) : AIState( machine, ST_FIGHT )
	{
	}

	void enter( base_type::entity_type *entity, const base_type::transition_type &tran );

	void receive_event( base_type::entity_type *entity, const base_type::event_type &ev );

	void execute( base_type::entity_type *entity )
	{
	}
};

class PeaceState : public AIState
{
public:
	PeaceState( MachineType *machine ) : AIState( machine, ST_PEACE )
	{
	}

	void receive_event( base_type::entity_type *entity, const base_type::event_type &ev );

	void leave( base_type::entity_type *entity, const base_type::transition_type &tran );

	void execute( base_type::entity_type *entity );
};

class HangupState : public AIState
{
public:
	HangupState( MachineType *machine ) : AIState( machine, ST_HANGUP )
	{
	}

	void receive_event( base_type::entity_type *entity, const base_type::event_type &ev );
	
	void execute( base_type::entity_type *entity ) { }
};

/// Construct a transition.
#define CT_TRAN( from, to ) kl_common::simple_transition<AIState>( from, to )

class MonsterAI
{
public:
	~MonsterAI()
	{
		for( int i = 0; i < ST_INVALID; ++ i )
		{
			delete m_States[i];
		}
		delete m_StateMachine;
	}

	bool Init()
	{
		m_StateMachine = new kl_common::state_machine<Wrapper>( this );

		m_States[ST_PEACE] = new PeaceState( m_StateMachine );
		m_States[ST_FIGHT] = new FightState( m_StateMachine );
		m_States[ST_HANGUP] = new HangupState( m_StateMachine );

		m_StateMachine->change( m_States[ST_PEACE], CT_TRAN( NULL, m_States[ST_PEACE] ) );
		return true;
	}

	void ChangeState( long state )
	{
		ChangeState( state, CT_TRAN( m_StateMachine->cur_state(), m_States[state] ) );
	}

	void ChangeState( long state, const Wrapper::transition_type &tran )
	{
		assert( state >= 0 && state < ST_INVALID );
		m_StateMachine->change( m_States[state], tran );
	}

	void OnHurted( long hurt, long targetId )
	{
		// can cache the target data for SearchEnemy.
		// send event to the focused state.
		m_StateMachine->cur_state()->receive_event( this, HurtEvent( hurt, targetId ) );
	}

	void OnWakeup( long targetId )
	{
		m_StateMachine->cur_state()->receive_event( this, WakeupEvent( targetId ) );
	}

	AIState *GetState( long state )
	{
		assert( state >= 0 && state < ST_INVALID );
		return m_States[state];
	}

private:
	AIState *m_States[ST_INVALID];
	kl_common::state_machine<Wrapper> *m_StateMachine;
};

void PeaceState::receive_event( base_type::entity_type *entity, const base_type::event_type &ev )
{
	if( ev.Type() == ET_HURT )
	{
		entity->ChangeState( ST_FIGHT );
		// or:
		// get_machine()->change( entity->GetState( ST_FIGHT ), some_transition );
	}
}

void PeaceState::leave( base_type::entity_type *entity, const base_type::transition_type &tran )
{
	if( tran._to == entity->GetState( ST_FIGHT ) )
	{
		// so it knows the transition will transform to FightState
	}
}

void PeaceState::execute( base_type::entity_type *entity )
{
	// check some conditions, like the enemy count around myself.
	// if no more enemies there, hangup the ai.
	entity->ChangeState( ST_HANGUP );
}

void FightState::enter( base_type::entity_type *entity, const base_type::transition_type &tran )
{
	if( tran._from == entity->GetState( ST_PEACE ) )
	{
		// so it knows the transition came from PeaceState
	}
}

void FightState::receive_event( base_type::entity_type *entity, const base_type::event_type &ev )
{
	if( ev.Type() == ET_HURT )
	{
		// ignore
	}
}

void HangupState::receive_event( base_type::entity_type *entity, const base_type::event_type &ev )
{
	if( ev.Type() == ET_WAKEUP )
	{
		entity->ChangeState( ST_PEACE );
	}
}

int main()
{
	MonsterAI ai;
	ai.Init();
	
	// outer event
	ai.OnHurted( 10, 1011 );

	// outer event, wakeup event
	ai.OnWakeup( 1012 );

	return 0;
}
