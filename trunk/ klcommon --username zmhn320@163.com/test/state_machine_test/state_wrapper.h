///
/// pre-declaration for kl_common::state.
///
#ifndef ___STATE_WRAPPER_H_
#define ___STATE_WRAPPER_H_

#include "kl_state.h"

/// entity type
class MonsterAI;

/// event type
class AIEvent
{
public:
	AIEvent( long type ) : m_Type( type )
	{
	}

	long Type() const
	{
		return m_Type;
	}
private:
	long m_Type;
};

/// use kl_common::simple_transition as the transition type.

/// state concrete type
class AIState ;

struct Wrapper
{
	typedef MonsterAI entity_type;
	typedef AIEvent event_type;
	typedef AIState concrete_type;
	typedef kl_common::simple_transition<concrete_type> transition_type;
};

typedef kl_common::state_machine<Wrapper> MachineType;

#endif