
class HangupState : public State<AIController>
{
};

class FightStateA : public State<AIController>
{
public:
	void execute( AIController *ai );
};

class FightStateB : public State<AIController>
{
	///...
};

/// more...

class PeaceStateA : public State<AIController>
{
	///...
};

class PeaceStateA : public State<AIController>
{
	///...
};

/// more...

class AIController
{
public:
	struct AIConfig
	{
		int HangupType;
		int FightType;
		int PeaceType;
		/// more state type
	};
public:
	/// initialize state policies.
	bool Init( const AIConfig &cfg )
	{
		m_pHangupState = CreateHangupState( cfg.HangupType );
		m_pFightState = CreateFightState( cfg.FightType );
		m_pPeaceState = CreateFightState( cfg.PeaceType );
		// more

		m_pStateMachine = new StateMachine<AIController>( this );
		// initial state
		m_pStateMachine->change( m_pHangupState );
		return true;
	}

	/// timer process entry.
	void Run()
	{
		m_pStateMachine->execute();
	}

	/// some common functions used by state class.
	/// ...

	/// used to change state
	/// void SomeState::execute( AIController *ai )
	/// {  
	///     ai->GetStateMachine()->change( ai->GetState( STATE_FIGHT ) );
	/// }
	State<AIController> *GetState( int type );

private:
	StateMachine<AIController> *m_pStateMachine;
	State<AIController> *m_pHangupState;
	State<AIController> *m_pFightState;
	State<AIController> *m_pPeaceState;
};

/*
 <!-- AI config -->
 <MonsterAICfg>
 	<!--A type of AI is combined by some states-->
 	<TypeConfig>
		<AI type="1">
			<Fight type="1"/>
			<Peace type="1/>
		</AI>
		<AI type="2">
			<Fight type="2"/>
			<Peace type="2"/>
		</AI>
	</TypeConfig>
	<!--Some special monster may have special AI-->
	<MonsterConfig>
		<Monster name="001FlossRabit" template="1">
			<Fight type="2"/>	
		</Monster>	
	</MonsterConfig>
 </MonsterAICfg>
*/
