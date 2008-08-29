///
/// @file CGame.h
/// @brief the main class ( also a huge class ) to manage most logic.
///
#ifndef ___CGAME_H_
#define ___CGAME_H_

#include "ConfigReader.h"

class CGame
{
public:
	/// ctor
	CGame();

	/// dtor
	~CGame();

	/// init
	bool init();

	/// reload_setup
	bool reload_setup();

	/// release
	void release();

	/// process message
	void process_message();

	/// get configer
	const ConfigReader &GetConfiger() { return mConfiger; }

private:
	/// config
	ConfigReader mConfiger;
};

CGame *GetGame();

void AddLogText( const char *fmt, ... );

void GameMainThread( void *p );

#endif // end ___CGAME_H_