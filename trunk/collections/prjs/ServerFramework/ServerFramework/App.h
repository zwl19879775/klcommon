/**
 * @file App.h
 * @author Kevin Lynx
 * @date 3.5.2008
 *
 */
#ifndef __APP_H
#define __APP_H

#include "../klwin/klwin_window.h"
#include "../klwin/klwin_listbox.h"
#include "../klwin/klwin_editbox.h"
#include "my_thread_wrapper.h"
#include "kl_thread.h"

/**
 * 应用程序类，主要负责处理界面交互
 *
 */
class App : public klwin::Window 
{
public:
	/// constructor
	App();
	
	/// destructor
	~App();

	/// init
	bool init();

	/// release
	void release();

	/// main loop
	void mainLoop();

	/// add log text into the edit box
	void addLogText( const char *format, ... );

	/// add client item
	void addClientItem( const char *ip_str, long id );

	/// del client item
	void delClientItem( const char *ip_str, long id );

	/// clear clients list
	void clearClientList();

	/// resize, when WM_RESIZE message comes, it will be called automatically.
	void onResize( int width, int height );

protected:
	/// message process function
	LRESULT onMessage( UINT uMsg, WPARAM wParam, LPARAM lParam );
	/// deal with menu commands
	void onMenuCommand( WORD id );
	/// quit
	void onQuit();
private:
	/// list box to display connected clients.
	klwin::ListBox mClientList;
	/// edit box to display log information
	klwin::EditBox mLogBox;

	/// game main thread handle
	kl_common::thread mGameThread;
};

/// the global App object
extern App gApp;

#endif
