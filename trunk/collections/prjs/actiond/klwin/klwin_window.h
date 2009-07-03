/**
 * @file kl_window.h
 * @author Kevin Lynx
 * @created 2.21.2008
 *
 */
#ifndef __KLWIN_WINDOW_H_
#define __KLWIN_WINDOW_H_

#include <string>
#include "klwin_windowbase.h"

namespace klwin
{
	/**
	 * a tiny class to wrap window creating details.
	 * It's the main class(top-most window) to manage all the child-controls, also manage the 
	 * application messages. Maybe the better desgin is to create a 'App' class to manage the messages.
	 * 
	 *
	 */
	class Window : public WindowBase
	{
	public:
		/// Constructor
		Window();
		
		/// Destructor
		virtual ~Window();

		/// create
		bool create( const std::string &title, int width, int height, DWORD style = WS_OVERLAPPEDWINDOW );

		/// destroy, called by destructor
		void destroy();

		/// close, whenever you want to close the window
		void close();

		/// reize, the parameter is the client area size
		/// when you set some properties for the Window, the operation may change the client
		/// size of the window. So i suggest you called 'resize' after these operation.
		bool resize( int width, int height );

		/// set resize-able
		/// The client area 's size maybe change.
		void setResizeable( bool able );

		/// get resize-able flag
		bool isResizeable() ;

		/// set background color
		void setBkColor( COLORREF color, bool erase = true );

		/// set menu
		/// The client area's size maybe change.
		bool setMenu( const char *menuResName );

		/// set icon, currently i donot know why 'const string&' will occur a bug.
		/// call this function before 'create'
		bool setIcon( const char *iconResName );

		/// set WNDCLASS if you want 'create' to use it.
		/// you must call this function before 'create' .
		void setCustomClass( const WNDCLASS &wndClass );

		/// checkMessage
		/// @param wait if true, the function will block until messages come
		/// @return return true if get WM_QUIT message
		bool checkMessage( bool wait = true );

	protected:
		/// message callback function, you can overwrite this function to handle messages yourself
		virtual LRESULT onMessage( UINT uMsg, WPARAM wParam, LPARAM lParam ) { return 0; }
		/// a tiny function to handle default message : WM_DESTORY ,PostQuitMessage
		LRESULT handleDefaultMessage( UINT uMsg, WPARAM wParam, LPARAM lParam );
		/// handle messages.
		LRESULT handleMessage( UINT uMsg, WPARAM wParam, LPARAM lParam );
		///
		static LRESULT __stdcall WndProc( HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam );

	protected:
		/// flag whether WNDCLASS is set by you
		bool mCustomWndClass;
		/// wndclass
		WNDCLASS mWndClass;
	};
}

#endif
