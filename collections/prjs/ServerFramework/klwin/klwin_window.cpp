/**
 * @file kl_window.cpp
 * @author Kevin Lynx
 *
 */
#include <assert.h>
#include "klwin_window.h"

#ifdef _MSC_VER
/// you know what, i really hate these fucking warings from the 
/// compiler. so i disable them
#pragma warning( push )
#pragma warning( disable : 4311 )
#pragma warning( disable : 4312 )
#endif

namespace klwin
{
	#define WINDOW_CLASS_NAME "KL_WINDOW_CLASS" 

	/// window process
	LRESULT __stdcall Window::WndProc( HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam )
	{
		LONG_PTR lptr = ::GetWindowLongPtr( hWnd, GWLP_USERDATA );

		if( lptr != NULL )
		{
			Window *window = reinterpret_cast<Window*>( lptr );
			return window->handleMessage( message, wParam, lParam );
		}
		return ::DefWindowProc( hWnd, message, wParam, lParam );
	}

	Window::Window() : WindowBase()
	{
		mCustomWndClass = false;
		mAppInstance = 0;
	}

	Window::~Window()
	{
		destroy();
	}

	bool Window::create( const std::string &title, int width, int height, DWORD style )
	{
		mWidth = width;
		mHeight = height;

		mAppInstance = GetModuleHandle( 0 );

		/// ready to create
		if( !mCustomWndClass )
		{
			mWndClass.style			= CS_HREDRAW | CS_VREDRAW;
			mWndClass.lpfnWndProc	= WndProc;
			mWndClass.cbClsExtra	= 0;
			mWndClass.cbWndExtra	= 0;	
			mWndClass.hInstance		= mAppInstance;
			mWndClass.hCursor		= LoadCursor( NULL, IDC_ARROW );
			mWndClass.hbrBackground	= (HBRUSH)GetStockObject( BLACK_BRUSH );
			mWndClass.lpszMenuName	= NULL;		
			mWndClass.lpszClassName	= WINDOW_CLASS_NAME;
			mWndClass.hIcon			= LoadIcon( mAppInstance, IDI_APPLICATION );
		}
		else
		{
			/// the mWndClass is set by you
		}

		if( !RegisterClass( &mWndClass ) )
		{
			return false;
		}

		///
		RECT rect;
		width = mWidth + GetSystemMetrics( SM_CXSIZEFRAME ) * 2;
		height = mHeight + GetSystemMetrics( SM_CYSIZEFRAME ) * 2 + GetSystemMetrics( SM_CYCAPTION );
		
		rect.left  = ( GetSystemMetrics( SM_CXSCREEN ) - width ) / 2;
		rect.top   = ( GetSystemMetrics( SM_CYSCREEN ) - height ) / 2;
		rect.right =  rect.left + width;
		rect.bottom=  rect.top + height;

		mX = rect.left;
		mY = rect.top;

		mHandle = CreateWindowEx( 0, WINDOW_CLASS_NAME, title.c_str(), style, rect.left, rect.top, rect.right - rect.left, rect.bottom - rect.top, 
			NULL, NULL, mAppInstance, NULL );

		::SetWindowLongPtr( mHandle, GWLP_USERDATA, static_cast<LONG>( reinterpret_cast<LONG_PTR>( this ) ) );

		return true;
	}

	void Window::destroy()
	{
		UnregisterClass( WINDOW_CLASS_NAME, mAppInstance );
	}

	void Window::close()
	{
		::SendMessage( mHandle, WM_CLOSE, 0, 0 );
	}

	bool Window::resize( int width, int height )
	{
		mWidth = width;
		mHeight = height;

		/// make sure the client area is 'mWidth' * 'mHeight'
		bool resizeable = isResizeable();
		if( resizeable )
		{
			width = mWidth + 2 * GetSystemMetrics( SM_CXSIZEFRAME );
			height = mHeight + 2 * GetSystemMetrics( SM_CYSIZEFRAME );
		}
		else
		{
			width = mWidth + 2 * GetSystemMetrics( SM_CXFIXEDFRAME );
			height = mHeight + 2 * GetSystemMetrics( SM_CYFIXEDFRAME );			
		}

		height += GetSystemMetrics( SM_CYCAPTION );

		/// if the window has menu bar
		HMENU menu = ::GetMenu( mHandle );
		if( menu != NULL )
		{
			height += GetSystemMetrics( SM_CYMENU );
		}

		return ::MoveWindow( mHandle, mX, mY, width, height, TRUE ) != 0 ? true : false;
	}

	void Window::setResizeable( bool able )
	{
		assert( "Window::setResizeable : invalid window's handle." );
		long style = ::GetWindowLong( mHandle, GWL_STYLE );
		if( able )
		{
			style |= WS_SIZEBOX;
		}
		else
		{
			style &= ( ~WS_SIZEBOX );
			style &= ( ~WS_THICKFRAME );
		}
		::SetWindowLong( mHandle, GWL_STYLE, style );
		/// update the window, because ceratain window data is cached
		::SetWindowPos( mHandle, 0, 0, 0, 0, 0, SWP_NOZORDER | SWP_NOSIZE | SWP_NOMOVE | SWP_FRAMECHANGED );
	}

	bool Window::isResizeable()
	{
		assert( "Window::isResizeable : invalid window's handle." );

		long style = ::GetWindowLong( mHandle, GWL_STYLE );
		return style & WS_SIZEBOX || style & WS_THICKFRAME ;
	}

	void Window::setBkColor( COLORREF color, bool erase )
	{
		assert( "Window::setBkColor : invalid window's handle." );
		HBRUSH newBrush = CreateSolidBrush( color );
		HBRUSH oldBrush = (HBRUSH) SetClassLong( mHandle, GCL_HBRBACKGROUND, (LONG)newBrush );

		DeleteObject( oldBrush );

		InvalidateRect( mHandle, 0, erase );
	}

	bool Window::setIcon( const char *iconResName )
	{
		assert( "Window::setIcon : invalid window's handle." );
		HICON icon = LoadIcon( mAppInstance, iconResName );
		if( icon == NULL )
		{
			return false;
		}

		SetClassLong( mHandle, GCL_HICON, reinterpret_cast<LONG>( icon ) );
		SendMessage( mHandle, WM_SETICON, ICON_SMALL, (LPARAM) icon );

		return true;
	}

	bool Window::setMenu( const char *menuResName )
	{	
		assert( "Window::setMenu : invalid window's handle." );
		HMENU newMenu = ::LoadMenu( mAppInstance, menuResName );
		if( newMenu == NULL )
		{
			return false;
		}

		HMENU oldMenu = ::GetMenu( mHandle );
		::SetMenu( mHandle, newMenu );

		if( oldMenu != NULL )
		{
			::DestroyMenu( oldMenu );
		}
		
		return true;
	}

	void Window::setCustomClass( const WNDCLASS &wndclass )
	{
		mCustomWndClass = true;
		mWndClass = wndclass;
	}

	bool Window::checkMessage( bool wait )
	{
		assert( "Window::checkMessage : invalid window's handle." );
		if( wait )
		{
			::WaitMessage();
		}

		MSG  msg;
		memset( &msg, 0, sizeof( MSG ) );
/*		while( PeekMessage( &msg, NULL, 0, 0, PM_REMOVE ) )
		{
			TranslateMessage( &msg );
			DispatchMessage( &msg );
		}

		return msg.message == WM_QUIT;
//*/
		/// TODO : to test these codes below, maybe the codes above will occur some 
		/// bugs sometime.
		if( PeekMessage( &msg, NULL, 0, 0, PM_NOREMOVE ) )
		{
			if( !GetMessage( &msg, NULL, 0, 0 ) )
			{
				return true;
			}
			TranslateMessage( &msg );
			DispatchMessage( &msg );
		}

		return false;
//*/	
	}

	LRESULT Window::handleMessage( UINT message, WPARAM wParam, LPARAM lParam )
	{
		if( message == WM_SIZE )
		{
			mWidth = LOWORD( lParam );
			mHeight = HIWORD( lParam );
		}
		else if( message == WM_MOVE )
		{
			/// update position
			RECT rect;
			GetWindowRect( mHandle, &rect );
			mX = rect.left;
			mY = rect.top;
		}

		return onMessage( message, wParam, lParam );
	}

	LRESULT Window::handleDefaultMessage( UINT message, WPARAM wParam, LPARAM lParam )
	{
		if( message == WM_DESTROY )
		{
			PostQuitMessage( 0 );
		}
		else
		{
			return ::DefWindowProc( mHandle, message, wParam, lParam );
		}		

		return 0;
	}
}

#ifdef _MSC_VER
#pragma warning( pop )
#endif

