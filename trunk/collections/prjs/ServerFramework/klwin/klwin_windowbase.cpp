/**
 * @file klwin_windowbase.cpp
 * @author Kevin Lynx
 *
 */
#include <assert.h>
#include <algorithm>
#include "klwin_windowbase.h"

namespace klwin
{
	/// used by 'std::find_if' to find a Child
	class _Predicate
	{
	public:
		_Predicate( HWND handle ) : _handle( handle ) {}

		bool operator() ( const WindowBase *child )
		{
			return child->getHandle() == _handle;
		}

	private:
		HWND _handle;
	};

	WindowBase::WindowBase()
	{
		mHandle = 0;
		mX = mY = 0;
		mWidth = mHeight = 0;
		mVisible = false;
		mParent = NULL;
		mAppInstance = ::GetModuleHandle( 0 );
	}

	void WindowBase::destroy()
	{
		if( mHandle == NULL )
		{
			return ;
		}

		if( mParent != NULL )
		{
			mParent->removeChild( this );
			mParent = NULL;
		}
	
		::DestroyWindow( mHandle );
		mHandle = NULL;		
	}

	bool WindowBase::resize( int width, int height )
	{
		assert( mHandle != NULL && "WindowBase::resize : invalid window's handle" );
		mWidth = width;
		mHeight = height;
		return ::MoveWindow( mHandle, mX, mY, mWidth, mHeight, TRUE ) != 0 ? true : false;
	}

	bool WindowBase::move( int x, int y )
	{
		assert( mHandle != NULL && "WindowBase::move : invalid window's handle." );
		mX = x;
		mY = y;
		return ::MoveWindow( mHandle, mX, mY, mWidth, mHeight, TRUE ) != 0 ? true : false;
	}

	void WindowBase::show()
	{
		assert( mHandle != NULL && "WindowBase::show : invalid window's handle." );
		::ShowWindow( mHandle, SW_SHOW );
		mVisible = true;
		/// recurve to show its children
		for( ChildList::iterator it = mChildList.begin(); it != mChildList.end(); ++ it )
		{
			(*it)->show();
		}
	}

	void WindowBase::hide()
	{
		assert( mHandle != NULL && "WindowBase::hide : invalid window's handle." );
		::ShowWindow( mHandle, SW_HIDE );		
		mVisible = false;
		/// recurve to hide its children
		for( ChildList::iterator it = mChildList.begin(); it != mChildList.end(); ++ it )
		{
			(*it)->hide();
		}
	}

	void WindowBase::addChild( WindowBase *child )
	{
		assert( child != NULL && "WindowBase::addChild : invalid parameter." );

		ChildList::iterator it = std::find( mChildList.begin(), mChildList.end(), child );

		if( it == mChildList.end() )
		{
			mChildList.push_back( child );
		}
	}

	void WindowBase::removeChild( WindowBase *child )
	{
		ChildList::iterator it = std::find( mChildList.begin(), mChildList.end(), child );

		if( it != mChildList.end() )
		{
			mChildList.erase( it );
		}
	}

	WindowBase *WindowBase::getChildFromHandle( HWND handle )
	{
		ChildList::iterator it = std::find_if( mChildList.begin(), mChildList.end(), _Predicate( handle ) );

		return it != mChildList.end() ? *it : NULL;
	}

	void WindowBase::setWindowText( const char *text )
	{
		assert( mHandle != NULL && "WindowBase::setWindowText : invalid window handle." );
		::SetWindowText( mHandle, text );
	}

	int WindowBase::getWindowText( char *buf, int size )
	{
		assert( mHandle != NULL && "WindowBase::getWindowText : invalid window handle." );
		return ::GetWindowText( mHandle, buf, size );
	}

	//////////////////////////////////////////////////////////////////////////////////////////////
	bool WindowBase::_create( const char *wc_name, int x, int y, int width, int height, 
		WindowBase *parent, DWORD style )
	{
		mX = x;
		mY = y;
		mWidth = width;
		mHeight = height;
		mParent = parent;

		if( parent != NULL )
		{
			style |= WS_CHILD;
		}

		mHandle = ::CreateWindowEx( 0, wc_name, 0, style, mX, mY, mWidth, mHeight, 
			parent->getHandle(), 0, mAppInstance, 0 );

		if( mHandle == NULL )
		{
			return false;
		}

		if( parent != NULL )
		{
			parent->addChild( this );
		}
		return true;
	}
}
