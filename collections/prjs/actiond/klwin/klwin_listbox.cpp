/**
 * @file klwin_listbox.cpp
 * @author Kevin Lynx
 * @created 2.21.2008
 *
 */
#include <assert.h>
#include "klwin_listbox.h"

namespace klwin
{
	ListBox::ListBox() : WindowBase()
	{
	}

	ListBox::~ListBox()
	{
		destroy();
	}

	bool ListBox::create( int x, int y, int width, int height, WindowBase *parent, DWORD style )
	{
		return _create( "LISTBOX", x, y, width, height, parent, style );
	}

	void ListBox::destroy()
	{
		WindowBase::destroy();
	}

	int ListBox::getCount()
	{
		assert( mHandle != NULL && "ListBox::getCount : invalid window's handle." );
		return static_cast<int>( ::SendMessage( mHandle, LB_GETCOUNT, 0, 0 ) );
	}

	int ListBox::getCurSel()
	{
		assert( mHandle != NULL && "ListBox::getCurSel : invalid window's handle." );
		return static_cast<int>( ::SendMessage( mHandle, LB_GETCURSEL, 0, 0 ) );
	}

	void ListBox::setCurSel( int sel )
	{
		assert( mHandle != NULL && "ListBox::setCurSel : invalid window's handle." );
		::SendMessage( mHandle, LB_SETCURSEL, (WPARAM) sel, 0 );
	}

	int ListBox::findString( const char *str )
	{
		assert( mHandle != NULL && "ListBox::findString : invalid window's handle." );
		int index = static_cast<int>( ::SendMessage( mHandle, LB_FINDSTRING, 0, (LPARAM) str ) );
		return index;
	}

	int ListBox::addString( const char *str )
	{
		assert( mHandle != NULL && "ListBox::addString : invalid window's handle." );
		int index = static_cast<int>( ::SendMessage( mHandle, LB_ADDSTRING, 0, (LPARAM) str ) );
		return index;
	}

	void ListBox::delString( int index )
	{
		assert( mHandle != NULL && "ListBox::delString : invalid window's handle." );
		::SendMessage( mHandle, LB_DELETESTRING, (WPARAM) index, 0 );
	}

	void ListBox::delString( const char *str )
	{
		assert( mHandle != NULL && "ListBox::delString : invalid window's handle." );
		int index = findString( str );
		if( index != -1 )
		{
			delString( index );
		}
	}

	void ListBox::clear()
	{
		assert( mHandle != NULL && "ListBox::clear : invalid window's handle." );
		::SendMessage( mHandle, LB_RESETCONTENT, 0, 0 );
	}
}