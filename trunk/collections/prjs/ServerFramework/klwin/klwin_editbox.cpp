/**
 * @file klwin_editbox.cpp
 * @author Kevin Lynx
 * @created 2.21.2008
 *
 */
#include <assert.h>
#include "klwin_editbox.h"

#ifdef _MSC_VER
#if _MSC_VER >= 1400
#pragma warning( push )
#pragma warning( disable : 4996 )
#endif
#endif

namespace klwin
{
	EditBox::EditBox() : WindowBase()
	{
	}

	EditBox::~EditBox()
	{
		destroy();
	}

	bool EditBox::create( int x, int y, int width, int height, WindowBase *parent, DWORD style )
	{
		return _create( "EDIT", x, y, width, height, parent, style );
	}

	void EditBox::destry()
	{
		WindowBase::destroy();
	}

	void EditBox::setReadOnly( bool read_only )
	{
		assert( mHandle != NULL && "EditBox::setReadOnly : invalid window handle." );
/*		/// why does not these codes work ?
		long style = ::GetWindowLong( mHandle, GWL_STYLE );
		if( read_only )
		{
			style |= ES_READONLY;
		}
		else
		{
			style &= (~ES_READONLY );
		}

		::SetWindowLong( mHandle, GWL_STYLE, style );*/
		::SendMessage( mHandle, EM_SETREADONLY, (WPARAM) read_only ? 1 : 0, 0 );
	}

	bool EditBox::isReadOnly()
	{
		assert( mHandle != NULL && "EditBox::isReadOnly : invalid window handle." );
		long style = ::GetWindowLong( mHandle, GWL_STYLE );

		return ( style & ES_READONLY ) != 0 ? true : false; 
	}

	void EditBox::setLimitText( unsigned int max )
	{
		assert( mHandle != NULL && "EditBox::setLimiteText : invalid window handle." );
		::SendMessage( mHandle, EM_SETLIMITTEXT, (WPARAM) max, 0 );
	}

	unsigned int EditBox::getLimitText()
	{
		assert( mHandle != NULL && "EditBox::getLimiteText : invalid window handle." );
		return (unsigned int)::SendMessage( mHandle, EM_GETLIMITTEXT, 0, 0 );
	}

	void EditBox::addText( const char *str, bool auto_scroll )
	{
		assert( mHandle != NULL && "EditBox::addText : invalid window handle." );

		unsigned int max = getLimitText();
		char *text = new char [max + 1];
		
		getWindowText( text, max );
		//assert( strlen( text ) + strlen( str ) < max + 1 );
		if( strlen( text ) + strlen( str ) >= max + 1 )
		{
			// clear
			text[0] = '\0';
		}

		strcat( text, str );

		setWindowText( text );

		delete [] text;

		if( auto_scroll )
		{
			::SendMessage( mHandle, WM_VSCROLL, SB_BOTTOM, 0 );
		}
	}
}

#ifdef _MSC_VER
#if _MSC_VER >= 1400
#pragma warning( pop )
#endif
#endif

