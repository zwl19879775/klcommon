/**
 * @file klwin_button.cpp
 * @author Kevin Lynx
 * @created 2.22.2008
 * 
 */
#include <assert.h>
#include "klwin_button.h"

namespace klwin
{
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	//
	//	ButtonBase class
	//
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	ButtonBase::ButtonBase() : WindowBase()
	{
		mType = BT_INVALIDTYPE;
	}

	ButtonBase::~ButtonBase()
	{
		destroy();
	}

	bool ButtonBase::create( int x, int  y, int width, int height, WindowBase *parent, DWORD style )
	{
		return WindowBase::_create( "BUTTON", x, y, width, height, parent, style );
	}

	void ButtonBase::destroy()
	{
		WindowBase::destroy();
	}

	bool ButtonBase::isChecked()
	{
		assert( "ButtonBase::isChecked : invalid window's handle." );
		LRESULT state = ::SendMessage( mHandle, BM_GETCHECK, 0, 0 );

		return ( state & BST_CHECKED ) != 0 ? true : false;
	}

	void ButtonBase::setChecked( bool checked )
	{
		assert( "ButtonBase::setChecked : invalid window's handle." );
		::SendMessage( mHandle, BM_SETCHECK, (WPARAM) ( checked ? 1 : 0 ), 0 );
	}

	//////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	//
	//	PushButton class
	//
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	bool PushButton::create( int x, int y, int width, int height, WindowBase *parent, DWORD style )
	{
		if( !( style == BS_PUSHBUTTON || style & BS_DEFPUSHBUTTON ) ) /// BS_PUSHBUTTON == 0
		{
			assert( "PushButton::create : You must specify the button as a 'PushButton'. " && 0 );
			return false;
		}
		mType = BT_PUSHBUTTON;
	
		return ButtonBase::create( x, y, width, height, parent, style );
	}

	//////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	//
	//	RadioButton class
	//
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	bool RadioButton::create( int x, int y, int width, int height, WindowBase *parent, DWORD style )
	{
		if( !( style & BS_RADIOBUTTON || style & BS_AUTORADIOBUTTON ) )
		{
			assert( "RadioButton::create : You must specify the button as a 'RadioButton'. " && 0 );
			return false;
		}
		mType = BT_RADIOBUTTON;

		return ButtonBase::create( x, y, width, height, parent, style );
	}

	//////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	//
	//	CheckBox class
	//
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	bool CheckBox::create( int x, int y, int width, int height, WindowBase *parent, DWORD style )
	{
		if( !( style & BS_CHECKBOX || style && BS_AUTOCHECKBOX ) )
		{
			assert( "CheckBox::create : You must specify the button as a 'CheckBox'. " && 0 );
			return false;
		}
		mType = BT_CHECKBOX;

		return ButtonBase::create( x, y, width, height, parent, style );
	}

	//////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	//
	//	GroupBox class
	//
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	bool GroupBox::create( int x, int y, int width, int height, WindowBase *parent, DWORD style )
	{
		if( !( style & BS_GROUPBOX ) )
		{
			assert( "GroupBox::create : You must specify the button as a 'GroupBox'." && 0 );
			return false;
		}
		mType = BT_GROUPBOX;
		
		return ButtonBase::create( x, y, width, height, parent, style );
	}
}
