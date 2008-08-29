/**
 * @file klwin_button.h
 * @author Kevin Lynx
 * @created 2.22.2008
 *
 */
#ifndef __KLWIN_BUTTON_H
#define __KLWIN_BUTTON_H

#include "klwin_windowbase.h"

namespace klwin
{
	/**
	 * button base class. You'd better donot use this class directly. Instead i suggest you use
	 * 'PushButton', 'CheckBox', 'RadionButton' etc.
	 *
	 * Messages :
	 * BM_CLICKED : PushButton, CheckBox, RadioButton
	 *
	 */
	class ButtonBase : public WindowBase
	{
	public:
		/// button type
		enum ButtonType
		{
			BT_INVALIDTYPE = -1,
			BT_PUSHBUTTON,
			BT_RADIOBUTTON,
			BT_CHECKBOX,
			BT_GROUPBOX,
			BT_BUTTONTYPE_MAX
		};

	public:
		/// Constructor
		ButtonBase();

		/// Destructor
		~ButtonBase();

		/// create the button
		bool create( int x, int y,  int width, int height, WindowBase *parent, DWORD style ) ;

		/// destroy the button, you can call this function any time if you want to destroy the button
		void destroy();
		
		/// get the button type
		int getType() { return mType; }

	protected:
		/// these functions are only used in 'CheckBox' and 'RadioButton', so keep them protected
		/// and in 'CheckBox' and 'RadioButton' make them public. A good trick, isnot it ? 
		/// get the checked state for a 'RadioButton' or a 'CheckBox'
		bool isChecked();
		/// set the checked state for a 'RadioButton' or a 'CheckBox'
		void setChecked( bool checked );

	protected:
		/// button type
		int mType;
	};

	/**
	 * PushButton class, implements a 'PushButton'.
	 *
	 */
	class PushButton : public ButtonBase
	{
	public:
		/// create the push button
		bool create( int x, int y, int width, int height, WindowBase *parent, DWORD style = BS_PUSHBUTTON );
	};

	/**
	 * RadioButton class, implements a 'RadionButton'.
	 *
	 */
	class RadioButton : public ButtonBase
	{
	public:
		/// create the radio button
		bool create( int x, int y, int width, int height, WindowBase *parent, DWORD style =	BS_AUTORADIOBUTTON );

		/// retrive the check state of the RadioButton
		bool isChecked() { return ButtonBase::isChecked(); }

		/// set the check state of the RadioButton
		void setChecked( bool checked ) { ButtonBase::setChecked( checked ) ; }
	};

	/** 
	 * CheckBox class, implements a 'CheckBox' button.
	 *
	 */
	class CheckBox : public ButtonBase
	{
	public:
		/// create the check box button
		bool create( int x, int y, int width, int height, WindowBase *parent, DWORD style = BS_AUTOCHECKBOX );

		/// retrive the check state of the CheckBox
		bool isChecked() { return ButtonBase::isChecked(); }

		/// set the check state of the RadioButton
		void setChecked( bool checked ) { ButtonBase::setChecked( checked ); }
	};

	/**
	 * GroupBox, implements a 'group box'. In fact, it's not a button.
	 *
	 */
	class GroupBox : public ButtonBase
	{
	public:
		/// create the group box
		bool create( int x, int y, int width, int height, WindowBase *parent, DWORD style = BS_GROUPBOX );
	};
}

#endif // end __KLWIN_BUTTON_H