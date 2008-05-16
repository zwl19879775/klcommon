/**
 * @file klwin_editbox.h
 * @author Kevin Lynx
 * @created 2.21.2008
 *
 */
#ifndef __KLWIN_EDITBOX_H
#define __KLWIN_EDITBOX_H

#include "klwin_windowbase.h"

namespace klwin
{
	/**
	 * EditBox class, wrap the 'EDIT' in windows
	 *
	 */
	class EditBox : public WindowBase
	{
	public:
		/// Constructor
		EditBox();

		/// Destructor
		~EditBox();

		/// create
		bool create( int x, int y, int width, int height, WindowBase *parent, 
					 DWORD style = ES_MULTILINE | ES_WANTRETURN | ES_LEFT |
					 WS_HSCROLL | WS_VSCROLL );

		/// destroy, you can this function to destroy the edit box anytime
		void destry();

		/// set whether the edit box is read-only
		void setReadOnly( bool read_only );

		/// whether the edit box is read-only
		bool isReadOnly();

		/// set the text limit for the edit box
		void setLimitText( unsigned int max );

		/// get the text limit for the edit box
		unsigned int getLimitText();

		/// add text into the edit box
		void addText( const char *str, bool auto_scroll = true );
	};
}

#endif // end __KLWIN_EDITBOX_H
