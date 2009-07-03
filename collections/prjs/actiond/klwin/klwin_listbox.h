/**
 * @file klwin_listbox.h
 * @author Kevin Lynx
 * @created 2.21.2008
 *
 */
#ifndef __KLWIN_LISTBOX_H
#define __KLWIN_LISTBOX_H

#include "klwin_windowbase.h"

namespace klwin
{
	/**
	 * ListBox class, wrap the "LISTBOX" in windows.
	 * You can get the message of the ListBox within its 
	 * parent's window message process function.
	 *
	 * I know it's an ugly method to handle messages, but
	 * that doesnot mean i cannot make a better design.:D
	 *
	 * Currently , it's limited to use it, but if you like i'd 
	 * add more functions to use.
	 */
	class ListBox : public WindowBase
	{
	public:
		/// default constructor
		ListBox();

		/// destructor
		~ListBox();

		/// create
		bool create( int x, int y, int width, int height, WindowBase *parent, DWORD style = LBS_STANDARD );

		/// destroy, called by destructor
		/// this function will remove it from its parent.
		/// that means you can call it any time if you want.
		void destroy();
	
		/// retrives the number of items in the list box
		int getCount();

		/// Retrieves the zero-based index of the currently selected item, 
		/// if any, in a single-selection list box.
		/// If the list box is a multiple-selection list box or the function
		/// execute incorrectly, return -1.
		int getCurSel();

		/// select an item in the list box. If -1, no item will be selected.
		/// only used in single-selection list box
		void setCurSel( int sel );

		/// find a string in the listbox
		/// @return the index of the string ,-1 indicates error
		int findString( const char *str );

		/// add a string into the list box, and return the index of the string.
		int addString( const char *str );

		/// Deletes the item in position nIndex from the list box.
		void delString( int index );
		void delString( const char *str );

		/// clear all the string in the list box
		void clear();

	};
}

#endif // end __KLWIN_LISTBOX_H