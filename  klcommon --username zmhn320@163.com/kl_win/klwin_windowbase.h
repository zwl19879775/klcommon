/**
 * @file klwin_windowbase.h
 * @author Kevin Lynx
 *
 */
#ifndef _KLWIN_WINDOWBASE_H
#define _KLWIN_WINDOWBASE_H

#include <windows.h>
#include <list>

namespace klwin
{
	/**
	 * base window class
	 *
	 * About the parent-child mechanism, the parents will NOT manage the 
	 * memory of its children. So to be safe, you must be sure the children's
	 * deletion will happen before its parents'.
	 * I suggest use this programming style:
	 * 
	 * class Parent
	 * {
	 *  private:
	 *     WindowBase child;
	 *
	 */
	class WindowBase
	{
	public:
		/// Window's controls doesnot manage its children.
		/// So i manage it myself.
		typedef std::list<WindowBase*> ChildList;

	public:
		/// Constructor
		WindowBase() ;

		/// Destructor
		virtual ~WindowBase() { }

		/// create the window
		virtual bool create() { return true; }

		/// destroy the window
		/// the default action is 'DestroyWindow' and remove from its parent.
		virtual void destroy() ;

		/// add a child if the child isnot in the list.
		void addChild( WindowBase *child );

		/// remove a child if it's in the list.
		/// Note : it's only remove it from the list, but will NOT 
		/// remove the object from the memory.
		void removeChild( WindowBase *child );

		/// get a child from the list.If not found, return NULL.
		WindowBase *getChildFromHandle( HWND handle );
		///WindowBase *getChildFromID( WORD id );

		/// resize the window
		virtual bool resize( int width, int height );

		/// move the window
		bool move( int x, int y );

		/// show the window
		void show();

		/// hide the window
		void hide();

		/// set window text
		void setWindowText( const char *text );

		///  get window text, return the length it got.
		int getWindowText( char *buf, int size );

		/// get the window's handle
		HWND getHandle() const { return mHandle; }

		/// get application instance
		HINSTANCE getInstance() const { return mAppInstance; }

		/// get position
		int getX() { return mX; }
		int getY() { return mY; }

		///  get size
		int getWidth() { return mWidth; }
		int getHeight() { return mHeight; }

		/// visible
		bool isVisible() { return mVisible; }

		/// get parent
		WindowBase *getParent() { return mParent; }

	protected:
		/// a convenient functioin to create a child window like 'Edit', 'ListBox'
		bool _create( const char *wc_name, int x, int y, int width, int height, WindowBase *parent, DWORD style );

	protected:
		/// application instance, also you can GetModuleHandle( 0 ) to get this value
		HINSTANCE mAppInstance;
		/// window handle
		HWND mHandle;
		/// position
		int mX, mY;
		/// size
		int mWidth, mHeight;
		/// visible
		bool mVisible;
		/// children
		ChildList mChildList;
		/// parent
		WindowBase *mParent;
	};

}

#endif // end _KLWIN_WINDOWBASE_H