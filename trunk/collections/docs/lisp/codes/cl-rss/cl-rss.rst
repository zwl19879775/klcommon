Implement RSS reader in Common Lisp 
======================================

:Author: Kevin Lynx
:Date: 3.26.2011

Knownledge before implementation
-----------------------------------
RSS2.0 specification:
    http://feed2.w3.org/docs/rss2.html
Data/Time specification:
    RFC 822

Overview Design
-------------------

This RSS reader is based on a web server. All functions are in the server side,
 and it will parse these RSS feeds and generate html to web client.

Because i am not familiar with html technique, so as a practice, these generated
html files are very simple.

Page main: display RSS categories and a **add** function there. Every category
has a **remove** function, to remove that feed.

::

    rss-category(link) remove(link)
    rss-category(link) remove(link)
    rss-category(link) remove(link)
    rss-url(editbox)    add(button)

Page context: display RSS content of a specified category, every item has some
usful functions.

::

    rss-item[title, description, date etc]
    rss-item[title, description, date etc]
    rss-item[title, description, date etc]
    back(link)

22:37 update::
    
    i suppose this RSS reader is not necessary to output result to an html file,
    instead its output is generic. It can output to a console, also an html
    file.

IMPlEMENT DETAILS
--------------------

<ttl> is most possible the value which we can refresh our RSS.

**S-XML USAGE**

Always remember, these three callback functions, *seed* means the previous
element returned seed whatever the element is, and *parent-seed* means the seed
passed to the relative new-seed-callback.


HISTORY
--------------

1. Not sure about this::

    (push (cons :title "abc") (get-plist channel))
    => (channel-plist channel) is still nil, not modified.
    but if,
    (defvar plist (get-plist channel))
    (push (cons :title "abc") plist)
    => plist will be ok, but (channel-plist channel) is still nil, it seems
    plist is different from (channel-plist channel).

    Ok, instead used a generic function 'get-plist', i use the director accessor
    'channel-plist', everything goes ok:

    (push (cons :title "abc") (channel-plist channel))
    also,
    (setf (channel-plist channel) (list (cons :title "abc")))

    I guess that's the reason:

    (macroexpand-1 '(setf (channel-plist channel) (list (cons :title "a"))))
    =>
    (LET* ((#:TMP983 CHANNEL))
      (MULTIPLE-VALUE-BIND (#:NEW982)
          (LIST (CONS :TITLE "a"))
        (FUNCALL #'(SETF CHANNEL-PLIST) #:NEW982 #:TMP983)))
    THERE IS A FUNCTION NAMED 'setf channel-plist'!
    See examples in s-xml-rpc source code.
    Also (macroexpand-1 '(push (cons :name "hi") (channel-plist channel)))
    =>
    (LET* ((#:G980 (CONS :NAME "hi"))
           (#:TMP979 CHANNEL)
           (#:NEW978 (CONS #:G980 (CHANNEL-PLIST #:TMP979))))
      (FUNCALL #'(SETF CHANNEL-PLIST) #:NEW978 #:TMP979))

    When we define an **accessor** for a slot of a class, Lisp will generate
    these 'setf xxx' functions, but when i use another generic funciton, there
    is not such a function with setf. So, problem comes.

2. About http redirect problem, it seems a bit difficult to solve. Http redirect
   problem includes 301 and 302 response from server.

3. 3.30.2011
   The simple demo has been done. And now i want to write more codes to put the
   rss reader on a web server like ``hunchentoot``. And i need to store data in
   database, using third-party library like ``Elephant``.

4. 3.31.2011
   The item cache has been DONE. And now it can generate an index page to index
   these stored channles. Also, it can `add new rss`, `remove a rss`, `refresh a
   rss`.
   What left is, embedded into the web server. On the other hand, should i add
   an automatically refresh?

TODO
-----------

1. Some sites response 301 or 302.
    test sites: 
                /?feed=rss2   www.yulefox.com
                /?feed=rss2   coolshell.cn
 
  FIX: modify the request header ``host`` value, only contain host name,
  excluding port number.

2. Visit coolshell.cn with HTTP/1.1, the response context cannot be parsed as an
   xml. But HTTP/1.0 works fine.


