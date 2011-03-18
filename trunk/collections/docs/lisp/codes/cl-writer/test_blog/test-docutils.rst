.. -*- coding: utf-8 -*-
 
Test docutils
=============


What's this
-----------

This is a test file for docutils to generate a html file.

What do you want
----------------

通过docutils工具产生足够格式化的文本，然后通过lisp写的metaweblog客户端做部分调整
，然后发布到博客上。

How
----

This section describes how to implement this tool chain.

1. write blog as **reStructuredText** format, vim editor perform excellent on
   this

2. generate html file using docutils *font-end* tool

3. use lisp program written by me to process the file like including 

- remove <html> tags, so that it can be a valid blog format(actually it's not
        necessary)

- upload pictures if it has, and replace these image url by real url

- post the blog using metaweblog api

4. DONE

Here is a test picture.

    .. image:: images/test1.jpg
        :alt: 图示

End of test picture.

Test xml file::
    
    <?xml version='1.0'?>
    <methodResponse>
        <params>
            <param>
                <value><string>Welcome to Zope.org</string></value>
        </param>
        </params>
    </methodResponse>

And test some plain text::

    (defun testfn (x)
      (format t "testfn")
      x)

EOF

