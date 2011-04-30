;;;
;;; pages.lisp
;;; Kevin Lynx
;;; 4.16.2011
;;;

(in-package :blog)

(defconstant +entry-count-per-page+ 4)

(defclass ext-blog (blog:blog)
  ((owner :initarg :owner
          :accessor blog-owner
          :initform nil)
   (ext-links :initarg :ext-links
              :accessor blog-ext-links
              :initform nil)
   (ext-html :initarg :ext-html
             :accessor blog-ext-html
             :initform "")
   (about-id :initarg :about-id
             :accessor blog-about-id
             :initform 0
             :documentation "the about entry id")
   (notice :initarg :notice
           :initform ""
           :accessor blog-ext-notice)
   (comments :accessor blog-comments
            :initform nil)
   (comments-storage-path :initarg :comments-storage-path
                          :accessor blog-comments-storage-path
                          :initform nil)))

(defun format-display-date (time)
  (multiple-value-bind (s m h day month year)
    (decode-universal-time time)
    (declare (ignore s))
    (format nil "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d" 
            year month day h m)))

(defun format-display-day (time)
  (multiple-value-bind (s m h day month year)
    (decode-universal-time time)
    (declare (ignore s m h))
    (format nil "~4,'0d-~2,'0d-~2,'0d" 
            year month day)))

(defun get-search-func ()
 "function SearchGoogle(key,evt,site) {
    if(evt.keyCode==13 || evt.keyCode==0 || evt.type =='click') {
        key.focus();
        var keystr = encodeURIComponent(key.value);
        url = \"http://www.google.com/search?q=\";
        url = url+keystr;
        url += \"&ie=UTF-8&oe=GB2312&hl=zh-CN&domains=\"+site+\"&sitesearch=\"+site;
        window.location=url;
        return false;
    }
 }")

(defun get-call-search-func (blog)
  (format nil
          "return SearchGoogle(document.getElementById(\"q\"),event, \"~a\")"
          (blog-blog-links blog)))

;;; not used now.
(defun ext-blog-search (blog)
  (with-html
    (box (:class "nuclblog-nav-box")
         (:h2 "搜索本站")
         (:script :language "JavaScript" (str (get-search-func)))
         (:input :style "width:150px" :type "text" :name "q" :id "q" :onkeydown (get-call-search-func blog))
         (:input :onclick (get-call-search-func blog) :type "button"
                 :value "搜索" :name "sa"))))

(defun ext-blog-external-links (blog) 
  (with-html
    (box (:class "nuclblog-nav-box")
         (:h2 "我的项目")
         (:ul
           (loop for link in (blog-ext-links blog)
                 do
                 (destructuring-bind (&key url desc) link
                   (htm
                     (:li
                       (:a :href url (str desc))))))))))

(defun ext-blog-notice (blog) 
  (with-html
    (box (:class "nuclblog-nav-box" :id "nuclblog-nav-box-notice")
         (:h2 "本站公告")
         (:p (str (blog-ext-notice blog))))))

(defun ext-blog-status (blog)
  (let ((comm (length (blog-comments blog)))
        (un-comm (unconfirmed-comment-count blog))
        (msg-comm (message-comment-count blog))
        (time (when (> (length (blog-entries blog)) 0) 
                (blog-entry-time (car (blog-entries blog))))))
    (with-html
      (box (:class "nuclblog-nav-box")
           (:h2 "统计")
           (:ul
             (:li (str (format nil "文章：~a" (length (blog-entries blog)))))
             (:li (str (format nil "评论：~a" (- comm un-comm msg-comm))))
             (:li "留言："
                  (:a :href (comment-manage-url blog) (str msg-comm)))
             (:li "待验证评论：" 
                  (:a :href (comment-manage-url blog) (str un-comm)))
             (:li (str (format nil "最后发表：~a" (format-display-day time)))))))))

(defun ext-blog-recent-entries (blog)
  (with-html
    (box (:class "nuclblog-nav-box" :id "nuclblog-nav-box-recent-entries")
         (:h2 "最新文章")
         (:ul :class "nuclblog-recent-entries"
              (loop for i from 1 to 5
                 for j in (blog-entries blog)
                 do (htm
                     (:li
                      (:a :href (entry-link blog j)
                          (str (blog-entry-title j))))))))))

(defmethod nav-boxes ((blog ext-blog))
  (ext-blog-notice blog)
  (ext-blog-status blog)
  (categories blog)
  (ext-blog-recent-entries blog)
  (recent-comments blog)
  (ext-blog-external-links blog))

(defmethod footer ((blog ext-blog))
  (with-html-output 
    (*standard-output*)
    (:div 
      :class "nuclblog-footer"
      (:p "Powered by:"
        (:br "Lisp Hunchentoot nuclblog")
        (:br (str (format nil "Copyright (c) ~a" (blog-owner blog)))))
      (htm (str (blog-ext-html blog))))))
    
 
(defun blog-about-url (blog)
  (concatenate-url (blog-url-root blog) "/about"))

(defun blog-leave-message-url (blog)
  (concatenate-url (blog-url-root blog) "/leave-message"))

;;; not used now.
(defun ext-main-nav (blog)
  (box (:class "nuclblog-nav-box" :id "nuclblog-nav-box-1")
       (:h2 (str (blog-short-name blog)))
       (:ul
        (:li (:a :href (blog-url-root blog) "首页"))
        (:li (:a :href (blog-archives-url blog) "标题显示所有"))
        (:li (:a :href (archives-url blog :rss t) "订阅 (RSS)"))
        (:li (:a :href (blog-leave-message-url blog) "给我留言"))
        (:li (:a :href (blog-about-url blog) "关于")))))

(defun blog-search-html (blog)
  (with-html
    (:div :id "nuclblog-menu-search"
          (:script :language "JavaScript" (str (get-search-func)))
          (:input :style "width:150px" :type "text" :name "q" :id "q"
                  :onkeydown (get-call-search-func blog))
          (:input :onclick (get-call-search-func blog) :type "button"
                 :value "搜索本站" :name "sa"))))

(defmethod banner :after ((blog ext-blog))
  (with-html-output 
    (*standard-output*)
    (:div :id "nuclblog-nav-menu"
       (:ul
        (:li (:a :href (blog-url-root blog) "首页"))
        (:li (:a :href (blog-archives-url blog) "标题显示所有"))
        (:li (:a :href (archives-url blog :rss t) "订阅 (RSS)"))
        (:li (:a :href (blog-leave-message-url blog) "给我留言"))
        (:li (:a :href (blog-about-url blog) "关于")))
       (blog-search-html blog))))

(defun make-entry-url-from-id (blog id)
  (concatenate-url (blog-url-root blog)
                   "/display?id=" (princ-to-string id)))

(defun ext-blog-about (blog)
  (hunchentoot:redirect (make-entry-url-from-id blog (blog-about-id blog))))

(defun entry-title-html (blog entry)
  (with-html
    (:div :class "nuclblog-entry"
          (:div :class "nuclblog-entry-head"
                (:div :class "nuclblog-entry-title"
                      (:h1 (:a :href (make-entry-url blog entry)
                               (str (blog-entry-title entry)))))
                (:div :class "nuclblog-entry-date"
                      (:h2 (str (hunchentoot::rfc-1123-date
                                  (blog-entry-time entry)))
                           (unless (< (abs (- (blog-entry-time entry)
                                              (blog-entry-revised-time entry)))
                                      10)
                             (htm
                               " revised at: "
                               (str (hunchentoot::rfc-1123-date
                                      (blog-entry-revised-time entry)))))))

                (let ((user (blog-entry-user entry)))
                  (when user
                    (htm (:div :class "nuclblog-entry-user"
                               (:h3 "posted by " (str user)
                                    " in " 
                                    (:a :href
                                        (make-archives-url
                                          blog (blog-entry-category entry))
                                        (str (blog-entry-category entry))))))))))))
(defmethod page-url ((blog blog))
  (concatenate-url (blog-url-root blog) "/page"))

(defun page-url-index (blog i)
  (concatenate-url (page-url blog) "?id=" (prin1-to-string i)))

(defun entry-id-url (blog id)
  (concatenate-url (blog-url-root blog) 
                   (format nil "/display?id=~a" id)))

(defun ext-blog-main (blog)
  (with-blog-page
    blog
    (blog-title blog)
    (loop for entry in (sorted-blog-entries blog)
           for i below +entry-count-per-page+ 
           do (entry-html blog entry))
    (when (> (length (get-blog-entries blog)) +entry-count-per-page+)
      (with-html
        (:div :class "nuclblog-content-nav"
              (:a :href (page-url-index blog 2) "下一页"))))))

(defun ext-blog-archives (blog &key category)
  (with-blog-page
      blog
      (format nil "~A: archives" (blog-title blog))
    (loop for entry in (sorted-blog-entries blog)
       when (or (null category)
                (equal (blog-entry-category entry)
                       category))
       do (entry-title-html blog entry))))

(defun nth-list (n lst)
  (if (> n (length lst))
    nil
    (if (<= n 1) lst (nth-list (- n 1) (cdr lst)))))

(defun gen-page-index-nav (blog id)
  (with-html
   (:div 
     :class "nuclblog-content-nav"
     (str "页数：")
     (let* ((length (length (get-blog-entries blog)))
            (mod (mod length +entry-count-per-page+))
            (max-c (/ length +entry-count-per-page+)))
       (when (> mod 0) (incf max-c))
       (loop for i from 1 to max-c
             do 
             (if 
              (= id i)
              (with-html (str i) (str "&nbsp;&nbsp;"))
              (with-html
                (:a :href (page-url-index blog i) (str i))
                (str "&nbsp;&nbsp;"))))))))
                  
(defun get-nth-page-entry (blog id)
  (setf id (- id 1))
  (nth-list (1+ (* id +entry-count-per-page+)) (sorted-blog-entries blog)))

(defun ext-blog-page (blog &key id)
  (with-blog-page
    blog
    (blog-title blog)
    (loop for entry in (get-nth-page-entry blog id)
           for i below +entry-count-per-page+ 
           do (entry-html blog entry))
    (gen-page-index-nav blog id)))

(defun comment-sub-url (blog comment entry)
  (concatenate-url (make-entry-url blog entry) 
                   (format nil "#~a" (comment-id comment))))

(defun comment-display-preheader (blog comment entry)
  (if entry
    (with-html
      (:a :href (comment-sub-url blog comment entry) "#")
      (:a :name (comment-id comment))
      (str (format nil "re: ~a" (blog-entry-title entry))))
    (with-html
      (str "#留言"))))

(defun comment-display (blog comment entry)
  (with-html
    (:p
      (:h4 (comment-display-preheader blog comment entry)
           (:span (str (format-display-date (comment-time comment))))
           (if (string-emptyp (comment-url comment))
             (str (comment-author comment))
             (with-html (:a :href (comment-url comment) (str (comment-author comment))))))
      (:p
        (str (comment-desc comment))))))

(defun comment-list-display (blog entry)
 (let ((comments (get-entry-comments blog (blog-entry-number entry))))
   (loop for comment in comments
         do (comment-display blog comment entry))))

(defun entry-html-with-comment (blog entry)
  (entry-html blog entry)
  (with-html
    (:div 
      :class "nuclblog-comment"
      (:h3 "评论")
      (comment-list-display blog entry)
      (:h3 "添加回复(带*为必填项)")
      (:p 
        (:form :action (comment-url blog) :method :post
               (:input :type :hidden :name "entryid" :value 
                       (blog-entry-number entry))
               (:p (:input :type :text :name "author" "名字(*)"))
               (:p (:input :type :text :name "email" "Email(*不会公开)"))
               (:p (:input :type :text :name "url" "网址"))
               (:p (:textarea :name "comment" :rows "10" :cols "60" ""))
               (:p (:input :type :submit :value "提交评论")))))))

(defun leave-message-html (blog) 
  (with-html
    (:div
      :class "nuclblog-comment"
      (:h3 "留言(带*为必填项)")
      (:p
        (:form :action (comment-url blog) :method :post
               (:input :type :hidden :name "entryid" :value -1)
               (:p (:input :type :text :name "author" "名字(*)"))
               (:p (:input :type :text :name "email" "Email(*不会公开)"))
               (:p (:input :type :text :name "url" "网址"))
               (:p (:textarea :name "comment" :rows "10" :cols "60" ""))
               (:p (:input :type :submit :value "提交")))))))
          
(defun ext-blog-leave-message (blog)
  (with-blog-page
    blog
    "Leave a message"
    (leave-message-html blog)))

(defun ext-blog-display (blog &key id)
  (let ((entry (get-entry id blog)))
    (with-blog-page
      blog
      (format nil "~a  --~a" (if entry (blog-entry-title entry) "Invalid entry") 
              (blog-title blog))
      (if entry
        (entry-html-with-comment blog entry)
        (with-html
          (:p "Invalid entry."))))))

(defmethod comment-url ((blog blog))
  (concatenate-url (blog-url-root blog) "/comment"))

(defun string-emptyp (s)
  (or (null s) (= 0 (length s))))

(defun ext-blog-comment (blog &key entryid author email url comment)
  (with-blog-page
    blog
    (blog-title blog)
    (if (or (string-emptyp author) (string-emptyp email) (string-emptyp comment))
      (with-html 
        (:p "Oops! You bad guy please input necessary fields!"))
      (progn
        (create-blog-comment blog entryid author email url comment)
        (if (>= entryid 0)
          (with-html
            (:p (:h4 "评论成功") (:br)
                "评论内容被审核后才会出现在评论列表里，如有不便敬请谅解！"))
          (with-html
            (:p (:h4 "留言成功") (:br)
                "如果有必要，作者会通过邮件联系你。And thank you for your message!")))))
      (with-html (:a :href 
                  (if (>= entryid 0) 
                    (entry-id-url blog entryid) (blog-leave-message-url blog)) "Back"))))

(defun comment-delete-url (blog comment)
  (concatenate-url (blog-url-root blog) 
                   (format nil "/comment-delete?id=~a" (comment-id comment))))

(defun comment-confirm-url (blog comment)
  (concatenate-url (blog-url-root blog) 
                   (format nil "/comment-confirm?id=~a" (comment-id comment))))

(defun comment-manage-url (blog)
  (concatenate-url (blog-url-root blog) "/comment-manage"))

(defun comment-display-manage (blog comment)
  (let ((entry (get-entry (comment-entryid comment) blog)))
    (comment-display blog comment entry)
    (unless (comment-confirmed comment)
      (with-html (:a :href (comment-confirm-url blog comment) "Confirm")
                 (str "&nbsp;&nbsp;")))
    (with-html 
      (:a :href (comment-delete-url blog comment) "Delete")
      (str (format nil "&nbsp;&nbsp;E-mail: ~a" (comment-email comment))))))
      

(defun string-brief (s len)
  (if (<= len (length s))
    (concatenate 'string (subseq s 0 (- len 3)) "...")
    s))

(defun comment-title (blog comment)
  (let ((entry (get-entry (comment-entryid comment) blog)))
    (format nil "re: ~a" (blog-entry-title entry))))

(defun recent-comments (blog)
  (with-html
    (box (:class "nuclblog-recent-comments")
         (:h2 "最新评论")
         (:ul 
           (loop for i from 1 to 5
                 for c in (get-confirmed-comments blog)
                 do (htm
                      (:li :style "list-style-type:none;"
                        (:a :href (comment-sub-url 
                                    blog c 
                                    (get-entry (comment-entryid c) blog))
                            (str (format nil "~a.~a" i (comment-title blog c)))))
                      (:li :style "list-style-type:none;" (str (string-brief (comment-desc c) 15)))
                      (:li :style "list-style-type:none;text-align: right;margin-right:4px;"
                           (str (format nil "-- ~a" (comment-author c))))))))))

(defun ext-blog-comment-manage (blog &key user password)
  (hunchentoot-auth:authorized-page
    ((blog-realm blog)
     :ssl-port (blog-ssl-port blog)
     :login-page-function (lambda ()
                            (blog-login-page blog user password)))
    (with-blog-page
      blog
      (blog-title blog)
      (with-html
        (:div 
          :class "nuclblog-comment"
          (:h3 "All comments" (str "&nbsp;&nbsp;")
               (:a :href (blog-logout-url blog) "Logout"))
          (loop for comment in (get-all-comments blog)
                do (comment-display-manage blog comment)))))))

(defun ext-blog-comment-confirm (blog &key id user password)
  (hunchentoot-auth:authorized-page
    ((blog-realm blog)
     :ssl-port (blog-ssl-port blog)
     :login-page-function (lambda ()
                            (blog-login-page blog user password)))
    (when id (confirm-comment blog id))
    (hunchentoot:redirect (comment-manage-url blog))))

(defun ext-blog-comment-delete (blog &key id user password)
  (hunchentoot-auth:authorized-page
    ((blog-realm blog)
     :ssl-port (blog-ssl-port blog)
     :login-page-function (lambda ()
                            (blog-login-page blog user password)))
    (when id (delete-comment blog id))
    (hunchentoot:redirect (comment-manage-url blog))))

(defun ext-define-blog-handlers (blog)
  (define-blog-handlers blog)
  (define-blog-handler (blog :uri "/about") () #'ext-blog-about)
  (define-blog-handler (blog) () #'ext-blog-main)
  (define-blog-handler (blog :uri "/page") 
                       ((id :parameter-type 'integer))
                        #'ext-blog-page)
  (define-blog-handler (blog :uri "/display")
                       ((id :parameter-type 'integer))
                       #'ext-blog-display)
  (define-blog-handler (blog :uri "/leave-message") () #'ext-blog-leave-message)
  (define-blog-handler (blog :uri "/comment" :default-request-type :post)
                       ((entryid :parameter-type 'integer)
                        author email url comment)
                       #'ext-blog-comment)
  (define-blog-handler (blog :uri "/comment-manage")
                       ((user :init-form (hunchentoot-auth:session-realm-user (blog-realm blog)))
                        password)
                       #'ext-blog-comment-manage)
  (define-blog-handler (blog :uri "/comment-confirm")
                       ((id :parameter-type 'integer)
                        (user :init-form (hunchentoot-auth:session-realm-user (blog-realm blog)))
                        password)
                       #'ext-blog-comment-confirm)
  (define-blog-handler (blog :uri "/comment-delete")
                       ((id :parameter-type 'integer)
                        (user :init-form (hunchentoot-auth:session-realm-user (blog-realm blog)))
                        password)
                       #'ext-blog-comment-delete)
  (define-blog-handler (blog :uri "/archives")
                       (category)
                       #'ext-blog-archives))

(defmethod shared-initialize :after ((blog ext-blog) slot-names &rest initargs)
  (declare (ignore slot-names initargs))
  (read-blog-entries blog)
  (read-blog-comments blog)
  (ext-define-blog-handlers blog))

