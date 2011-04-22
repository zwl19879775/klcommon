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
   (about-content :initarg :about-content
                  :accessor blog-about
                  :initform "No description")
   (comments :accessor blog-comments
            :initform nil)
   (comments-storage-path :initarg :comments-storage-path
                          :accessor blog-comments-storage-path
                          :initform nil)))

(defun format-display-date (time)
  (multiple-value-bind (s m h day month year)
    (decode-universal-time time)
    (format nil "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d" 
            year month day h m s)))

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

(defun ext-blog-search (blog)
  (with-html
    (box (:class "nuclblog-nav-box")
         (:h2 "搜索本站")
         (:script :language "JavaScript" (str (get-search-func)))
         (:input :style "width:140px" :type "text" :name "q" :id "q"
                     :onkeydown (get-call-search-func blog))
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

(defmethod nav-boxes ((blog ext-blog))
  (ext-main-nav blog)
  (ext-blog-search blog)
  (categories blog)
  (recent-entries blog)
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

(defun ext-main-nav (blog)
  (box (:class "nuclblog-nav-box" :id "nuclblog-nav-box-1")
       (:h2 (str (blog-short-name blog)))
       (:ul
        (:li (:a :href (blog-url-root blog) "首页"))
        (:li (:a :href (blog-archives-url blog) "标题显示所有"))
        (:li (:a :href (archives-url blog :rss t) "订阅 (RSS)"))
        (:li (:a :href (blog-email-redirect-url blog) "联系"))
        (:li (:a :href (blog-about-url blog) "关于")))))

(defun ext-blog-about (blog)
  (with-blog-page 
    blog
    "关于"
    (with-html
      (:div :class "nuclblog-entry"
            (str (blog-about blog))))))

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
  (concatenate-url (blog-url-root blog) "/display?id=" id))

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

(defun comment-display (comment title)
  (with-html
    (:p
      (:h4 (str (format nil "re: ~a" title))
           (:span (str (format-display-date (comment-time comment))))
           (if (string-emptyp (comment-url comment))
             (str (comment-author comment))
             (with-html (:a :href (comment-url comment) (str (comment-author comment))))))
      (:p
        (str (comment-desc comment))))))

(defun comment-list-display (blog entry)
 (let ((comments (get-entry-comments blog (blog-entry-number entry))))
   (loop for comment in comments
         do (comment-display comment (blog-entry-title entry)))))

(defun entry-html-with-comment (blog entry)
  (entry-html blog entry)
  (with-html
    (:div 
      :class "nuclblog-comment"
      (:h3 "评论")
      (comment-list-display blog entry)
      (:h3 "添加回复")
      (:p 
        (:form :action (comment-url blog) :method :post
               (:input :type :hidden :name "entryid" :value 
                       (blog-entry-number entry))
               (:p (:input :type :text :name "author" "名字(*)"))
               (:p (:input :type :text :name "email" "Email(*不会公开)"))
               (:p (:input :type :text :name "url" "网址"))
               (:p (:textarea :name "comment" :rows "10" :cols "60" ""))
               (:p (:input :type :submit :value "提交评论")))))))

(defun ext-blog-display (blog &key id)
  (with-blog-page
    blog
    (format nil "~A: display" (blog-title blog))
    (if (and id (numberp id))
      (let ((entry (get-entry id blog)))
        (if entry
          (entry-html-with-comment blog entry)
          (with-html
            (:p "Invalid entry."))))
      (with-html
        (:p "Please select a blog entry for display.")))))

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
        (create-blog-comment blog (parse-integer entryid) author email url comment)
        (with-html
          (:p (:h4 "评论成功") (:br)
              "评论内容被审核后才会出现在评论列表里，如有不便敬请谅解！"))))
      (with-html (:a :href (entry-id-url blog entryid) "Back"))))

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
  (define-blog-handler (blog :uri "/comment" :default-request-type :post)
                       (entryid author email url comment)
                       #'ext-blog-comment)
  (define-blog-handler (blog :uri "/archives")
                       (category)
                       #'ext-blog-archives))

(defmethod shared-initialize :after ((blog ext-blog) slot-names &rest initargs)
  (declare (ignore slot-names initargs))
  (read-blog-entries blog)
  (read-blog-comments blog)
  (ext-define-blog-handlers blog))

