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
                  :initform "No description")))

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

(defun ext-define-blog-handlers (blog)
  (define-blog-handlers blog)
  (define-blog-handler (blog :uri "/about") () #'ext-blog-about)
  (define-blog-handler (blog) () #'ext-blog-main)
  (define-blog-handler (blog :uri "/page") 
                       ((id :parameter-type 'integer))
                        #'ext-blog-page)
  (define-blog-handler (blog :uri "/archives")
                       (category)
                       #'ext-blog-archives))

(defmethod shared-initialize :after ((blog ext-blog) slot-names &rest initargs)
  (declare (ignore slot-names initargs))
  (read-blog-entries blog)
  (ext-define-blog-handlers blog))

