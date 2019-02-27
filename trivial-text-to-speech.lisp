
(defpackage #:trivial-text-to-speech
  (:use #:cl
        #:easy-routes
        #:parenscript))

(in-package #:trivial-text-to-speech)


(defparameter *listener* nil)

(defun start-speech ()
  (unless *listener*
    (setf *listener* (make-instance 'easy-routes-acceptor
                                    :address "127.0.0.1"
                                    :port 8080))
    (hunchentoot:start *listener*))
  (start-boxer-thread))


(defparameter *append-box* (mailbox:make-mailbox))
(defparameter *overwrite-box* (mailbox:make-mailbox))

(defun cap-string (string)
  (let* ((splitted (split-sequence:split-sequence #\Space string))
         (first-word (string-capitalize (car splitted)))
         (correct (format nil "~{~a~^ ~}" (append (list first-word) (cdr splitted)))))
    correct))

(defun start-boxer-thread ()
  (bt:make-thread (lambda ()
                    (loop
                      (boxer)
                      (sleep 1)))
                  :name "boxer"))

(defun boxer ()
  (labels ((write-all-overwrites ()
             (multiple-value-bind (item more)
                 (mailbox:read-mail *overwrite-box*)
               (when item
                 (log:info "overwrite")
                 (with-open-file (out "C:/Users/cmoore/Desktop/speech.txt"
                                      :direction :output
                                      :if-exists :supersede)
                   (format out "~a.~%" (cap-string item)))
                 (when more
                   (write-all-overwrites)))))
           (write-all-appends ()
             (multiple-value-bind (item more)
                 (mailbox:read-mail *append-box*)
               (when item
                 (log:info "append")
                 (with-open-file (out "C:/Users/cmoore/Desktop/speech.txt"
                                      :direction :output
                                      :if-exists :append)
                   (format out "~a." item))
                 (when more
                   (write-all-appends))))))
    (write-all-overwrites)
    (write-all-appends)))

(defroute append-speech ("/speech/add/append" :method :post) (bundle)
  (mailbox:post-mail bundle *append-box*)
  "ok")

(defroute add-speech ("/speech/add/overwrite" :method :post) (bundle)
  (mailbox:post-mail bundle *overwrite-box*)
  "ok")

(ps:defpsmacro with-document-ready (&body body)
  `(progn
     ((@ ($ document) ready) ,@body)))

(ps:defpsmacro jq (selector &body body)
  `(-> (sel ,selector)
       ,@body))

(ps:defpsmacro sel (name)
  `($ ,name))

(ps:defpsmacro $. (name)
  `(@ (sel ,name)))

(ps:defpsmacro -> (&body body)
  `(ps:chain ,@body))

(ps:defpsmacro map (func list)
  `(do ((i 0 (incf i)))
       ((>= i (@ ,list length)))
     (funcall ,func (aref ,list i))))


(defmacro with-page ((&key
                        (title nil)
                        (css nil)
                        (white-header t)
                        (body-class nil)
                        (enable-top-toolbar nil)
                        (enable-page-header nil)
                        (js nil)
                        (js-init nil)) &body body)
  `(let ((who:*html-no-indent-tags* (list :pre :textarea :input :form)))
     (cl-who:with-html-output-to-string
         (*standard-output* nil :indent nil :prologue t)
       (who:htm
        (:html :lang "en"
          (:head
            (:meta :charset "utf-8")
            ,(if title
                 `(:title ,title)
                 `(:title "Nugget"))
            (:meta :name "viewport" :content "width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no")
            (:link :rel "stylesheet" :type "text/css" :href "https://r.ivy.io/am/lib/stroke-7/style.css")
            (:link :rel "stylesheet" :type "text/css" :href "https://r.ivy.io/am/lib/jquery.nanoscroller/css/nanoscroller.css")
            ,(unless enable-top-toolbar
               '(:style :type "text/css"
                 (who:str (css-lite:css ((".am-wrapper") (:padding-top "0px !important"))))))
            ,@(when css
                `,css)
            (:link :rel "stylesheet" :type "text/css" :href "https://r.ivy.io/am/css/style.css")
            (:script :src "https://r.ivy.io/am/lib/jquery/jquery.min.js"))
          (:body ,@(when body-class `(:class ,body-class))
            (:div :class ,(format nil "am-wrapper am-nosidebar-left ~a" (or (and white-header "am-white-header")
                                                                            ""))
              ,(when enable-top-toolbar
                 `(:nav :class "navbar navbar-default navbar-fixed-top am-top-header"
                    (:div :class "container-fluid"
                      (:div :class "navbar-header"
                        (:div :class "page-title"
                          (:span ,title))
                        (:a :href "/" :class "navbar-brand"))
                      (:a :href "#" :data-toggle "collapsed" :data-target "#am-navbar-collapse" :class "am-toggle-top-header-menu collapsed"
                        (:span :class "icon s7-angle-down"))
                      (:div :id "am-navbar-collapse" :class "collapse navbar-collapse"
                        (:ul :class "nav navbar-nav am-nav-right"
                          (:li (:a :href "/" "Home"))
                          (when (string= (person-role *greg-person*) "customer")
                            (who:htm
                             (:li (:a :href "/media" "Media"))
                             (:li (:a :href "/customer/setup" "Add Business")))))
                        (:ul :class "nav navbar-nav navbar-right am-icons-nav"
                          (if *greg-person*
                              (who:htm (:li :class "pull-right "(:a :href "/logout" "Logout")))
                              (who:htm (:li :class "pull-right" (:a :href "/login" "Login")))))))))
              (:div :class "am-content"
                ,(when enable-page-header
                   '(:div :class "page-head"
                     (:h2 "Calendar")
                     (:ol :class "breadcrumb"
                       (:li (:a :href "#" "Home"))
                       (:li (:a :href "#" "Pages")))))
                (:div :class "main-content"
                  ,@body)))
            (:script :src "https://r.ivy.io/am/lib/jquery.nanoscroller/javascripts/jquery.nanoscroller.min.js")
            (:script :src "https://r.ivy.io/am/js/main.min.js")
            (:script :src "https://r.ivy.io/am/lib/bootstrap/dist/js/bootstrap.min.js")
            (:script :src "https://r.ivy.io/am/lib/jquery-ui/jquery-ui.min.js")
            (:script :src "https://r.ivy.io/gfycat.min.js")
            ,@(when js
                `,js)))))))

(defroute test ("/speech") ()
  (with-page ()
    (:div :class "row"
      (:div :style "font-size:24px;" :id "output" :class "col-md-6 col-md-offset-3" ""))
    (:script
      (who:str
       (ps
         (defvar speech nil)
         ;; (defun append-full (sentence)
         ;;   (setf (@ (-> document (get-element-by-id "output")) inner-h-t-m-l) sentence))
         ;; (defun append-partial (sentence)
         ;;   (incf (@ (-> document (get-element-by-id "output")) inner-h-t-m-l) sentence))
         (with-document-ready
             (lambda ()
               (setf speech (new (webkit-speech-recognition)))
               (setf (@ speech lang) "en-US")
               (setf (@ speech continuous) t)
               (setf (@ speech interim-results) false)
                                
               (setf (@ speech onresult) (lambda (event)
                                           (map (lambda (block)
                                                  (-> console (log "result"))
                                                  (when (@ block is-final)
                                                    (let ((output (@ block 0 transcript))
                                                          (reg (regex "/^ /")))
                                                      (if (-> reg (exec output))
                                                          ((@ $ ajax) (create url "http://localhost:8080/speech/add/append"
                                                                              data (create "bundle" output)
                                                                              method "POST"))
                                                          ((@ $ ajax) (create url "http://localhost:8080/speech/add/overwrite"
                                                                              data (create "bundle" output)
                                                                              method "POST"))))))
                                                (@ event results))))
               (setf (@ speech onend) (lambda (event)
                                        (-> console (log "onend"))
                                        (-> speech (start))))
               (-> speech (start)))))))))
