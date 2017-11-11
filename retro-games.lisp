(defpackage :retro-games
  (:use :cl :cl-who :hunchentoot :parenscript :cl-mongo)) ; import all symbols

;; eval in repl
;;;  (ql:quickload '(cl-who hunchentoot parenscript cl-mongo))
(in-package :retro-games)
;;; (start-server 8080)


;;; game representation (CLOS)

(defclass game ()
  ((name :reader name ; r
         :initarg :name) ; set on init
   (votes :accessor votes ; rw
          :initarg votes ; used when reading in from db
          :initform 0))) ; always on init

(defmethod print-object ((object game) stream)
  (print-unreadable-object (object stream :type t) ; print type (GAME) before string rep
    (with-slots (name votes) object ; only access object once (no code duplication :)
      (format stream "name: ~S with ~D votes" name votes))))

(defmethod vote-for (game)
  (incf (votes game)))


;;; backend (prototype)
#|
(defvar *games* nil)

;; find returns obj only
;;; member return obj and cdr
(defun game-from-name (name)
  (find name *games* :test #'string-equal
                     :key #'name))

(defun game-stored-p (name)
  (game-from-name name))

(defun games ()
  (sort (copy-list *games*) ; sort is destructive, pass a copy to it
        #'> :key #'votes)) ; sort by votes in decending order

(defun add-game (name)
  (unless (game-stored-p name)
    (push (make-instance 'game :name name) *games*)))
|#


;;; backend (mongo)
;; start mongo
;; $: create document to add into db (by structure)

;; use this db
(db.use "games")

;; collection w/in db
(defparameter *game-collection* "game")

(defun unique-index-on (field)
  (db.ensure-index *game-collection*
                   ($ field 1)
                   :unique t))

(unique-index-on "name")

(defun doc->game (game-doc)
  (make-instance 'game :name (get-element "name" game-doc)
                       :votes (get-element "votes" game-doc)))

(defun game->doc (game)
  ($ ($ "name" (name game))
     ($ "votes" (votes game))))

(defun game-from-name (name)
  (let ((found-games (docs (db.find *game-collection*
                                    ($ "name" name)))))
    (when found-games
      (doc->game (car found-games)))))

(defun game-stored-p (name)
  (game-from-name name))

(defun add-game (name)
  (let ((game (make-instance 'game :name name)))
    (db.insert *game-collection* (game->doc game))))

;;; html

;;; cl-who notes:
;; function (html-mode) sets printing style
;; str, fmt, esc, htm: eval form before converting :)

(setf (html-mode) :html5)

;; add optional script
(defmacro standard-page ((&key title script) &body body)
  `(with-html-output-to-string
       (*standard-output* nil :prologue t :indent t)
     (:html :lang "en"
            (:head
             (:meta :charset "utf-8")
             (:title ,title)
             (:link :type "text/css"
                    :rel "stylesheet"
                    :href "/retro.css")
             ,(when script ; add script tag here if applicable
                `(:script :type "text/javascript"
                          (str ,script))))
            (:body
             (:div :id "header"
                   (:img :src "/logo.jpg"
                         :alt "Commodore 64"
                         :class "logo")
                   (:span :class "strapline"
                          "Vote on your favorite video games"))
             ,@body))))


;;; javascript

;;; parenscript notes:
;; @: obj value access x.y.z
;; chain: chain fn calls x.y.z()

;; wrap js in macro that expands into :script tag for 'new-game' page
(defmacro script ()
  `(ps
     (defvar add-form nil)
     (defun validate-game-name (evt) ; vet user input for new game
       (when (= (@ add-form name value) "") ; if null input
         (chain evt (prevent-default)) ; prevent following defined route
         (alert "Please enter a name."))) ; and alert instead
     (defun init () ; bind event handler to page element
       (setf add-form (chain document (get-element-by-id "addform")))
       (chain add-form (add-event-listener "submit" validate-game-name)))
     (setf (chain window onload) init))) ; call init() on window load


;;; server (hunchentoot)
;; add hunchentoot handlers

;; run in repl
(defun start-server (port)
  (start (make-instance 'easy-acceptor :port port)))

;; homepage
(define-easy-handler (retro-games :uri "/retro-games") ()
  (standard-page (:title "Top Retro Games")
    (:h1 "Vote on your all time favorite video games!")
    (:p "Don't see your favorite game? Add it "
        (:a :href "new-game" "here"))
    (:h2 "Leaderboard")
    (:div :id "chart"
          (:ol
           (dolist (game (games)) ; calling (games) reorders list on page reload
             (htm
              (:li (:a :href (format nil "vote?name=~A" ; why need explicit format here?
                                     (url-encode (name game))) "Vote!")
                   (fmt "~A: ~D votes" (escape-string (name game)) (votes game)))))))))

;; vote
(define-easy-handler (vote :uri "/vote") (name) ; get name from url param
  (when (game-stored-p name)
    (vote-for (game-from-name name)))
  (redirect "/retro-games")) ; redirect to homepage

;; new-game
(define-easy-handler (new-game :uri "/new-game") ()
  (standard-page (:title "Add a new game"
                  :script (str (script)))
    (:h1 "Add a new game to the leaderboard")
    (:form :action "/game-added" :method "post" :id "addform"
           (:p "What is the name of the game?" (:br)
               (:input :type "text" :name "name" :class "txt"))
           (:p (:input :type "submit" :value "Add" :class "btn")))))

;; game-added
(define-easy-handler (game-added :uri "/game-added") (name)
  (unless (or (null name) (zerop (length name)))
    (add-game name))
  (redirect "/retro-games"))
