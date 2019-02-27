;;;; trivial-text-to-speech.asd

(asdf:defsystem #:trivial-text-to-speech
  :description "Easy Text to speech for streaming."
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (:hunchentoot
               :easy-routes
               :mailbox
               :cl-who
               :css-lite
               :parenscript)
  :components ((:file "trivial-text-to-speech")))
