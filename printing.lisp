;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;; ==========================================================================
;;;; printing.lisp --- This class simply prints all the data
;;;;
;;;; Copyright (c) 2011-2013, Nikhil Shetty <nikhil.j.shetty@gmail.com>
;;;;   All rights reserved.
;;;;
;;;; Redistribution and use in source and binary forms, with or without
;;;; modification, are permitted provided that the following conditions
;;;; are met:
;;;;
;;;;  o Redistributions of source code must retain the above copyright
;;;;    notice, this list of conditions and the following disclaimer.
;;;;  o Redistributions in binary form must reproduce the above copyright
;;;;    notice, this list of conditions and the following disclaimer in the
;;;;    documentation and/or other materials provided with the distribution.
;;;;  o Neither the name of the author nor the names of the contributors may
;;;;    be used to endorse or promote products derived from this software
;;;;    without specific prior written permission.
;;;;
;;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;; A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT
;;;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;;;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;;;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;;;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;;;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;;;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;; ==========================================================================

(in-package :x3d)

(defparameter *is-nmtoken* NIL)

;; ----------------------------------------------------------------------------
(defun draw-line(fd)
  (format 
   fd
   "~&;; ----------------------------------------------------------------------------~%"
   ))

;; ----------------------------------------------------------------------------
(defmethod get-superclass((self complexType))
  (if (complexContent self)
      (if (extension (complexContent self))
          (base (extension (complexContent self)))
          "")
      ""))

;; ----------------------------------------------------------------------------
(defmethod is-NMTOKEN((self complexType) fd)
  (with-slots (attributes) self
    (loop for item in attributes
         do (print-attribute item fd)))
  (if (complexContent self)
      (if (extension (complexContent self))
          (loop for item in (attributes (extension (complexContent self)))
               do (print-attribute item fd)))))

;; ----------------------------------------------------------------------------
(defmethod format-attributes((self complexType) fd)
  (with-slots (attributes) self
    (loop for item in attributes
         do (print-attribute item fd)))
  (if (complexContent self)
      (if (extension (complexContent self))
          (loop for item in (attributes (extension (complexContent self)))
               do (print-attribute item fd)))))

;; ----------------------------------------------------------------------------
(defmethod visit ((self schema))
  (with-slots (simpleTypes complexTypes elements) self
    (dolist (item simpleTypes)
      (visit item))
    (dolist (item complexTypes)
      (visit item))
    (dolist (item elements)
      (visit item))))

;; ----------------------------------------------------------------------------
(defmethod visit ((self simpleType))
  (let ((file-name *X3D-SIMPLE-TYPES*))
    (if file-name
        (with-open-file (fd file-name :direction :output :if-exists :append)
          (draw-line fd)
          (format fd "~&(defclass ~a ()~%  ()~%  (:documentation \"\"))~%~%" (name self))))))

;; ----------------------------------------------------------------------------
(defmethod visit ((self complexType))
  (let ((file-name (gethash (string-upcase (name self)) *X3D-CLASS-TO-FILE-MAP*)))
    (if file-name
        (with-open-file (fd file-name :direction :output :if-exists :append)
          (draw-line fd)
          (format fd "~&(defclass ~a (~a)~%"
                  (name self)
                  (get-superclass self))
          (format fd "~&  (~%")
          (format-attributes self fd)
          (format fd "~&  )~%")
          (format fd "~&  (:documentation \"\"))~%~%")
          (format-add-subobject (name self) fd))
        (format t "~& NOT FOUND: ~a ~%" (name self)))))

;; ----------------------------------------------------------------------------
(defun format-add-subobject (name fd)
  (when *is-nmtoken*
      (progn 
        (draw-line fd)
        (format fd "~&(defmethod add-subobject ((self ~a) (stuff X3DNode))~%" name)
        (format fd "~&   (add-object-to-slot self stuff 'containerField))~%")
        (format fd "~&~%")
        (setf *is-nmtoken* nil))))

;; ----------------------------------------------------------------------------
(defmethod visit ((self element))
  (let ((file-name (gethash (string-upcase (name self)) *X3D-CLASS-TO-FILE-MAP*)))
    (if file-name
        (with-open-file (fd file-name :direction :output :if-exists :append)
          (draw-line fd)
          ;; (format fd "~&~a~%" (gethash (string-upcase (name self))
          ;;                              *X3D-CLASS-TO-FILE-MAP*))
          (format fd "~&(defclass ~a (~a)~%"
                  (name self)
                  (get-superclass (complexType self)))
          (format fd "~&  (~%")
          (format-attributes (complexType self) fd)
          (format fd "~&  )~%")
          (format fd "~&  (:documentation \"\"))~%~%")
          (format-add-subobject (name self) fd))
        (format t "~& NOT FOUND: ~a ~%" (name self)))))

;; ----------------------------------------------------------------------------
(defmethod print-attribute ((self attribute) fd)
  (if (string= (type self) "xs:NMTOKEN")
      (setf *is-nmtoken* t))
  (if (string= (type self) "xs:NMTOKEN")
      (format fd "~&    (~a~%" (name self))
      (format fd "~&    (~a :initarg ~a~%" (name self) (concatenate 'string ":" (name self))))
  (if (type self)
      (if (string= (type self) "xs:NMTOKEN")
          (format fd "~&        :initform NIL~%")
          (format fd "~&        :initform ~a~%" (if (string= (type self) "MFString")
                                                    (concatenate 'string " `(" (default self) ")")
                                                    (concatenate 'string " \"" (default self) "\""))))
      (format fd "~&        :initform ~a~%" (when (default self) (concatenate 'string " \"" (default self) "\""))))
  (format fd "~&        :accessor ~a~%" (name self))
  ;; for now we will disable this field. This is an optimization which will be revisited -nix
  ;; (when (type self)
  ;;     (if (string= (type self) "xs:NMTOKEN")
  ;;         (format fd "~&        :type X3DNode~%")
  ;;         (format fd "~&        :type ~a~%" (type self))))
  (format fd "~&        :documentation \"\")~%"))



  ;; (format fd "~&    (~a :initarg ~a
  ;;                      :initform ~a
  ;;                      :accessor ~a
  ;;                      :type ~a
  ;;                      :documentation \"\")~%"
  ;;                      (name self)
  ;;                      (concatenate 'string ":" (name self))
  ;;                      (when (default self)  (concatenate 'string "(" (type self) " " (default self) ")"))
  ;;                      (name self)
  ;;                      (type self)))







;; (defmethod print-object ((self schema) s)
;;   "printing the schema"
;;   (with-slots(simpleTypes) self
;;     (loop for item in simpleTypes
;;          do (print-object item))))


;; (defmethod print-object ((self complexType) s)
;;   (with-slots (name
;;                abstract
;;                mixed
;;                annotation
;;                complexContent
;;                attributes
;;                sequence-cl
;;                attributeGroups) self
;;     (format s name)
;;     (format s abstract)))


;; (defmethod print-object ((self element) s)
;;   (with-slots (file
;;                ref) self
;;     (format s ":file ~s" file)
;;     (format s ":ref ~s" ref)))
  
