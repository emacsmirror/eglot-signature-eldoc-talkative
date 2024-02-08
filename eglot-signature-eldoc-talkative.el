;;; eglot-signature-eldoc-talkative.el --- Make Eglot make Eldoc echo docs -*- lexical-binding: t -*-

;; Copyright (C) 2024 Free Software Foundation, Inc.

;; Author: João Távora <joaotavora@gmail.com>, Mekeor Melire <mekeor@posteo.de>
;; Created: 2024
;; Homepage: https://codeberg.org/mekeor/emacs-eglot-signature-eldoc-talkative
;; Keywords: convenience, documentation, eglot, eldoc, languages, lsp
;; Maintainer: Mekeor Melire <mekeor@posteo.de>
;; Package-Requires: (emacs eglot)
;; Version: 0.0.1

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see
;; <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Eglot does not instruct Eldoc to echo the field "documentation" of
;; LSP-objects "signature information" and "parameter information"
;; (into the echo-area). To read those fields, the user instead has to
;; open the *eldoc* buffer by calling the command `eldoc-doc-buffer'
;; (bound to C-h . by default).

;; This package offers a function, `eglot-signature-eldoc-talkative',
;; that makes Eglot instruct Eldoc to echo the field "documentation"
;; of LSP-objects "signature information" and "parameter information"
;; (into the echo-area).

;;; Code:

(defun eglot-signature-eldoc-talkative--sig-info
  (sig-help-sig &optional sig-help-active-param-i briefp)
  (eglot--dbind
    ((SignatureInformation)
      ((:label sig-info-label))
      ((:documentation sig-info-doc))
      ((:parameters sig-info-params))
      ((:activeParameter sig-info-active-param-i)))
    sig-help-sig
    (with-temp-buffer
      ;; Insert label of signature
      (insert sig-info-label)
      ;; Attempt to naively highlight syntax of signature's label as
      ;; <name>(<params>)
      (save-excursion
        (goto-char (point-min))
        (when (looking-at "\\([^(]*\\)(\\([^)]+\\))")
          (add-face-text-property (match-beginning 1) (match-end 1)
            'font-lock-function-name-face)))
      (let
        ((sig-info-doc-formatted
           (when sig-info-doc
             (concat "\n\n"
               (if (stringp sig-info-doc)
                 sig-info-doc (eglot--format-markup sig-info-doc))))))
        ;; If there is an active parameter, ...
        (if-let
          ((active-param
             (seq--elt-safe sig-info-params
               (or sig-help-active-param-i sig-info-active-param-i))))
          (eglot--dbind
            ((ParameterInformation) ((:documentation param-info-doc)))
            active-param
            ;; then try to highlight its label, ...
            (pcase-let
              ((`(,beg . ,end)
                 (eglot--dbind ((ParameterInformation)
                                 ((:label param-info-label)))
                   active-param
                   (if (stringp param-info-label)
                     (let ((case-fold-search nil))
                       (save-excursion
                         (goto-char (point-min))
                         (and
                           (search-forward param-info-label
                             (line-end-position) t)
                           (cons (match-beginning 0) (match-end 0)))))
                     (mapcar #'1+ param-info-label)))))
              (if (and beg end)
                (add-face-text-property
                  beg end
                  'eldoc-highlight-function-argument)))
            ;; ... insert its documentation, ...
            (when param-info-doc
              (insert "\n\n"
                (propertize
                  (if (stringp param-info-doc) param-info-doc
                    (eglot--format-markup param-info-doc))
                  'face
                  (list :foreground
                    (face-attribute
                      'eldoc-highlight-function-argument
                      :foreground)))))
            ;; ... and finally, insert signature's documentation.
            (when sig-info-doc-formatted
              (insert sig-info-doc-formatted)))
          ;; But if there's no active parameter, first insert
          ;; signature's documentation, ...
          (when sig-info-doc-formatted
            (insert sig-info-doc-formatted))
          ;; and finally insert all parameters' documentation.
          (seq-do
            (eglot--lambda
              ((ParameterInformation)
                ((:documentation param-info-doc)))
              (when param-info-doc
                (insert "\n\n"
                  (if (stringp param-info-doc) param-info-doc
                    (eglot--format-markup param-info-doc)))))
            sig-info-params)))
      (buffer-string))))

(defun eglot-signature-eldoc-talkative (cb)
  "A member of `eldoc-documentation-functions', for signatures."
  (when (eglot--server-capable :signatureHelpProvider)
    (let ((buf (current-buffer)))
      (jsonrpc-async-request
        (eglot--current-server-or-lose)
        :textDocument/signatureHelp
        (eglot--TextDocumentPositionParams)
        :success-fn
        (eglot--lambda
          ((SignatureHelp)
            signatures active-sig active-param)
          (eglot--when-buffer-window buf
            (if-let
              ((active-sig
                 (or
                   (seq--elt-safe signatures active-sig)
                   ;; if active-sig is out of range, try 0, as
                   ;; recommended by LSP specification.
                   (seq--elt-safe signatures 0))))
              (funcall cb
                (mapconcat
                  (lambda (sig)
                    (eglot-signature-eldoc-talkative--sig-info
                      active-sig active-param))
                  signatures "\n\n")
                :echo
                (eglot-signature-eldoc-talkative--sig-info
                  active-sig active-param t))
              (funcall cb
                (and signatures
                  (mapconcat
                    #'eglot-signature-eldoc-talkative--sig-info
                    signatures "\n\n"))))))
        :deferred :textDocument/signatureHelp))
    t))

(provide 'eglot-signature-eldoc-talkative)

;;; eglot-signature-eldoc-talkative.el ends here
