;;; eglot-signature-eldoc-talkative.el --- Make Eglot make ElDoc echo docs -*- lexical-binding: t -*-

;; Copyright (C) 2024 Free Software Foundation, Inc.

;; Author: João Távora <joaotavora@gmail.com>,
;;         Mekeor Melire <mekeor@posteo.de>
;; Created: 2024
;; Homepage: https://codeberg.org/mekeor/emacs-eglot-signature-eldoc-talkative
;; Keywords: convenience, documentation, eglot, eldoc, languages, lsp
;; Maintainer: Mekeor Melire <mekeor@posteo.de>
;; Package-Requires: ((emacs "29.1") (eglot "1.16") (eldoc "1.14.0") (jsonrpc "1.0.23"))
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Version: 0.0.7

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see
;; <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Eglot does not instruct ElDoc to echo the field "documentation" of
;; LSP-objects "signature information" and "parameter information"
;; (into the echo-area).  To read those fields, the user instead has
;; to open the *eldoc* buffer by calling the command
;; `eldoc-doc-buffer' (bound to C-h . by default).

;; This package offers a function, `eglot-signature-eldoc-talkative',
;; that makes Eglot instruct ElDoc to echo the field "documentation"
;; of LSP-objects "signature information" and "parameter information"
;; (into the echo-area).

;;; Code:

(require 'eglot)
(require 'jsonrpc)
(require 'seq)

(defgroup eglot-signature-eldoc-talkative nil
  "Make Eglot make ElDoc echo more docs."
  :group 'eglot)

(defcustom eglot-signature-eldoc-talkative-separator "\n\n"
  "Separator used between different echoed information."
  :type 'string)

(defun eglot-signature-eldoc-talkative--sig-info (sig-help-sig &optional sig-help-active-param-i _)
  "A talkative alternative to `eglot--sig-info'.

Unlike the original, it additionally returns the documentation of
both the LSP-objects signature-information and
parameter-information.

Argument SIG-HELP-SIG should be a member of the field
`signatures' of a LSP-object `SignatureHelp' as declared in
variable `eglot--lsp-interface-alist'.

Optional argument SIG-HELP-ACTIVE-PARAM-I should be the value of
the field `activeParameter' of a LSP-object `SignatureHelp' as
declared in variable `eglot--lsp-interface-alist'."
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
             (concat eglot-signature-eldoc-talkative-separator
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
              ((`(,beg ,end)
                 (eglot--dbind
                   ((ParameterInformation)
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
                     (seq-map #'1+ param-info-label)))))
              (if (and beg end)
                (add-face-text-property
                  beg end
                  'eldoc-highlight-function-argument)))
            ;; ... insert its documentation, ...
            (when param-info-doc
              (insert eglot-signature-eldoc-talkative-separator
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
                (insert eglot-signature-eldoc-talkative-separator
                  (if (stringp param-info-doc) param-info-doc
                    (eglot--format-markup param-info-doc)))))
            sig-info-params)))
      (buffer-string))))

(defun eglot-signature-eldoc-talkative (cb)
  "A talkative alternative to `eglot-signature-eldoc-function'.

Just like the original, it's meant to be used as a member of
variable `eldoc-documentation-functions', for signatures.  Unlike
the original, it additionally echoes the documentation of both
the LSP-objects signature-information and parameter-information.

Argument CB should be a callback as described in the docstring of
the variable `eldoc-documentation-functions'."
  ;; We depend on eglot version 1.16 because here we use
  ;; `eglot-server-capable' which in that version replaced
  ;; `eglot--server-capable'.
  (when (eglot-server-capable :signatureHelpProvider)
    (let ((buf (current-buffer)))
      (jsonrpc-async-request
        (eglot--current-server-or-lose)
        :textDocument/signatureHelp
        (eglot--TextDocumentPositionParams)
        :success-fn
        (eglot--lambda
          ((SignatureHelp)
            ((:signatures sig-help-sigs))
            ((:activeSignature sig-help-active-sig))
            ((:activeParameter sig-help-active-param-i)))
          (eglot--when-buffer-window buf
            (if-let
              ((sig-help-active-sig
                 (and sig-help-sigs
                   (or
                     (and sig-help-active-sig
                       (seq--elt-safe
                         sig-help-sigs sig-help-active-sig))
                     ;; If sig-help-active-sig is out of range, try 0,
                     ;; as recommended by LSP specification:
                     ;; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#signatureHelp
                     (seq--elt-safe sig-help-sigs 0)))))
              (funcall cb
                (mapconcat
                  (lambda (sig)
                    (eglot-signature-eldoc-talkative--sig-info
                      sig sig-help-active-param-i))
                  sig-help-sigs
                  eglot-signature-eldoc-talkative-separator)
                :echo
                (eglot-signature-eldoc-talkative--sig-info
                  sig-help-active-sig sig-help-active-param-i t))
              (funcall cb
                (and sig-help-sigs
                  (mapconcat
                    #'eglot-signature-eldoc-talkative--sig-info
                    sig-help-sigs
                    eglot-signature-eldoc-talkative-separator))))))
        :deferred :textDocument/signatureHelp))
    t))

(provide 'eglot-signature-eldoc-talkative)

;;; eglot-signature-eldoc-talkative.el ends here
