;;; chatgpt.el --- Emacs ChatGPT Mode -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Alessandro Wollek
;;
;; Author: Alessandro Wollek <contact@wollek.ai>
;; Homepage: https://github.com/emacs-chatgpt/emacs-chatgpt
;; SPDX-License-Identifier: GPL-3.0-only
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Demo implementation of an Emacs ChatGPT mode.
;;  Full, ready-to-use version at
;;  https://github.com/SFTtech/emacs-elaiza.
;;
;;; Code:
(require 'json)
(require 'markdown-mode)
(require 'url)

(defgroup chatgpt nil
  "Your own ChatGPT."
  :group 'external)

(defcustom chatgpt-api-key nil
  "OpenAI API key. See https://platform.openai.com/account/api-keys."
  :group 'chatgpt
  :type 'string)

(defun chatgpt-get-api-key ()
  "Get OpenAI API key from auth-source, create if needed."
  (let* ((auth-source-creation-defaults
          '((description . "OpenAI API key")))
         (auth-source-creation-prompts
          '((secret . "OpenAI API key for %h: ")))
         (auth-info (nth 0 (auth-source-search
                            :max 1
                            :host "api.openai.com"
                            :user "chatgpt"))))
    (if auth-info (auth-info-password auth-info)
      (error "Could not retrieve API key\nSave machine api.openai.com port https login chatgpt password <your-api-key> in ~/.authinfo.gpg"))))

(defvar-keymap chatgpt-mode-map
  :parent markdown-mode-map
  "C-c RET" #'chatgpt-continue-conversation)

(define-derived-mode chatgpt-mode markdown-mode "ChatGPT"
  "Major mode for interacting with ChatGPT.")

(defun chatgpt (prompt)
  "Chat with ChatGPT."
  (interactive "sPrompt: \n")
  (switch-to-buffer (get-buffer-create "*ChatGPT*"))
  (erase-buffer)
  (chatgpt-mode)
  (insert  "> User\n" prompt)
  (add-hook 'after-change-functions
            #'chatgpt--mark-user-input
            nil
            t)
  (chatgpt-request `[((role . user)
                      (content . ,prompt))]))

(defun chatgpt-continue-conversation ()
  "Continue ChatGPT conversation inside `*ChatGPT*' buffer."
  (interactive)
  (with-current-buffer (get-buffer "*ChatGPT*")
    (let ((messages (chatgpt--split-by-role)))
      (chatgpt-request messages))))

(defun chatgpt-request (messages)
  "Request to ChatGPT.
API documentation:
https://platform.openai.com/docs/api-reference/chat/create"
  (let ((url-request-method "POST")
        (url-request-extra-headers `(("Content-Type" . "application/json")
                                     ("Authorization" . ,(concat "Bearer " (if chatgpt-api-key
                                                                               chatgpt-api-key
                                                                             (chatgpt-get-api-key))))))
        (url-request-data
         (encode-coding-string
          (json-encode `((model . gpt-4o)
                         (messages . ,messages)))
          'utf-8)))
    (url-retrieve
     "https://api.openai.com/v1/chat/completions"
     (lambda (_status)
       (let ((response (chatgpt--parse-response)))
         (with-current-buffer (get-buffer "*ChatGPT*")
           (add-text-properties 0
                                (length response)
                                '(chatgpt-role assistant)
                                response)
           (insert "\n\n> Assistant\n"
                   response
                   "\n\n> User\n")
           (goto-char (point-max))))))))

(defun chatgpt--parse-response ()
  "Parse and return the ChatGPT response. See `chatgpt-request' for details."
  (goto-char (point-min))
  (re-search-forward "\n\n")
  (let* ((response (buffer-substring-no-properties
                    (point) (point-max)))
         (json-object (json-read-from-string response))
         (choices (cdr (assoc 'choices json-object))))
    (decode-coding-string
     (cdr (assoc 'content
                 (cdr (assoc 'message
                             (aref choices 0))))) 'utf-8)))

(defun chatgpt--mark-user-input (beg end _length)
  "Mark role using `after-change-functions' hook."
  (unless (text-property-any beg end 'chatgpt-role 'assistant)
    (add-text-properties beg end '(chatgpt-role user))))

(defun chatgpt--split-by-role ()
  "Split `*ChatGPT*' buffer by `chatgpt-role'."
  (with-current-buffer (get-buffer "*ChatGPT*")
    (let ((result '())
          match)
      (save-excursion
        (goto-char (point-max))
        (while
            (setq match
                  (text-property-search-backward
                   'chatgpt-role))
          (push (list
                 (cons :role (prop-match-value match))
                 (cons :content (buffer-substring-no-properties
                                 (prop-match-beginning match)
                                 (prop-match-end match))))
                result)))
      result)))
