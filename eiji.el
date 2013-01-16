;;; -*- coding: utf-8 -*-
;;; eiji.el --- EIJIRO search from Emacs

;; Copyright (C) 2012 by Yuta Yamada
;; Author: Yuta Yamada <cokesboy"at"gmail.com>

;;; License:
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; ...WIP...

;;; Requirement:
;; converted EIJI-version-no.txt file of EIJIRO data to UTF-8
;; execute below command from shell
;; cd /path/to/EIJIRO/
;; mkdir -p ./utf8
;; nkf -w8 ./EIJI-135.TXT ./utf8/EIJI-135.txt

(eval-when-compile (require 'cl))
(require 'logalimacs)
(require 'thingatpt)
(require 'popwin)

(defvar eiji:search-path-eiji "")
(defvar eiji:search-path-reiji "")
(defvar eiji:search-path-ryaku "")
(defvar eiji:search-path-waei "")
(defvar eiji:search-word "")
(defvar eiji:popwin-buffer-list '())

(defun eiji:decide-source-word ()
  (if mark-active
      (buffer-substring-no-properties
       (region-beginning) (region-end))
    (if current-prefix-arg
        (read-string "Search word: " eiji:search-word)
      (loga-singularize (loga-return-word-on-cursor)))))

(defun eiji:format (type word &optional dict)
  (lexical-let*
      ((file-name
        (case dict
          (:eiji  eiji:search-path-eiji)
          (:reiji eiji:search-path-reiji)
          (:ryaku eiji:search-path-ryaku)
          (:waei  eiji:search-path-waei)
          (t      eiji:search-path-eiji)))
       (word-and-regexp
        (case type
          (:single
           (concat "\"^■"     word " \\+\\({.\\+\\)\\?: \" "))
          (:global
           (concat "\"^■.\\+" word ".\\+ \\+\\({.\\+\\)\\?: \" ")))))
    (if (string< "" file-name)
        (concat "\\grep " word-and-regexp file-name)
      (error (format "Dictionary file path is noting for %s.
set file path to eiji:search-path-eiji, reiji, ryaku, and waei."
                     (symbol-name dict))))))

(defun eiji:concat-commands (word order)
  (lexical-let*
      ((stem-word (stem:stripping-inflection word))
       (format
        (lambda (dict)
          (eiji:concat
           (list
            (when (eiji:capital-p word)
              (eiji:format :single (downcase word) dict))
            (eiji:format :single word      dict)
            (eiji:format :single stem-word dict)
            (eiji:format :global word      dict)
            (eiji:format :global stem-word dict))))))
    (eiji:concat
     (loop for dict in order
           collect (funcall format dict)))))

(defun eiji:concat (list)
  (lexical-let ((squash (lambda (commands)
                          (loop for command in commands
                                unless (null command)
                                collect command))))
    (mapconcat 'identity (funcall squash list) " \|\| ")))

(defun* eiji:search (&optional search-word &key popwin)
  (interactive)
  (lexical-let* ((word
                  (or search-word (eiji:decide-source-word)))
                 (dictionary
                  (if (or (equal eiji:search-word word) current-prefix-arg)
                      (eiji:query)
                    '(:eiji :reiji :ryaku)))
                 (command
                  (if (loga-japanese-p word)
                      (eiji:concat-commands word '(:waei))
                    (eiji:concat-commands word dictionary)))
                 (width
                  (if (one-window-p)
                      (/ (window-width) 2)
                    (window-width))))
    (setq eiji:search-word word)
    (when (or popwin (eiji:popwin-buffer-p))
      (popwin:popup-buffer
       (get-buffer-create "*EIJIRO*")
       :noselect t :stick t :height 10 :position :top))
    (save-current-buffer
      (with-temp-buffer
        (async-shell-command command "*EIJIRO*")))))

(defun eiji:popwin-buffer-p ()
  (loop for buffer in eiji:popwin-buffer-list
        if (string-match buffer (buffer-name))
        do (return t)))

(defun eiji:query ()
  (lexical-let*
      ((command-list `((?m . :eiji)
                       (?e . :reiji)
                       (?a . :ryaku)))
       (event (read-event "M)eaning E)xample A)bbreviation"))
       (dictionary (assoc-default event command-list)))
    `(,dictionary)))

(defun eiji:capital-p (word)
  (string-match "[A-Z]"
                (char-to-string (aref word 0))))

(provide 'eiji)
