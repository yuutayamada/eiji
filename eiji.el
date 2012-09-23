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

(defun eiji:decide-source-word ()
  (if mark-active
      (buffer-substring-no-properties
       (region-beginning) (region-end))
    (if current-prefix-arg
        (read-string "Search word: " eiji:search-word)
      (loga-singularize (loga-return-word-on-cursor)))))

(defun eiji:format (type word &optional dict)
  (lexical-let*
      ((file
        (case dict
          (:eiji  eiji:search-path-eiji)
          (:reiji eiji:search-path-reiji)
          (:ryaku eiji:search-path-ryaku)
          (:waei  eiji:search-path-waei)
          (t      eiji:search-path-eiji)))
       (word-and-regexp
        (case type
          (:normal
           (concat "\"^■" word " \\+\\({.\\+\\)\\?:\" "))
          (:global
           (concat "\"^■.\\+" word ".\\+ \\+\\({.\\+\\)\\?:\" ")))))
    (concat "\\grep " word-and-regexp file)))

(defun eiji:concat-commands (word order)
  (lexical-let*
      ((striped-word (stem:stripping-inflection word))
       (format
        (lambda (dict)
          (concat
           (eiji:format :normal word dict)  " \|\| "
           (eiji:format :normal striped-word dict) " \|\| "
           (eiji:format :global word dict)  " \|\| "
           (eiji:format :global striped-word dict)))))
    (mapconcat 'identity
               (loop for dict in order
                     collect (funcall format dict))
               " \|\| ")))

(defun* eiji:search (&optional search-word &key popwin)
  (interactive)
  (lexical-let* ((word (or search-word (eiji:decide-source-word)))
                 (command
                  (eiji:concat-commands word '(:eiji :reiji :ryaku)))
                 (width  (if (one-window-p)
                             (/ (window-width) 2)
                           (window-width))))
    (setq eiji:search-word word)
    (if popwin
        (popwin:popup-buffer
         (get-buffer-create "*EIJIRO*")
         :noselect t :stick t :height 10 :position :top))
    (save-current-buffer
      (with-temp-buffer
        (async-shell-command command "*EIJIRO*")))))

(provide 'eiji)
