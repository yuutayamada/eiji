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
;; --EIJIRO Search--
;; Requirement:
;; converted EIJI-version-no.txt file of EIJIRO data
;; execute below command from shell
;; cd /path/to/EIJIRO/
;; mkdir -p ./utf8
;; nkf -w8 ./EIJI-135.TXT ./utf8/EIJI-135.txt

(eval-when-compile (require 'cl))
(require 'logalimacs)
(require 'thingatpt)

(defvar eiji:search-path "")
(defvar eiji:search-word "")

(defun eiji:decide-source-word ()
  (if mark-active
      (buffer-substring-no-properties
       (region-beginning) (region-end))
    (if current-prefix-arg
        (read-string "Search word: " eiji:search-word)
      (loga-singularize (loga-return-word-on-cursor)))))

(defun eiji:format (type word)
  (let* ((file eiji:search-path))
    (case type
      (:normal
       (concat "\\grep " "\"^■" word " \\+\\({.\\+\\)\\?:\"" " " file))
      (:global
       (concat "\\grep " "\"^■.\\+" word ".\\+ \\+\\({.\\+\\)\\?:\"" " " file)))))

(defun eiji:search ()
  (interactive)
  (let* ((word
          (if (equal (eiji:decide-source-word) eiji:search-word)
              (stem:stripping-inflection eiji:search-word)
            (eiji:decide-source-word)))
         (striped-word (stem:stripping-inflection eiji:search-word))
         (command
          (concat
           (eiji:format :normal word)  " \|\| "
           (eiji:format :normal striped-word) " \|\| "
           (eiji:format :global word)  " \|\| "
           (eiji:format :global striped-word)))
         (width  (if (one-window-p)
                     (/ (window-width) 2)
                   (window-width)))
         (base     (current-buffer)))
    (setq eiji:search-word word)
    (save-current-buffer
      (with-temp-buffer
        (async-shell-command command "*EIJIRO*")))))

(provide 'eiji)
