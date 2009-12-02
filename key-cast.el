;;; key-cast.el --- Key comannd casting interface.

;; Filename: key-cast.el
;; Description: Key comannd casting interface.
;; Author: Kenichirou Oyama <k1lowxb@gmail.com>
;; Maintainer: Kenichirou Oyama <k1lowxb@gmail.com>
;; Copyright (C) 2009, 101000Kenichirou, all rights reserved.
;; Created: 2009-01-29 18:19:38
;; Version: 0.0.1
;; URL: http://trac.codecheck.in
;;    : http://code.101000lab.org
;; Keywords: key log command KeyCaster
;; Compatibility: GNU Emacs 22 ~
;;
;; Features that might be required by this library:
;;
;; `cl'
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; This package cast key command.
;;
;;; Installation:
;;
;; Put key-cast.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'key-cast)
;; (global-key-cast t)
;;
;;
;; If you casting key command like KeyCaster (Mac Application),
;; you set cast-function `key-cast-cast-func'.
;;
;; For example. if you use notify-send command, set like this:
;; (setq key-cast-cast-func
;;  'key-cast-notify-send)
;;

;;; Commands:
;;
;; Below are complete command list:
;;
;;  `key-cast'
;;    Key cast minor mode.
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;

;;; Change log:
;;
;; 2009/04/24
;;   * Kenichirou Oyama:
;;      * First released.

;;; TODO:
;; * Fix minor-mode-line bug

;;; Code:

;;require
(require 'cl)
(require 'easy-mmode)

(defgroup key-cast nil
  "Key cast minor mode"
  :group 'convenience
  :prefix "key-")

(defvar key-cast-global nil)

(defvar key-cast-cast-func
  "cast fuction."
  'key-cast-header-line)

(define-minor-mode key-cast
  "Key cast minor mode."
  :lighter " Key"
  :group 'key-cast
  (if key-cast
      (progn
        (setq key-cast-global t)
        (add-hook 'pre-command-hook 'key-cast-cast-keys nil nil)
        )
    (setq key-cast-global nil)
    (remove-hook 'pre-command-hook 'key-cast-cast-keys nil)))

(if (fboundp 'define-global-minor-mode)
    (define-global-minor-mode global-key-cast
      key-cast key-cast-maybe
      :group 'key-cast))

(defun key-cast-maybe ()
  "What buffer `key-cast' prefers."
  ;;(message (concat "maybe:" (format "%s" key-cast-global)))
  (if key-cast-global
      (key-cast 1)
    (key-cast -1)))

(defun key-cast-cast-keys ()
  "Cast key command string to `key-cast-cast-func'."
  (let ((keys (this-command-keys)) (str nil))
    (if (stringp keys)
        (mapcar (function (lambda (s)
                            (setq str (concat str
                                              (cdr (nth (string-to-char s) key-cast-ascii-map))))
                            ))
                (delete "" (split-string keys "")))
      (setq str (format "%s" keys)))
    (fset 'func (symbol-function key-cast-cast-func))
    (func str)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Cast Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; notify-send
(defun key-cast-notify-send (str)
  "Key cast to notify-send."
  (call-process-shell-command
   (concat "notify-send -t 1000 'key-cast' '<span size=\"xx-large\">" str "</span>'")))

;;; message
(defun key-cast-message (str)
  "Key cast to message command."
  (message "%s" str))


;;; header-line-format
(defvar key-cast-header-list nil)

(defun key-cast-header-line (str)
  (delete "" (push str key-cast-header-list))
  (key-cast-header-line-set)
  (run-with-timer 1.0 nil 'key-cast-header-refresh))

(defun key-cast-header-line-set()
  (let ((display-string nil))
    (if (not key-cast-header-list)
        (setq-default header-line-format nil)
      (setq display-string (apply 'concat (reverse key-cast-header-list)))
      (setq-default header-line-format display-string))))

(defun key-cast-header-refresh ()
  (setq-default header-line-format nil)
  (force-mode-line-update t)
  (pop key-cast-header-list)
  (key-cast-header-line-set))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Key Map ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar key-cast-ascii-map
  '((0 . "C-@ ")     ;;^@
    (1 . "C-a ")     ;;^A
    (2 . "C-b ")     ;;^B
    (3 . "C-c ")     ;;^C
    (4 . "C-d ")     ;;^D
    (5 . "C-e ")     ;;^E
    (6 . "C-f ")     ;;^F
    (7 . "C-g ")     ;;^G
    (8 . "C-h ")     ;;^H
    (9 . "C-i")      ;;TAB ^I
    (10 . "C-j ")    ;;\n ^J
    (11 . "C-k ")    ;;^K
    (12 . "C-l ")    ;;^L
    (13 . "C-m ")    ;;RET ^M
    (14 . "C-n ")    ;;^N
    (15 . "C-o ")    ;;^O
    (16 . "C-p ")    ;;^P
    (17 . "C-q ")    ;;^Q
    (18 . "C-r ")    ;;^R
    (19 . "C-s ")    ;;^S
    (20 . "C-t ")    ;;^T
    (21 . "C-u ")    ;;^U
    (22 . "C-v ")    ;;^V
    (23 . "C-w ")    ;;^W
    (24 . "C-x ")    ;;^X
    (25 . "C-y ")    ;;^Y
    (26 . "C-z ")    ;;^Z
    (27 . "M-")      ;;^[
    (28 . "C-\\ ")   ;;^\
    (29 . "C-] ")    ;;^]
    (30 . "C-^ ")    ;;^^
    (31 . "C-_ ")    ;;^_
    (32 . " ")
    (33 . "!")
    (34 . "\"")
    (35 . "#")
    (36 . "$")
    (37 . "%")
    (38 . "&")
    (39 . "'")
    (40 . "(")
    (41 . ")")
    (42 . "*")
    (43 . "+")
    (44 . ",")
    (45 . "-")
    (46 . ".")
    (47 . "/")
    (48 . "0")
    (49 . "1")
    (50 . "2")
    (51 . "3")
    (52 . "4")
    (53 . "5")
    (54 . "6")
    (55 . "7")
    (56 . "8")
    (57 . "9")
    (58 . ":")
    (59 . ";")
    (60 . "<")
    (61 . "=")
    (62 . ">")
    (63 . "?")
    (64 . "@")
    (65 . "A")
    (66 . "B")
    (67 . "C")
    (68 . "D")
    (69 . "E")
    (70 . "F")
    (71 . "G")
    (72 . "H")
    (73 . "I")
    (74 . "J")
    (75 . "K")
    (76 . "L")
    (77 . "M")
    (78 . "N")
    (79 . "O")
    (80 . "P")
    (81 . "Q")
    (82 . "R")
    (83 . "S")
    (84 . "T")
    (85 . "U")
    (86 . "V")
    (87 . "W")
    (88 . "X")
    (89 . "Y")
    (90 . "Z")
    (91 . "[")
    (92 . "\\")
    (93 . "]")
    (94 . "^")
    (95 . "_")
    (96 . "`")
    (97 . "a")
    (98 . "b")
    (99 . "c")
    (100 . "d")
    (101 . "e")
    (102 . "f")
    (103 . "g")
    (104 . "h")
    (105 . "i")
    (106 . "j")
    (107 . "k")
    (108 . "l")
    (109 . "m")
    (110 . "n")
    (111 . "o")
    (112 . "p")
    (113 . "q")
    (114 . "r")
    (115 . "s")
    (116 . "t")
    (117 . "u")
    (118 . "v")
    (119 . "w")
    (120 . "x")
    (121 . "y")
    (122 . "z")
    (123 . "{")
    (124 . "|")
    (125 . "}")
    (126 . "~")
    (127 . "^?")
    (128 . "\200")
    (129 . "\201")
    (130 . "\202")
    (131 . "\203")
    (132 . "\204")
    (133 . "\205")
    (134 . "\206")
    (135 . "\207")
    (136 . "\210")
    (137 . "\211")
    (138 . "\212")
    (139 . "\213")
    (140 . "\214")
    (141 . "\215")
    (142 . "\216")
    (143 . "\217")
    (144 . "\218")
    (145 . "\221")
    (146 . "\222")
    (147 . "\223")
    (148 . "\224")
    (149 . "\225")
    (150 . "\226")
    (151 . "\227")
    (152 . "\230")
    (153 . "\231")
    (154 . "\232")
    (155 . "\233")
    (156 . "\234")
    (157 . "\235")
    (158 . "\236")
    (159 . "\237")
    (160 . "��")
    (161 . "��")
    (162 . "���")
    (163 . "���")
    (164 . "��")
    (165 . "��")
    (166 . "��")
    (167 . "���")
    (168 . "���")
    (169 . "��")
    (170 . "��")
    (171 . "��")
    (172 . "���")
    (173 . "��")
    (174 . "��")
    (175 . "��")
    (176 . "���")
    (177 . "���")
    (178 . "��")
    (179 . "��")
    (180 . "���")
    (181 . "��")
    (182 . "���")
    (183 . "��")
    (184 . "��")
    (185 . "��")
    (186 . "��")
    (187 . "��")
    (188 . "��")
    (189 . "��")
    (190 . "��")
    (191 . "��")
    (192 . "��")
    (193 . "��")
    (194 . "��")
    (195 . "��")
    (196 . "��")
    (197 . "��")
    (198 . "��")
    (199 . "��")
    (200 . "��")
    (201 . "��")
    (202 . "��")
    (203 . "��")
    (204 . "��")
    (205 . "��")
    (206 . "��")
    (207 . "��")
    (208 . "��")
    (209 . "��")
    (210 . "��")
    (211 . "��")
    (212 . "��")
    (213 . "��")
    (214 . "��")
    (215 . "���")
    (216 . "��")
    (217 . "��")
    (218 . "��")
    (219 . "��")
    (220 . "��")
    (221 . "��")
    (222 . "��")
    (223 . "��")
    (224 . "��")
    (225 . "��")
    (226 . "��")
    (227 . "��")
    (228 . "��")
    (229 . "��")
    (230 . "��")
    (231 . "��")
    (232 . "��")
    (233 . "��")
    (234 . "��")
    (235 . "��")
    (236 . "��")
    (237 . "��")
    (238 . "��")
    (239 . "��")
    (240 . "��")
    (241 . "��")
    (242 . "��")
    (243 . "��")
    (244 . "��")
    (245 . "��")
    (246 . "��")
    (247 . "���")
    (248 . "��")
    (249 . "��")
    (250 . "��")
    (251 . "��")
    (252 . "��")
    (253 . "��")
    (254 . "��")
    (255 . "��")
    ))

(provide 'key-cast)

;;; key-cast.el ends here