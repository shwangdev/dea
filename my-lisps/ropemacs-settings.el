;; Copyright (C) 2010 ahei

;; Author: ahei <ahei0802@gmail.com>
;; URL: http://code.google.com/p/dea/source/browse/trunk/my-lisps/ropemacs-settings.el
;; Time-stamp: <2011-03-20 17:02:14 Sunday by taoshanwen>

;; This  file is free  software; you  can redistribute  it and/or
;; modify it under the terms of the GNU General Public License as
;; published by  the Free Software Foundation;  either version 3,
;; or (at your option) any later version.

;; This file is  distributed in the hope that  it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR  A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You  should have  received a  copy of  the GNU  General Public
;; License along with  GNU Emacs; see the file  COPYING.  If not,
;; write  to  the Free  Software  Foundation,  Inc., 51  Franklin
;; Street, Fifth Floor, Boston, MA 02110-1301, USA.

(require 'pymacs)
(pymacs-load "ropemacs" "rope-")

(defun ropemacs-settings ()
  "Settings for `ropemacs'."
  (setq ropemacs-enable-autoimport t)

  (defun ropemacs-settings-4-emaci ()
    "`ropemacs' settings for `emaci'."  
    (emaci-add-key-definition
     "." 'rope-goto-definition
     '(memq major-mode dev-modes)))

  (eval-after-load "emaci"
    `(ropemacs-settings-4-emaci)))

(eval-after-load "python"
  `(ropemacs-settings))

(provide 'ropemacs-settings)
