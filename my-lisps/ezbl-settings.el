;; Copyright (C) 2010 ahei

;; Author: ahei <ahei0802@gmail.com>
;; URL: http://code.google.com/p/dea/source/browse/trunk/my-lisps/ezbl-settings.el
;; Time-stamp: <2010-12-02 19:56:04 Thursday by taoshanwen>

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

(setq ezbl-exec-path (or (executable-find "uzbl") "/usr/bin/uzbl"))

(require 'xwidget)
(require 'ezbl)

(eal-define-keys
 'ezbl-mode-map
 `(("F"     ezbl-command-forward)
   ("B"     ezbl-command-back)
   ("r"     ezbl-command-reload)
   ("SPC"   ezbl-command-scroll)
   ("M-g"   ezbl-command-stop)
   ("C-c <" ezbl-command-zoom_out)
   ("C-c >" ezbl-command-zoom_in)
   ("G"     ezbl-command-uri)
   ("C-k"   ezbl-command-exit)
   ("'"     switch-to-other-buffer)))

(defun ezbl-settings ()
  "Settings for `ezbl'."
  (defun ezbl-xwidget-resize-at (pos width height)
    "Resize xwidget at postion POS to WIDTH and HEIGHT.

There is no corresponding resize-id fn yet, because of display
property/xwidget id impedance mismatch."
    (let* ((xwidget-prop (cdr (get-text-property pos 'display)))
           (id (plist-get  xwidget-prop ':xwidget-id)))

      (setq xwidget-prop (plist-put xwidget-prop ':width width))
      (setq xwidget-prop (plist-put xwidget-prop ':height height))

      (put-text-property pos (+ 1 pos) 'display (cons 'xwidget xwidget-prop))
      (xwidget-resize-internal id width height))))

(eval-after-load "ezbl"
  `(ezbl-settings))

(provide 'ezbl-settings)
