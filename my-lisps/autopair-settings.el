;; -*- Emacs-Lisp -*-

;; Time-stamp: <2011-06-06 17:52:54 Monday by taoshanwen>

(require 'autopair)
(require 'util)

(defun autopair-settings ()
  "settings for `autopair'."
  (define-minor-mode autopair-mode
    "Automagically pair braces and quotes like in TextMate."
    nil " pair" nil
    (cond (autopair-mode
           ;; Setup the dynamic emulation keymap
           ;;
           (let ((map (make-sparse-keymap)))
             (define-key map [remap delete-backward-char] 'autopair-backspace)
             (define-key map [remap backward-delete-char-untabify] 'autopair-backspace)
             (define-key map (kbd "<backspace>") 'autopair-backspace)
             (define-key map [backspace] 'autopair-backspace)
             (define-key map (kbd "DEL") 'autopair-backspace)
             (define-key map (kbd "RET") 'autopair-newline)
             (dotimes (char 256) ;; only searches the first 256 chars,
               ;; TODO: is this enough/toomuch/stupid?
               (unless (member char
                               (getf autopair-dont-pair :never))
                 (let* ((syntax-entry (aref (syntax-table) char))
                        (class (and syntax-entry
                                    (syntax-class syntax-entry)))
                        (pair (and syntax-entry
                                   (cdr syntax-entry))))
                   (cond ((eq class (car (string-to-syntax "(")))
                          (define-key map (string char) 'autopair-insert-opening)
                          (when pair
                            (define-key map (string pair) 'autopair-skip-close-maybe)))
                         ((eq class (car (string-to-syntax "\"")))
                          (define-key map (string char) 'autopair-insert-or-skip-quote))))))
             ;; read `autopair-extra-pairs'
             (dolist (pairs-list (remove-if-not #'listp autopair-extra-pairs))
               (dolist (pair pairs-list)
                 (define-key map (string (car pair)) 'autopair-extra-insert-opening)
                 (define-key map (string (cdr pair)) 'autopair-extra-skip-close-maybe)))

             (setq autopair-emulation-alist (list (cons t map))))

           (setq autopair-action nil)
           (add-hook 'emulation-mode-map-alists 'autopair-emulation-alist nil)
           (add-hook 'post-command-hook 'autopair-post-command-handler 'append 'local))
          (t
           (setq autopair-emulation-alist nil)
           (remove-hook 'emulation-mode-map-alists 'autopair-emulation-alist)
           (remove-hook 'post-command-hook         'autopair-post-command-handler 'local))))

  ;; After do this, isearch any string, M-: (match-data) always return (0 3)
  (autopair-global-mode 1)

  (setq autopair-extra-pairs `(:everywhere ((?` . ?'))))

  (defun change-autopair-insert-opening ()
    "Change definition of `autopair-insert-opening'."

    (defun autopair-insert-opening-internal ()
      (interactive)
      (when (autopair-pair-p)
        (setq autopair-action (list 'opening (autopair-find-pair last-input-event) (point))))
      (autopair-fallback))

    (defun autopair-insert-opening ()
      (interactive)
      (if (and (fboundp 'skeleton-c-mode-left-brace)
               (memq major-mode modes-use-self-opening)
               (equal last-command-event ?{))
          (call-interactively 'skeleton-c-mode-left-brace)
        (call-interactively 'autopair-insert-opening-internal))))
  
  (defvar modes-use-self-opening
    '(c-mode c++-mode java-mode awk-mode php-mode)
    "*Modes use themselves insert opening function.")

  (eal-eval-by-modes
   modes-use-self-opening
   (lambda (mode)
     (change-autopair-insert-opening))))

(eval-after-load "autopair"
  '(autopair-settings))

(provide 'autopair-settings)
