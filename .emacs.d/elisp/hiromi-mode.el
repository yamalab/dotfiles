; -*- Emacs-Lisp -*-
; ひろみモード Version 2 by hiromi
(defvar hiromi-mode-map (make-sparse-keymap) "ひろみモードのキーマップ")
;; とりあえずa〜zまで define-key
(let ((key ?a))
  (while (<= key ?z)
    (define-key hiromi-mode-map (char-to-string key) 'hiromi-i-am)
	(setq key (+ 1 key))))

;; 乱数で自爆キーを決めるぞー
(let*((jibaku (random 26))				;0〜25の乱数
	  (key (char-to-string (+ ?a jibaku))))
  (define-key hiromi-mode-map key 'hiromi-jibaku))

;; ここから本体
(defun hiromi-mode ()
  "ひろみモードだよー!"
  (interactive)
  (setq major-mode 'hiromi-mode
		mode-name "ひろみモード")
  (use-local-map hiromi-mode-map))

(defun hiromi-i-am ()						;パッケージ共通の接頭辞を持つように
  "僕ひろみ〜を挿入する関数"				;変えてみた
  (interactive)
  (insert (format "ひろみ%sナリ" (this-command-keys))))

(defun hiromi-jibaku ()
  "自爆関数"
  (interactive)
  (let ((visible-bell t))
	(ding)
	(sleep-for 1)
	(ding)
	(sleep-for 1)
	(ding)
	(sleep-for 1)
	(erase-buffer)
	(message "自爆!")))
