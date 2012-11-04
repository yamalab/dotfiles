; -*- Emacs-Lisp -*-
; Unity開発環境用デバッグプリント関数Insert

;; ここから本体
(defun my-debug-print-unity ()
  "デバッグ表示"
  (interactive)
  (let (input_str)
	(setq input_str (read-string "variable:"))
;;	(setq input_str (read-variable "variable:"))
;;	(insert (format "CmnDebug.Log(\"%s:\" + %s);\n" input_str input_str))
	(insert (format "CmnDebug.PushDispLog(\"%s:\" + %s);\n" input_str input_str))
	))

(provide 'my-debug-print-unity)
