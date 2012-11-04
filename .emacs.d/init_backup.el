;; load-path を追加する関数を定義
(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory
	      (expand-file-name (concat user-emacs-directory path))))
	(add-to-list 'load-path default-directory)
	(if (fboundp 'normal-top-level-add-subdirs-to-load-path)
	    (normal-top-level-add-subdirs-to-load-path))))))

;; 引数のディレクトリとそのサブディレクトリをload-pathに追加
(add-to-load-path "elisp" "conf" "public_repos")

;; package.elの設定
(when (require 'package nil t)
  ;; パッケージリポジトリにMarmaladeと開発者運営のELPAを追加
  (add-to-list 'package-archives
	       '("marmalade" . "http://marmalade-repo.org/packages/"))
  (add-to-list 'package-archives '("ELPA" . "http://tromey.com/elpa"))
  ;; インストールしたパッケージにロードパスを通して読み込む
  (package-initialize))

;(add-to-list 'auto-mode-alist '("¥¥.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("¥¥.js$" . js-mode))

;; C-mにnewline-and-indentを割り当てる。
(global-set-key (kbd "C-m") 'newline-and-indent)

;; 折り返しトグルコマンド
(define-key global-map (kbd "C-c l") 'toggle-truncate-lines)

;; "C-w" でウィンドウを切り替える。初期値はtranspose-chars
;;(define-key global-map (kbd "C-w") 'other-window)
;;; C-wにウィンドウの切替機能を割り当てる、分割していないときは、左右分割して新しいウィンドウに移る
(defun other-window-or-split()
  (interactive)
  (when (one-window-p) (split-window-vertically))
  (other-window 1))
(global-set-key (kbd "C-t") 'other-window-or-split)

;; Mac OS Xの場合のファイル名の設定
(when (eq system-type 'darwin)
  (require 'ucs-normalize)
  (set-file-name-coding-system 'utf-8-hfs)
  (setq locale-codeing-system 'utf-8-hts))

;; Windowsの場合のファイル名の設定
(when (eq window-system 'w32)
  (set-file-name-coding-system 'cp932)
  (setq locale-coding-system 'cp932))

;; カラム番号も表示
(column-number-mode t)

;; ファイルサイズを表示
(size-indication-mode t)
;; 時計を表示(好みに応じてフォーマットを変更可能)
;; (setq display-time-day-and-date t) ; 曜日・月・日を表示
(setq display-time-24hr-format t) ; 24時表示
(display-time-mode t)
;; バッテリー残量を表示
(display-battery-mode t)

;; タイトルバーにファイルのフルパスを表示
(setq frame-title-format "%f")
;; 行番号を常に表示する
(global-linum-mode t)
;;; リージョンに色をつける
(transient-mark-mode 1)
;;; GCを減らして軽くする(デフォルトの10倍)
;; 現在のマシンパワーではもっと大きくしてもよい
(setq gc-cons-threshold (* 10 gc-cons-threshold))
;;; ログの記録行数を増やす
(setq message-log-max 10000)
;;; ミニバッファを再帰的に呼び出せるようにする
(setq enable-recursive-minibuffers t)
;;; ダイアログボックスを使わないようにする
(setq use-dialog-box nil)
(defalias 'message-box 'message)
;;; 履歴をたくさん保存する
(setq history-length 1000)
;;; キーストロークをエコーエリアに早く表示する
(setq echo-keystorokes 0.1)
;;; 大きいファイルを開こうとしたときに警告を発生させる
;;; デフォルトは10MBなので25MBに拡張する
(setq large-file-warning-threshold (* 25 1024 1024))
;;; ミニバッファで入力を取り消しても履歴に残す
;;; 誤って取り消して入力が失われるのを防ぐため
(defadvice abort-recursive-edit (before minibuffer-save activate)
  (when (eq ( selected-window) (active-minibuffer-window))
    (add-to-history minibuffer-history-variable (minibuffer-contents))))
;;; yesと入力するのは面倒なのでyで十分
(defalias 'yes-or-no-p 'y-or-n-p)
;;; ツールバーとスクロールバーを消す
(tool-bar-mode -1)
;; (scroll-bar-mode -1)

(when (require 'color-theme nil t)
      ;; テーマを読み込むための設定
      (color-theme-initialize)
      ;; 
      (color-theme-taylor))

(defface my-hl-line-face
   ;; 背景がdarkならば背景色を紺に
   '((((class color) (background dark))
      (:background "NavyBlue" t))
     ;; 背景がlightならば背景色を緑に
     (((class color) (background light))
      (:background "LightGoldenrodYellow" t))
     (t (:bold t)))
   "hl-line's my face")
(setq hl-line-face 'my-hl-line-face)
(global-hl-line-mode t)

;; paren-mode : 対応する括弧を強調して表示する
(setq show-paren-delay 0) ; 表示までの秒数。初期値は0.125
(show-paren-mode t) ; 有効化
;; parenのスタイル : expressionは括弧内も強調表示
(setq show-paren-style 'expression)
;; フェイスを変更する
(set-face-background 'show-paren-match-face nil)
(set-face-underline-p 'show-paren-match-face "yellow")

;; バックアップとオートセーブファイルを~/.emacs.d/backups/へ集める
(add-to-list 'backup-directory-alist
	     (cons "." "~/.emacs.d/backups/"))
;;(setq auto-save-file-name-transforms
;;       '((".*",(expand-file-name "~/.emacs.d/backups/") t)))

;; auto-installの設定
(when (require 'auto-install nil t)
  ;; インストールディレクトリを設定する 最初は~/.emacs.d/auto-install/
  (setq auto-install-directory "~/.emacs.d/elisp/")
  ;; EmacsWikiに登録されているelisp の名前を取得する
  (auto-install-update-emacswiki-package-name t)
  ;; 必要であればプロキシの設定を行う
  ;; (setq url-proxy-services '(("http" . "localhost:8339")))
  ;; install-elisp の関数を利用可能にする
  (auto-install-compatibility-setup))

;; redo+の設定
(when (require 'redo+ nil t)
  (global-set-key (kbd "C-M-/") 'redo)
  (setq undo-no-redo t)		; 過去のundoがredoされないようにする
  ;; 大量のundoに耐えられるようにする
  (setq undo-limit 600000)
  (setq undo-strong-limit 900000))

;;;最近使ったファイルを開く
(setq recentf-max-saved-items 3000)
;; 最近使ったファイルに加えないファイルを正規表現で指定する
(setq recentf-exclude '("/TAG$" "/var/tmp/"))
(require 'recentf-ext)

;;; 矩形を選択しやすくする
;; (require 'sense-region)
;; (sense-region-on)
;;; sense-region
;; http://taiyaki.org/elisp/sense-region/
(require 'sense-region)
(sense-region-on)
(defun sense-region-to-rectangle ()
  (interactive)
  (setq sense-region-status 'rectangle)

  ;; フェイスまわりのデフォルトの挙動が変なので
  ;; この2行を追加
  (copy-face mell-region-face 'sense-region-face)
  (copy-face 'region 'sense-region-region-face)

  (mell-sign-reset-face mell-region-face))

;;; anything
;; (auto-intstall-batch "anything")
(when (require 'anything nil t)
  (setq
   ;; 候補を表示するまでの時間。デフォルトは0.5
   anything-idle-delay 0.3
   ;; タイプして再描画するまでの時間。デフォルトは0.1
   anything-input-idle-delay 0.2
   ;; 候補の最大表示数。デフォルトは50
   anything-candidate-number-limit 100
   ;; 候補が多いときに体感速度を早くする
   anything-quick-update t
   ;; 候補選択ショートカットをアルファベットに
   anything-enable-shortcuts 'alphabet)

  (when (require 'anything-config nil t)
    ;; root権限ではアクションを実行するときのコマンド
    ;; デフォルトは"su"
    (setq anything-su-or-sudo "sudo"))

  (require 'anything-match-plugin nil t)

  (when (and (executable-find "cmigemo")
	     (require 'migemo nil t))
    (require 'anything-migemo nil t))

  (when (require 'anything-complete nil t)
    ;; lispシンボルの補完候補の再検索時間
    (anything-lisp-complete-symbol-set-timer 150))

  (require 'anything-show-complete nil t)

  (when (require 'auto-install nil t)
    (require 'anything-auto-install nil t))

  (when (require 'descbinds-anything nil t)
    ;; describe-bindingsをAnythingに置き換える
    (descbinds-anything-install)))

;; M-yにanything-show-kill-ringを割り当てる
(define-key global-map (kbd "M-y") 'anything-show-kill-ring)

;; color-moccurをrequire関数で読み込みます。
(when (require 'color-moccur nil t)
  ;; M-oにoccur-by-moccurを割り当て
  (define-key global-map (kbd "M-o") 'occur-by-moccur)
  ;; スペース区切りでAND検索
  (setq moccur-split-word t)
  ;; ディレクトリ検索のとき除外するファイル
  (add-to-list 'dmoccur-exclusion-mask "¥¥.DS_Store")
  (add-to-list 'dmoccur-exclusion-mask "^#.+#$")
  ;; Migemoを利用できる環境であればMigemoを使う
  (when (and (executable-find "cmigemo")
	     (require 'migemo nil t))
    (setq moccur-use-migemo t)))

;; moccur-editの設定
(require 'moccur-edit nil t)

;; moccur-edit-finish-editと同時にファイルを保存する
(defadvice moccur-edit-change-file
  (after save-after-moccur-edit-buffer activate)
  (save-buffer))

;; 要color-moccur.el
(when (require 'anything-c-moccur nil t)
  (setq
   ;; anything-c-moccur用 'anything-idle-delay'
   anything-c-moccur-anything-idle-delay 0.1
   ;; バッファの情報をハイライトする
   lanything-c-moccur-higligt-info-line-flag t
   ;; 現在選択中の候補の位置をほかのwindowに表示する
   anything-c-moccur-enable-auto-look-flag t
   ;; 起動時にポイントの位置の単語を初期パターンにする
   anything-c-moccur-enable-initial-pattern t)
  ;; C-M-oにanything-c-moccur-occur-by-moccurを割り当てる
  (global-set-key (kbd "C-M-o") 'anything-c-moccur-occur-by-moccur))

;; auto-completeの設定
(when (require 'auto-complete-config nil t)
  (add-to-list 'ac-dictionary-directories
	       "~/.emacs.d/elisp/ac-dict")
  (define-key ac-mode-map (kbd "M-TAB") 'auto-complete)
  (ac-config-default))

;; wgrepの設定
(require 'wgrep nil t)

;; undohistの設定
(when (require 'undohist nil t)
  (undohist-initialize))

;; undo-treeの設定
(when (require 'undo-tree nil t)
  (global-undo-tree-mode))

;; cua-modeの設定
(cua-mode t) ; cua-modeをオン
(setq cua-enable-cua-keys nil) ; CUAキーバインドを無効にする

;; HTML5
(eval-after-load "rng-loc"
  '(add-to-list 'rng-schema-locating-files
		"~/.emacs.d/public_repos/html5-el/schemas.xml"))
(require 'whattf-dt)

;; </を入力すると自動的にタグを閉じる
(setq nxml-slash-auto-complete-flag t)
;; M-TABでタグを補完する
(setq nxml-bind-meta-tab-to-complete-flag t)
;; nxml-modeでauto-complete-modeを利用する
(add-to-list 'ac-modes 'nxml-mode)

;; cssm-modeの基本設定
(defun css-mode-hooks()
  "css-mode hooks"
  ;; インデントをCスタイルにする
  (setq cssm-indent-function #'cssm-c-style-indenter)
  ;; インデント幅を2にする
  (setq cssm-indent-level 2)
  ;; インデントにタブ文字を使わない
  (setq-default indent-tabs-mode nil)
  ;; 閉じ括弧の前に改行を挿入する
  (setq cssm-newline-before-closing-bracket t))

(add-hook 'css-mode-hook 'css-mode-hooks)

(defun js-indent-hook()
  ;; インデント幅を4にする
  (setq js-indent-level 2
	jp-expr-indent-offset 2
	indent-tabs-mode nil)
  ;; switch文のcaseラベルをインデントする関数を定義する
  (defun my-js-indent-line()
    (interactive)
    (let* ((parse-status (save-excursion (syntax-ppss (point-at-bol))))
	   (offset (- (current-column) (current-indentation)))
	   (indentation (js--proper-indentation parse-status)))
      (back-to-indentation)
      (if (looking-at "case¥¥s-")
	  (indent-line-to (+ indentation 2))
	(js-indent-line))
      (when (> offse 0) (forward-char offset))))
  ;; caseラベルのインデント処理をセットする
  (set (make-local-variable 'indent-line-function) 'my-js-indent-line)
  ;; ここまでcaseラベルを調整する設定
  )

;; js-modeの起動時にhookを追加
(add-hook 'js-mode-hook 'js-indent-hook)

;; php-modeの設定
(when (require 'php-mode nil t)
  (add-to-list 'auto-mode-alist '("¥¥.ctp¥¥'" . php-mode))
  (setq php-search-url "http://jp.php.net/ja/")
  (setq php-manual-url "http://jp.php.net/manual/ja/"))

;; php-modeのインデント設定
(defun php-indent-hook ()
  (setq indent-tabs-mode nil)
  (setq c-basic-offset 4)
  ;; (c-set-offset 'case-label '+) ; switch文のcaseラベル
  (c-set-offset 'arglist-intro '+) ; 配列の最初の要素が改行した場合
  (c-set-offset 'arglist-close 0)) ; 配列の閉じ括弧

(add-hook 'php-mode-hook 'php-indent-hook)

;; php-modeの補完を強化する
(defun php-completion-hook ()
  (when (require 'php-completion nil t)
    (php-completion-mode t)
    (define-key php-mode-map (kbd "C-o") 'phpcmp-complete)

    (when (require 'auto-complete nil t)
    (make-variable-buffer-local 'ac-sources)
    (add-to-list 'ac-sources 'ac-source-php-completion)
    (auto-complete-mode t))))

(add-hook 'php-mode-hook 'php-completion-hook)

;; smarty-mode
(add-to-list 'auto-mode-alist (cons "\\.tpl\\'" 'smarty-mode))
(autoload 'smarty-mode "smarty-mode" "Smarty Mode" t)


;; dtwをdelete-trailing-whitespaceのエイリアスにする
(defalias 'dtw 'delete-trailing-whitespace)

;; XML用Flymakeの設定
(defun flymake-xml-init ()
  (list "xmllint" (list "--valid"
			(flymake-init-create-temp-buffer-copy
			 'flymake-create-temp-inplace))))

;; HTML用Flymakeの設定
(defun flymake-html-init ()
  (list "tidy" (list (flymake-init-create-temp-buffer-copy
		      'flymake-create-temp-inplace))))

(add-to-list 'flymake-allowed-file-name-masks
	     '("¥¥.html¥¥'" flymake-html-init))

;; tidy error pattern
(add-to-list 'flymake-err-line-patterns
	     '("line ¥¥([0-9]+¥¥) column ¥¥([0-9]+¥¥) - ¥¥(Warning¥¥|Error¥¥): ¥¥(.*¥¥)"
	       nil 1 2 4))

;; JS用Flymakeの初期化関数の定義
(defun flymake-jsl-init ()
  (list "jsl" (list "-process" (flymake-init-create-temp-buffer-copy
				'flymake-create-temp-inplace))))

;; JavaScript編集でFlymakeを起動する
(add-to-list 'flymake-allowed-file-name-masks
	     '("¥¥.js¥¥'" flymake-jsl-init))

(add-to-list 'flymake-err-line-patterns
	     '("^¥¥(.+¥¥)(¥¥([0-9]+¥¥)): ¥¥(.*warning¥¥|SyntaxError¥¥): ¥¥(.*¥¥)"
	       1 2 nil 4))

;; gtags-modeのキーバインドを有効化する
;(setq gtags-suggested-key-mapping t) ; 無効化する場合はコメントアウト
(require 'gtags nil t)
;; gtags用キーバインド設定
(setq gtags-mode-hook
      '(lambda ()
	 (local-set-key "\M-t" 'gtags-find-tag)
	 (local-set-key "\M-r" 'gtags-find-rtag)
	 (local-set-key "\M-s" 'gtags-find-symbol)
	 (local-set-key "\C-t" 'gtags-pop-stack)))
(add-hook 'c-mode-common-hook
	  '(lambda()
	     (gtags-mode 1)
	     (gtags-make-complete-list)))

;; ctags.elの設定
(require 'ctags nil t)
(setq tags-revert-without-query t)
;; ctagsを呼び出すコマンドライン。パスが通っていればフルパスでなくてもよい
;; etags互換タグを利用する場合はコメントを外す
;; (setq ctags-command "ctags -e -R ")
;; anything-exuberant-ctags.elを利用しない場合はコメントアウトする
(setq ctags-command "ctags -R --fields=¥"+afikKlmnsSzt¥" ")
(global-set-key (kbd "<f5>") 'ctags-create-or-update-tags-table)

;; AnythingからTAGSを利用しやすくするコマンド作成
(when (and (require 'anything-exuberant-ctags nil t)
	   (require 'anything-gtags nil t))
  ;; anything-for-tags用のソースを定義
  (setq anything-for-tags
	(list anything-c-source-imenu
	      anything-c-source-gtags-select
	      ;; etagsを利用する場合はコメントを外す
	      ;; anything-c-source-etags-select
	      anything-c-source-exuberant-ctags-select
	      ))
  
  ;; anything-for-tagsコマンドを作成
  (defun anything-for-tags ()
    "Preconfigured 'anything' for anything-for-tags."
    (interactive)
    (anything anything-for-tags
	      (thing-at-point 'symbol)
	      nil nil nil "*anything for tags*"))

  ;; M-tにanything-for-tagsを割り当て
  (define-key global-map (kbd "M-t") 'anything-for-tags))

;;; 履歴を次回Emacs起動時にも保存する
(savehist-mode 1)

;; SQLサーバへ接続するためのデフォルト情報(sql-interactive-mode)
;; hoge.sqlを作成してC-c C-c
;; (setq sql-user "root" ; デフォルトユーザ名
;;       sql-database "database_name" ; データベース名
;;       sql-server "localhost" ; ホスト名
;;       sal-product 'mysql) ; データベースの種類

;; Subversionフロントエンドpsvnの設定
(when (executable-find "svn")
  (setq svn-status-verbose nil)
  (autoload 'svn-status "psvn" "Run 'svn status'." t))

;; GitフロントエンドEggの設定
(when (executable-find "git")
  (require 'egg nil t))

;; multi-termの設定
(when (require 'multi-term nil t)
  ;; 使用するシェルを指定
  (setq multi-term-program "/usr/local/bin/zsh"))

;; TRAMPでバックアップファイルを作成しない
(add-to-list 'backup-directory-alist
	     (cons tramp-file-name-regexp nil))

;;; キーバインド変更.
(global-set-key "\C-o" 'occur-by-moccur)
(global-set-key "\C-xf" 'recentf-open-files)
(global-set-key [f2] 'moccur-grep-find)
(global-set-key [backspace] 'backward-delete-char)
(global-set-key [f5] 'debug_OutPut)

