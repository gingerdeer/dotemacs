;; Packages I always want to have
(setq package-list '(ein
                     color-theme
		     cyberpunk-theme
		     markdown-mode
		     ;;ido-ubiquitous
		     json-reformat
		     rainbow-mode
		     magit
		     nyan-mode
		     monokai-theme
		     reykjavik-theme
		     haskell-mode))
;; melpa
(require 'package) ;; You might already have this line
(add-to-list 'package-archives 
            '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize) ;; You might already have this line

;; ORG
(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)
(setq org-agenda-files (list "~/org/personal.org"))

;; Download packages that aren't installed
(unless package-archive-contents
  (package-refresh-contents))
(dolist (package package-list)
  (unless (package-installed-p package)
    (message "Hey, it seems you're missing some packages. gonna download them for you now.")
    (package-install package)))

;; nice to have imo
(global-linum-mode t)

;; So much ido
(ido-mode 1)
(ido-everywhere 1)

;; Custom key bindings
(global-set-key (kbd "C-x C-m") 'compile)

;; ipython
;; (require 'ein)

;; fuck tabs
(setq-default indent-tabs-mode nil)

;; hehe
(nyan-mode t)
;; ipython(
(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i")
(setq-default py-shell-name "ipython")
(setq-default py-which-bufname "IPython")
					; use the wx backend, for both mayavi and matplotlib
(setq py-python-command-args
      '("--gui=wx" "--pylab=wx" "-colors" "Linux"))
(setq py-force-py-shell-name-p t)

;; haskell mode compile
(eval-after-load "haskell-mode"
  '(define-key haskell-mode-map (kbd "C-c C-c") 'haskell-compile))

;; "colors"
(require 'color-theme)
(setq my-color-themes (list 'color-theme-billw 'color-theme-jsc-dark 
                            'color-theme-sitaramv-solaris 'color-theme-resolve
                            'color-theme-classic 'color-theme-jonadabian-slate
                            'color-theme-kingsajz 'color-theme-shaman
                            'color-theme-subtle-blue 'color-theme-snowish
                            'color-theme-sitaramv-nt 'color-theme-wheat))
(color-theme-initialize)

(defun random-color-theme ()
  (interactive)
  (random t)
  (funcall (car (nth (random (length color-themes)) color-themes))))
;;
(random-color-theme)
;;
(run-with-timer 1 (* 60 60) 'random-color-theme)
(global-set-key (kbd "C-c +") 'random-color-theme)

;; switch to prev window
(defun prev-window ()
   (interactive)
   (other-window -1))

 (define-key global-map (kbd "C-x p") 'prev-window)

;; (load (expand-file-name "~/quicklisp/slime-helper.el"))
(setq inferior-lisp-program "sbcl")
(add-to-list 'load-path "C:/slime-2.22")
(require 'slime)
(slime-setup)

;; spotify (?)

(defun pl-transparency (value)
  "Set the transparency of the frame window.
Argument VALUE 0 is transparent, 100 is opaque."
  (interactive "nTransparency Value (0 - 100): ")
  (set-frame-parameter (selected-frame) 'alpha value))

(pl-transparency 75)
(menu-bar-mode -1)
(define-key global-map (kbd "C-c C-o -") (lambda () (interactive) (pl-transparency 75)))
(define-key global-map (kbd "C-c C-o +") (lambda () (interactive) (pl-transparency 100)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["#272822" "#F92672" "#A6E22E" "#E6DB74" "#66D9EF" "#FD5FF0" "#A1EFE4" "#F8F8F2"])
 '(company-quickhelp-color-background "#e8e8e8")
 '(company-quickhelp-color-foreground "#444444")
 '(compilation-message-face (quote default))
 '(custom-enabled-themes (quote (autumn-light)))
 '(custom-safe-themes
   (quote
    ("190a9882bef28d7e944aa610aa68fe1ee34ecea6127239178c7ac848754992df" "868f73b5cf78e72ca2402e1d48675e49cc9a9619c5544af7bf216515d22b58e7" "a1171b32b45efb833f313f86a909c349ff2dfdfbb1d34deab5673ea92c29cfb3" "c02740468fd0158f6f44fab295d1feacbd4cbe008cf9a8db373fcef137b602b7" "b65a3bb7dd1c43bf2e301143969a456a5cc380627076196f5529ce8fbf9fb8ac" "66881e95c0eda61d34aa7f08ebacf03319d37fe202d68ecf6a1dbfd49d664bc3" "5acb6002127f5d212e2d31ba2ab5503df9cd1baa1200fbb5f57cc49f6da3056d" "2dd51a71783f5b3aa6570e53d526350c8fe690ee7d13c26169a13a2e72436436" "e269026ce4bbd5b236e1c2e27b0ca1b37f3d8a97f8a5a66c4da0c647826a6664" "55d31108a7dc4a268a1432cd60a7558824223684afecefa6fae327212c40f8d3" "e9460a84d876da407d9e6accf9ceba453e2f86f8b86076f37c08ad155de8223c" "1db337246ebc9c083be0d728f8d20913a0f46edc0a00277746ba411c149d7fe5" "28ec8ccf6190f6a73812df9bc91df54ce1d6132f18b4c8fcc85d45298569eb53" "58f090ea19f5bc674a5a58738dedfb1907107f4953eb0e2ed493253c49356348" "c3d4af771cbe0501d5a865656802788a9a0ff9cf10a7df704ec8b8ef69017c68" "d6922c974e8a78378eacb01414183ce32bc8dbf2de78aabcc6ad8172547cb074" "d3a406c5905923546d8a3ad0164a266deaf451856eca5f21b36594ffcb08413a" "8ed752276957903a270c797c4ab52931199806ccd9f0c3bb77f6f4b9e71b9272" "a800120841da457aa2f86b98fb9fd8df8ba682cebde033d7dbf8077c1b7d677a" "01ce486c3a7c8b37cf13f8c95ca4bb3c11413228b35676025fdf239e77019ea1" "557c283f4f9d461f897b8cac5329f1f39fac785aa684b78949ff329c33f947ec" "71ecffba18621354a1be303687f33b84788e13f40141580fa81e7840752d31bf" default)))
 '(fci-rule-color "#20240E")
 '(highlight-changes-colors (quote ("#FD5FF0" "#AE81FF")))
 '(highlight-tail-colors
   (quote
    (("#20240E" . 0)
     ("#679A01" . 20)
     ("#4BBEAE" . 30)
     ("#1DB4D0" . 50)
     ("#9A8F21" . 60)
     ("#A75B00" . 70)
     ("#F309DF" . 85)
     ("#20240E" . 100))))
 '(hl-paren-colors (quote ("#FCE8C3" "#519F50" "#2C78BF" "#918175")))
 '(magit-diff-use-overlays nil)
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#a7a6d4" "#9796c4" "#b48ead")))
 '(package-selected-packages
   (quote
    (counsel-spotify ivy color-theme color-theme-approximate afternoon-theme ample-zen-theme atom-dark-theme autumn-light-theme dakrone-theme doneburn-theme espresso-theme forest-blue-theme lavender-theme srcery-theme sunburn-theme zen-and-art-theme zenburn-theme json-reformat ido-ubiquitous haskell-mode reykjavik-theme monokai-theme nyan-mode magit rainbow-mode markdown-mode ein cyberpunk-theme)))
 '(pdf-view-midnight-colors (quote ("#dedded" . "#4A4159")))
 '(pos-tip-background-color "#A6E22E")
 '(pos-tip-foreground-color "#272822")
 '(tool-bar-mode nil)
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#F92672")
     (40 . "#CF4F1F")
     (60 . "#C26C0F")
     (80 . "#E6DB74")
     (100 . "#AB8C00")
     (120 . "#A18F00")
     (140 . "#989200")
     (160 . "#8E9500")
     (180 . "#A6E22E")
     (200 . "#729A1E")
     (220 . "#609C3C")
     (240 . "#4E9D5B")
     (260 . "#3C9F79")
     (280 . "#A1EFE4")
     (300 . "#299BA6")
     (320 . "#2896B5")
     (340 . "#2790C3")
     (360 . "#66D9EF"))))
 '(vc-annotate-very-old-color nil t)
 '(weechat-color-list
   (unspecified "#272822" "#20240E" "#F70057" "#F92672" "#86C30D" "#A6E22E" "#BEB244" "#E6DB74" "#40CAE4" "#66D9EF" "#FB35EA" "#FD5FF0" "#74DBCD" "#A1EFE4" "#F8F8F2" "#F8F8F0")))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
