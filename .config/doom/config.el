;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "quunnb"
      user-mail-address "quunnb@amideus.fi")


(add-to-list 'custom-theme-load-path "~/.config/emacs/themes/")

(require 'doom-two-tone-themes)

;; Visual
(setq doom-font (font-spec :family "InconsolataGo Nerd Font Mono" :size 20)
      doom-variable-pitch-font (font-spec :family "InconsolataGo Nerd Font Propo" :size 20)
      doom-theme 'doom-rose-pine
      display-line-numbers-type 'relative
      )

;; Red cursor
(with-eval-after-load 'evil
  (setq evil-normal-state-cursor '("#eb6f92" box)
        evil-insert-state-cursor '("#eb6f92" bar)
        evil-visual-state-cursor '("#eb6f92" box)))


;; Move by visual lines
(define-key evil-normal-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
(define-key evil-motion-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
(define-key evil-motion-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
(setq-default evil-cross-lines t)


;; Load some paths
(setq org-directory "~/org/"
      projectile-project-search-path '("~/dev/" "~/.config/anki/"))


;; Get environmental variables for API keys
(defun get-env-var (var-name)
  (getenv var-name))

(setq gptel-model   'codestral-latest
      gptel-backend
      (gptel-make-openai "LeChat - Codestral"
        :host "codestral.mistral.ai"
        :endpoint "/v1/chat/completions"
        :protocol "https"
        :key (getenv "CODESTRAL_API_KEY")
        :models '("codestral-latest")))

;; Indent width for web-modes
(setq-default css-indent-offset 2)
(setq sgml-basic-offset 2)

;; Resize windows
(require 'softresize)
(global-set-key (kbd "M-K") 'softresize-enlarge-window)
(global-set-key (kbd "M-J") 'softresize-reduce-window)
(global-set-key (kbd "M-L") 'softresize-enlarge-window-horizontally)
(global-set-key (kbd "M-H") 'softresize-reduce-window-horizontally)
