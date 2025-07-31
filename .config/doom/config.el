;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "quunnb"
      user-mail-address "quunnb@amideus.fi")


(add-to-list 'custom-theme-load-path "~/.config/emacs/themes/")

(require 'doom-two-tone-themes)

;; Visual
(setq doom-font (font-spec :family "ShureTechMono Nerd Font" :size 20)
      doom-variable-pitch-font (font-spec :family "ShureTechMono Nerd Font" :size 20)
      ;; doom-theme 'doom-burgundy-rose
      ;; doom-theme 'rose-pine-color
      doom-theme 'doom-rose-pine
      ;; doom-theme 'tao-yin
      ;; doom-theme '0xF00BAE
      display-line-numbers-type 'relative
      )

;; Red cursor
(with-eval-after-load 'evil
  (setq evil-normal-state-cursor '("#EC8E9E" box)
        evil-insert-state-cursor '("#EC8E9E" bar)
        evil-visual-state-cursor '("#EC8E9E" box)))


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

;; Setup AI
(setq gptel-model   'mistral-medium-latest
      gptel-backend
      (gptel-make-openai "LeChat"   ;can be any name
        :host "api.mistral.ai"
        :endpoint "/v1/chat/completions"
        :protocol "https"
        :key (getenv "MISTRAL_API_KEY")
        :models '("mistral-medium-latest")))


;; Indent width for web-modes
(setq css-indent-offset 2)
(setq sgml-basic-offset 2)
