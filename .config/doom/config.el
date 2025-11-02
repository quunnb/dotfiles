;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "quunnb"
      user-mail-address "quunnb@amideus.fi")

(add-to-list 'custom-theme-load-path "~/.config/emacs/themes/")
(require 'doom-two-tone-themes)

;; Visual stuff
(setq doom-font (font-spec :family "ShureTechMono Nerd Font" :size 20)
      doom-variable-pitch-font (font-spec :family "ShureTechMono Nerd Font" :size 20)
      doom-theme 'doom-rose-pine
      display-line-numbers-type 'relative
      )

;; Resize windows with Meta-Shift-direction
(require 'softresize)
(global-set-key (kbd "M-K") 'softresize-enlarge-window)
(global-set-key (kbd "M-J") 'softresize-reduce-window)
(global-set-key (kbd "M-L") 'softresize-enlarge-window-horizontally)
(global-set-key (kbd "M-H") 'softresize-reduce-window-horizontally)

;; Evil keybindings
(with-eval-after-load 'evil
  ;; Move by visual lines
  (define-key evil-normal-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
  ;; Swap movement repetition keys (this fucks up using s/S for repeated movements though)
  (define-key evil-motion-state-map (kbd ",") 'evil-snipe-repeat)
  (define-key evil-motion-state-map (kbd ";") 'evil-snipe-repeat-reverse)

  ;; Create multiple cursors at the beginning or end of line of visual selection
  (evil-global-set-key 'visual (kbd "I") 'evil-mc-make-cursor-in-visual-selection-beg)
  (evil-global-set-key 'visual (kbd "A") 'evil-mc-make-cursor-in-visual-selection-end)
  ;; Use avy to copy / move regions of text
  (evil-global-set-key 'normal (kbd "gsm") 'avy-move-region)
  (evil-global-set-key 'normal (kbd "gsc") 'avy-copy-region )

  ;; Move across lines with f, t, etc.
  (setq-default evil-cross-lines t)

  ;; Red cursor
  (setq evil-normal-state-cursor '("#eb6f92" box)
        evil-insert-state-cursor '("#eb6f92" bar)
        evil-visual-state-cursor '("#eb6f92" box)))

;; Load some paths
(setq org-directory "~/org/"
      projectile-project-search-path '("~/dev/kood/" "~/.config/anki/"))


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

(add-hook 'gptel-post-stream-hook 'gptel-auto-scroll)
(map! :leader
      :desc "Rewrite area"
      "r" #'gptel-rewrite)


;; Indent width for web-modes
(setq-default css-indent-offset 2)
(setq sgml-basic-offset 2)


(use-package colorful-mode
  ;; :diminish
  ;; :ensure t ; Optional
  :custom
  (colorful-use-prefix t)
  (colorful-only-strings 'only-prog)
  (css-fontify-colors nil)
  :config
  (global-colorful-mode t)
  (add-to-list 'global-colorful-modes 'helpful-mode))
