;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "quunnb"
      user-mail-address "quunnb@amideus.fi")

(add-to-list 'custom-theme-load-path "~/.config/doom/themes/")

;; Visual stuff
(setq
 ;; doom-font (font-spec :family "ShureTechMono Nerd Font" :size 20)
 ;; doom-variable-pitch-font (font-spec :family "ShureTechMono Nerd Font" :size 20)
 doom-font (font-spec :family "Iosevka Nerd Font Mono" :size 20)
 doom-variable-pitch-font (font-spec :family "Iosevka Nerd Font Mono" :size 20)
 doom-theme 'doom-rose-pine
 display-line-numbers-type 'relative
 initial-scratch-message nil
 )

;; Transparency
(defvar bg-transparency 70)
(set-frame-parameter nil 'alpha-background bg-transparency)
(add-to-list 'default-frame-alist `(alpha-background . ,bg-transparency))

(defun toggle-transparency ()
  (interactive)
  (let ((current-alpha (frame-parameter nil 'alpha-background)))
    (if (= current-alpha 100)
        (set-frame-parameter nil 'alpha-background bg-transparency)
      (set-frame-parameter nil 'alpha-background 100))))

;; Gots to see the prompt
(custom-set-faces
 ;; '(minibuffer-prompt ((t (:foreground "#ff0000" )))))
 '(minibuffer-prompt ((t (:foreground "#ebbcba" )))))

;; General remaps
(global-set-key (kbd "M-o") 'other-window)

(defvar-keymap gptel-prefix-map
  :doc "gptel LLM bindings"
  "a" #'gptel-add
  "n" #'gptel
  "r" #'gptel-rewrite
  "s" #'gptel-send
  )

(defvar-keymap custom-map
  :doc "Custom keymaps"
  "a" gptel-prefix-map ; AI
  "s" #'avy-goto-char-2 ; search
  "t" #'toggle-transparency
  )

(define-key custom-map "a" gptel-prefix-map)
(define-key doom-leader-map "j" custom-map)

(with-eval-after-load 'which-key
  (which-key-add-keymap-based-replacements doom-leader-map
    "j" `("custom" . ,custom-map))
  )

;; Evil keybindings
(with-eval-after-load 'evil
  ;; Move by visual lines
  (evil-global-set-key 'normal (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
  (evil-global-set-key 'normal (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)

  ;; Swap movement repetition keys
  (evil-global-set-key 'normal (kbd ",") 'evil-repeat-find-char)
  (evil-global-set-key 'normal (kbd ";") 'evil-repeat-find-char-reverse)

  ;; Create multiple cursors at the beginning or end of line of visual selection
  (evil-global-set-key 'visual (kbd "I") 'evil-mc-make-cursor-in-visual-selection-beg)
  (evil-global-set-key 'visual (kbd "A") 'evil-mc-make-cursor-in-visual-selection-end)
  ;; Use avy to copy / move regions of text
  (evil-global-set-key 'normal (kbd "gsm") 'avy-move-region)
  (evil-global-set-key 'normal (kbd "gsc") 'avy-copy-region )

  ;; Move across lines with f, t, etc.
  (setq-default evil-cross-lines t)

  ;; Red cursor
  (setq
   evil-normal-state-cursor '("#eb6f92" box)
   evil-insert-state-cursor '("#eb6f92" bar)
   evil-visual-state-cursor '("#eb6f92" box)
   ;; evil-normal-state-cursor '("#ff0000" box)
   ;; evil-insert-state-cursor '("#ff0000" bar)
   ;; evil-visual-state-cursor '("#ff0000" box)
   ))

;; Resize windows with Meta-Shift-direction
(use-package softresize
  :config
  (global-set-key (kbd "M-K") 'softresize-enlarge-window)
  (global-set-key (kbd "M-J") 'softresize-reduce-window)
  (global-set-key (kbd "M-L") 'softresize-enlarge-window-horizontally)
  (global-set-key (kbd "M-H") 'softresize-reduce-window-horizontally))

;; Load some project etc. paths
(setq org-directory "~/org/"
      projectile-project-search-path '("~/dev/kood/" "~/.config/anki/" "~/dotfiles/"))

;; Get environmental variables for API keys
(defun get-env-var (var-name)
  (getenv var-name))

;; Set up AI
(setq gptel-model 'codestral-latest
      gptel-backend
      (gptel-make-openai "LeChat - Codestral"
        :host "codestral.mistral.ai"
        :endpoint "/v1/chat/completions"
        :protocol "https"
        :key (getenv "CODESTRAL_API_KEY")
        :models '("codestral-latest")))

;; TODO this doesn't seem to work?
(add-hook 'gptel-post-stream-hook 'gptel-auto-scroll)

;; Indent width
(setq-default tab-width 4
              evil-shift-width 4
              sgml-basic-offset 2) ;; Markup

(use-package colorful-mode
  :custom
  (colorful-use-prefix t)
  (colorful-only-strings 'only-prog)
  (css-fontify-colors nil)
  :config
  (global-colorful-mode t)
  (add-to-list 'global-colorful-modes 'helpful-mode)
  :bind (("C-c c" . colorful-mode)))

(use-package evil-replace-with-register
  :config
  (setq evil-replace-with-register-key (kbd "s")) ; "substitute"
  (evil-replace-with-register-install))

;; Add, change and delete surrounding delimiters
;; https://github.com/emacs-evil/evil-surround
(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

(use-package move-text
  :bind (("M-j" . move-text-down)
         ("M-k" . move-text-up)))
