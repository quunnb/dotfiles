;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "quunnb"
      user-mail-address "quunnb@amideus.fi")

(add-to-list 'custom-theme-load-path "~/.config/doom/themes/")
(add-to-list 'auto-mode-alist '("\\tridactylrc\\'" . vimrc-mode))
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; Visual stuff
(setq
 ;; doom-font (font-spec :family "ShureTechMono Nerd Font" :size 20)
 ;; doom-variable-pitch-font (font-spec :family "ShureTechMono Nerd Font" :size 20)
 ;; doom-font (font-spec :family "Iosevka Nerd Font Mono" :size 20)
 doom-font (font-spec :family "DepartureMono Nerd Font" :size 20)
 doom-variable-pitch-font (font-spec :family "Iosevka Nerd Font" :size 20)
 doom-theme 'less
 display-line-numbers-type 'relative
 initial-scratch-message nil
 )

;; Transparency
;; (defvar bg-transparency 70)
(defvar bg-transparency 70)
(set-frame-parameter nil 'alpha-background bg-transparency)
(add-to-list 'default-frame-alist `(alpha-background . ,bg-transparency))

;; Toggle global background transparency
(defun toggle-transparency ()
  (interactive)
  (let ((current-alpha (frame-parameter nil 'alpha-background)))
    (if (= current-alpha 100)
        (set-frame-parameter nil 'alpha-background bg-transparency)
      (set-frame-parameter nil 'alpha-background 100))))

;; Gots to see the prompt
(custom-set-faces
 '(minibuffer-prompt ((t (:foreground "#ff0000" )))))
 ;; '(minibuffer-prompt ((t (:foreground "#ebbcba" )))))

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
  "t" #'toggle-transparency
  )

(defvar-keymap mc-skip-map
  "n" #'evil-mc-skip-and-goto-next-match
  "p" #'evil-mc-skip-and-goto-prev-match
  )

(defvar-keymap mc-map
  :doc "Multi-cursor keymaps"
  "n" #'evil-mc-make-and-goto-next-match
  "N" #'evil-mc-make-and-goto-last-cursor
  "p" #'evil-mc-make-and-goto-prev-match
  "P" #'evil-mc-make-and-goto-first-cursor
  "s" mc-skip-map
  "m" #'evil-mc-make-all-cursors
  "u" #'evil-mc-undo-cursor
  "h" #'+multiple-cursors/evil-mc-toggle-cursor-here
  "t" #'+multiple-cursors/evil-mc-toggle-cursors
  )

(define-key doom-leader-map "j" custom-map)
(define-key doom-leader-map "k" mc-map)
(define-key doom-leader-map "l" #'avy-goto-char-2)

(with-eval-after-load 'which-key
  (which-key-add-keymap-based-replacements doom-leader-map
    "j" `("custom" . ,custom-map))
  (which-key-add-keymap-based-replacements doom-leader-map
    "k" `("mc" . ,mc-map))
  )

;; Evil keybindings
(with-eval-after-load 'evil
  ;; Move by visual lines
  (evil-global-set-key 'normal (kbd "<remap> <evil-next-line>") #'evil-next-visual-line)
  (evil-global-set-key 'normal (kbd "<remap> <evil-previous-line>") #'evil-previous-visual-line)

  ;; Swap movement repetition keys for Finnish keyboard layout
  (evil-global-set-key 'normal (kbd ",") #'evil-repeat-find-char)
  (evil-global-set-key 'normal (kbd ";") #'evil-repeat-find-char-reverse)

  ;; Create multiple cursors at the beginning or end of line of visual selection
  (evil-global-set-key 'visual (kbd "I") #'evil-mc-make-cursor-in-visual-selection-beg)
  (evil-global-set-key 'visual (kbd "A") #'evil-mc-make-cursor-in-visual-selection-end)

  ;; Avy
  (evil-global-set-key 'normal (kbd "gsm") 'avy-move-region)
  (evil-global-set-key 'normal (kbd "gsc") 'avy-copy-region)

  ;; Surround
  (evil-global-set-key 'normal (kbd "S") 'evil-embrace-evil-surround-region)

  ;; Move across lines with f, t, et.al
  (setq-default evil-cross-lines t)

  ;; Red cursor
  (setq
   ;; evil-normal-state-cursor '("#eb6f92" box)
   ;; evil-insert-state-cursor '("#eb6f92" bar)
   ;; evil-visual-state-cursor '("#eb6f92" box)
   evil-normal-state-cursor '("#ff0000" box)
   evil-insert-state-cursor '("#ff0000" bar)
   evil-visual-state-cursor '("#ff0000" box)
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

;; Disable flycheck on gptel buffers.
(add-hook 'gptel-mode-hook (lambda () (flycheck-mode -1)))
;; Auto-scroll answers
(add-hook 'gptel-post-stream-hook 'gptel-auto-scroll)

;; Disable auto parens
(remove-hook 'doom-first-buffer-hook #'smartparens-global-mode)

(setq
  css-indent-offset 2
  css-indent-level 2
  web-mode-css-indent-offset 2
  js-indent-level 2
  sgml-basic-offset 2
  +default-want-RET-continue-comments nil
  +evil-want-o/O-to-continue-comments nil)

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
  ;; (setq-default evil-surround-pairs-alist
  ;;             (push '(?< . ("<" . ">")) evil-surround-pairs-alist))
  (global-evil-surround-mode 1))

;; Move (mostly lines of) text easily
(use-package move-text
  :bind (("M-j" . move-text-down)
         ("M-k" . move-text-up)))


;; https://github.com/edkolev/evil-lion
(use-package evil-lion
  :config
  (evil-lion-mode))

;; Default browser
(setq browse-url-browser-function 'browse-url-firefox)

;; Etc.
(after! circe
  (set-irc-server! "irc.libera.chat"
    `(:tls t
      :port 6697
      :nick "quunnb"
      :sasl-username "quunnb"
      :sasl-password ""
      :channels ("#emacs" "tridactyl"))))
