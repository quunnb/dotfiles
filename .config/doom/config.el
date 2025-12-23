;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;;
(setq user-full-name "quunnb"
      user-mail-address "quunnb@amideus.fi")

(add-to-list 'auto-mode-alist '("\\tridactylrc\\'" . vimrc-mode)) ;; Use vimrc mode with tridactylrc
(add-to-list 'initial-frame-alist '(fullscreen . maximized))      ;; Maximize by default


;; Visual stuff
(setq doom-font (font-spec :family "DepartureMono Nerd Font" :size 20)
      ;; doom-variable-pitch-font (font-spec :family "DepartureMono Nerd Font Propo" :size 20)
      ;; doom-theme 'doric-fire
      doom-theme 'doric-valley
      ;; display-line-numbers-type 'relative ;; Turns out I almost never use these
      display-line-numbers t
      initial-scratch-message nil)

;; Try to disable current line highlighting
(global-hl-line-mode -1)

;; Add frame transparency
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

;; Golden ratio for windows
;; (golden-ratio-mode 1)

;; General remaps
(global-set-key (kbd "M-o") 'other-window)

;; GPTEL map
(defvar-keymap gptel-prefix-map
  :doc "gptel LLM bindings"
  "a" #'gptel-add
  "n" #'gptel
  "r" #'gptel-rewrite
  "s" #'gptel-send
  )

(defvar-keymap custom-map
  :doc "Custom keymaps"
  "a" gptel-prefix-map
  ;; "c" colorful-mode ;; This is defined it's own bindings
  )

(defvar-keymap mc-skip-map
  "n" #'evil-mc-skip-and-goto-next-match
  "p" #'evil-mc-skip-and-goto-prev-match)

;; Multiple cursor map
(defvar-keymap mc-map
  :doc "Multi-cursor keymaps"
  "n" #'evil-mc-make-and-goto-next-match
  "N" #'evil-mc-make-and-goto-last-cursor
  "p" #'evil-mc-make-and-goto-prev-match
  "P" #'evil-mc-make-and-goto-first-cursor
  "s" mc-skip-map
  "m" #'evil-mc-make-all-cursors
  "u" #'evil-mc-undo-cursor
  "t" #'+multiple-cursors/evil-mc-toggle-cursors
  "j" #'evil-mc-make-cursor-move-next-line
  "k" #'evil-mc-make-cursor-move-prev-line
  )

;; Custom doom-leader maps
(define-key doom-leader-map "j" custom-map)
(define-key doom-leader-map "k" mc-map)
(define-key doom-leader-map "l" #'avy-goto-char-2)

;; Redefine project mappings
(with-eval-after-load 'evil
  (define-key doom-leader-project-map "k" #'project-kill-buffers)
  (define-key doom-leader-project-map "s" #'+vertico/project-search)
  (define-key doom-leader-project-map "S" #'+vertico/project-search-from-cwd)
  (define-key doom-leader-project-map "p" #'project-switch-project)
  (define-key doom-leader-project-map "r" #'project-query-replace-regexp)
  (define-key doom-leader-project-map "c" #'project-compile)
  (define-key doom-leader-project-map "a" #'project-remember-project)
  (define-key doom-leader-project-map "d" #'project-forget-project)
  (define-key doom-leader-project-map "f" #'project-find-file)
  )

;; Add toggle mappings
(with-eval-after-load 'evil
  (define-key doom-leader-toggle-map "t" #'toggle-transparency))

;; Don't reformat by default
(define-key doom-leader-map "fs" #'+format/save-buffer-no-reformat)
(define-key doom-leader-map "fo" #'+format/save-buffer)

;; Add custom keymaps to which-key buffer
(with-eval-after-load 'which-key
  (which-key-add-keymap-based-replacements doom-leader-map
    "j" `("custom" . ,custom-map))
  (which-key-add-keymap-based-replacements doom-leader-map
    "k" `("mc" . ,mc-map))
  )

;; Define substitute bindings
(with-eval-after-load 'substitute
  (define-key global-map (kbd "C-c s") #'substitute-prefix-map)

  (define-key substitute-prefix-map (kbd "b") #'substitute-target-in-buffer)
  (define-key substitute-prefix-map (kbd "d") #'substitute-target-in-defun)
  (define-key substitute-prefix-map (kbd "r") #'substitute-target-above-point)
  (define-key substitute-prefix-map (kbd "s") #'substitute-target-below-point)

  ;; Always case-sensitive
  (setq substitute-fixed-letter-case t)

  ;; Report changes
  (add-hook 'substitute-post-replace-functions #'substitute-report-operation))


;; Evil keybindings
(with-eval-after-load 'evil
  ;; Move by visual lines
  (evil-global-set-key 'normal (kbd "<remap> <evil-next-line>") #'evil-next-visual-line)
  (evil-global-set-key 'normal (kbd "<remap> <evil-previous-line>") #'evil-previous-visual-line)

  ;; Swap movement repetition keys for Finnish keyboard layout
  (evil-global-set-key 'normal (kbd ",") #'evil-repeat-find-char)
  (evil-global-set-key 'normal (kbd ";") #'evil-repeat-find-char-reverse)

  ;; Create multiple cursors at the beginning or end of line of visual selection with `I` and `A`
  (evil-global-set-key 'visual (kbd "I") #'evil-mc-make-cursor-in-visual-selection-beg)
  (evil-global-set-key 'visual (kbd "A") #'evil-mc-make-cursor-in-visual-selection-end)

  ;; Avy: move or copy visible region to point
  (evil-global-set-key 'normal (kbd "gsm") 'avy-move-region)
  (evil-global-set-key 'normal (kbd "gsc") 'avy-copy-region)

  ;; Surround selected region using `S`
  (evil-global-set-key 'normal (kbd "S") 'evil-embrace-evil-surround-region)

  ;; `f`, `t`, `F`, and `T` movements move across newlines
  (setq-default evil-cross-lines t)

  ;; Highlight and shape the cursor
  (setq evil-normal-state-cursor '("#ff7700" box)
        evil-insert-state-cursor '("#ff7700" bar)
        evil-visual-state-cursor '("#ff7700" box)
        ))

;; Define org directory
(setq org-directory "~/org/")

;; Get environmental variables
(defun get-env-var (var-name)
  (getenv var-name))

;; All "web" indents and "tabs" to 2
(setq css-indent-offset 2
      css-indent-level 2
      web-mode-css-indent-offset 2
      js-indent-level 2
      typescript-indent-level 2
      sgml-basic-offset 2
      +default-want-RET-continue-comments nil
      +evil-want-o/O-to-continue-comments nil)

;; Disable tab suggestions
(setq tab-always-indent t)

;; Set default browser
(setq browse-url-browser-function 'browse-url-librewold)

;; Disable auto parens
(remove-hook 'doom-first-buffer-hook #'smartparens-global-mode)

;;
;; Language specific formatting and linting
;;

;; Go
(set-formatter! 'gofumpt '("gofumpt") :modes '(go-mode))

;; JS, TS, HTML, CSS, JSON, TSX, JSX
(use-package lsp-biome
  :vc (:url "https://github.com/cxa/lsp-biome" :rev :newest)
  :preface
  (defun my/lsp-biome-active-hook ()
    (setq-local apheleia-formatter '(biome)))

  :config
  (add-hook 'lsp-biome-active-hook #'my/lsp-biome-active-hook))

;;
;; Resize windows with Meta-Shift-direction
;;
(use-package softresize
  :config
  (global-set-key (kbd "M-K") 'softresize-enlarge-window)
  (global-set-key (kbd "M-J") 'softresize-reduce-window)
  (global-set-key (kbd "M-L") 'softresize-enlarge-window-horizontally)
  (global-set-key (kbd "M-H") 'softresize-reduce-window-horizontally))

;; In config.el
(use-package! grease
  :commands (grease-open grease-toggle grease-here)
  :init
  (setq grease-sort-method 'type
        grease-show-hidden nil
        grease-preview-window-width 0.4)
  :config
  (map! :leader
        (:prefix ("o g" . "Grease")
         :desc "Toggle Grease"           "g" #'grease-toggle
         :desc "Open Grease (current)"   "o" #'grease-open
         :desc "Open at project root"    "h" #'grease-here)))

;;
;; GPTEL
;;

;; Codestral
(setq gptel-model 'codestral-latest
      gptel-backend
      (gptel-make-openai "Codestral"
        :host "codestral.mistral.ai"
        :endpoint "/v1/chat/completions"
        :protocol "https"
        :key (getenv "CODESTRAL_API_KEY")
        :models '("codestral-latest")))

;; Mistral
(gptel-make-openai "MistralLeChat"
  :host "api.mistral.ai"
  :endpoint "/v1/chat/completions"
  :protocol "https"
  :key (getenv "MISTRAL_API_KEY")
  :models '("mistral-small-latest"))

;; Claude
(gptel-make-anthropic "Claude"
  :stream t
  :key (getenv "ANTHROPIC_API_KEY"))

;; Deepseek
(gptel-make-deepseek "DeepSeek"
  :stream t
  :key (getenv "DEEPSEEK_API_KEY"))

;; Perplexity
(gptel-make-perplexity "Perplexity"
  :stream t
  :key (getenv "PERPLEXITY_API_KEY"))


;; Disable flycheck on gptel buffers.
(add-hook 'gptel-mode-hook (lambda () (flycheck-mode -1)))

;; Auto-scroll answers (this doesn't work...)
(add-hook 'gptel-post-stream-hook 'gptel-auto-scroll)

;;
;; Colorize color codes in text
;;
(use-package colorful-mode
  :custom
  (colorful-use-prefix t)
  (colorful-only-strings 'only-prog)
  (css-fontify-colors nil)
  :config
  (global-colorful-mode t)
  (add-to-list 'global-colorful-modes 'helpful-mode)
  :bind (:map custom-map
              ("c" . colorful-mode)))

;;
;; Imitate vim-substitute
;;
(use-package evil-replace-with-register
  :config
  (setq evil-replace-with-register-key (kbd "s")) ; "substitute"
  (evil-replace-with-register-install))

;;
;; Surround things
;;
(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

;;
;; Move lines or selections of text easily up and down
;;
(use-package move-text
  :bind (("M-j" . move-text-down)
         ("M-k" . move-text-up)))


;;
;; Align selection
;;
(use-package evil-lion
  :config
  (evil-lion-mode))

;; Use native project instead of projectile
(use-package project
  :bind-keymap
  (("M-p" . project-prefix-map)))

;; Always show root in dired when switching project
(setq project-switch-commands 'project-dired)

;;
;; IRC
;;
(after! circe
  (set-irc-server! "irc.libera.chat"
    `(:tls t
      :port 6697
      :nick "quunnb"
      :sasl-username "quunnb"
      :sasl-password ""
      :channels ("#emacs" "tridactyl"))))

