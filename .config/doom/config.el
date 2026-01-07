;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;;
(setq user-full-name "quunnb"
      user-mail-address "quunnb@amideus.fi")

(add-to-list 'auto-mode-alist '("\\tridactylrc\\'" . vimrc-mode)) ;; Use vimrc mode with tridactylrc
(add-to-list 'initial-frame-alist '(fullscreen . maximized))      ;; Maximize by default

;;
;; Visual stuff
;;

(setq doom-font (font-spec :family "DepartureMono Nerd Font" :size 20)
      ;; doom-variable-pitch-font (font-spec :family "DepartureMono Nerd Font Propo" :size 20)
      ;; doom-theme 'doric-fire
      ;; doom-theme 'doric-valley
      ;; doom-theme 'doric-water
      ;; doom-theme 'doric-wind
      doom-theme 'doric-oak
      display-line-numbers-type 'relative
      display-line-numbers t
      initial-scratch-message nil)

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

;; General remaps
(global-set-key (kbd "M-o") 'other-window)

(with-eval-after-load 'consult
  (global-set-key (kbd "M-s r") 'consult-ripgrep))

;; Need a binding for "non-normal" modes
(with-eval-after-load 'embark
  (global-set-key (kbd "C-,") 'embark-act))


;; GPTEL map
(defvar-keymap gptel-prefix-map
  :doc "gptel LLM bindings"
  "a" #'gptel-add
  "m" #'gptel-menu
  "n" #'gptel
  "r" #'gptel-rewrite
  "s" #'gptel-send)

(defvar-keymap custom-map
  :doc "Custom keymaps"
  "a" gptel-prefix-map
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
  "k" #'evil-mc-make-cursor-move-prev-line)

;;Project mappings
(with-eval-after-load 'project
  (define-key doom-leader-project-map "k" #'project-kill-buffers)
  (define-key doom-leader-project-map "s" #'+vertico/project-search)
  (define-key doom-leader-project-map "S" #'+vertico/project-search-from-cwd)
  (define-key doom-leader-project-map "p" #'project-switch-project)
  (define-key doom-leader-project-map "r" #'project-query-replace-regexp)
  (define-key doom-leader-project-map "c" #'project-compile)
  (define-key doom-leader-project-map "a" #'project-remember-project)
  (define-key doom-leader-project-map "d" #'project-forget-project)
  (define-key doom-leader-project-map "f" #'project-find-file))

(with-eval-after-load 'evil-bindings
  (define-key doom-leader-toggle-map "t" #'toggle-transparency))

(with-eval-after-load 'evil
  ;; Transparency

  ;; Custom doom-leader maps
  (define-key doom-leader-map "j" custom-map)
  (define-key doom-leader-map "k" mc-map)
  (define-key doom-leader-map "l" #'avy-goto-char-2)

  ;; Don't reformat by default
  (define-key doom-leader-map "fs" #'+format/save-buffer-no-reformat)
  (define-key doom-leader-map "fo" #'+format/save-buffer)

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

  ;; Other evil things
  ;; `f`, `t`, `F`, and `T` movements move across newlines
  (setq-default evil-cross-lines t)

  ;; Remove hl-line-mode once and for all
  (setq global-hl-line-modes nil)

  ;; Highlight and shape the cursor
  (setq evil-normal-state-cursor '("#DD888C" box)
        evil-insert-state-cursor '("#DD888C" bar)
        evil-visual-state-cursor '("#DD888C" box)))


;; Add custom keymaps to which-key buffer
(with-eval-after-load 'which-key
  (which-key-add-keymap-based-replacements doom-leader-map
    "j" `("custom" . ,custom-map))
  (which-key-add-keymap-based-replacements doom-leader-map
    "k" `("mc" . ,mc-map))
  )

;;
;; Substitute
;;
(use-package substitute
  :config

  ;; Always treat the letter casing literally.
  (setq substitute-fixed-letter-case t)

  ;; Report the matches that changed in the given context.
  (add-hook 'substitute-post-replace-functions #'substitute-report-operation)

  ;; Use C-c s as a prefix for all Substitute commands.
  (define-key global-map (kbd "C-c s") #'substitute-prefix-map))


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

(use-package emacs
  :custom
  ;; Disable indentation+completion using the TAB key.
  (tab-always-indent t)

  ;; Hide commands in M-x which do not apply to the current mode.
  (read-extended-command-predicate #'command-completion-default-include-p))

;; TAB-only configuration
(use-package corfu
  :config
  ;; Free the RET key for less intrusive behavior.
  (keymap-unset corfu-map "RET"))

;; Set default browser
(setq browse-url-browser-function 'browse-url-firefox)

;; Disable auto parens
(remove-hook 'doom-first-buffer-hook #'smartparens-global-mode)

;;
;; Language specific formatting and linting
;;

;; Go
(set-formatter! 'gofumpt '("gofumpt") :modes '(go-mode))

;; TS, JS, HTML, CSS et. al.
(use-package lsp-biome
  :preface
  (defun my/lsp-biome-active-hook ()
    (setq-local apheleia-formatter '(biome)))

  :config
  (add-hook 'lsp-biome-active-hook #'my/lsp-biome-active-hook))

;; Resize windows with Meta-Shift-(hjkl)
(use-package softresize
  :bind (("M-H" . (lambda () (interactive) (softresize-reduce-window-horizontally 8)))
         ("M-J" . (lambda () (interactive) (softresize-reduce-window 8)))
         ("M-K" . (lambda () (interactive) (softresize-enlarge-window 8)))
         ("M-L" . (lambda () (interactive) (softresize-enlarge-window-horizontally 8)))))

;; Editable file manager buffer
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

(use-package! javelin
  :config
  (global-javelin-minor-mode 1))

;;
;; GPTEL
;;

;; Disable flycheck on gptel buffers.
(add-hook 'gptel-mode-hook (lambda () (flycheck-mode -1)))
;; Auto-scroll answers
(add-hook 'gptel-post-stream-hook 'gptel-auto-scroll)
;; Move cursor to next prompt
;; (add-hook 'gptel-post-response-functions 'gptel-end-of-response)

(setq gptel-use-tools nil
      gptel-highlight-methods "fringe")

;; GPT models

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

;; Presets

(gptel-make-preset 'coding
  :description "Coding preset"
  :backend "Codestral"
  :model 'codestral-latest
  :system "You are an expert coding assistant. Your role is to provide high-quality code solutions, refactorings, and explanations."
  :tools '("read_buffer" "modify_buffer")) ;gptel tools or tool names

;;
;; Colorize color codes in text
;;
(use-package colorful-mode
  :custom
  (colorful-use-prefix nil)
  (colorful-only-strings 'only-prog)
  (css-fontify-colors nil)
  :config
  (global-colorful-mode t)
  (add-to-list 'global-colorful-modes 'helpful-mode))

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

;; Always do find-file when switching project
(setq project-switch-commands 'project-find-file)

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

