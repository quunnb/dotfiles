;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;;
(setq user-full-name "quunnb"
      user-mail-address "quunnb@amideus.fi")

(add-to-list 'auto-mode-alist '("\\tridactylrc\\'" . vimrc-mode)) ;; Use vimrc mode with tridactylrc
(add-to-list 'initial-frame-alist '(fullscreen . maximized))      ;; Maximize by default

;; Remove hooks
(remove-hook 'after-change-major-mode-hook 'smartparens-global-mode-enable-in-buffer)
(remove-hook 'doom-first-buffer-hook #'smartparens-global-mode)

;;;;;;;;;;;;;
;; VISUALS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;

(setq doom-font (font-spec :family "DepartureMono Nerd Font" :size 20)
      ;; doom-variable-pitch-font (font-spec :family "DepartureMono Nerd Font Propo" :size 20)
      ;; doom-theme 'doric-fire
      doom-theme 'doric-valley
      ;; doom-theme 'doric-water
      doom-theme 'doric-wind
      doom-theme 'doric-oak
      display-line-numbers-type 'relative
      display-line-numbers t
      initial-scratch-message nil)

;; Add frame transparency
(defvar bg-transparency 100)
(set-frame-parameter nil 'alpha-background bg-transparency)
(add-to-list 'default-frame-alist `(alpha-background . ,bg-transparency))

;; Toggle global background transparency
(defun toggle-transparency ()
  (interactive)
  (let ((current-alpha (frame-parameter nil 'alpha-background)))
    (if (= current-alpha 100)
        (set-frame-parameter nil 'alpha-background bg-transparency)
      (set-frame-parameter nil 'alpha-background 100))))

(with-eval-after-load 'evil
  ;; Highlight and shape the cursor
  ;; (setq evil-normal-state-cursor '("#35474B" box)
  ;;       evil-insert-state-cursor '("#35474B" bar)
  ;;       evil-visual-state-cursor '("#35474B" box))
  (setq evil-normal-state-cursor '("#FF8800" box)
        evil-insert-state-cursor '("#FF8800" bar)
        evil-visual-state-cursor '("#FF8800" box))

  ;; Remove hl-line-mode once and for all
  (setq global-hl-line-modes nil))

(with-eval-after-load 'evil-bindings
  ;; Transparency
  (define-key doom-leader-toggle-map "t" #'toggle-transparency))

;;;;;;;;;;;;;;;;;
;; KEYBINDINGS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;

;; General
(global-set-key (kbd "M-o") 'other-window)

(with-eval-after-load 'vertico
  (global-set-key (kbd "M-s p") '+vertico/project-search)
  (global-set-key (kbd "M-s c") '+vertico/project-search-from-cwd))

(with-eval-after-load 'consult
  (global-set-key (kbd "M-s r") 'consult-ripgrep))

;; Need a binding for input modes in menus
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

(with-eval-after-load 'evil
  ;; Custom doom-leader maps
  (define-key doom-leader-map "j" custom-map)
  (define-key doom-leader-map "l" #'avy-goto-char-2)

  ;; Don't reformat accidentally
  (define-key doom-leader-map "fs" #'+format/save-buffer-no-reformat)
  (define-key doom-leader-map "fo" #'+format/save-buffer)

  ;; Move by visual lines
  (evil-global-set-key 'normal (kbd "<remap> <evil-next-line>") #'evil-next-visual-line)
  (evil-global-set-key 'normal (kbd "<remap> <evil-previous-line>") #'evil-previous-visual-line)

  ;; Swap movement repetition keys; more natural in Finnish keyboard layout
  (evil-global-set-key 'normal (kbd ",") #'evil-repeat-find-char)
  (evil-global-set-key 'normal (kbd ";") #'evil-repeat-find-char-reverse)

  ;; Create multiple cursors at the beginning or end of line of visual selection with `I` and `A`
  (evil-global-set-key 'visual (kbd "I") #'evil-mc-make-cursor-in-visual-selection-beg)
  (evil-global-set-key 'visual (kbd "A") #'evil-mc-make-cursor-in-visual-selection-end)

  ;; Exchange selections
  (evil-global-set-key 'visual (kbd "x") #'evil-exchange)

  ;; Tabs (switch context)
  (evil-global-set-key 'normal (kbd "M-n") #'tab-previous)
  (evil-global-set-key 'normal (kbd "M-p") #'tab-next)

  ;; Avy: move or copy visible region to point
  (evil-global-set-key 'normal (kbd "gsm") 'avy-move-region)
  (evil-global-set-key 'normal (kbd "gsc") 'avy-copy-region)

  ;; Surround selected region using `S`
  (evil-global-set-key 'visual (kbd "S") 'evil-embrace-evil-surround-region)

  ;; Other evil movements
  ;; `f`, `t`, `F`, and `T` movements move across newlines
  (setq-default evil-cross-lines t))

;; Add custom keymaps to which-key buffer
(with-eval-after-load 'which-key
  (which-key-add-keymap-based-replacements doom-leader-map
    "j" `("custom" . ,custom-map)))

;; TAB-only configuration
(use-package emacs
  :custom
  ;; Disable indentation+completion using the TAB key.
  (tab-always-indent t)

  ;; Hide commands in M-x which do not apply to the current mode.
  (read-extended-command-predicate #'command-completion-default-include-p))

(use-package corfu
  :config
  ;; Free the RET key for less intrusive behavior.
  (keymap-unset corfu-map "RET"))


;;;;;;;;;;;;;;;;;;;
;; LANG SETTINGS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;

;; Go
(set-formatter! 'gofumpt '("gofumpt") :modes '(go-mode))

;; TS, JS, HTML, CSS et. al.
(use-package lsp-biome
  :preface
  (defun my/lsp-biome-active-hook ()
    (setq-local apheleia-formatter '(biome)))

  :config
  (add-hook 'lsp-biome-active-hook #'my/lsp-biome-active-hook))


;;;;;;;;;;
;; MISC ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;

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

;; Set default browser
(setq browse-url-browser-function 'browse-url-firefox)


;;;;;;;;;;;;;;;;
;; SUBSTITUTE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;

(use-package substitute
  :config

  ;; Always treat the letter casing literally.
  (setq substitute-fixed-letter-case t)

  ;; Report the matches that changed in the given context.
  (add-hook 'substitute-post-replace-functions #'substitute-report-operation)

  ;; Use C-c s as a prefix for all Substitute commands.
  (define-key global-map (kbd "C-c s") #'substitute-prefix-map))


;;;;;;;;;;;;;;;;;
;; SOFT-RESIZE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;

(use-package softresize
  :bind (("M-H" . softresize-reduce-window-horizontally)
         ("M-J" . softresize-reduce-window)
         ("M-K" . softresize-enlarge-window)
         ("M-L" . softresize-enlarge-window-horizontally))
  :config
  (setq softresize-delta 8))

;;;;;;;;;;;;
;; GREASE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;

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

;;;;;;;;;;;;;
;; JAVELIN ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;

(use-package! javelin
  :config
  (global-javelin-minor-mode 1))


;;;;;;;;;;;;;;;;;;;
;; COLORFUL-MODE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;

(use-package colorful-mode
  :custom
  (colorful-use-prefix nil)
  (colorful-only-strings 'only-prog)
  (css-fontify-colors nil)
  :config
  (global-colorful-mode t)
  (add-to-list 'global-colorful-modes 'helpful-mode))

;; Disable Colorful in active region
(add-hook 'post-command-hook
          (lambda ()
            "delete colorful overlay on active mark"
            (when-let* (colorful-mode
                        (beg (use-region-beginning))
                        (end (use-region-end)))
              ;; Remove full colorful overlay instead only the part where
              ;; the region is.
              (dolist (ov (overlays-in beg end))
                (when (overlay-get ov 'colorful--overlay)
                  (delete-overlay ov))))))

(add-hook 'deactivate-mark-hook
          (lambda ()
            "refontify deleted mark"
            (when-let* (colorful-mode
                        (beg (region-beginning))
                        (end (region-end)))
              (font-lock-flush beg end))))

;; Disable rainbow-mode
(remove-hook 'css-mode-hook 'rainbow-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EVIL-REPLACE-WITH-REGISTER ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package evil-replace-with-register
  :config
  (setq evil-replace-with-register-key (kbd "s")) ; "substitute"
  (evil-replace-with-register-install))

;;;;;;;;;;;;;;;;;;;
;; EVIL-SURROUND ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;

(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

;;;;;;;;;;;;;;;;
;; COOL-MOVES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;

(use-package cool-moves
  :load-path "~/.emacs.d/lisp/cool-moves"
  :config
  (general-define-key
   :keymaps 'override
   "<C-down>" 'cool-moves/paragraph-forward
   "<C-up>" 'cool-moves/paragraph-backward
   "C-S-j" 'cool-moves/line-forward
   "C-S-k" 'cool-moves/line-backward
   "C-M-n" 'cool-moves/word-forward
   "C-M-p" 'cool-moves/word-backwards))

;;;;;;;;;;;;;;;
;; EVIL-LION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;

(use-package evil-lion
  :config
  (evil-lion-mode))

;; Always do find-file when switching project
(setq project-switch-commands 'project-find-file)


;;;;;;;;;;;;;;;;;;
;; EVIL-NUMBERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;
;; HYDRA ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;

;; Increment and decrement numbers at point with = and -
(defun hydra-num/body-and (fn)
  (interactive)
  (call-interactively fn)
  (hydra-num/body))

(defun hydra-num/body-and-inc ()
  (interactive) (call-interactively #'evil-numbers/inc-at-pt) (hydra-num/body))
(defun hydra-num/body-and-dec ()
  (interactive) (call-interactively #'evil-numbers/dec-at-pt) (hydra-num/body))

(define-key evil-normal-state-map (kbd "g =") #'hydra-num/body-and-inc)
(define-key evil-normal-state-map (kbd "g -") #'hydra-num/body-and-dec)

;;;;;;;;;;;
;; CIRCE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;

(after! circe
  (set-irc-server! "irc.libera.chat"
    `(:tls t
      :port 6697
      :nick "quunnb"
      :sasl-username "quunnb"
      :sasl-password ""
      :channels ("#emacs" "tridactyl"))))


;;;;;;;;;;;;;;;
;; NYAN-MODE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;

(use-package nyan-mode
  :hook (after-init . nyan-mode)
  :custom
  (nyan-animate-nyancat t)
  (nyan-wavy-trail t))


;;;;;;;;;;;
;; GPTEL ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;

;; Disable flycheck on gptel buffers.
(add-hook 'gptel-mode-hook (lambda () (flycheck-mode -1)))
;; Auto-scroll answers
(add-hook 'gptel-post-stream-hook 'gptel-auto-scroll)
;; Move cursor to next prompt
;; (add-hook 'gptel-post-response-functions 'gptel-end-of-response)

(setq gptel-use-tools nil
      gptel-highlight-methods "fringe")

;;
;; Models

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

;;
;; LLM presets

;; Coding
(gptel-make-preset 'coding
  :description "Coding preset"
  :backend "Codestral"
  :model 'codestral-latest
  :system "You are an expert coding assistant. Your role is to provide high-quality code solutions, refactorings, and explanations."
  :tools '("read_buffer" "modify_buffer")) ;gptel tools or tool names

;; TODO: more presets
