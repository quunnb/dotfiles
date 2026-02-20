;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;;
(setq user-full-name "quunnb"
      user-mail-address "quunnb@amideus.fi")

(add-to-list 'auto-mode-alist '("\\tridactylrc\\'" . vimrc-mode)) ;; Use vimrc mode with tridactylrc
(add-to-list 'initial-frame-alist '(fullscreen . maximized))      ;; Maximize by default

;; Remove hooks
(remove-hook 'after-change-major-mode-hook #'smartparens-global-mode-enable-in-buffer)
(remove-hook 'doom-first-buffer-hook #'smartparens-global-mode)

;;;;;;;;;;;;;
;; VISUALS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;

(setq doom-font (font-spec :family "DepartureMono Nerd Font" :size 20)
      ;; doom-variable-pitch-font (font-spec :family "DepartureMono Nerd Font Propo" :size 20)
      ;; doom-theme 'doric-fire
      doom-theme 'doric-valley
      ;; doom-theme 'doric-water
      ;; doom-theme 'doric-wind
      ;; doom-theme 'doric-oak
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
  (setq evil-normal-state-cursor '("#FF8800" box)
        evil-insert-state-cursor '("#FF8800" bar)
        evil-visual-state-cursor '("#FF8800" box))

  ;; Remove hl-line-mode once and for all
  (setq global-hl-line-modes nil))

;; FIXME
(with-eval-after-load 'evil-bindings
  ;; Transparency
  (define-key doom-leader-toggle-map "t" #'toggle-transparency))

;;;;;;;;;;;;;;;;;
;; KEYBINDINGS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;

(defun quunnb/keyboard-quit-dwim ()
  "Do-What-I-Mean behaviour for a general `keyboard-quit'.

The generic `keyboard-quit' does not do the expected thing when
the minibuffer is open.  Whereas we want it to close the
minibuffer, even without explicitly focusing it.

The DWIM behaviour of this command is as follows:

- When the region is active, disable it.
- When a minibuffer is open, but not focused, close the minibuffer.
- When the Completions buffer is selected, close it.
- In every other case use the regular `keyboard-quit'."
  (interactive)
  (cond
   ((region-active-p)
    (keyboard-quit))
   ((derived-mode-p 'completion-list-mode)
    (delete-completion-window))
   ((> (minibuffer-depth) 0)
    (abort-recursive-edit))
   (t
    (keyboard-quit))))

(define-key global-map (kbd "C-g") #'my/keyboard-quit-dwim)


;; General
(global-set-key (kbd "M-o") 'other-window)

(with-eval-after-load 'projectile
  (global-set-key (kbd "M-s p") #'project-search)
  (global-set-key (kbd "M-s c") #'project-search-from-cwd)
  (global-set-key (kbd "M-s m") #'projectile-multi-occur))

(with-eval-after-load 'consult
  (global-set-key (kbd "M-s r") #'consult-ripgrep))

;; Need a binding for input modes in menus
(with-eval-after-load 'embark
  (global-set-key (kbd "C-,") #'embark-act))

;; (defvar-keymap custom-map
;;   :doc "Custom keymaps"
;;   "a" quunnb-prefix-map)

(with-eval-after-load 'evil
  ;; Custom doom-leader maps
  ;; (define-key doom-leader-map "j" custom-map)
  (define-key doom-leader-map "l" #'avy-goto-char-2)

  ;; Don't reformat accidentally
  (define-key doom-leader-map "fs" #'+format/save-buffer-no-reformat)
  (define-key doom-leader-map "fo" #'+format/save-buffer)

  ;; Narrowing (though leader "b-" is probably enough)
  (define-key doom-leader-map "nf" #'narrow-to-defun)
  (define-key doom-leader-map "nr" #'narrow-to-region)
  (define-key doom-leader-map "np" #'narrow-to-page)
  (define-key doom-leader-map "nw" #'doom/widen-indirectly-narrowed-buffer)
  (define-key doom-leader-map "N" #'doom/widen-indirectly-narrowed-buffer)

  ;; Move by visual lines
  (evil-global-set-key 'normal (kbd "<remap> <evil-next-line>") #'evil-next-visual-line)
  (evil-global-set-key 'normal (kbd "<remap> <evil-previous-line>") #'evil-previous-visual-line)

  ;; Swap movement repetition keys; more natural in Finnish keyboard layout
  (evil-global-set-key 'normal (kbd ",") #'evil-repeat-find-char)
  (evil-global-set-key 'normal (kbd ";") #'evil-repeat-find-char-reverse)

  ;; Switch perspective
  (define-key doom-leader-map "P" #'persp-switch)

  ;; Default to current workspace buffers
  (define-key doom-leader-map "bi" #'+ibuffer/open-for-current-workspace)
  (define-key doom-leader-map "bI" #'ibuffer)

  ;; Create multiple cursors at the beginning or end of line of visual selection with `I` and `A`
  (evil-global-set-key 'visual (kbd "I") #'evil-mc-make-cursor-in-visual-selection-beg)
  (evil-global-set-key 'visual (kbd "A") #'evil-mc-make-cursor-in-visual-selection-end)

  ;; Exchange selections
  (evil-global-set-key 'visual (kbd "x") #'evil-exchange)

  ;; Tabs (switch context)
  (evil-global-set-key 'normal (kbd "M-n") #'tab-previous)
  (evil-global-set-key 'normal (kbd "M-p") #'tab-next)

  ;; Flycheck
  (define-key doom-leader-map "F" #'consult-flycheck)

  ;; Buffer navigation
  (define-key doom-leader-map "bn" #'projectile-next-project-buffer)
  (define-key doom-leader-map "bp" #'projectile-previous-project-buffer)

  ;; Avy: move/copy/delete visible region (to point)
  (evil-global-set-key 'normal (kbd "gsm") #'avy-move-region)
  (evil-global-set-key 'normal (kbd "gsc") #'avy-copy-region)
  (evil-global-set-key 'normal (kbd "gsd") #'avy-kill-region)
  (evil-global-set-key 'normal (kbd "gsk") #'avy-kill-region)

  ;; Surround selected region using `S`
  (evil-global-set-key 'visual (kbd "S") #'evil-embrace-evil-surround-region)

  ;; Other evil movements
  ;; `f`, `t`, `F`, and `T` movements move across newlines
  (setq-default evil-cross-lines t))

;; Add custom keymaps to which-key buffer
;; (with-eval-after-load 'which-key
  ;; (which-key-add-keymap-based-replacements doom-leader-map

  ;;   "j" `("custom" . ,custom-map)))

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

;; Stricter Go formatter
(with-eval-after-load 'go-mode
  (set-formatter! #'gofumpt '("gofumpt") :modes '(go-mode)))

;; Use golangci linter with flycheck
(use-package flycheck-golangci-lint
  :hook (go-mode . #'flycheck-golangci-lint-setup))

(with-eval-after-load 'compile
  ;; set cursor to follow compilation output
  (setq compilation-scroll-output t)
  (define-key compilation-mode-map (kbd "C-n") #'compilation-next-error)
  (define-key compilation-mode-map (kbd "C-p") #'compilation-previous-error)
  (define-key compilation-mode-map (kbd "g") nil)
  (define-key compilation-mode-map (kbd "r") #'recompile)
  (define-key compilation-mode-map (kbd "h") nil))


;;;;;;;;;;
;; MISC ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;

(setq show-trailing-whitespace t)
(setq global-auto-revert-non-file-buffers t)
(global-auto-revert-mode 1)

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
      ;; Don't add comment prefixes on new lines
      +default-want-RET-continue-comments nil
      +evil-want-o/O-to-continue-comments nil)

;; Set default browser
(setq browse-url-browser-function 'browse-url-firefox)

;; Always do find-file when switching project
(setq project-switch-commands 'project-find-file)
;; Disable projectile cache
(setq projectile-enable-caching nil)

;;;;;;;;;;;;;;;
;; TABSPACES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;

(use-package tabspaces
  ;; :hook (after-init . 'tabspaces-mode)
  :commands (tabspaces-switch-or-create-workspace
             tabspaces-open-or-create-project-and-workspace)
  :custom
  (tabspaces-use-filtered-buffers-as-default t)
  (tabspaces-default-tab "Default")
  (tabspaces-remove-to-default t)
  (tabspaces-include-buffers '("*scratch*"))
  ;; sessions
  (tabspaces-session t)
  ;; (tabspaces-session-auto-restore t)
  ;; additional options
  (tabspaces-fully-resolve-paths t)  ; Resolve relative project paths to absolute
  (tabspaces-exclude-buffers '("*Messages*" "*Compile-Log*"))  ; Additional buffers to exclude
  (tab-bar-new-tab-choice "*scratch*"))

(defvar tabspaces-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C") #'tabspaces-clear-buffers)
    (define-key map (kbd "b") #'tabspaces-switch-to-buffer)
    (define-key map (kbd "d") #'tabspaces-close-workspace)
    (define-key map (kbd "k") #'tabspaces-kill-buffers-close-workspace)
    (define-key map (kbd "o") #'tabspaces-open-or-create-project-and-workspace)
    (define-key map (kbd "r") #'tabspaces-remove-current-buffer)
    (define-key map (kbd "R") #'tabspaces-remove-selected-buffer)
    (define-key map (kbd "s") #'tabspaces-switch-or-create-workspace)
    (define-key map (kbd "t") #'tabspaces-switch-buffer-and-tab)
    (define-key map (kbd "w") #'tabspaces-show-workspaces)
    (define-key map (kbd "T") #'tabspaces-toggle-echo-area-display)
    map)
  "Keymap for tabspace/workspace commands after `tabspaces-keymap-prefix'.")

;;;;;;;;;;;;;;;;
;; SUBSTITUTE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;

;; Substitute text in range
(use-package substitute
  :config
  ;; Always treat the letter casing literally.
  (setq substitute-fixed-letter-case t)
  ;; Report the matches that changed in the given context.
  (add-hook 'substitute-post-replace-functions 'substitute-report-operation)
  ;; Use C-c s as a prefix for all Substitute commands.
  (define-key global-map (kbd "C-c s") 'substitute-prefix-map))


;;;;;;;;;;;;;;;;;
;; SOFT-RESIZE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;

;; Resize windows
(use-package softresize
  :bind (("M-H" . #'softresize-reduce-window-horizontally)
         ("M-J" . #'softresize-reduce-window)
         ("M-K" . #'softresize-enlarge-window)
         ("M-L" . #'softresize-enlarge-window-horizontally))
  :config
  (setq softresize-delta 8)) ;; 8 is not too fine-grained and fast enough

;;;;;;;;;;;;
;; GREASE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;

;; FIXME: mapping doesn't work
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

;; Fast bookmarks à la Primagen's harpoon
(use-package! javelin
  :config
  (global-javelin-minor-mode 1))

;;;;;;;;;;;;;;;;;;;
;; COLORFUL-MODE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;

;; Better rainbow-mode
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

;; Replace targets á la svermeulen/vim-subversive without losing clipboard
(use-package evil-replace-with-register
  :config
  (setq evil-replace-with-register-key (kbd "s")) ; "substitute"
  (evil-replace-with-register-install))

;;;;;;;;;;;;;;;;;;;
;; EVIL-SURROUND ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;

;; Surround things
(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

;;;;;;;;;;;;;;;
;; EVIL-LION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;

;; Align stuff
(use-package evil-lion
  :bind (:map evil-normal-state-map
         ("g a" . #'evil-lion-left)
         ("g A" . #'evil-lion-right)
         :map evil-visual-state-map
         ("g a" . #'evil-lion-left)
         ("g A" . #'evil-lion-right))
  :config
  (evil-lion-mode))

;;;;;;;;;;;;
;; HYDRAS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;

(after! hydra

  ;; Increment and decrement numbers at point with = and -
  (use-package evil-numbers
    :config
    (defhydra hydra-num ()
      ("=" #'evil-numbers/inc-at-pt)
      ("-" #'evil-numbers/dec-at-pt))

    (defun hydra-num/body-and-inc ()
      (interactive)
      (call-interactively #'evil-numbers/inc-at-pt)
      (hydra-num/body))

    (defun hydra-num/body-and-dec ()
      (interactive)
      (call-interactively #'evil-numbers/dec-at-pt)
      (hydra-num/body))
    :bind (:map evil-normal-state-map
           ("g =" . #'hydra-num/body-and-inc)
           ("g -" . #'hydra-num/body-and-dec)
           :map evil-visual-state-map
           ("g =" . #'hydra-num/body-and-inc)
           ("g -" . #'hydra-num/body-and-dec)))

  ;; Create cursors
  (use-package evil-mc
    :config
    (defhydra hydra-mc (:columns 4)
      "cursors"
      ("j" #'evil-mc-make-and-goto-next-match "make & next")
      ("k" #'evil-mc-make-and-goto-prev-match "make & prev")
      ("J" #'evil-mc-skip-and-goto-next-match "skip & next")
      ("K" #'evil-mc-skip-and-goto-prev-match "skip & prev")
      ("I" #'evil-mc-make-cursor-in-visual-selection-start "prepend")
      ("A" #'evil-mc-skip-cursor-in-visual-selection-end "append")
      ("m" #'evil-mc-make-all-cursors "make all")
      ("p" #'evil-mc-pause-cursors "toggle all")
      ("q" nil "quit"))
    :bind (:map doom-leader-map
                ("k" . hydra-mc/body)))

  ;; Move text
  (use-package cool-moves
    :config
    (defhydra hydra-cool-moves ()
      "move"
      ("<escape>" nil)
      ("u" nil)

      ("j" #'cool-moves/line-forward)
      ("k" #'cool-moves/line-backward)

      ("J" #'cool-moves/paragraph-forward)
      ("K" #'cool-moves/paragraph-backward)

      ("w" #'cool-moves/word-forward)
      ("b" #'cool-moves/word-backwards)

      ("l" #'cool-moves/character-forward)
      ("h" #'cool-moves/character-backward)

      ("n" #'cool-moves/sentence-forward)
      ("p" #'cool-moves/sentence-backward)

      ("x" #'cool-moves/sexp-forward)
      ("X" #'cool-moves/sexp-backward))
    :bind (:map doom-leader-map
                ("m" . #'hydra-cool-moves/body)))


  ;; Jump to / stage / revert changes
  (defhydra hydra-diff (:columns 2)
    "diff"
    ("j" #'diff-hl-next-hunk "next hunk")
    ("k" #'diff-hl-previous-hunk "previous hunk")
    ("s" #'diff-hl-stage-current-hunk "stage hunk")
    ("r" #'diff-hl-revert-hunk "revert hunk")
    ("q" nil "quit"))

  (define-key doom-leader-map "gh" #'hydra-diff/body))


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

;; The paramount scrollbar
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

(setq gptel-use-tools nil)

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
