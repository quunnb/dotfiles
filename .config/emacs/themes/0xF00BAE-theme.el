;;; 0xF00BAE.el --- inspired by TIS-100 -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Added: May 22, 2025
;; Author: quunnb <https://github.com/quunnb
;;
;;; Commentary:
;;
;;; Code:

(require 'doom-themes)


;;
;;; Variables

(defgroup 0xF00BAE-theme nil
  "Options for the `0xF00BAE' theme."
  :group 'doom-themes)

(defcustom 0xF00BAE-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group '0xF00BAE-theme
  :type 'boolean)

(defcustom 0xF00BAE-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group '0xF00BAE-theme
  :type 'boolean)

(defcustom 0xF00BAE-comment-bg 0xF00BAE-brighter-comments
  "If non-nil, comments will have a subtle highlight to enhance their legibility."
  :group '0xF00BAE-theme
  :type 'boolean)

(defcustom 0xF00BAE-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line.
Can be an integer to determine the exact padding."
  :group '0xF00BAE-theme
  :type '(choice integer boolean))


;;
;;; Theme definition

(def-doom-theme 0xF00BAE
  "A dark theme inspired by TIS-100."
  :family '0xF00BAE
  :background-mode 'dark

  ;; name        default   256           16
  ((bg         '("#000000" "#000000"     "black"  ))
   (fg         '("#FFFFFF" "#FFFFFF"     "brightwhite"  ))

   ;; These are off-color variants of bg/fg, used primarily for `solaire-mode',
   ;; but can also be useful as a basis for subtle highlights (e.g. for hl-line
   ;; or region), especially when paired with the `doom-darken', `doom-lighten',
   ;; and `doom-blend' helper functions.
   (bg-alt     '("#222222" "#222222"     "black"        ))
   (fg-alt     '("#CCCCCC" "#CCCCCC"     "white"        ))

   ;; These should represent a spectrum from bg to fg, where base0 is a starker
   ;; bg and base8 is a starker fg. For example, if bg is light grey and fg is
   ;; dark grey, base0 should be white and base8 should be black.
   (base0      '("#000000" "#000000"     "black"        ))
   (base1      '("#222222" "#222222"     "brightblack"  ))
   (base2      '("#333333" "#333333"     "brightblack"  ))
   (base3      '("#444444" "#444444"     "brightblack"  ))
   (base4      '("#555555" "#555555"     "brightblack"  ))
   (base5      '("#666666" "#666666"     "brightblack"  ))
   (base6      '("#888888" "#888888"     "brightblack"  ))
   (base7      '("#AAAAAA" "#AAAAAA"     "brightblack"  ))
   (base8      '("#FFFFFF" "#FFFFFF"     "white"        ))

   (grey       base4)
   (red        '("#FF0000" "#FF0000" "red"          ))
   (orange     '("#AAAAAA" "#AAAAAA" "brightred"    ))
   (green      '("#FF0000" "#FF0000" "green"        ))
   (teal       '("#CCCCCC" "#CCCCCC" "brightgreen"  ))
   (yellow     '("#FF0000" "#FF0000" "yellow"       ))
   (blue       '("#888888" "#888888" "brightblue"   ))
   (dark-blue  '("#999999" "#999999" "blue"         ))
   (magenta    '("#EEEEEE" "#EEEEEE" "brightmagenta"))
   (violet     '("#CCCCCC" "#CCCCCC" "magenta"      ))
   (cyan       '("#DDDDDD" "#DDDDDD" "brightcyan"   ))
   (dark-cyan  '("#BBBBBB" "#BBBBBB" "cyan"         ))

   ;; These are the "universal syntax classes" that doom-themes establishes.
   ;; These *must* be included in every doom themes, or your theme will throw an
   ;; error, as they are used in the base theme defined in doom-themes-base.
   (highlight      blue)
   (vertical-bar   (doom-darken base1 0.1))
   (selection      dark-blue)
   (builtin        magenta)
   (comments       (if 0xF00BAE-brighter-comments dark-cyan base5))
   (doc-comments   (doom-lighten (if 0xF00BAE-brighter-comments dark-cyan base5) 0.25))
   (constants      violet)
   (functions      magenta)
   (keywords       blue)
   (methods        cyan)
   (operators      blue)
   (type           yellow)
   (strings        green)
   (variables      (doom-lighten magenta 0.4))
   (numbers        orange)
   (region         `(,(doom-lighten (car bg-alt) 0.15) ,@(doom-lighten (cdr base1) 0.35)))
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)

   ;; These are extra color variables used only in this theme; i.e. they aren't
   ;; mandatory for derived themes.
   (modeline-fg              fg)
   (modeline-fg-alt          base5)
   (modeline-bg              (if 0xF00BAE-brighter-modeline
                                 (doom-darken blue 0.45)
                               (doom-darken bg-alt 0.1)))
   (modeline-bg-alt          (if 0xF00BAE-brighter-modeline
                                 (doom-darken blue 0.475)
                               `(,(doom-darken (car bg-alt) 0.15) ,@(cdr bg))))
   (modeline-bg-inactive     `(,(car bg-alt) ,@(cdr base1)))
   (modeline-bg-inactive-alt `(,(doom-darken (car bg-alt) 0.1) ,@(cdr bg)))

   (-modeline-pad
    (when 0xF00BAE-padded-modeline
      (if (integerp 0xF00BAE-padded-modeline) 0xF00BAE-padded-modeline 4))))


  ;;;; Base theme face overrides
  (((line-number &override) :foreground base4)
   ((line-number-current-line &override) :foreground fg)
   ((font-lock-comment-face &override)
    :background (if 0xF00BAE-comment-bg (doom-lighten bg 0.05) 'unspecified))
   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis :foreground (if 0xF00BAE-brighter-modeline base8 highlight))

   ;;;; css-mode <built-in> / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector             :foreground blue)
   ;;;; doom-modeline
   (doom-modeline-bar :background (if 0xF00BAE-brighter-modeline modeline-bg highlight))
   (doom-modeline-buffer-file :inherit 'mode-line-buffer-id :weight 'bold)
   (doom-modeline-buffer-path :inherit 'mode-line-emphasis :weight 'bold)
   (doom-modeline-buffer-project-root :foreground green :weight 'bold)
   ;;;; elscreen
   (elscreen-tab-other-screen-face :background "#353a42" :foreground "#1e2022")
   ;;;; ivy
   (ivy-current-match :background dark-blue :distant-foreground base0 :weight 'normal)
   ;;;; LaTeX-mode
   (font-latex-math-face :foreground green)
   ;;;; markdown-mode
   (markdown-markup-face :foreground base5)
   (markdown-header-face :inherit 'bold :foreground red)
   ((markdown-code-face &override) :background (doom-lighten base3 0.05))
   ;;;; rjsx-mode
   (rjsx-tag :foreground red)
   (rjsx-attr :foreground orange)
   ;;;; solaire-mode
   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-alt)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-inactive-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-alt))))

  ;;;; Base theme variable overrides-
  ())

;;; 0xF00BAE-theme.el ends here
