;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package:
;;
;;   1. Declare them here in a `package!' statement,
;;   2. Run 'doom sync' in the shell,
;;   3. Restart Emacs.
;;
;; Use 'C-h f package\!' to look up documentation for the `package!' macro.


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;; (package! some-package)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/radian-software/straight.el#the-recipe-format
;; (package! another-package
;;   :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;; (package! this-package
;;   :recipe (:host github :repo "username/repo"
;;            :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
;; (package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;; (package! builtin-package :recipe (:nonrecursive t))
;; (package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see radian-software/straight.el#279)
;; (package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
;; (package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;; (unpin! pinned-package)
;; ...or multiple packages
;; (unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;; (unpin! t)

;;
;; DISABLE DOOM PACKAGES
;;

(package! evil-snipe :disable t)
;; (package! autoparens :disable t)
(package! rainbow-mode :disable t)

;;
;; UTIL
;;

(package! autothemer)

;;
;; THEMES & COLORS
;;

(package! gruber-darker-theme)
(package! doric-themes)
(package! doom-two-tone-themes
  :recipe (:host github
           :repo "eliraz-refael/doom-two-tone-themes"
           :files ("doom-two-tone-themes.el" "themes/*.el")))
(package! nibelung-theme
  :recipe (:host github
           :repo "veschin/nibelung-theme"))

;;
;; LAYOUT
;;

;; https://github.com/mclear-tools/tabspaces
(package! tabspaces
  :recipe (:host github
           :repo "mclear-tools/tabspaces"))

;; https://github.com/quunnb/softresize
(package! softresize
  :recipe (:host github
           :repo "quunnb/softresize"))

;; https://github.com/TeMPOraL/nyan-mode
(package! nyan-mode)

;;
;; FUNCTIONALITY
;;

;; https://github.com/Dewdrops/evil-ReplaceWithRegister
(package! evil-replace-with-register
  :recipe (:host github
           :repo "Dewdrops/evil-ReplaceWithRegister"))

;; Indicate colors of hex values etc.
;; https://github.com/DevelopmentCool2449/colorful-mode
(package! colorful-mode)

;; Align stuff
;; https://github.com/edkolev/evil-lion
(package! evil-lion
  :recipe (:host github
           :repo "edkolev/evil-lion"))

;; Substitute selection or movement with register/clipboard
;; https://github.com/protesilaos/substitute
(package! substitute)

;; Editable file browser view
;; https://github.com/mwac-dev/grease.el
(package! grease
  :recipe (:host github
           :repo "mwac-dev/grease.el"))

;; Quick file bookmarks for Emacs, inspired by ThePrimeagen's Harpoon.
;; https://github.com/DamianB-BitFlipper/javelin.el
(package! javelin)

;; Move line or region forwards and backwards
;; https://github.com/alienbogart/cool-moves
(package! cool-moves
  :recipe (:host github
           :repo "alienbogart/cool-moves"))

;; {In/de}crement numbers at point
;; https://github.com/cofi/evil-numbers
(package! evil-numbers
  :recipe (:host github
           :repo "cofi/evil-numbers"))


;; https://github.com/abo-abo/hydra
(package! hydra)

;;
;; AI
;;

;;
;; LANGUAGE MODES ETC.
;;

;; https://github.com/dominikh/go-errcheck.el
(package! go-errcheck
  :recipe (:host github
           :repo "dominikh/go-errcheck.el"))

;; https://github.com/mcandre/vimrc-mode
(package! vimrc-mode)

;; https://github.com/holomorph/systemd-mode
(package! systemd)

;; https://github.com/jcs-elpa/impatient-showdown
(package! impatient-showdown)

;; https://github.com/taquangtrung/emacs-kdl-mode
(package! kdl-mode)

