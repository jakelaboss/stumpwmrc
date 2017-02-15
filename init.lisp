;;; init.lisp --- Vital settings and loading other files



;; Copyright © 2013-2016 Jake LaBossier <jakelaboss@gmail.com>

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; ;; This file should be symlinked to "~/.stumpwmrc".
;; ;; I compile stumpwm with swank, so i don't need to load it.

;; ;;; Code:
;; ;;------------------------------------------------------------------------------------------------------------------------ ;;
(in-package :stumpwm)

;; (stumpwm:run-shell-command "xmodmap -e 'clear ['")
(setf *home-dir*      (user-homedir-pathname)
      *lisp-dir*     (merge-pathnames (make-pathname :directory '(:relative "common-lisp")) *home-dir*)
      *stump-dir*     (merge-pathnames (make-pathname :directory '(:relative "libraries/linux/stumpwm")) *lisp-dir*)
      *data-dir*      (merge-pathnames (make-pathname :directory '(:relative "storage")) *stump-dir*)
      *load-dir*      (merge-pathnames (make-pathname :directory '(:relative "lisp")) *stump-dir*)
      *undo-data-dir* (merge-pathnames (make-pathname :directory '(:relative "undo")) *data-dir*)
      *debug-file*    (merge-pathnames (make-pathname :name "debug") *data-dir*)
      *scratchpad-group-name* ".scratch" )


;; (set-module-dir
;;  (pathname-as-directory (concat (getenv "HOME")
;;                                 "/src/stumpwm-cont rib")))


(load "/home/vagabond/.emacs.d/elpa/slime-20170209.1240/swank-loader.lisp")

(swank-loader:init)

;; (setf *stump-true-path* "/home/vagabond/common-lisp/libraries/linux/stumpwm")
;;; END init.lisp


(mapcar #'load '(
                 "/home/vagabond/common-lisp/libraries/linux/stumpwm/lisp/load.lisp"
                 "/home/vagabond/common-lisp/libraries/linux/stumpwm/lisp/var/global.lisp"
                 "/home/vagabond/common-lisp/libraries/linux/stumpwm/lisp/startup.lisp"
                 "/home/vagabond/common-lisp/libraries/linux/stumpwm/lisp/vim.lisp"
                 "/home/vagabond/common-lisp/libraries/linux/stumpwm/lisp/appearance.lisp"
                 "/home/vagabond/common-lisp/libraries/linux/stumpwm/lisp/commands/commands.lisp"
                 "/home/vagabond/common-lisp/libraries/linux/stumpwm/lisp/commands/tile-group.lisp"
                 "/home/vagabond/common-lisp/libraries/linux/stumpwm/lisp/keymap.lisp"
                 "/home/vagabond/common-lisp/libraries/linux/stumpwm/lisp/map.lisp"
                  ))
