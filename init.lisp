;;; init.lisp --- Vital settings and loading other files

;; Copyright © 2013-2016 Alex Kost <alezost@gmail.com>

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file should be symlinked by "~/.stumpwmrc".
;; I compile stumpwm with swank, so i don't need to load it.

;;; Code:
;;------------------------------------------------------------------------------------------------------------------------ ;;

(in-package :stumpwm)

(stumpwm:run-shell-command "xmodmap -e 'clear ['")
(setf *home-dir*      (user-homedir-pathname)
      *stump-dir*     (merge-pathnames (make-pathname :directory '(:relative ".stumpwm.d")) *home-dir*)
      *data-dir*      (merge-pathnames (make-pathname :directory '(:relative "storage")) *stump-dir*)
      *load-dir*      (merge-pathnames (make-pathname :directory '(:relative "lisp")) *stump-dir*)
      *undo-data-dir* (merge-pathnames (make-pathname :directory '(:relative "undo")) *data-dir*)
      *debug-file*    (merge-pathnames (make-pathname :name "debug") *data-dir*)
      *scratchpad-group-name* ".scratch" )


(defvar al/init-directory
  (directory-namestring
   (truename (merge-pathnames (user-homedir-pathname)
                              ".stumpwmrc")))
  "A directory with initially loaded files.")

(defun al/load (filename)
  "Load a file FILENAME (without extension) from `al/init-directory'."
  (let ((file (merge-pathnames (concat filename ".lisp")
                               al/init-directory)))
    (if (probe-file file)
        (load file)
        (format *error-output* "File '~a' doesn't exist." file))))

(set-module-dir
 (pathname-as-directory (concat (getenv "HOME")
                                "/src/stumpwm-contrib")))


;; For Creating a Swank Server to Interact with Stump
;; (directory "/home/arch/.emacs.d/elpa/*/swank-loader.lisp")
;; (pathname-as-directory "/home/arch/.emacs.d/elpa/*/swank-loader.lisp")
;; (pathname-directory "/home/arch/.emacs.d/elpa/*/swank-loader.lisp")

;; (pathname-directory "/home/arch/.emacs.d/elpa/*/swank-loader.lisp")
;; (load (make-pathname 
 ;; (directory "/home/arch/.emacs.d/elpa/*/swank-loader.lisp"))
 ;; (load (format nil "/home/arch/.emacs.d/elpa/slime-~a/swank-loader.lisp" :20161109.640))
(load "/home/arch/quicklisp/setup.lisp")
(load "/home/arch/.emacs.d/elpa/slime-20161109.640/swank-loader.lisp")

(swank-loader:init)
;;------------------------------------------------------------------------------------------------------------------------ ;;
;; example usage: (al/load "lisp/example-directory/example-filename")

(dolist (m '(
             "lisp/load"
             "lisp/startup"
             "lisp/remember"
             "lisp/commands"
             "lisp/keymap"
             "lisp/vim"
             "lisp/toggle"
             "lisp/map"
             ;; "lisp/macro"
             "lisp/appearance"
             "lisp/modules"))
  (progn
    (al/load m)))

;;; END init.lisp
