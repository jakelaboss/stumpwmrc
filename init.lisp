;;; init.lisp --- Vital settings and loading other files

;; Copyright © 2013-2016 Alex Kost <alezost@gmail.com>

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file should be symlinked by "~/.stumpwmrc".
;; I compile stumpwm with swank, so i don't need to load it.

;;; Code:

(in-package :stumpwm)

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
(load "/home/arch/.emacs.d/elpa/slime-20161102.711/swank-loader.lisp")
(swank-loader:init)

;; Loading quicklisp
;; (defvar quicklisp-path "~/quicklisp")
;; (load (concat quicklisp-path "/slime-helper"))

;; (ql:quickload :swank)
;; (ql:update-all-dists)
;; (ql:update-client)


;; (al/load "lisp/*")
(al/load "lisp/startup")
(al/load "lisp/keymap")
(al/load "lisp/vim")
(al/load "lisp/toggle")
(al/load "lisp/map")
(al/load "lisp/macro")
(al/load "lisp/appearance")

;;; init.lisp ends here
