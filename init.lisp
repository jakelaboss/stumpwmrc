
;;; init.lisp --- Vital settings and loading other files

;; Copyright © 2013-2016 Jake LaBossier <jakelaboss@gmail.com>

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file should be symlinked to "~/.stumpwmrc".
;; I compile stumpwm with swank, so i don't need to load it. (but I do anyway to be safe)

;; Code:
;;------------------------------------------------------------------------------------------------------------------------ ;;

(in-package :stumpwm)

;; (load "/home/vagabond/.emacs.d/elpa/slime-20170209.1240/swank-loader.lisp")

;; (swank-loader:init)

;; (load "/home/vagabond/common-lisp/libraries/linux/stumpwm/lisp/startup.lisp")
;; (load "/home/vagabond/common-lisp/libraries/linux/stumpwm/lisp/commands/tile-group.lisp")

(mapcar #'load '(
                 ;; "/home/vagabond/common-lisp/libraries/linux/stumpwm/lisp/load.lisp"
                 "/home/vagabond/common-lisp/libraries/linux/stumpwm/lisp/var/global.lisp"
                 "/home/vagabond/common-lisp/libraries/linux/stumpwm/lisp/startup/startup.lisp"
                 "/home/vagabond/common-lisp/libraries/linux/stumpwm/lisp/vim.lisp"
                 "/home/vagabond/common-lisp/libraries/linux/stumpwm/lisp/appearance.lisp"
                 "/home/vagabond/common-lisp/libraries/linux/stumpwm/lisp/commands/commands.lisp"
                 "/home/vagabond/common-lisp/libraries/linux/stumpwm/lisp/commands/tile-group.lisp"
                 ;; "/home/vagabond/common-lisp/libraries/linux/stumpwm/lisp/modules.lisp"
                 "/home/vagabond/common-lisp/libraries/linux/stumpwm/lisp/keymap.lisp"
                 "/home/vagabond/common-lisp/libraries/linux/stumpwm/lisp/map.lisp"
                  ))

;;; END init.lisp

;; ssh -i "/path/to/keyfile" root@ec2-54-173-185-85.compute-1.amazonaws.com
