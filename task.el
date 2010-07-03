;;; task.el --- Enhanced window configuration manager

;; Copyright (C) 2004  Matthieu Lemerre

;; Author: Matthieu Lemerre <racin@free.fr>
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Presentation:

;; task-mode is a global minor mode which enables you to effectively
;; do many tasks in the same time. For instance, you might want to
;; edit some Lisp code, then some people on ERC tell you about a bug
;; in one of your C program, and you proceed on editing some C
;; code. Every time you switch from these different tasks, your window
;; configuration will be saved so that you can switch back and forth
;; without thinking about saving your window configurations in
;; registers.


;; There is other packages similar to this one available for emacs, you will
;; find a list there: http://www.emacswiki.org/cgi-bin/wiki/CategoryWindows.
;; This package differs from the others because the normal way to
;; switch from task to task is to switch buffer. Task-mode will
;; regognize if a buffer is not in the current task and then will
;; bring the good task.

;; This method has the following advantages:
;; * You don't have to remember a task name or number like in other
;;   packages.
;; * Switching from task is not attached to a keybinding, so you can
;;   take advantage of this package with any function which switch
;;   buffer. (For instance, M-x gnus, erc-track-switch-buffer,
;;   tla-add-log-entry ...)

;; It has the following drawback:
;; * You have to use a keybinding when putting a buffer from a task to
;;   an other.

;; Automatically switching of window-configurations is the "visible"
;; part of task-mode. Everything that could benefinit from the fact that
;; buffers are grouped together has a place in this package. For
;; instance, in this release is included
;; task-switch-buffer-within-task-i{do,switch} which enable quick
;; switching between buffers in a task. 

;; Installation:

;; Put task.el in your load-path, and put (require 'task) in your .emacs.
;; Put (task-mode) in your .emacs if you want task-mode to be
;; permanently activated, else use M-x task-mode


;; Configuration:

;; Use M-x customize-group RET task RET to get an overview of all the
;; variables you can tweak.  You should read the documentation on my
;; home page (http://racin.free.fr)

;; Usage:

;; See the documentation (task-mode's home page is currently
;; http://racin.free.fr)

;; Suggestions:

;; All suggestions, bug reports, feature requests, critiques, elisp
;; advices, patches are welcome! (I'm an elisp newbie).  Please mail
;; to me (racin at free dot fr).

;; As task-mode is still young, some of the internal structures may
;; change, so you should write to me if you plan to make an important
;; contribution, so that we can coordinate our work. I'll then put
;; this on a free software hosting service like gna! or savannah.

;; Todo:

;; *Saving of current point when switching between buffers.

;; Planned:

;; * Possibility to merge two tasks in the rules, and interactively
;; * Better doc on each function
;; * Strong integration between frames and tasks
;; * Buffers who can belong to several tasks

;; Thanks:

;; All the GNU Project's contributor (and especially Richard Stallman)
;; for Emacs and the GNU project.

;; Ivar Rummelhoff for winner-mode (some ideas on
;; window-configurations handling are inspired by it)


;; Stefan Reichör and Christophe Garion for their contributions, and
;; Lucas Bonnet and Gaetan Leurent for testing.


;;; Code:

(require 'ring)

;Configuration variables

(defvar task-global-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [?c] 'task-create)
    (define-key map [?k] 'task-kill)
    (define-key map [?r] 'task-reload)
    (define-key map [?Q] 'task-quit)
    (define-key map [?m] 'task-move-current-buffer-to-task)
    (define-key map [?b] 'task-move-buffer-to-current-task)
    (define-key map [?g] 'task-goto)
    (define-key map [?e] 'task-edit-rules)
    ;; (define-key map [?f] 'task-make-buffer-flying)
    (define-key map [?F] 'task-set-task-flying)
    (define-key map [?\C-z] 'task-quick-switch)
    (define-key map [?z] (if window-system 
			     'iconify-or-deiconify-frame
			   'suspend-emacs))
    (define-key map [?p] 'task-previous-conf)
    (define-key map [?n] 'task-next-conf)
    map)
  "Global keymap used by task-mode")

(defcustom task-prefix-key [(control z)]
  "*Prefix key for all task commands"
  :type '(choice (const [(control z)])
                 (const [(control x) (7)])
                 (const [(control /)])
                 (sexp))
  :group 'task
  :set  (lambda (var value)
          (if (boundp var)
              (global-unset-key (symbol-value var)))
          (set var value)
          (global-set-key (symbol-value var) task-global-keymap)))

(defcustom task-display-task 't
  "* If non-nil, display the name of the buffer's task in the mode-line."
  :type '(boolean)
  :group 'task)


(defcustom task-properties
  '((task-Flying . (:rules nil))
    ;;The misc task is by default flying, so that it acts as normal and does 
    ;;not disturb the user by default.
    (task-Misc . (:rules "^ ?\\*.*\\*$"
			 :flying t)))
  "*Set of properties associated with each task.
 You can define the following properties:
*rules: 
  A buffer BUFFER is said to match the rule when one of the following requirement is fulfilled:
    -RULES is a regexp string and BUFFER's name matches it.
    -RULES is the construct (mode \"MAJOR-MODE\"), and MAJOR-MODE is BUFFER's major mode.
    -RULES is the construct (dir \"DIR\"), and BUFFER comes from a file located under DIR's subtree.
    -RULES is a function, its return value is a rule that BUFFER match.
    -RULES is a list whose every atom is one of the above elements, and BUFFER match one of them.
When creating a new buffer BUFFER, it is added to the first task whose BUFFER match its rules.
If there isn't any, it is added to the current task.
 -kill-function: function called when the task is manually killed
 -flying: says whether the buffers in the task are flying by default"
;;  :type '(alist :key-type symbol :value-type '(plist))
  :type '(alist)
  :group 'task)

(defcustom task-window-configuration-ring-max 20
  "*Maximum length of window configuration ring of each task before oldest elements are thrown away.
Currently, more than 2 does not work well."
  :type 'integer
  :group 'task)


(defcustom task-create-hook nil
  "*Hook run at the end of function `task-create'"
  :type 'hook
  :group 'task)

(defcustom task-edit-rules-after-create nil
  "*If non nil, call `task-edit-rules' after a call to `task-create'.
Generally a good idea if you already have a consequent `task-properties'.

Setting this variable directly does not take effect;
use \\[customize] or put (add-hook 'task-create-hook 'task-edit-rules)
in your .emacs ."
  :set (lambda (symbol value) 
	 (if value
	     (add-hook 'task-create-hook 'task-edit-rules)
	   (remove-hook 'task-create-hook 'task-edit-rules)))
  :initialize 'custom-initialize-default
  :type 'boolean
  :group 'task)

(defcustom task-iswitchb-put-task-to-beginning nil
  "*If non nil, then call to `iswitchb-buffer' will put buffers in the current task in the beginning.
Note that it can be annoying if you use C-x b RET to quickly switch
between tasks, you should use `task-quick-switch' instead.

Setting this variable directly does not take effect; use \\[customize]
or put (add-hook 'iswitchb-make-buflist-hook
'task-iswitchb-task-to-beginning) in your .emacs ."
  :require 'iswitchb
  :set (lambda (symbol value) 
	 (if value 
	     (add-hook 'iswitchb-make-buflist-hook 'task-iswitchb-task-to-beginning)
	   (remove-hook 'iswitchb-make-buflist-hook 'task-iswitchb-task-to-beginning)))
  :initialize 'custom-initialize-default
  :type 'boolean
  :group 'task)


;;Generic "accessing" functions and test functions

(defun task-name (task)
  "Get the name of a task, or nil if it does not exist"
  (if task
      (substring (symbol-name task) 5)))

(defun task-get-task (name)
  "Get the task named NAME (a string).
If there is no existing task named NAME, return nil.
NAME may also be a task; if so; the value is that task."
  (if (stringp name)
      (intern-soft name)
    (if (taskp name)
	name)))

(defun task-rules (task)
  "Get the rules of a task, or nil if it does not exist"
  (get task :rules))

(defun taskp (object)
  "Returns t if OBJECT is a task"
  (memq object task-list))




(defvar task-list nil
  "The list of all defined tasks.")

;;We associate to each buffer the name of a task
(defvar task-buffer-hash nil
  "The hash in wich each buffer is associated one task.")

(defun task-create (task-name)
  "Creates a task.
   A task is caracterised by several things:
-Its name
-Its list of buffers
-Its rules
...
When creating a task, all buffer matching the rules of the buffer are
automatically added to the task. When creating a new buffer, this
buffer belongs to the first task matching the rules. When creating a
task and whithout an argument, the buffer in which you are determine
the default task and rules, so that the buffer is automatically added
to the new task"
  (interactive
   (let ((future-task (task-find-initial-corresponding-task (current-buffer))))
     (if (eq future-task task-current) (setq future-task nil))
     (list 
      (task-read-task "What is the task name? " future-task t))))
  (unless task-mode (error "Please launch task-mode before using this command"))

  (if (memq (task-get task-name) (task-current-list))
      (message "This task already exists. Please choose another one")
    
    (let ((buffer (current-buffer)))
      (if task-current
	  (task-replace-buffer-in-windows buffer task-current))
      (task-new task-name buffer))
    (run-hooks 'task-create-hook)))



;;; Functions for handling the attributes

(defun task-persistentp (task)
  (get task :persistent))


(defun task-flyingp (task)
  (get task :flying))


(defun task-set-task-flying (&optional set task)
  "Toggle the flyingness of task TASK. 
When called interactively, TASK is the `task-current'.
If SET is non nil and is a number, make it flying if and only if positive."
  (interactive "p")
  (if (interactive-p) (setq task task-current))
  (let ((flying-status 
	 (if (> set 0) t
	   (not (task-flyingp task)))))
    (put task :flying flying-status)))


(defun task-current-list ()
  "Computes the list of all task currently present in the
task-buffer-hash hashtable"
  (let (task-list)
    (maphash (lambda (x y)
	       (unless (member y task-list)
		 (setq task-list (cons y task-list))))
	     task-buffer-hash)
    task-list))



(defun task-match (buffer p)
  "Returns non-nil iff buffer matches p.
 See `task-properties' to see the different rules available."
  (when p
    (cond ((stringp p)
	   (string-match p (buffer-name buffer)))
	  ((symbolp p)
	   p)
	  ((listp p)
	   (cond
	    ((eq (car p) 'mode)
	     (save-excursion
	       (set-buffer buffer)
	       (eq (cadr p) major-mode)))
	    ((eq (car p) 'dir)
	     (save-excursion
	       (set-buffer buffer)
	       (when buffer-file-name
		 (string= (file-name-directory (expand-file-name buffer-file-name))
			  (expand-file-name (cadr p))))))
	    ((functionp (car p))
	     (task-match buffer (eval p)))
	    (t (or (task-match buffer (car p))
		   (task-match buffer (cdr p)))))))))

(defun task-find-initial-corresponding-task (buffer &optional tell-if-not-found)
  "Search the corresponding task according to the rules defined in
each symbol task-smth. The list of symbols is defined in symbol list.
If TELL-IF-NOT-FOUND is non nil, returns nil if not suitable task was found,
else returns the current task"
  (let ((corresponding-task)
	(list task-list))
    (while (and list (null corresponding-task))
      (if (task-match buffer (get (car list) :rules))
	  (setq corresponding-task
		(if (get (car list) :add) task-current
		  (car list)))
	(setq list (cdr list))))
    (if corresponding-task
	corresponding-task
      (unless tell-if-not-found  task-current))))

(defun task-find-corresponding-task (buffer)
  "Find in which task is a a buffer"
  (gethash buffer task-buffer-hash))


;;TODO: replace task-current and task-old by a task ring.
;;When we are switching of buffer, the hook is executed twice.
;;When there is only a change in the window configuration, 
;;it is executed only once.
(setq task-current nil)

;;The last task
(setq task-old nil)


;;Task-hook function plus dependences

(defvar task-is-switching nil
  "Non-nil if window configuration changes are not handled by `task-hook-function'.  
This variable must be set if you want to change the window
configuration without switching between tasks. Use
`task-prevent-switching' for setting this function.")
;;(setq task-is-switching nil)


;;When we are switching buffer, it is in fact called twice: The first
;;time current-buffer is the buffer in which we are, the second time
;;it is the buffer that is requested
(defun task-hook-function ()
  "Core function of task-mode.
This function is called whenever a window configuration change, thus
placing new buffers in the right tasks, switching beetween tasks, etc."
  (let  ((current-task
	  (task-find-corresponding-task (current-buffer)))
	 (first-character
	  (string-to-char (buffer-name (current-buffer)))))
    (when (not (char-equal first-character ? ))
      	  ;;The task exist
	  (if current-task
	      (if  (and (not (task-flyingp current-task))
			(not task-is-switching))
		  (if (eq task-current current-task)
		      (task-save-conf current-task)
		    ;;We are changing of task
		    (let ((target-buffer (current-buffer)))
		      (task-goto current-task)
		      (task-display-buffer-in-appropriate-window target-buffer))))
	    (task-new-buffer))))
  (setq task-is-switching-by-kill nil))

(defun task-save-conf (current-task)
  "Push the current window configuration on top of this task's window configuration."
  ;;We get rid of all unwanted window-configurations
  (unless (eq (get current-task :window-configuration-counter) 0);is it speeder?
    (dotimes (i (get current-task :window-configuration-counter))
      (ring-remove (get current-task :window-configuration-ring) 0))
    (put current-task :window-configuration-counter 0))
  (ring-insert (get current-task :window-configuration-ring) 
	       (list 
		(current-window-configuration)
		(selected-window)
		(window-point))))


(defun task-load-conf (current-task &optional number)
  "Reload the saved window configuration.
  This function should not be used directly, use task-goto instead.
  With an argument, loads the last nthieme window configuration"
  (task-prevent-switching
   (let* ((real-number (if number number 0))
	  (window-conf (ring-ref (get current-task :window-configuration-ring)
				 real-number)))
     (when window-conf 
	 (set-window-configuration (car window-conf))
	 (if (window-live-p (cadr window-conf))
	     (set-window-point (cadr window-conf) (caddr window-conf)))))))


(defmacro task-prevent-switching (&rest body)
  "You must use this macro to prevent infinite recursion when
operating on tasks.  
Executes BODY just like `progn'"
  `(let ((task-is-switching t))
     ,@body))


(defun task-goto (task &optional save-current-conf)
  "Respawn the window configuration named taskname, changing task-current and task-old.
If SAVE-CURRENT-CONF is non-nil, we save the configuration of task-current (done if called interactively)"
  (interactive (list
		(task-read-task "Go to what task? " task-old )
		t))
  (if task
      (if (not (eq task task-current))
	  (list
;	   (task-save-conf task-current)
	   (task-load-conf task)
	   (setq task-old task-current)
	   (setq task-current task))
	   ;;If we switched of task, we have saved the window configuration of
	   ;;task-old. So we erase this save, otherwise we have too much window
	   ;;configurations
	   ;(ring-remove (get task :window-configuration-ring) 0))

	(if (interactive-p) 
	    (message (concat "You were already in task " (task-name task)))))
    (error "Task does not exist")))


;;Maybe we should have all our function have task argument "task or name",
;;like for buffers
(defun task-get (taskname)
  "Get the task symbol of task name task, or nil if not found."
  (let ((task-list (task-current-list))
	task)
    (while (and (not task) task-list)
      (if (string-equal taskname (task-name (car task-list)))
	  (setq task (car task-list)))
      (setq task-list (cdr task-list)))
    task))

(defun task-switch-after-kill ()
  "This function is here to prevent task-switching when killing a buffer.
   It selects th first buffer of the buffer list which is in the task.
   Later, we might add the possibilty to delete the window when there
is no more useful buffer in the task to display."
  (setq task-is-switching-by-kill nil)
  
  (let ((the-chosen-buffer (task-best-buffer task-current)))
    (if the-chosen-buffer
	(task-prevent-switching
	 (switch-to-buffer the-chosen-buffer)))))


(defun task-replace-buffer-in-windows (buffer task &optional default-task)
  "Hides buffer BUFFER from task TASK, choosing good windows to
replace it, or deleting window when it is smart to do so and 
`task-delete-window-if-needed' is set.
If BUFFER was the last buffer in its task, we switch to DEFAULT-TASK.
If it does not exist, we find another task."
  (let ((displayed-buffers (mapcar 'window-buffer (window-list))))
    (if (>= (length (delq buffer (task-buffer-list task))) 1)
	;;we can remplace BUFFER
	(save-task-excursion
	 (task-goto task)
	 (save-selected-window
	   ;;get-buffer-window-list
	   (dolist (window (window-list))
	     (when (eq (window-buffer window) buffer)
	       (select-window window)
	       (let ((replacement-buffer 
		      (task-best-buffer 
		       task 
		       (cons buffer displayed-buffers))))
		 (if replacement-buffer
		     (switch-to-buffer replacement-buffer)
		   (delete-window)))))
	   (task-save-conf task)))
      ;;We need to switch to another task
      (let ((current-list  (task-current-list)))
	(if (and default-task (memq default-task current-list))
	    (task-goto default-task)
	  (task-goto (car current-list)))))))
  
(defun task-best-buffer (task &optional not-these-buffers)
  "Returns the buffer to be displayed in task TASK, nil if there is no good candidate.
If buffer list NOT-THESE-BUFFERS is provided, the result can not belong to NOT-THESE-BUFFERS"
  (let ((buffer-list (buffer-list))
	(potential-buffers (task-buffer-list task))
	the-chosen-one)
    (if not-these-buffers
	(mapc (lambda (buffer) 
		(setq potential-buffers
		      (delq buffer potential-buffers)))
	      not-these-buffers))
    (when potential-buffers
      (while (not the-chosen-one)
	(if (memq (car buffer-list) potential-buffers)
	    (setq the-chosen-one (car buffer-list))
	  (setq buffer-list (cdr buffer-list))))
      the-chosen-one)))


(defun task-buffer-list (task)
  "Returns the list of buffers of task TASK"
  (let (list)
    (maphash
     (lambda (x y) (if (equal y task) (setq list (cons x list))))
     task-buffer-hash)
    list))


(defun task-display-buffer-in-appropriate-window (buffer)
  "When switching task, select in which window will the requested
buffer be displayed.

*If the buffer is already displayed, we choose that window, else we
 choose the first we can.

*If it does not please the user, he can use `task-previous-conf' and place
the buffer himself."
  ;;No need to use task-prevent-switching for this function
  (let ((potential-windows (get-buffer-window-list buffer)))
		  (if potential-windows
		      (select-window (car potential-windows))
		    (switch-to-buffer buffer))))


(defun task-new-buffer ()
  "If current buffer is not in a task, this function add it to one,
according to task-new-buffer-handler. It creates a task if needed."
  (let ((this-task (task-find-initial-corresponding-task (current-buffer))))
    (if (not (member this-task (task-current-list)))
	(task-new this-task (current-buffer))
      (puthash (current-buffer)
	       this-task
	       task-buffer-hash))))

(defun task-new (task &optional buffer)
  "Function to call when a new task is created.

   TASK can either be a symbol or a name, if it didn't exist it will
be interned.  If BUFFER is specified, then the new task will contains
only one window with BUFFER in it.

   It returns an interned symbol of name task-NAME, inits it, and sets
task-current and task-old .  If it existed before, returns nil."
  (let ((real-task
	 (if (symbolp task)
	     task
	   (if (stringp task)
	       (intern (concat "task-" task))))))
    (if real-task
	(list
	 (setq task-old task-current)
	 (setq task-current real-task)
	 (push real-task task-list)
	 (put real-task :window-configuration-ring
	      (make-ring task-window-configuration-ring-max))
	 (put real-task :window-configuration-counter 0)
	 (if (stringp task) (put real-task :name task))
	 (when buffer
	   (puthash buffer real-task task-buffer-hash)
	   (setq task-is-switching t)
	   (switch-to-buffer buffer)
	   (delete-other-windows)
	   (setq task-is-switching nil)
	   (task-save-conf real-task)
	   (force-mode-line-update t)))
      (error "Task is neither an existing symbol nor a string"))
    real-task))



;;Initialisation functions

(define-minor-mode task-mode
  "Toggle Task mode.
With prefix ARG, turn Task mode on if and only if ARG is positive.
Returns the new status of Task mode (non-nil means on).

When Task mode is enabled, you can group your buffers in tasks, and
use various commands which make use of this grouping. Moreover,
switching between buffers of different tasks respawn the window
configuration associated with the called task. Read the tutorial on
http://racin.free.fr" 
  :global t :group 'task 
  (if task-mode (task-init) (task-quit)))



;; Should we do this? maybe we should only save task-current and task-old here.
(defadvice save-window-excursion (around task-save-window-excursion (&rest args))
  "Prevent switching tasks in save-window-excursion."
  (let ((task-is-switching t))
    ad-do-it))


(defmacro  save-task-excursion (&rest body)
  "Returns to the current task after switching. If it does not exist, find another task."
  `(let ((save-task-current task-current)
	 (save-task-old task-old))
	 (unwind-protect 
	     (progn ,@body)
	   (let* ((after-task-old 
		   (if (memq save-task-old (task-current-list))
		       save-task-old))
		  (after-task-current 
		   (if (memq save-task-current (task-current-list))
		       save-task-current
		     (if after-task-old 
			 after-task-old
		       (car (task-current-list))))))
	     (task-goto after-task-current)
	     (setq task-current after-task-current)
	     (setq task-old after-task-old)))))



(defun task-init ()
  "Initializes task-mode. Don't use this directly"
  (global-set-key task-prefix-key task-global-keymap)
  ;;Do we need to look inthe hook if we are already here?
  (add-hook 'window-configuration-change-hook 'task-hook-function)
  (add-hook 'kill-buffer-hook 'task-kill-hook-function t)
  (task-init-task-properties)
  (setq task-buffer-hash (make-hash-table
			  :test 'eq
			  :weakness t))
  (let ((first-task (task-find-initial-corresponding-task (current-buffer) t)))
    (if (not first-task) (setq first-task 'task-Misc))
    (task-new  first-task (current-buffer)))
  (ad-activate 'save-window-excursion)
  (when (and task-display-task
             (not (member 'task-mode (mapcar 'car-safe global-mode-string))))
    (setq global-mode-string
	  (append global-mode-string
		  '((task-mode (:eval
				(concat "[" 
					(task-name (gethash
						    (current-buffer)
						    task-buffer-hash))
					"]" ))))))))

(defun task-init-task-properties ()
  "Initialize the plist"
  (setq task-list nil)
  (mapc (lambda (x) (setplist (car x) (cdr x))) task-properties)
  (mapc (lambda (x) (push (car x) task-list)) task-properties)
  ;;The order must be reversed.
  (setq task-list (reverse task-list)))

(defun task-kill-hook-function ()
   "This function clean things up when a buffer is killed"
   (remhash (current-buffer) task-buffer-hash)
   (task-replace-buffer-in-windows (current-buffer) task-current task-old))

(defun task-quit ()
  "Removes switch-buffer-goto-screen.
   If task-mode is hang, use this to be able to use emacs normally"
  (interactive)
  (remove-hook 'window-configuration-change-hook 'task-hook-function)
  (remove-hook 'kill-buffer-hook 'task-kill-hook-function))


;;Window configuration management functions

;(defun task-reload ()
;  "Reload the saved window configuration for the current task. Useful
;for exemple to get rid of pop-up buffers, help entries, or any buffers
;that are not part of your task"
;  (interactive)
;  (task-previous-conf 0))


(defun task-previous-conf (&optional n)
  "Respawn the previous window configuration of the task.
  With an argument, spawn the last nieme window configuration
  (0 is the current window-configuration)"
  (interactive "p")
  (let* ((number (if n n 1))
	 (index (+ number (get task-current :window-configuration-counter)))
	 (total-length (ring-length (get task-current :window-configuration-ring))))
    (if (< index total-length)
	(list
	 (put task-current :window-configuration-counter index)
	 (task-load-conf task-current index)
	 (message "Undo window configuration for task %s : %d/%d" 
		  (task-name task-current) index (1- total-length)))
	 (message "You have reached the last undo information"))))


(defun task-next-conf (&optional n)
  "Respawn the previous window configuration of the task.
  With an argument, spawn the last nieme window configuration
  (0 is the current window-configuration)"
  (interactive "p")
  (let* ((number (if n n 1))
	 (index (- (get task-current :window-configuration-counter) number))
	 (total-length (ring-length (get task-current :window-configuration-ring))))
    (if (>= index 0)
	(list
	 (put task-current :window-configuration-counter index)
	 (task-load-conf task-current index)
	 (message "Undo window configuration for task %s : %d/%d" 
		  (task-name task-current) index (1- total-length)))
	 (message "You have reached the last redo information"))))





(defun task-edit-rules (&optional dont-find-matching-buffers task)
  "Edit TASK's rules in the minibuffer (`task-current' if ommitted).
If there is no prefix arg, all present buffers matching this rule will
be added to the task; else this operation is not performed."
  (interactive "P")
  (unless task
    (setq task task-current))
  (let ((task-rules
	 (task-read-rules "Which is the task rule? " 
			  nil
			  task
			  (current-buffer))))
    (put task-current :rules task-rules)
    (unless dont-find-matching-buffers
      (dolist (element (buffer-list) nil)
	(if (task-match element task-rules)
	    (puthash element  task-current  task-buffer-hash))))))


;;; Changing buffers of task

(defun task-move-buffer-to-task (buffer task)
  "Move a buffer to a task, modifying window configurations which need to."
  (interactive (list
		(get-buffer (read-buffer "Move which buffer? " (current-buffer)))
		(task-read-task "Move to what task? " task-old)))
  (let ((task-orig (task-find-corresponding-task buffer)))
    (puthash buffer task task-buffer-hash)
    (task-replace-buffer-in-windows buffer task-orig task)
    (message (format "Moved buffer \"%s\" to task \"%s\"" buffer task))))
		   
(defun task-test ()
  (interactive)
  (task-mode)
  (find-file "a")
  (task-create "test")
  (find-file-other-window "b")
  (find-file "c")
  (setq command-history '((task-move-buffer-to-task))))


(defun task-move-buffer-to-current-task (buffer)
  "Move a buffer to current task, then display it"
  (interactive (list
		(get-buffer (read-buffer "Move which buffer? " (other-buffer)))))
  (task-save-conf task-current)
  (task-move-buffer-to-task buffer task-current)
  (switch-to-buffer buffer))

(defun task-move-current-buffer-to-task (task)
  "Move the current buffer to task."
  (interactive (list (task-read-task "Move to what task? " task-old)))
  (task-move-buffer-to-task (current-buffer) task))


(defun task-make-buffer-flying (choose-buffer)
  (interactive "P")
  (let ((buffer
	 (if choose-buffer
	     (get-buffer (read-buffer "Add which buffer? " (other-buffer)))
	   (current-buffer))))
    (puthash buffer 'task-Flying task-buffer-hash)))



;;Switching buffers in a task

;(modified) contribution from Christophe Garion
(defun task-switch-buffer-within-task-ido ()
  "Enable to choose a buffer within the current task. Requires iswitchb"
  (interactive)
  (if (require 'ido nil t)
      (let ((save-ido ido-ignore-buffers)
	     ido-ignore-buffers
	    (enable-buffers-from-task;removes the buffers in the task from ignore list.
	     (lambda (task)
	       (mapc (lambda (element)
		       (delq element  ido-ignore-buffers))
		     (task-buffer-list task))))
	    display-buffer-task-list)
	(setq  ido-ignore-buffers (buffer-list))
	(mapc (lambda (task)
		(if (task-persistentp task)
		    (push task display-buffer-task-list)))
	      (task-current-list))
	(add-to-list 'display-buffer-task-list task-current)
	(mapc enable-buffers-from-task display-buffer-task-list)
	(setq  ido-ignore-buffers (mapcar (lambda (elt)
					       (concat "^" (regexp-quote (buffer-name elt)) "$"))
					      ido-ignore-buffers))
	(setq  ido-ignore-buffers (append save-ido  ido-ignore-buffers))
	(ido-switch-buffer))
    (message "ido could not be found")))

(defun task-switch-buffer-within-task-ido ()
  "Enable to choose a buffer within the current task. Requires ido"
  (interactive)
  (if (require 'ido nil t)
      (let ((save-ido ido-ignore-buffers))
	(setq ido-ignore-buffers (buffer-list))
	(mapc (lambda (element)
		(delq element ido-ignore-buffers))
	      (task-buffer-list task-current))
	(setq ido-ignore-buffers (mapcar (lambda (elt)
					   (regexp-quote (buffer-name elt)))
                                       ido-ignore-buffers))
	(print ido-ignore-buffers)
	(ido-switch-buffer)
      (setq ido-ignore-buffers save-ido))
    (message "ido could not be found")))

;Iswitchb version of above
(defun task-switch-buffer-within-task-iswitchb ()
  "Enable to choose a buffer within the current task. Requires iswitchb"
  (interactive)
  (if (require 'iswitchb nil t)
      (let ((save-iswitchb iswitchb-buffer-ignore)
	    iswitchb-buffer-ignore
	    (enable-buffers-from-task;removes the buffers in the task from ignore list.
	     (lambda (task)
	       (mapc (lambda (element)
		       (delq element iswitchb-buffer-ignore))
		     (task-buffer-list task))))
	    display-buffer-task-list)
	(setq iswitchb-buffer-ignore (buffer-list))
	(mapc (lambda (task)
		(if (task-persistentp task)
		    (push task display-buffer-task-list)))
	      (task-current-list))
	(add-to-list 'display-buffer-task-list task-current)
	(mapc enable-buffers-from-task display-buffer-task-list)
	(setq iswitchb-buffer-ignore (mapcar (lambda (elt)
					       (concat "^" (regexp-quote (buffer-name elt)) "$"))
					     iswitchb-buffer-ignore))
	(setq iswitchb-buffer-ignore (append save-iswitchb iswitchb-buffer-ignore))
	(iswitchb-buffer))
    (message "iswitchb could not be found")))

;;There should be a normal version too, but iswitchb is included in emacs 21


(defun task-iswitchb-task-to-beginning ()
  "Put the buffers in the current task to the beginning of the iswitchb list"
  (let  (this-task-buffer-list) ;they are keep in the right order
    (mapc (lambda (buffer-name) 
	    (when (eq (task-find-corresponding-task (get-buffer buffer-name)) task-current)
	      (setq this-task-buffer-list (cons buffer-name this-task-buffer-list))))
	  iswitchb-temp-buflist)
    (mapc (lambda (x)
	    (delete x iswitchb-temp-buflist))
	  this-task-buffer-list)
    (setq iswitchb-temp-buflist (append (reverse this-task-buffer-list) iswitchb-temp-buflist))))



(defun task-quick-switch ()
  "Go to task-old, and sets task-current as task-old. Useful when quickly switching beetween two tasks"
  (interactive)
  (task-goto task-old t))


;; Killing task

(defun task-default-kill-function (task)
  "Kill all buffers in the task."
  (mapc 'kill-buffer (task-buffer-list task)))


(defun task-kill (task)
  "Kill the current task, thus calling the function defined in :kill-function, then `task-default-kill-function'"
  (interactive (list task-current))
  (let ((kill-function (get task :kill-function)))
    (if kill-function
	(funcall kill-function task))
    (task-default-kill-function task)))

;; Input functions

(defun task-read-task (prompt-string &optional default new)
  "Ask for a task. 
PROMPT-STRING is a string. DEFAULT is a task or a task name, which is
the default.  If NEW is defined and non nil, you can create a new
task , completion is done on the task rules and only the string is
returned, not the symbol.  Note: you should most of the times use
either task-current or task-old for the default."
  (if (not (stringp prompt-string))
      (error "Wrong argument: stringp, prompt")
    (let* ((completion-list
	    (mapcar (lambda (task)
		      (cons (task-name task) task))
		    (if new task-list (task-current-list))))
	   (string (if default
		       (format "%s(default %s) " prompt-string (task-name default))
		     prompt-string))
	   (task-name (completing-read string completion-list
				 nil (not new) nil 'task-history
				 (task-name default))))
	   (if new task-name (task-get task-name)))))


(defun task-read-rules (prompt-string &optional default task buffer)
  "Asks for rules of a task.

 PROMPT is a string. The string (use nil if none) is concatened to
PROMPT-STRING.

If DEFAULT is provided, it is used as the default.
Else, If TASK is provided, its current rule is the default.
Else, if BUFFER is provided, we use
 the rule of the first matching task in task-properties.
If there isn't any, it is (mode buffer's major-mode).
There is noticiables exceptions:
-If current buffer is in dired mode, default is (dir current-buffer)
-If it is in fundamental mode, default is nil. Most people will not want
a non-flying task with all their buffer in fundamental mode.

If neither DEFAULT nor TASK nor BUFFER is provided, then default is nil."
  (unless default
    (if (get task :rules)
	(setq default (get task :rules))
      (setq default
      (when buffer
	(save-excursion
	  (set-buffer buffer)
	  (case major-mode
	    (dired-mode (list 'dir (file-name-directory dired-directory)))
	    (fundamental-mode nil)
	    (t
	     (let ((initial-task
		    (task-find-initial-corresponding-task buffer t)))
	       (setq default 
		     (if initial-task
			 (get initial-task :rules)
		       `(mode ,major-mode)))))))))))
 (read-from-minibuffer (concat prompt-string "(type nil if none): ")
			(prin1-to-string default)
			nil t 'task-rule-history))


;;;Misc functions

;(defun task-rassoc-all (hashtable value)
;  "Find all keys whose value is 'equal' to VALUE"
;  (let (list)
;    (maphash 
;;(lambda (x y) (if (equal y value) (setq list (cons x list)))) hashtable)
;    list))
	
	

(provide 'task)
;;; task.el ends here

