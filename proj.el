(require 'pp)
(require 'cl-lib)

;; (defun proj-bs-not-in-project (buffer)
;;   "Filter buffers for current projects or none"
;;   (interactive))
;; 
;; (defun proj-bs-different-mode-different-project (buffer)
;;   "Filter buffers for current projects or none and same mode"
;; 	(interactive)
;; 	(or
;; 	 (proj-bs-different-project buffer)
;; 	 (bs-different-mode buffer)))
;; 
;; (add-to-list 'bs-configurations
;;              '("proj/mode"
;;                nil nil nil
;;                proj-bs-different-mode-different-project
;;                bs-sort-buffer-interns-are-last)
;;              )
;; 
;; (add-to-list 'bs-configurations
;;              '("proj"
;;                nil nil nil
;;                proj-bs-different-project
;;                bs-sort-buffer-interns-are-last)
;;              )

(defun nth-first (n list)
  (let ((tmp))
    (setq tmp list)
    (setq list (nthcdr n list))
    (setq tmp (butlast tmp (safe-length list)))
    (append list tmp)
    ))

(defun dump-obj-to-buffer (obj &optional buffer pretty erase)
  "Append arbitrary lisp object dump to buffer"
  (with-current-buffer (if buffer buffer
                         (get-buffer-create "*Dump*"))
    (buffer-disable-undo)
    (if erase
        (erase-buffer))
    (goto-char (point-max))
    (if (equal (point-min) (point-max))
        (insert ";; -*- mode: emacs-lisp  -*-")
      (insert ";; ----- Next object starts here ----"))
    (newline)
    (insert
     (if pretty
         (pp-to-string obj)
       (prin1-to-string obj)))
    (unless pretty (newline))
    (emacs-lisp-mode)
    ))

(defun rewind-read (&optional buffer)
  (with-current-buffer (if buffer buffer
                         (get-buffer-create "*Dump*"))
    (goto-char (point-min))))

(defun safe-read (&optional buffer eof-message)
  (condition-case err
      (read buffer)
    (end-of-file
     (if eof-message
         (message eof-message))
     nil)
    ))

(defun buffer-mode (buffer-or-string)
  "Returns the major mode associated with a buffer."
  (with-current-buffer buffer-or-string
    major-mode))

(defun buffer-dir (buffer-or-string)
  (with-current-buffer buffer-or-string
    default-directory))

(defun get-buffer-list ()
  "List of buffers in current frame"
  (let* ((wl (window-list))
         (bl (mapcar 'window-buffer wl)))
    (mapcar (lambda (b)
              (list (buffer-name b)
                    (buffer-file-name b)
                    (buffer-mode b)))
            bl)))


(defun read-all (buffer)
  "Read all expressions into a list"
  (with-current-buffer buffer
    (goto-char (point-min))
    (let* ((res nil)
           (item nil))
      (while (setq item (safe-read buffer))
        (setq res (append res (list item))))
      res)
    ))

(defun setassoc (i list val)
  (setf (alist-get i list) val))

(defun append-to (l val)
  (nconc l (list val)))

(defun delete-nth (i l)
  (delq (nth i l) l))

(defun read-file (path)
  (with-temp-buffer
    (insert-file-contents path)
    (goto-char (point-min))
    (read (current-buffer))))

;; ------------- buffers ------------------

(defun proj-buffer-info (buffer)
  (let* ((name (buffer-name buffer))
         (path (buffer-file-name buffer))
         (mode (buffer-mode buffer))
         (dir  (buffer-dir buffer)))
    (list path mode dir)
    ))

(defun proj-add-buffer (buffer)
  (interactive "b")
  (if (stringp buffer)
      (setq buffer (get-buffer buffer)))
  (let* ((bl (frame-parameter nil 'buffer-info-list))
         (name (buffer-name buffer))
         (binfo (proj-buffer-info buffer)))
    (unless (alist-get name bl nil nil 'equal)
      (setf (alist-get name bl nil nil 'equal) binfo)
      (message "Added new buffer '%s' to project" name)
      (set-frame-parameter nil 'buffer-info-list bl))
    ))

(defun proj-remove-buffer (buffer)
  (interactive
   (let ((string (completing-read "Name of buffer to remove:"
                                  (proj-buffer-list))))
     (list string)))
  (if (stringp buffer)
      (setq buffer (get-buffer buffer)))
  (let* ((bl (frame-parameter nil 'buffer-info-list))
         (name (buffer-name buffer)))
    (setf (alist-get name bl nil t 'equal) nil)
    (message "Removed buffer '%s' from project" name)
    (set-frame-parameter nil 'buffer-info-list bl)))

(defun proj-add-current-buffer ()
  (interactive)
  (proj-add-buffer (current-buffer)))

(defun proj-remove-current-buffer ()
  (interactive)
  (proj-remove-buffer (current-buffer))
  (bury-buffer (current-buffer)))

(defun proj-list-buffer-info-list ()
  (frame-parameter nil 'buffer-info-list))

(defun proj-clear-buffer-info-list ()
  (set-frame-parameter nil 'buffer-info-list nil))

(defun proj-get-buffer-info (name)
  (let* ((bl (frame-parameter nil 'buffer-info-list)))
    (append (list name) (alist-get name bl nil nil 'equal))
    ))

(defun proj-set-mode (buffer mode)
  "Set major mode of buffer"
  (with-current-buffer buffer
    (funcall mode))
  buffer)

(defun proj-dump-buffer-info-list (&optional  buffer pretty erase)
  (let* ((dump-buffer (if buffer buffer
                        (get-buffer-create "*Dump*")))
         (bl (frame-parameter nil 'buffer-info-list)))
    (mapc (lambda (b)
            (when (get-buffer (car b))
              (dump-obj-to-buffer b dump-buffer pretty erase)))
          bl)
    buffer))

(defvar proj-handled-modes nil)
(push 'inferior-emacs-lisp-mode proj-handled-modes)

(defun proj-add-buffer-for-import (buffer)
  (if (or (buffer-file-name buffer)
          (memq (buffer-mode buffer)
                proj-handled-modes))
      (proj-add-buffer buffer)))

(defun proj-import-buffers ()
  "Import frame buffer list into project"
  (interactive)
  (when (proj-get-name)
    (let* ((windows (window-list))
           (buffers (mapcar 'window-buffer windows)))
      (mapc 'proj-add-buffer-for-import buffers))))

(defun proj-buffer-list (&optional import)
  (when import
    (proj-import-buffers))
  (mapcar 'car (proj-list-buffer-info-list)))

(defun proj-buffer-in-project (&optional buffer)
  (unless buffer
    (setq buffer (current-buffer)))
  (let* ((bname (buffer-name buffer))
         (proj-buffers (proj-buffer-list)))
    (> (length (member bname proj-buffers)) 0)))

(defun proj-buffer-switch ()
  (interactive)
  (let ((buffer (completing-read "Switch to buffer:"
                                 (proj-buffer-list))))
    (switch-to-buffer buffer)))

(defun proj-buffer-info-update (frame)
  (let* ((bufnames (proj-buffer-list t))
         (buffer-info-list
          (loop for bufname in bufnames
                for buffer = (get-buffer bufname)
                if buffer
                collect (cons bufname
                              (proj-buffer-info buffer)))
          ))
    (set-frame-parameter frame 'buffer-info-list buffer-info-list)))


(defun proj-save-buffers (&optional frame)
  (interactive)
  (unless frame
    (setq frame (selected-frame)))
  (proj-buffer-info-update frame)
  (let ((bl (frame-parameter frame 'buffer-info-list)))
    (loop for bi in bl
          do (proj-buffer-info-save-buffer bi))))

(defun proj-buffer-info-save-buffer (buffer-info)
  (let* ((name (nth 0 buffer-info))
         (path (nth 1 buffer-info))
         (buffer (get-buffer name)))
    (when (and name buffer path)
      (message "Saving buffer '%s' in '%s'" name path)
      (with-current-buffer buffer
        (save-buffer)))))

;; --------- Recreating buffers ------------------

(defvar proj-recreating-buffers-p nil)

(defun proj-recreate-buffers()
  "Recreate all project buffers"
  (let* ((buffer-info-list (frame-parameter nil 'buffer-info-list))
         (proj-recreating-buffers-p t))
    (loop for buffer-info in buffer-info-list
          do (apply 'proj-buffer-recreate buffer-info))
    ))


(defun proj-buffer-recreate (name path mode &rest rest)
  "Find, reload or recreate buffer"
  (ignore-errors
    (if path
        (let* ((buffer (get-file-buffer path)))
          (unless buffer
            (setq buffer (proj-buffer-recreate-from-file name path mode rest)))
          buffer)
      (proj-buffer-recreate-for-mode name mode rest))
    ))


(defun proj-buffer-recreate-from-file (name path mode &rest rest)
  (let* ((buffer (find-file-noselect path))
         (autosave-name (with-current-buffer buffer (make-auto-save-file-name))))
    (message "Recreating %s from '%s'" name path)
    (when (recent-auto-save-p)
      (erase-buffer)
      (insert-file-contents autosave-name nil)
      )
    ;; (ignore-errors (proj-set-mode buffer mode))
    buffer))


(defvar proj-buffer-recreation-methods)

(defun proj-buffer-recreate-for-mode (name mode &rest rest)
  (let* ((recreation-method (alist-get mode proj-buffer-recreation-methods))
         (buffer (if recreation-method
                     (apply recreation-method (append (list name) rest))
                   (get-buffer-create name))))
    (message "Recreated %s with mode '%s'" name mode)
    ;; (ignore-errors (proj-set-mode buffer mode))
    buffer))

(setq proj-buffer-recreation-methods nil)

(defun proj-buffer-recreate-dired (name dir &rest rest)
  (message "Recreating dired-buffer %s in directory %s" name dir)
  (dired-noselect dir))

(push '(dired-mode . proj-buffer-recreate-dired)
      proj-buffer-recreation-methods)
(push 'dired-mode proj-handled-modes)

(defun proj-buffer-recreate-mu4e (&rest rest)
  (mu4e))

(push '(mu4e-main-mode . proj-buffer-recreate-mu4e)
      proj-buffer-recreation-methods)
(push 'mu4e-main-mode proj-handled-modes)

(defun proj-buffer-recreate-org-agenda (&rest rest)
  (org-agenda-list))

(push '(org-agenda-mode . proj-buffer-recreate-org-agenda)
      proj-buffer-recreation-methods)
(push 'org-agenda-mode proj-handled-modes)

(defun proj-buffer-recreate-iess (name dir &rest rest)
  (setq dir (car dir))
  (message "recreating iess-buffer %s in directory %s" name dir)
  (let ((buffer (get-buffer-create name))
        (ess-ask-for-ess-directory nil))
    (with-current-buffer buffer
      (setq default-directory dir)
      (when (string-match "*R" name)
        (run-ess-r)))))

(push '(inferior-ess-mode . proj-buffer-recreate-iess)
      proj-buffer-recreation-methods)
(push 'inferior-ess-mode proj-handled-modes)


;; --------- Project home dir --------------------


;; (defun proj-home-dir ()
;;   (interactive)
;;   (frame-parameter nil 'proj-home-dir))
;; 
;; (defun proj-set-home-dir (&optional path)
;;   "Set project home directory"
;; 	(interactive
;; 	 (list (read-directory-name "Choose directory: ")))
;;   (when path
;;     (set-frame-parameter nil 'proj-home-dir path))
;;   (frame-parameter nil 'proj-home-dir))


;; --------- winconfig ---------------------------


(defun proj-activate-winconfig (id)
  "Activate n-th window configuration in current frame"
  (let* ((wconfig-list (frame-parameter nil 'wconfig-list))
         (wstate (nth id wconfig-list)))
    (window-state-put wstate (frame-root-window))
    (set-frame-parameter nil 'wconfig-current-id id)
    wstate))

(defun proj-store-winconfig (id)
  "Store window configuration in current frame in n-th slot"
  (let* ((wconfig-list (frame-parameter nil 'wconfig-list))
         (lwc (length wconfig-list))
         (window-persistent-parameters nil)
         (wstate (window-state-get)))
    (when (and (>= id 0) (< id lwc)) 
      (setf (nth id wconfig-list) wstate)
      (set-frame-parameter nil 'wconfig-list wconfig-list)
      (proj-import-buffers)
      id)))

(defun proj-winconfig-length ()
  (length (frame-parameter nil 'wconfig-list)))

(defun proj-add-winconfig ()
  (let* ((wconfig-list (frame-parameter nil 'wconfig-list))
         (lwc (length wconfig-list))
         (window-persistent-parameters nil)
         (wstate (window-state-get)))
    (setq wconfig-list (append wconfig-list (list wstate)))
    (set-frame-parameter nil 'wconfig-list wconfig-list)
    (proj-import-buffers)
    lwc))

(defun proj-del-winconfig (id)
  (let* ((wconfig-list (frame-parameter nil 'wconfig-list))
         (lwc (length wconfig-list)))
    (when (and (>= id 0) (< id lwc)) 
      (setq wconfig-list (delete-nth id wconfig-list))
      (set-frame-parameter nil 'wconfig-list wconfig-list)
      id)))

(defun proj-winconfig-clear ()
  (interactive)
  (set-frame-parameter nil 'wconfig-list nil)
  (set-frame-parameter nil 'wconfig-current-id nil)
  )

(defun proj-save-current-winconfig (&optional frame)
  (interactive)
  (unless frame (setq frame (selected-frame)))
  (let ((id (frame-parameter frame 'wconfig-current-id)))
    (if id
        (proj-store-winconfig id)
      (progn 
        (proj-add-winconfig)
        (setq id 0)
        (set-frame-parameter frame 'wconfig-current-id id)))
    id))

(defun proj-winconfig-current-id ()
  (frame-parameter nil 'wconfig-current-id))

(defun proj-winconfig-next ()
  (interactive)
  (proj-save-current-winconfig)
  (let* ((wconfig-list (frame-parameter nil 'wconfig-list))
         (lwc (length wconfig-list))
         (id (+ (frame-parameter nil 'wconfig-current-id) 1)))
    (when (>= id lwc)
      (setq id 0))
    (proj-activate-winconfig id)
    id))

(defun proj-winconfig-prev ()
  (interactive)
  (proj-save-current-winconfig)
  (let* ((wconfig-list (frame-parameter nil 'wconfig-list))
         (lwc (length wconfig-list))
         (id (- (frame-parameter nil 'wconfig-current-id) 1)))
    (when (< id 0)
      (setq id (- lwc 1)))
    (proj-activate-winconfig id)
    id))

(defun proj-winconfig-add ()
  (interactive)
  (proj-save-current-winconfig)
  (let* ((id (proj-add-winconfig)))
    (proj-activate-winconfig id)))

(defun proj-winconfig-remove ()
  (interactive)
  (let* ((wconfig-list (frame-parameter nil 'wconfig-list))
         (lwc (length wconfig-list))
         (id (frame-parameter nil 'wconfig-current-id)))
    (when (> lwc 1)
      (proj-del-winconfig id)
      (unless (= id 0)
        (setq id (- id 1)))
      (proj-activate-winconfig id))))

;; (defun proj-winconfig-switch (id)
;;   (interactive)
;;   (let* ((wconfig-list (frame-parameter nil 'wconfig-list))
;;          (lwc (length wconfig-list)))
;;     (when (and (>= id 0) (< id lwc))
;;       (proj-save-current-winconfig)
;;       (proj-activate-winconfig id))))


(defun proj-winconfig-move-current-first ()
  (interactive)
  (proj-save-current-winconfig)
  (let* ((wconfig-list (frame-parameter nil 'wconfig-list))
         (id (frame-parameter nil 'wconfig-current-id)))
    (setq wconfig-list (nth-first id wconfig-list))
    (set-frame-parameter nil 'wconfig-list wconfig-list)
    (proj-activate-winconfig 0)
    ))


;; ------- 'synchronize' buffer list ---------------------------------


(defun proj-winconfig-activate ()
  (let ((id (frame-parameter nil 'wconfig-current-id)))
    (when id (proj-activate-winconfig id))))


(defun proj-winconfig-find-buffers (w)
  (let (w0 (w1 w) r result)
    (setq w1 w)
    (while (and w1 (listp w1)) 
      (setq w0 (car w1))
      (setq r (cond
               ((listp w0)
                (proj-winconfig-find-buffers w0))
               ((eq w0 'hc) nil)
               ((eq w0 'vc) nil)
               ((eq w0 'leaf) nil)
               ((eq w0 'buffer)
                (nth 1 w1))
               (t nil)))
      (when r
        (setq result
              (cond
               ((listp r)
                (append result r))
               ((stringp r)
                (append result (list r))))))
      (setq w1 (cdr w1)))
    result))

(defun proj-find-buffers-in-use ()
  (let* ((wlist (frame-parameter nil 'wconfig-list))
         (blist (loop for w in wlist
                      append (proj-winconfig-find-buffers w))))
    (delete-dups blist)))

(defun proj-add-buffers-in-use ()
  (let ((blist (proj-find-buffers-in-use)))
    (loop for bname in blist
          do (proj-add-buffer bname))))

;; (defun proj-sync-buffers ()
;;   (let ((all-buffers (frame-parameter nil 'buffer-info-list))
;;         (buffers-in-use (proj-find-buffers-in-use))
;;         buffer-in-use-info-list)
;;     (setq buffer-in-use-info-list
;;           (loop for b in buffers-in-use
;;                 collect (assoc b all-buffers)))
;;     (set-frame-parameter nil 'buffer-info-list
;;                          buffer-in-use-info-list)))

;; ------- all project info ---------------------------------

(defvar proj-keys '(buffer-info-list
                    wconfig-list
                    wconfig-current-id
                    ;; proj-home-dir
                    proj-path
                    proj-name
                    fullscreen))

(defun proj-get-info (frame)
  (let (value)
    (loop for key in proj-keys
          for value = (frame-parameter frame key)
          if value
          collect (cons key value))))


(defun proj-clear-info ()
  (loop for key in proj-keys
        do (set-frame-parameter nil key nil)))


(defun proj-dump-info (frame &optional buffer pretty erase)
  (let* ((buffer (if buffer buffer
                   (get-buffer-create "*Dump*")))
         (data (proj-get-info frame)))
    (dump-obj-to-buffer data buffer pretty erase)
    (with-current-buffer buffer
      (goto-char (point-min))
      (while (re-search-forward "\n[ ]+>" nil t)
        (replace-match ">"))
      (goto-char (point-min))
      (while (re-search-forward "marker\n+.*\n+.*at" nil t)
        (replace-match "marker at"))
      (goto-char (point-min))
      (while (re-search-forward "\n*[ ]*(moves after insertion)[ ]*\n*[ ]*" nil t)
        (replace-match " "))
      (goto-char (point-min))
      (while (re-search-forward "#<marker at \\([0-9]+\\) in.*>" nil t)
        (replace-match "\\1"))
      (goto-char (point-min))
      (while (search-forward "#<marker in no buffer>" nil t)
        (replace-match "0"))
      (goto-char (point-min))
      (while (search-forward "#<marker ation*>" nil t)
        (replace-match "0"))
      (goto-char (point-min))
      (while (search-forward "#<killed buffer>" nil t)
        (replace-match "*scratch*"))
      (goto-char (point-min))
      ;; (while (search-forward "#<buffer " nil t)
      ;;   (replace-match "\""))
      ;; (goto-char (point-min))
      ;; (while (search-forward ">" nil t)
      ;;   (replace-match "\""))
      )
    buffer))

(defun proj-save-info-to-file (frame path &optional confirm)
  (proj-save-buffers frame)
  (proj-save-current-winconfig frame)
  ;; (proj-sync-buffers)
  (with-temp-buffer
    (proj-dump-info frame (current-buffer) t t)
    (write-file path confirm)))

(defun proj-load-info-from-file (path)
  (ignore-errors
    (let* ((proj-info (read-file path)))
      (loop for (key . value) in proj-info
            do (when value
                 (set-frame-parameter nil key value))
            )
      path)))


(defun proj-set-name (name)
  (set-frame-parameter nil 'proj-name name))

(defun proj-set-path (path)
  (set-frame-parameter nil 'proj-path path))

(defun proj-get-name ()
  (frame-parameter nil 'proj-name))

;; ------- Centralized load/save --------------------------------

(defvar proj-save-dir (expand-file-name "proj-conf/" user-emacs-directory))
(defvar proj-catalogue-file (expand-file-name "proj-catalog.el" user-emacs-directory))
(defvar proj-catalogue nil)

(defun proj-save-catalogue-to-file (path &optional confirm)
  (with-temp-buffer
    (dump-obj-to-buffer proj-catalogue (current-buffer) nil t)
    (write-file path confirm)))

(defun proj-load-catalogue ()
  (setq proj-catalogue (read-file proj-catalogue-file)))


(defun proj-save ()
  (interactive)
  (let* ((frame (selected-frame))
         (name (frame-parameter frame 'proj-name)))
    (unless name
      (setq name (read))
      (set-frame-parameter frame 'proj-name name))
    (let* ((path (expand-file-name name proj-save-dir)))
      (message "Saving project '%s' in frame %s into '%s' ..." name frame path)
      (setf (alist-get name proj-catalogue nil nil 'equal) path)
      (proj-save-buffers frame)
      (proj-save-info-to-file frame path nil)
      (proj-save-catalogue-to-file proj-catalogue-file nil)
      (set-frame-parameter frame 'proj-path path)
      )))

(defun proj-save-as (name)
  (interactive "MNew project name: ")
  (let* ((frame (selected-frame)))
    (set-frame-parameter frame 'proj-name name)
    (with-selected-frame frame
      (proj-save))
    ))

(defun proj-catalogue-get-names ()
  (proj-load-catalogue)
  (sort
   (loop for (name . path) in proj-catalogue
         collect name)
   'string-collate-lessp))

(defun proj-switch (&optional name)
  (interactive
   (let* ((helm-full-frame t)
          (string (completing-read "Name of project to load: "
                                   (proj-catalogue-get-names))))
     (list string)))
  (when (proj-get-name)
    (proj-save))
  (if (member name (proj-active-projects))
      (progn
        (delete-frame)
        (proj-switch-frame name))
    (progn (message "Loading project` %s'" name)
           (let* ((path (alist-get name proj-catalogue nil nil 'equal)))
             (message "Loading project` %s' from '%s'" name path)
             (proj-load-internal name path)))
    ))

(defun proj-active-projects ()
  (let ((frames (frame-list)))
    (loop for frame in frames
          for name = (frame-parameter frame 'proj-name)
          if name
          collect name)))

(defun proj-load (&optional name)
  (interactive
   (let* ((helm-full-frame t)
          (string (completing-read "Name of project to load: "
                                   (proj-catalogue-get-names))))
     (list string)))
  (if (member name (proj-active-projects))
      (proj-switch-frame name)
    (let ((frame (make-frame))
          (path (alist-get name proj-catalogue nil nil 'equal)))
      (select-frame-set-input-focus frame)
      (message "Loading project` %s' from '%s'" name path)
      (proj-load-internal name path)
      (proj-activate-winconfig 0))
    ))

(defun proj-load-internal (name path)
  (if (proj-load-info-from-file path)
      (progn
        (proj-set-path path)
        (proj-set-name name)
        (proj-recreate-buffers)
        (proj-winconfig-activate))
    (progn
      (proj-set-path 'nil)
      (proj-set-name 'nil)
      (proj-clear-buffer-info-list)
      (delete-other-windows)
      (find-file path)
      (message "An error occurred. Please correct the config file.")
      )
    ))

(defun proj-close (&optional save)
  (interactive)
  (when (proj-get-name)
    (when save
      (proj-save))
    (proj-set-path 'nil)
    (proj-set-name 'nil)
    (proj-clear-buffer-info-list)
    (delete-other-windows)
    (switch-to-buffer "*scratch*")
    ))

(defun proj-remove (&optional name)
  (interactive
   (let ((string (completing-read "Name of project to remove:"
                                  (proj-catalogue-get-names))))
     (list string)))
  (message "Remove project` %s'" name)
  (let* ((path (alist-get name proj-catalogue nil nil 'equal)))
    (setf (alist-get name proj-catalogue nil t 'equal) nil)
    (proj-save-catalogue-to-file proj-catalogue-file nil)
    (delete-file path)
    ))

(defun proj-new (&optional name)
  (interactive "MProject name: ")
  (if (member name (proj-catalogue-get-names))
      (message "Project %s already exists!" name)
    (progn
      (when (proj-get-name)
        (let ((frame (make-frame)))
          (select-frame-set-input-focus frame)))
      (proj-set-name name))))

(defun proj-remove-or-kill-buffer (string-or-buffer)
  (interactive "b")
  (let (bufname buffer)
    (cond
     ((stringp string-or-buffer)
      (setq bufname string-or-buffer)
      (setq buffer (get-buffer string-or-buffer)))
     ((bufferp string-or-buffer)
      (setq bufname (buffer-name string-or-buffer))
      (setq buffer string-or-buffer)))
    (when (and buffer bufname)
      (when (proj-get-name)
        (proj-remove-buffer buffer))
      ;; (if (one-window-p)
      ;;     (proj-winconfig-remove)
      ;;   (delete-window))
      (if (length (bs-buffer-list))
          (bs-next-buffer)
        (switch-to-buffer "*scratch*"))
      (unless (member bufname (proj-all-active-buffers))
        (message "Killed buffer '%s'" bufname)
        (kill-buffer buffer)))))


(defun proj-remove-or-kill-this-buffer ()
  (interactive)
  (let ((buffer (current-buffer)))
    (quit-window)
    (proj-remove-or-kill-buffer buffer)))


(defun proj-global-buffer-info-list ()
  (let ((frames (frame-list)))
    (loop for frame in frames
          for buffer-info-list = (frame-parameter frame 'buffer-info-list)
          if buffer-info-list
          append buffer-info-list)))

(defun proj-all-active-buffers ()
  (let ((buffer-infos (proj-global-buffer-info-list)))
    (mapcar 'car buffer-infos)))

(defun proj-get-frame (name)
  (loop for frame being the frames
        for pname = (frame-parameter frame 'proj-name)
        until (equal name pname)
        finally return (if (equal name pname)
                           frame nil)))

(defun proj-switch-frame (name)
  (let ((frame (proj-get-frame name)))
    (select-frame-set-input-focus frame)))

;; --------------- Hooks for frames --------------------------------

(defun proj-delete-frame-hook (frame)
  (let* ((name (frame-parameter frame 'proj-name)))
    (when name
      (let* ((do-save (y-or-n-p (concat "Save project as \'" name "\'?"))))
        (proj-close do-save)
        ))))

(defun proj-after-make-frame-hook (frame)
  (set-frame-parameter frame 'buffer-list nil)
  (let ((rootwin (frame-root-window frame)))
    (with-selected-window rootwin
      (switch-to-buffer "*scratch*"))
    ))

(defun proj-emacs-kill-hook ()
  (loop for frame in (frame-list)
        do (let* ((name (frame-parameter frame 'proj-name))
                  (path (frame-parameter frame 'proj-path)))
             (when name
               (setf (alist-get name proj-catalogue nil nil 'equal) path)
               (proj-save-info-to-file frame path nil))))
  (proj-save-catalogue-to-file proj-catalogue-file nil))

;; (defun proj-cronjob ()
;;   (loop for frame in (frame-list)
;;         do (let* ((name (frame-parameter frame 'proj-name))
;;                   (path (frame-parameter frame 'proj-path)))
;;              (when name
;;                (message "Saving project as '%s' in '%s' ..." name path)
;;                (proj-save-info-to-file frame path nil)))))
;; 
;; (run-with-timer 120 600 'proj-cronjob)


(defun proj-find-file-hook ()
  (when (and (proj-get-name)
             (not proj-recreating-buffers-p))
    (proj-add-current-buffer)))

(add-hook 'find-file-hook 'proj-find-file-hook)
(add-hook 'window-configuration-change-hook 'proj-import-buffers)



(provide 'proj)
