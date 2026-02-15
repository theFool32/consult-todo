;;; consult-todo.el --- Search hl-todo keywords in consult -*- lexical-binding: t -*-

;; Copyright (C) 2023 liuyinz
;; Author: liuyinz <liuyinz@gmail.com>
;; Maintainer: liuyinz <liuyinz@gmail.com>
;; Created: 2021-10-03 03:44:36
;; Version: 0.5.0
;; Package-Requires: ((emacs "29.1") (consult "0.35") (hl-todo "3.1.2"))
;; Homepage: https://github.com/liuyinz/consult-todo
;; License: GPL-3.0-or-later

;;; Commentary:
;; Provide commands `consult-todo' to search, filter, jump to hl-todo keywords.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'pcase)
  (require 'subr-x))

(require 'consult)
(require 'hl-todo)
(require 'compile)
(require 'grep)

(declare-function project-root "project")

(defgroup consult-todo nil
  "Search hl-todo keywords in consult."
  :group 'consult-todo)

(defcustom consult-todo-narrow nil
  "Alist of (NARROW . KEYWORD) to display."
  :type '(repeat (cons (character :tag "Narrow")
                       (string :tag "Keyword")))
  :group 'consult-todo)

(defcustom consult-todo-other (cons ?. "OTHER")
  "Cons mapping for narrow and missing keywords."
  :type '(cons character string)
  :group 'consult-todo)

(defcustom consult-todo-only-comment nil
  "If non-nil, only search todo keywords in comments.
Only effective on buffers."
  :type 'boolean
  :group 'consult-todo)

(defcustom consult-todo-use-rg (if (executable-find "rg") t nil)
  "If non-nil, use `rg' to search keywords in directory.
This automatically respects .gitignore."
  :type 'boolean
  :group 'consult-todo)

(defcustom consult-todo-dir-preview-key nil
  "Preview trigger keys for `consult-todo-dir' related command."
  :type '(choice (const :tag "Any key" any)
                 (list :tag "Debounced" (const :debounce) (float :tag "Seconds" 0.1) (const any))
                 (const :tag "No preview" nil)
                 (key :tag "Key")
                 (repeat :tag "List of keys" key))
  :group 'consult-todo)

(defconst consult-todo--narrow
  '((?t . "TODO")
    (?f . "FIXME")
    (?b . "BUG")
    (?h . "HACK"))
  "Default mapping of narrow and keywords.")

(defvar consult-todo--narrow-extend nil
  "Default mapping of narrow and keywords include OTHER if exists.")

(defun consult-todo--narrow ()
  "Return narrow alist."
  (or consult-todo-narrow consult-todo--narrow))

(defun consult-todo--narrow-extend ()
  "Return narrow alist include `consult-todo-other' if it's non-nil."
  (or consult-todo--narrow-extend
      (if-let* (((consp consult-todo-other))
                (narrow (car consult-todo-other))
                (group (cdr consult-todo-other))
                ((and (characterp narrow) (not (assoc narrow (consult-todo--narrow)))))
                ((and (stringp group) (not (rassoc group (consult-todo--narrow))))))
          (setq consult-todo--narrow-extend
                (cons consult-todo-other (consult-todo--narrow)))
        (setq consult-todo--narrow-extend nil))))

(defun consult-todo--format (candidates)
  "Return formatted string according to CANDIDATES."
  (when candidates
    (mapcar
     (pcase-lambda (`(,name ,line ,type ,pos ,narrow ,text))
       (propertize
        (format (apply #'format "%%-%ds %%-%ds %%-%ds %%s"
                       (cl-loop for i to 2
                                collect (seq-max (mapcar
                                                  (lambda(x) (length (nth i x)))
                                                  candidates))))
                (propertize name 'face 'consult-file)
                (propertize line 'face 'consult-line-number)
                (propertize type 'face (hl-todo--combine-face
                                        (cdr (assoc type hl-todo-keyword-faces))))
                text)
        'consult-location (cons pos line)
        'consult--type narrow))
     candidates)))

(defun consult-todo-grep-state ()
  "Lookup SELECTED in CANDIDATES list of `consult-location' category."
  (let ((open (consult--temporary-files))
        (jump (consult--jump-state)))
    (lambda (action cand)
      (unless cand (funcall open))
      (when cand
        (setq cand (car (get-text-property 0 'consult-location cand)))
        (funcall jump action (consult--marker-from-line-column
                              (ignore-errors
                                (funcall (or (and (not (eq action 'return)) open)
                                             #'find-file-noselect)
                                         (nth 0 cand)))
                              (nth 1 cand) (nth 2 cand)))))))

(defun consult-todo--candidates (buffers)
  "Return list of hl-todo keywords in current buffer."
  (cl-loop for buf in (or buffers (list (current-buffer)))
           append
           (with-current-buffer buf
             (save-excursion
               (save-restriction
                 (widen)
                 (goto-char (point-min))
                 (cl-loop while (hl-todo--search)
                          when (or (null consult-todo-only-comment)
                                   (nth 4 (syntax-ppss)))
                          collect
                          (let ((type (or (match-string-no-properties 2)
                                          (save-excursion
                                            (backward-to-word)
                                            (substring-no-properties
                                             (save-match-data (thing-at-point 'word)))))))
                            (list (buffer-name)
                                  (number-to-string (line-number-at-pos))
                                  type
                                  (copy-marker (point))
                                  (car (or (rassoc type (consult-todo--narrow))
                                           consult-todo-other))
                                  (string-trim (buffer-substring-no-properties
                                                (point) (line-end-position)))))))))))

(defun consult-todo--parse-grep-buffer (buffer)
  "Parse grep BUFFER content using regex. Strict case sensitive."
  (with-current-buffer buffer
    (goto-char (point-min))
    (let ((candidates '())
          ;; 强制大小写敏感
          (case-fold-search nil)
          ;; 匹配行：File:Line:Content (兼容 Windows 路径)
          (line-re "^\\(.*\\):\\([0-9]+\\):\\(.*\\)$")
          ;; 构建匹配关键字的正则，不包含冒号，因为我们要在 content 里找这个词
          ;; 但在 rgrep 阶段我们已经严格限制了冒号
          (keywords-re (regexp-opt (mapcar #'car hl-todo-keyword-faces))))
      (while (re-search-forward line-re nil t)
        (let* ((file (match-string-no-properties 1))
               (line (string-to-number (match-string-no-properties 2)))
               (content (match-string-no-properties 3))
               (full-path (expand-file-name file compilation-directory)))

          ;; 再次确认内容中包含关键字
          (when (string-match keywords-re content)
            (let* ((type (match-string 0 content))
                   (narrow (car (or (rassoc type (consult-todo--narrow))
                                    consult-todo-other))))
              (push (list (file-name-nondirectory file)
                          (number-to-string line)
                          type
                          (list full-path line 0)
                          narrow
                          (string-trim content))
                    candidates)))))
      (nreverse candidates))))

(defun consult-todo--candidates-rgrep (buffer message)
  "Sentinel function for async grep."
  (if (not (buffer-live-p buffer))
      (ignore)
    (consult--forbid-minibuffer)
    (let ((candidates
           (unwind-protect
               (when (and (string-match-p "^finished" message)
                          (string-prefix-p " *consult-todo-" (buffer-name buffer)))
                 (consult-todo--parse-grep-buffer buffer))
             ;; Cleanup
             (kill-buffer buffer))))
      (if candidates
          (consult--read
           (consult-todo--format candidates)
           :prompt "Go to hl-todo in dir: "
           :category 'consult-grep
           :require-match t
           :sort nil
           :preview-key consult-todo-dir-preview-key
           :group (consult--type-group (consult-todo--narrow-extend))
           :narrow (consult--type-narrow (consult-todo--narrow-extend))
           :lookup #'consult--lookup-member
           :state (consult-todo-grep-state))
        (message "No hl-todo keywords found.")))))

;;;###autoload
(defun consult-todo (&optional buffers)
  "Jump to hl-todo keywords."
  (interactive "P")
  (consult--forbid-minibuffer)
  (let ((candidates (consult-todo--candidates buffers)))
    (if candidates
        (consult--read
         (consult-todo--format candidates)
         :prompt "Go to hl-todo: "
         :category 'consult-location
         :require-match t
         :sort nil
         :group (consult--type-group (consult-todo--narrow-extend))
         :narrow (consult--type-narrow (consult-todo--narrow-extend))
         :lookup #'consult--lookup-location
         :state (consult--jump-state))
      (message "No hl-todo keywords found."))))

(defun consult-todo--make-rg-regexp ()
  "Construct regex: Word boundary + Keyword + Colon."
  (let ((keywords (mapcar #'car hl-todo-keyword-faces)))
    ;; 核心逻辑：(TODO|FIXME):
    ;; 1. mapconcat 生成 TODO|FIXME
    ;; 2. \\b 保证前边界
    ;; 3. 结尾加 : 保证只匹配带冒号的
    (concat "\\b(?:" (mapconcat #'regexp-quote keywords "|") "):")))

;;;###autoload
(defun consult-todo-dir (&optional directory files)
  "Jump to hl-todo keywords in FILES in DIRECTORY.
Strictly matches 'KEYWORD:' (case-sensitive)."
  (interactive)
  (let* ((directory (or directory default-directory)))

    (add-hook 'compilation-finish-functions #'consult-todo--candidates-rgrep)

    ;; 强制屏蔽窗口，使用 cl-letf 将 display-buffer 设为 ignore
    (cl-letf (((symbol-function 'display-buffer) #'ignore)
              (compilation-buffer-name-function
               (lambda (&rest _) (format " *consult-todo-%s*" directory))))

      (if (and consult-todo-use-rg (executable-find "rg"))
          ;; --- Ripgrep 逻辑 ---
          (let* ((regexp (consult-todo--make-rg-regexp))
                 ;; 参数说明：
                 ;; -s: 大小写敏感 (Case Sensitive)
                 ;; -e regexp: 指定正则
                 (cmd (format "rg -s --no-heading --line-number --color=never --with-filename -e %s %s"
                              (shell-quote-argument regexp)
                              (shell-quote-argument (directory-file-name directory)))))
            (compilation-start cmd 'grep-mode))

        ;; --- Fallback 逻辑 (不推荐，不支持 gitignore 和严格冒号匹配) ---
        (save-window-excursion
          (let ((files (or files "* .*")))
            (rgrep (hl-todo--regexp) files directory)))))))

;;;###autoload
(defun consult-todo-all ()
  "Jump to hl-todo keywords in all `hl-todo-mode' enabled buffers."
  (interactive)
  (consult-todo (seq-filter (lambda (x) (buffer-local-value 'hl-todo-mode x)) (buffer-list))))

;;;###autoload
(defun consult-todo-project ()
  "Jump to hl-todo keywords in current project."
  (interactive)
  (consult-todo-dir
   (when-let* ((project (project-current)))
     (expand-file-name
      (if (fboundp 'project-root)
          (project-root project)
        (car (with-no-warnings (project-roots project))))))))

(provide 'consult-todo)
;;; consult-todo.el ends here
