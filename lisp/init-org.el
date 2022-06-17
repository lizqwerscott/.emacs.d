
(defun evil-org-insert-item (&optional checkbox)
  "使用evil 在org mode 插入新行"
  (interactive "p")
  (evil-open-below 1)
  (org-insert-item))

(add-hook 'org-mode-hook
          (lambda ()
            (evil-local-set-key 'normal
                                (kbd "t")
                                'evil-org-insert-item)))

;; space ww
(defun open-my-org-file ()
  "打开我的org文件."
  (interactive)
  (find-file "~/Documents/Sync/org/index.org"))

(defun org-export-docx ()
  (interactive)
  (let ((docx-file (concat (file-name-sans-extension (buffer-file-name))
                           ".docx"))
        (template-file "/home/lizqwer/Documents/Templates/template.docx"))
    (shell-command
     (format "pandoc %s -o %s --reference-doc=%s"
             (buffer-file-name)
             docx-file
             template-file))
    (message "Convert finish: %s" docx-file)))

(defun org-insert-html-head ()
  (interactive)
  (insert "#+HTML_HEAD: <link rel=\"stylesheet\" type=\"text/css\" href=\"http://gongzhitaao.org/orgcss/org.css\"/>"))

(defun org-insert-html-theme-bigblow ()
  (interactive)
  (insert "#+SETUPFILE: https://fniessen.github.io/org-html-themes/org/theme-bigblow.setup"))

(defun org-insert-html-theme-readtheorg ()
  (interactive)
  (insert "#+SETUPFILE: https://fniessen.github.io/org-html-themes/org/theme-readtheorg.setup"))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (C . t)
   (python . t)
   (latex . t)))

(require 'org)

(setq org-startup-indented t
      org-src-tab-acts-natively t
      org-startup-folded t)

(setq org-fontify-done-headline t
      org-hide-leading-stars t
      org-pretty-entities t
      org-odd-levels-only t)

(setq org-list-demote-modify-bullet
      (quote (("+" . "-")
              ("-" . "+")
              ("*" . "-")
              ("1." . "-")
              ("1)" . "-")
              ("A)" . "-")
              ("B)" . "-")
              ("a)" . "-")
              ("b)" . "-")
              ("A." . "-")
              ("B." . "-")
              ("a." . "-")
              ("b." . "-"))))

(use-package org-fancy-priorities
  :ensure t
  :hook
  (org-mode . org-fancy-priorities-mode)
  :config
  (setq org-fancy-priorities-list
        '((?A . "A")
          (?B . "⬆")
          (?C . "⬇")
          (?D . "☕")
          (?1 . "⚡")
          (?2 . "2")
          (?3 . "3")
          (?4 . "☕")
          (?I . "Important"))))

(use-package org-bullets
  :ensure t
  :custom
  (org-bullets-bullet-list '("◉" "☯" "○" "☯" "✸" "☯" "✿" "☯" "✜" "☯" "◆" "☯" "▶"))
  (org-ellipsis "⤵")
  :hook (org-mode . org-bullets-mode))

(define-key org-mode-map (kbd "C-c TAB") 'org-insert-item)

(setq org-default-notes-file "~/Documents/Sync/org/index.org")

(setq org-format-latex-options
      (plist-put org-format-latex-options :scale 2.0))

(setq org-capture-templates nil)

(add-to-list 'org-capture-templates '("t" "Tasks"))
(add-to-list 'org-capture-templates
             '("tp" "Project Write Task" entry
               (file+olp "~/Documents/Sync/org/tasks.org" "Project")
               "* TODO %^{任务名字}\n%u\n%a\n" :clock-in t :clock-resume t))

(add-to-list 'org-capture-templates
             '("tr" "Book Reading Task" entry
               (file+olp "~/Documents/Sync/org/tasks.org" "Reading" "Book")
               "* TODO %^{书名字}\n%u\n%a\n" :clock-in t :clock-resume t))

(add-to-list 'org-capture-templates
             '("w" "Web Collections" entry
               (file+headline "~/Documents/Sync/org/index.org" "Web")
               "* %U %:annotation\n\n%:initial\n\n%?"))

(add-to-list 'org-capture-templates
             '("i" "Inbox" entry
               (file "~/Documents/Sync/org/index.org")
               "* %U - %^{heading} %^g\n %?\n"))

(setq org-todo-keywords
      '((sequence "TODO(t!)" "WAIT(w@/!)" "|" "DONE(d@/!)" "CANCEL(c@/!)")
        (sequence "REPORT(r!)" "BUG(b@/!)" "|" "FIXED(f@/!)")))

(setq org-log-done 'note)

(setq org-tag-alist
      '(("@work" . ?w)
        ("@home" . ?h)
        ("@laptop" . ?l)))

(defun me/non-western-notation-p (str)
  "西文以及常用符号的补集, 所谓的“中文”符号判定"
  (let ((non-w-notation-regex "[^[:ascii:]éèêàîïµΩçπœ]"))
    (cond
     ((equal str "before")
      (if (looking-back non-w-notation-regex)
	  t nil))
     ((equal str "after")
      (if (looking-at non-w-notation-regex)
	  t nil))
     (t (ding) (message "me/non-western-notation-p 参量错误")))))

(defun me/org-emphasize-dwim (mark)
  (let (len-to-cover-until-end
	pos-cen-of-pair
	start
	end)    
    (if (org-region-active-p) ; 如果有选中文字
	(progn
	  (setq start (region-beginning)
		end (region-end))
	  (goto-char end)
	  (skip-chars-backward "[:blank:]​") ; 跳过所有的 SPC \t 和零宽空格. 注意这里非常非常地 tricky, looking-at/back 中的参量必须是 [[:blank:]], 而 skip-chars-forward/backward 中的参量必须是 [:blank:]
	  (setq end (point)) 
	  (goto-char end)
	  (when (<= end start)
	    (ding)
	    (cl-return-from 'me/org-emphasize-dwim "选区只有空格和零宽空格"))
	  ;; 此时 cursor 在 end 位置, 开始处理右半边
	  (if (me/non-western-notation-p "before") ; 当左边为“中文”时
	      (cond
	       ((looking-at "[[:blank:]​.,:!?;'\"]") ; 当右边为 SPC \t 零宽空格, 或常用的标点符号时
		(insert mark) ; 仅仅插入 /
		(cl-incf end))  
	       ((or (me/non-western-notation-p "after")
		    (looking-at "\n")) ; 当右边同为“中文”时 (当右边换行, 也假设接下来我们可能在之后继续输入中文)
		(insert (concat mark "​")) ;插入 / + 零宽空格
		(cl-incf end 2)) 
	       (t
		(insert (concat mark " ")) ; 剩余情况(主要是西文和常用字符), 插入 / + 空格
		(cl-incf end 2)))
	    (cond ; 当左边不为“中文”时 (“西文”以及常用字符)
	     ((looking-at "[[:blank:]​.,:!?;'\"]") ; 当右边为 SPC \t 零宽空格, 或常用的标点符号时
	      (insert mark) ; 仅仅插入 /
	      (cl-incf end))  
	     ((or (me/non-western-notation-p "after")
		  (looking-at "\n")) ; 当右边为“中文”时 (当右边换行, 也假设接下来我们可能在之后继续输入中文)
	      (insert (concat mark " "))  ;插入 / + 空格
	      (cl-incf end 2))
	     (t
	      (insert (concat mark "​")) ; 剩余情况(主要是非“中文”符号)时, 插入 / + 零宽空格
	      (cl-incf end 2))))  
	  (goto-char start)
	  (skip-chars-forward "[:blank:]​") ; 跳过所有的 SPC \t 和零宽空格
	  (setq len-to-cover-until-end (- end (point)))
	  ;; 此时 cursor 在 start 位置, 开始处理左半边
	  (if (me/non-western-notation-p "after") ; 当右为“中文”时
	      (cond
	       ((looking-back "[[:space:]]") ; 当左边为 SPC \n \t 或零宽空格时
		(insert mark))  ; 仅仅插入 /
	       ((me/non-western-notation-p "before") ; 当左边同为“中文”时
		(insert (concat "​" mark))) ;插入 零宽空格 + /
	       (t (insert (concat " " mark)))) ; 剩余情况(主要是西文和常用字符), 插入 空格 + /
	    (cond ; 当右边不为“中文”时 (“西文”以及常用字符)
	     ((looking-back "[[:space:]]") ; 当左边为 SPC \n 或零宽空格时
	      (insert mark))  ; 仅仅插入 /
	     ((me/non-western-notation-p "before") ; 当左边为“中文”时
	      (insert (concat " " mark))) ;插入 / + 空格
	     (t (insert (concat "​" mark))))) ; 剩余情况(主要是非“中文”符号)时, 插入 / + 零宽空格
	  (forward-char len-to-cover-until-end))
      (cond ; 当没有区域选中时, 当 markup symbol 为斜体、粗体、中横线时, 直接假设我们接下来要输入的是中文
       ((or (equal mark "/")
	    (equal mark "*")
	    (equal mark "+"))		
	 (cond		; 分情况插入左半边的 mark
	  ((looking-back "[[:space:]]")
	   (insert mark))
	  ((me/non-western-notation-p "before")
	   (insert (concat "​" mark)))
	  (t
	   (insert (concat " " mark))))
	 (setq pos-cen-of-pair (point))
	 (cond				; 分情况插入右半边的 mark
	  ((looking-at "[[:blank:]​.,:!?;'\"]") 
	   (insert mark))
	  ((or (me/non-western-notation-p "after")
	       (looking-at "\n")) ; 当出现换行符时, 依然假设我们可能会在之后补写中文
	   (insert (concat mark "​")))
	  (t
	   (insert (concat mark " "))))
	 (goto-char pos-cen-of-pair))
       (t ; 剩余情况 (markup symbol 为 code 和 verbatim 时), 直接假设我们接下来要输入的是英文
	(cond		; 分情况插入左半边的 mark
	  ((looking-back "[[:space:]]")
	   (insert mark))
	  ((me/non-western-notation-p "before")
	   (insert (concat " " mark)))
	  (t
	   (insert (concat "​" mark))))
	 (setq pos-cen-of-pair (point))
	 (cond				; 分情况插入右半边的 mark
	  ((looking-at "[[:blank:]​.,:!?;'\"]") 
	   (insert mark))
	  ((or (me/non-western-notation-p "after")
	       (looking-at "\n")) ; 当出现换行符时, 依然假设我们可能会在之后补写中文
	   (insert (concat mark " ")))
	  (t
	   (insert (concat mark "​"))))
	 (goto-char pos-cen-of-pair))))))

(define-key org-mode-map (kbd "C-c e")
            #'(lambda (char)
                (interactive "sinput wrap:")
                (me/org-emphasize-dwim char)))

(use-package valign
  :ensure t
  :hook (org-mode . valign-mode))

;(setq org-hide-emphasis-markers t)

(provide 'init-org)
