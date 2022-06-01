;;; pyim-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "pyim" "pyim.el" (0 0 0 0))
;;; Generated autoloads from pyim.el

(defvar pyim-title "PYIM ")

(register-input-method "pyim" "UTF-8" #'pyim-activate pyim-title "")

(autoload 'pyim-activate "pyim" "\
pyim 启动函数.

pyim 是使用 `pyim-activate' 来启动输入法，这个命令主要做如下工作：
1. 重置所有的 local 变量。
2. 创建汉字到拼音和拼音到汉字的 hash table。
3. 创建词库缓存 dcache.
4. 运行 hook： `pyim-load-hook'。
5. 将 `pyim-kill-emacs-hook-function' 命令添加到 `kill-emacs-hook' , emacs 关闭
之前将用户选择过的词生成的缓存和词频缓存保存到文件，供以后使用。
6. 设定变量：
   1. `input-method-function'
   2. `deactivate-current-input-method-function'
7. 运行 `pyim-activate-hook'

pyim 使用函数 `pyim-activate' 启动输入法的时候，会将变量
`input-method-function' 设置为 `pyim-input-method' ，这个变量会影
响 `read-event' 的行为。

当输入字符时，`read-event' 会被调用，`read-event' 调用的过程中，
会执行 `pyim-input-method' 这个函数。

\(fn &optional ARGS)" t nil)

(autoload 'pyim-convert-string-at-point "pyim" "\
将光标前的用户输入的字符串转换为中文.

\(fn &optional _)" t nil)

(register-definition-prefixes "pyim" '("pyim-"))

;;;***

;;;### (autoloads nil "pyim-autoselector" "pyim-autoselector.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from pyim-autoselector.el

(register-definition-prefixes "pyim-autoselector" '("pyim-autoselector-xingma"))

;;;***

;;;### (autoloads nil "pyim-candidates" "pyim-candidates.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from pyim-candidates.el

(register-definition-prefixes "pyim-candidates" '("pyim-"))

;;;***

;;;### (autoloads nil "pyim-cloudim" "pyim-cloudim.el" (0 0 0 0))
;;; Generated autoloads from pyim-cloudim.el

(register-definition-prefixes "pyim-cloudim" '("pyim-cloudim"))

;;;***

;;;### (autoloads nil "pyim-codes" "pyim-codes.el" (0 0 0 0))
;;; Generated autoloads from pyim-codes.el

(register-definition-prefixes "pyim-codes" '("pyim-codes-create"))

;;;***

;;;### (autoloads nil "pyim-common" "pyim-common.el" (0 0 0 0))
;;; Generated autoloads from pyim-common.el

(register-definition-prefixes "pyim-common" '("pyim-"))

;;;***

;;;### (autoloads nil "pyim-cregexp" "pyim-cregexp.el" (0 0 0 0))
;;; Generated autoloads from pyim-cregexp.el

(register-definition-prefixes "pyim-cregexp" '("pyim-cregexp-"))

;;;***

;;;### (autoloads nil "pyim-cregexp-utils" "pyim-cregexp-utils.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from pyim-cregexp-utils.el

(autoload 'pyim-cregexp-convert-at-point "pyim-cregexp-utils" "\
将光标前的字符串按拼音的规则转换为一个搜索中文的 regexp.
用于实现拼音搜索中文的功能。

在 minibuffer 中，这个命令默认会自动运行 `exit-minibuffer'.
这个可以使用 INSERT-ONLY 参数控制。

\(fn &optional INSERT-ONLY)" t nil)

(defvar pyim-isearch-mode nil "\
Non-nil if pyim-isearch mode is enabled.
See the `pyim-isearch-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `pyim-isearch-mode'.")

(custom-autoload 'pyim-isearch-mode "pyim-cregexp-utils" nil)

(autoload 'pyim-isearch-mode "pyim-cregexp-utils" "\
这个 mode 为 isearch 添加拼音搜索功能.

This is a global minor mode.  If called interactively, toggle the
`pyim-isearch mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='pyim-isearch-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "pyim-cregexp-utils" '("pyim-"))

;;;***

;;;### (autoloads nil "pyim-cstring" "pyim-cstring.el" (0 0 0 0))
;;; Generated autoloads from pyim-cstring.el

(autoload 'pyim-cstring-to-pinyin "pyim-cstring" "\
将汉字字符串转换为对应的拼音字符串的工具.

如果 SHOU-ZI-MU 设置为 t, 转换仅得到拼音首字母字符串。当
RETURN-LIST 设置为 t 时，返回一个拼音列表，这个列表包含词条的一个
或者多个拼音（词条包含多音字时）；如果 IGNORE-DUO-YIN-ZI 设置为
t, 遇到多音字时，只使用第一个拼音，其它拼音忽略；当
ADJUST-DUO-YIN-Zi 设置为 t 时, `pyim-cstring-to-pinyin' 会使用 pyim 已
安装的词库来校正多音字，但这个功能有一定的限制:

1. pyim 普通词库中不存在的词条不能较正
2. 多音字校正速度比较慢，实时转换会产生卡顿。

BUG: 当 STRING 中包含其它标点符号，并且设置 SEPERATER 时，结果会
包含多余的连接符：比如： '你=好' --> 'ni-=-hao'

\(fn STRING &optional SHOU-ZI-MU SEPARATOR RETURN-LIST IGNORE-DUO-YIN-ZI ADJUST-DUO-YIN-ZI)" nil nil)

(autoload 'pyim-cstring-to-pinyin-simple "pyim-cstring" "\
简化版的 `pyim-cstring-to-pinyin', 不处理多音字。

\(fn STRING &optional SHOU-ZI-MU SEPARATOR RETURN-LIST)" nil nil)

(register-definition-prefixes "pyim-cstring" '("pyim-"))

;;;***

;;;### (autoloads nil "pyim-cstring-utils" "pyim-cstring-utils.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from pyim-cstring-utils.el

(register-definition-prefixes "pyim-cstring-utils" '("pyim-"))

;;;***

;;;### (autoloads nil "pyim-dcache" "pyim-dcache.el" (0 0 0 0))
;;; Generated autoloads from pyim-dcache.el

(register-definition-prefixes "pyim-dcache" '("pyim-dcache-"))

;;;***

;;;### (autoloads nil "pyim-dhashcache" "pyim-dhashcache.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from pyim-dhashcache.el

(register-definition-prefixes "pyim-dhashcache" '("pyim-dhashcache-"))

;;;***

;;;### (autoloads nil "pyim-dict" "pyim-dict.el" (0 0 0 0))
;;; Generated autoloads from pyim-dict.el

(register-definition-prefixes "pyim-dict" '("pyim-"))

;;;***

;;;### (autoloads nil "pyim-dict-manager" "pyim-dict-manager.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from pyim-dict-manager.el

(autoload 'pyim-dicts-manager "pyim-dict-manager" "\
pyim 词库管理器。

使用这个词库管理器可以方便的执行下列命令：
1. 添加词库。
2. 删除词库。
3. 向上和向下移动词库。
4. 保存词库设置。
5. 重启输入法。" t nil)

(register-definition-prefixes "pyim-dict-manager" '("pyim-dict-manager-"))

;;;***

;;;### (autoloads nil "pyim-dregcache" "pyim-dregcache.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from pyim-dregcache.el

(register-definition-prefixes "pyim-dregcache" '("pyim-dregcache-"))

;;;***

;;;### (autoloads nil "pyim-entered" "pyim-entered.el" (0 0 0 0))
;;; Generated autoloads from pyim-entered.el

(register-definition-prefixes "pyim-entered" '("pyim-entered-"))

;;;***

;;;### (autoloads nil "pyim-imobjs" "pyim-imobjs.el" (0 0 0 0))
;;; Generated autoloads from pyim-imobjs.el

(register-definition-prefixes "pyim-imobjs" '("pyim-imobjs"))

;;;***

;;;### (autoloads nil "pyim-indicator" "pyim-indicator.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from pyim-indicator.el

(register-definition-prefixes "pyim-indicator" '("pyim-indicator-"))

;;;***

;;;### (autoloads nil "pyim-liberime" "pyim-liberime.el" (0 0 0 0))
;;; Generated autoloads from pyim-liberime.el

(register-definition-prefixes "pyim-liberime" '("pyim-"))

;;;***

;;;### (autoloads nil "pyim-outcome" "pyim-outcome.el" (0 0 0 0))
;;; Generated autoloads from pyim-outcome.el

(register-definition-prefixes "pyim-outcome" '("pyim-"))

;;;***

;;;### (autoloads nil "pyim-page" "pyim-page.el" (0 0 0 0))
;;; Generated autoloads from pyim-page.el

(register-definition-prefixes "pyim-page" '("pyim-page-"))

;;;***

;;;### (autoloads nil "pyim-pinyin" "pyim-pinyin.el" (0 0 0 0))
;;; Generated autoloads from pyim-pinyin.el

(register-definition-prefixes "pyim-pinyin" '("pyim-pinyin-"))

;;;***

;;;### (autoloads nil "pyim-preview" "pyim-preview.el" (0 0 0 0))
;;; Generated autoloads from pyim-preview.el

(register-definition-prefixes "pyim-preview" '("pyim-preview-"))

;;;***

;;;### (autoloads nil "pyim-probe" "pyim-probe.el" (0 0 0 0))
;;; Generated autoloads from pyim-probe.el

(register-definition-prefixes "pyim-probe" '("pyim-probe-"))

;;;***

;;;### (autoloads nil "pyim-process" "pyim-process.el" (0 0 0 0))
;;; Generated autoloads from pyim-process.el

(register-definition-prefixes "pyim-process" '("pyim-"))

;;;***

;;;### (autoloads nil "pyim-punctuation" "pyim-punctuation.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from pyim-punctuation.el

(register-definition-prefixes "pyim-punctuation" '("pyim-punctuation-"))

;;;***

;;;### (autoloads nil "pyim-pymap" "pyim-pymap.el" (0 0 0 0))
;;; Generated autoloads from pyim-pymap.el

(register-definition-prefixes "pyim-pymap" '("pyim-pymap"))

;;;***

;;;### (autoloads nil "pyim-pymap-utils" "pyim-pymap-utils.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from pyim-pymap-utils.el

(register-definition-prefixes "pyim-pymap-utils" '("pyim-pymap-"))

;;;***

;;;### (autoloads nil "pyim-scheme" "pyim-scheme.el" (0 0 0 0))
;;; Generated autoloads from pyim-scheme.el

(autoload 'pyim-default-scheme "pyim-scheme" "\


\(fn &optional SCHEME-NAME)" t nil)

(register-definition-prefixes "pyim-scheme" '("pyim-"))

;;;***

;;;### (autoloads nil nil ("pyim-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8-emacs-unix
;; End:
;;; pyim-autoloads.el ends here
