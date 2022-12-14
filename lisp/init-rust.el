;;; init-rust.el --- init packages about rust        -*- lexical-binding: t; -*-

;; Copyright (C) 2022  lizqwer scott

;; Author: lizqwer scott <lizqwerscott@gmail.com>
;; Keywords: rust

;;; Commentary:

;;; Code:

(use-package rust-mode
  :ensure t
  :custom
  (rust-format-on-save t))

(use-package cargo-mode
  :ensure t)

(use-package cargo
  :ensure t)

;;; init-rust.el ends here.
