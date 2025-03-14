;;; init-calender.el --- calender                    -*- lexical-binding: t; -*-

;; Copyright (C) 2025  lizqwer scott

;; Author: lizqwer scott <lizqwerscott@gmail.com>
;; Keywords: lisp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:
(eval-after-load 'calender
  (progn
    (require 'cal-china-x)
    (cal-china-x-setup)
    (setq calendar-mark-holidays-flag t
          cal-china-x-important-holidays cal-china-x-chinese-holidays
          cal-china-x-general-holidays '((holiday-lunar 1 15 "元宵节")
                                         (holiday-lunar 7 7 "七夕节")
                                         (holiday-fixed 3 8 "妇女节")
                                         (holiday-fixed 3 12 "植树节")
                                         (holiday-fixed 5 4 "青年节")
                                         (holiday-fixed 6 1 "儿童节")
                                         (holiday-fixed 9 10 "教师节"))
          holiday-other-holidays '((holiday-fixed 2 14 "情人节")
                                   (holiday-fixed 4 1 "愚人节")
                                   (holiday-fixed 12 25 "圣诞节")
                                   (holiday-float 5 0 2 "母亲节")
                                   (holiday-float 6 0 3 "父亲节")
                                   (holiday-float 11 4 4 "感恩节"))
          calendar-holidays (append cal-china-x-important-holidays
                                    cal-china-x-general-holidays
                                    holiday-other-holidays))))

(provide 'init-calender)
;;; init-calender.el ends here
