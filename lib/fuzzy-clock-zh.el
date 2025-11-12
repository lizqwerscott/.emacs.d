;;; fuzzy-clock-zh.el --- fuzzy clock zh              -*- lexical-binding: t; -*-

;; Copyright (C) 2025  lizqwer scott

;; Author: lizqwer scott <lizqwerscott@gmail.com>
;; Keywords: tools

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

(defgroup fuzzy-clock-zh nil
  "Display time in a human-friendly, approximate way."
  :group 'calendar
  :prefix "fuzzy-clock-zh")

(defun fuzzy-clock-zh--hour-to-word (hour)
  "Convert HOUR (0-23) to word form (e.g., 3 -> `Three')."
  (nth (mod hour 12) '("十二" "一" "二" "三" "四" "五" "六" "七" "八" "九" "十" "十一")))

(defun fuzzy-clock-zh-time-string (level time)
  "Format TIME as fuzzy string based on FUZZINESS LEVEL.
HOUR is the hour (0-23) and MINUTE is the minute (0-59).
Optional: DAY (1-31), MONTH (1-12), YEAR, DOW (day-of-week: 0=Sun,
6=Sat), DST, UTCOFF.
FUZZINESS can be: five-minutes, fifteen-minutes, half-hour, hour,
part-of-day, day-of-week, part-of-month, month, part-of-season,
part-of-year, year."
  (let* ((decoded (decode-time time))
         (min (nth 1 decoded))
         (hour (nth 2 decoded))
         (day (nth 3 decoded))
         (month (nth 4 decoded))
         (year (nth 5 decoded))
         (weekday (nth 6 decoded)))
    (pcase level
      ;; 1. 每5分钟
      ('five-minutes (let* ((rounded (* 5 (round (/ min 5.0))))
                            (cn-hour (fuzzy-clock-zh--hour-to-word hour))
                            (cn-min (format "%d分" rounded)))
                       (format "%s点%s" cn-hour cn-min)))

      ;; 2. 每15分钟
      ('fifteen-minutes (let* ((rounded (/ min 15))
                               (past (mod min 15))
                               (desc (nth rounded '("整" "一刻" "半" "三刻")))
                               (cn-hour (fuzzy-clock-zh--hour-to-word hour)))
                          (if (and (= rounded 3)
                                   (>= past 10))
                              (format "快%s点了"
                                      (fuzzy-clock-zh--hour-to-word (mod (1+ hour) 24)))
                            (format "%s点%s%s"
                                    cn-hour
                                    (if (and (not (= 0 past))
                                             (= 0 rounded))
                                        ""
                                      desc)
                                    (if (= 0 past)
                                        ""
                                      "左右")))))

      ;; 3. 半小时
      ('half-hour (let* ((half (if (< min 30) "整" "半"))
                         (cn-hour (fuzzy-clock-zh--hour-to-word hour)))
                    (format "%s点%s" cn-hour half)))

      ;; 4. 小时
      ('hour (let ((cn-hour (fuzzy-clock-zh--hour-to-word hour)))
               (format "%s点钟" cn-hour)))

      ;; 5. 一天中的部分
      ('part-of-day (cond
                     ((< hour 6) "凌晨")
                     ((< hour 9) "早晨")
                     ((< hour 12) "上午")
                     ((< hour 14) "中午")
                     ((< hour 18) "下午")
                     ((< hour 22) "晚上")
                     (t "深夜")))

      ;; 6. 星期几
      ('day-of-week (nth weekday '("星期日" "星期一" "星期二" "星期三" "星期四" "星期五" "星期六")))

      ;; 7. 月份的部分
      ('part-of-month (let ((part (cond
                                   ((<= day 10) "上旬")
                                   ((<= day 20) "中旬")
                                   (t "下旬"))))
                        (format "%d月%s" month part)))

      ;; 8. 月份
      ('month (format "%d月" month))

      ;; 9. 季节部分
      ('part-of-season (let* ((season (cond
                                       ((memq month '(12 1 2)) "冬")
                                       ((memq month '(3 4 5)) "春")
                                       ((memq month '(6 7 8)) "夏")
                                       ((memq month '(9 10 11)) "秋")))
                              (part (cond
                                     ((<= day 10) "初")
                                     ((<= day 20) "仲")
                                     (t "暮"))))
                         (format "%s%s" part season)))

      ;; 10. 年份部分
      ('part-of-year (let ((part (cond
                                  ((<= month 4) "年初")
                                  ((<= month 8) "年中")
                                  (t "年底"))))
                       (format "%d年%s" year part)))

      ;; 11. 年
      ('year (format "%d年" year))

      ;; 默认 fallback
      (_ (format "%d-%02d-%02d" year month day)))))

;;; Modeline
(defcustom fuzzy-clock-zh-fuzziness 'hour
  "The level of fuzziness for the clock display.
Valid values are:
  `five-minutes'    - Every 5 minutes
  `fifteen-minutes' - Every 15 minutes
  `half-hour'       - Half hour
  `hour'            - Hour (default)
  `part-of-day'     - Part of day
  `day-of-week'     - Day of the week
  `part-of-month'   - Part of month (early/middle/late)
  `month'           - Month name
  `part-of-season'  - Part of season (early/middle/late)
  `part-of-year'    - Part of year (early/middle/late)
  `year'            - Year"
  :type '(choice (const :tag "Every 5 minutes" five-minutes)
                 (const :tag "Every 15 minutes" fifteen-minutes)
                 (const :tag "Half hour" half-hour)
                 (const :tag "Hour" hour)
                 (const :tag "Part of day" part-of-day)
                 (const :tag "Day of week" day-of-week)
                 (const :tag "Part of month" part-of-month)
                 (const :tag "Month" month)
                 (const :tag "Part of season" part-of-season)
                 (const :tag "Part of year" part-of-year)
                 (const :tag "Year" year))
  :group 'fuzzy-clock-zh)

(defcustom fuzzy-clock-zh-update-interval 60
  "The number of seconds between updates of the fuzzy clock.
Default is 60 seconds (1 minute)."
  :type 'integer
  :group 'fuzzy-clock-zh)

(defvar fuzzy-clock-zh-string nil
  "String displayed in the mode-line showing fuzzy time.")

(defvar fuzzy-clock-zh-timer nil
  "Timer object for updating the fuzzy clock.")

(defun fuzzy-clock-zh-update ()
  "Update the fuzzy clock string with the current time."
  (let ((time (current-time)))
    (setq fuzzy-clock-zh-string
          (concat " "
                  (fuzzy-clock-zh-time-string fuzzy-clock-zh-fuzziness time)
                  " "))))

;;;###autoload
(define-minor-mode fuzzy-clock-zh-mode
  "Toggle fuzzy clock display in the mode-line.
When enabled, displays the current time in a human-friendly,
approximate format in the mode-line."
  :global t
  :init-value nil
  (if fuzzy-clock-zh-mode
      ;; Mode is being enabled
      (progn
        (fuzzy-clock-zh-update)
        (unless (member '(:eval fuzzy-clock-zh-string) global-mode-string)
          (add-to-list 'global-mode-string '(:eval fuzzy-clock-zh-string) t))
        (setq fuzzy-clock-zh-timer
              (run-at-time t fuzzy-clock-zh-update-interval #'fuzzy-clock-zh-update)))
    ;; Mode is being disabled
    (when fuzzy-clock-zh-timer
      (cancel-timer fuzzy-clock-zh-timer)
      (setq fuzzy-clock-zh-timer nil))
    (setq global-mode-string
          (delete '(:eval fuzzy-clock-zh-string) global-mode-string))))

(provide 'fuzzy-clock-zh)
;;; fuzzy-clock-zh.el ends here
