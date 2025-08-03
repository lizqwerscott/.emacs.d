(require 'immersive-translate)

(setq immersive-translate-backend 'trans)
(setq immersive-translate-trans-default-args
      (concat " " immersive-translate-trans-default-args))

(provide 'init-immersive-translate)
