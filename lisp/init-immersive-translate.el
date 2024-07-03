(require 'immersive-translate)

(setq immersive-translate-backend 'trans)
(setq immersive-translate-trans-default-args
      (concat (if user/use-proxy
                  (format " -x %s:%s"
                          user/proxy-host
                          user/proxy-rule-port)
                "")
              " "
              immersive-translate-trans-default-args))

(provide 'init-immersive-translate)
