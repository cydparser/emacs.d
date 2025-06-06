;;; -*- lexical-binding: t -*-

(use-package dumb-jump
  :after xref
  :demand
  :custom
  (dumb-jump-fallback-search nil)
  (dumb-jump-force-searcher 'rg)
  (dumb-jump-rg-search-args "--multiline")
  (dumb-jump-selector 'ivy)
  :init
  (progn
    ;; Add dumb-jump to global xref-backend-functions before etags--xref-backend.
    (add-hook 'xref-backend-functions 'dumb-jump-xref-activate 89))
  :config
  (progn
    (unbind-key "C-M-g" dumb-jump-mode-map)
    (unbind-key "C-M-p" dumb-jump-mode-map)
    (unbind-key "C-M-q" dumb-jump-mode-map)

    ;; XXX upstream this
    (setq dumb-jump-find-rules
          (seq-concatenate
           'list
           '(
             (:type "module" :supports ("rg") :language "haskell"
                    :regex "^module\\s+JJJ($|\\s)")
             (:type "top-level binding" :supports ("rg") :language "haskell"
                    :regex "^JJJ($|(\\s+(::|([^:=|-]*(=|[|])))))")
             ;; (:type "local binding" :supports ("rg") :language "haskell"
             ;;        :regex "^\\s+((let|where)\\s+)?JJJ($|(\\s+(::|([^:=|-]*(= |[|])))))")
             (:type "type" :supports ("rg") :language "haskell"
                    :regex "^(data|type)\\s+(family\\s+)?JJJ(\\s|$)")
             (:type "data constructor" :supports ("rg") :language "haskell"
                    :regex "^(data|newtype)\\s+.*(((=|[|])\\s+JJJ($|\\s)))")
             (:type "data constructor newline" :supports ("rg") :language "haskell"
                    :regex "^\\s+[=|]\\s+(forall\\s+[^.]+\\.\\s+)?JJJ(\\s|$)")
             (:type "record field" :supports ("rg") :language "haskell"
                    :regex "^(((data|newtype)\\s+.*((=|[|])\\s+[^{]+[{]\\s+JJJ\\s+::))|(\\s+[,{]\\s+JJJ\\s+::))")
             (:type "GADT constructor" :supports ("rg") :language "haskell"
                    :regex "^\\s+JJJ\\s+::\\s")
             (:type "pattern synonym" :supports ("rg") :language "haskell"
                    :regex "^pattern\\s+JJJ\\s")
             (:type "typeclass" :supports ("rg") :language "haskell"
                    :regex "^class\\s+([^=]+=>\\s+)?JJJ(\\s|$)")
             (:type "associated type family" :supports ("rg") :language "haskell"
                    :regex "^\\s+type\\s+JJJ\\s+[^:]+::"))
           (seq-remove (lambda (rule) (string-equal "haskell" (plist-get rule :language)))
                       dumb-jump-find-rules)))))
