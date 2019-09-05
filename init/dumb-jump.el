;;; -*- lexical-binding: t -*-

(use-package dumb-jump
  :bind (("C-c j j" . dumb-jump-go)
         ("C-c j p" . dumb-jump-back)
         ("C-c j l" . dumb-jump-quick-look))
  :init (setq dumb-jump-force-searcher 'rg
              dumb-jump-rg-search-args ""
              dumb-jump-selector 'ivy)
  :config
  (progn
    (unbind-key "C-M-g" dumb-jump-mode-map)
    (unbind-key "C-M-p" dumb-jump-mode-map)
    (unbind-key "C-M-q" dumb-jump-mode-map)

    (setq dumb-jump-find-rules
          (seq-concatenate
           'list
           '((:type "top-level binding" :supports ("rg") :language "haskell"
                    :regex "^JJJ($|(\\s+(::|([^:=|-]*(= |[|])))))")
             (:type "local binding" :supports ("rg") :language "haskell"
                    :regex "^\\s+((let|where)\\s+)?JJJ($|(\\s+(::|([^:=|-]*(= |[|])))))")
             (:type "data constructor" :supports ("rg") :language "haskell"
                    :regex "^(data|newtype)\\s+.*(((= |[|])\\s+JJJ($|\\s)))")
             (:type "data constructor newline" :supports ("rg") :language "haskell"
                    :regex "^\\s+[=|]\\s+(forall\\s+[^.]+\\.\\s+)?JJJ(\\s|$)")
             (:type "record field" :supports ("rg") :language "haskell"
                    :regex "^((data|newtype)\\s+.*((= |[|])\\s+[^{]+[{]\\s+JJJ\\s+::))|(\\s+[,{]\\s+JJJ\\s+::)")
             (:type "GADT constructor" :supports ("rg") :language "haskell"
                    :regex "^\\s+JJJ\\s+::\\s")
             (:type "module" :supports ("rg") :language "haskell"
                    :regex "^module\\s+JJJ(\\s|$)")
             (:type "type-like" :supports ("rg") :language "haskell"
                    :regex "^((data(\\s+family)?)|(newtype)|(type(\\s+family)?))\\s+JJJ(\\s|$)")
             (:type "typeclass" :supports ("rg") :language "haskell"
                    :regex "^class\\s+([^=]+=>\\s+)?JJJ(\\s|$)")
             (:type "associated type family" :supports ("rg") :language "haskell"
                    :regex "^\\s+type\\s+Key\\s+[^:]+::"))
           (seq-remove (lambda (rule) (string-equal "haskell" (plist-get rule :language)))
                       dumb-jump-find-rules)))))
