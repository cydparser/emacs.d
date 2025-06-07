;;; -*- lexical-binding: t -*-

(use-package all-the-icons)

(use-package color-theme-sanityinc-tomorrow
  :demand
  :hook (after-init-hook . (lambda () (load-theme 'sanityinc-tomorrow-night 'no-confirm)))
  :custom-face
  (hl-todo ((t (:foreground "#cc9393" :weight semi-bold))))
  (logview-edit-filters-type-prefix ((t (:background "#de935f" :weight bold))))
  (logview-error-entry ((t (:background "#181818"))))
  (logview-highlight ((t (:background "#282a2e"))))
  (logview-information-entry ((t nil)))
  (logview-pulse ((t (:background "#373b41"))))
  (logview-trace-entry ((t nil)))
  (logview-warning-entry ((t (:background "#181818"))))
  (smerge-refined-added ((t (:inherit smerge-refined-change :background "#113311"))))
  (smerge-refined-removed ((t (:inherit smerge-refined-change :background "#331f21"))))
  (whitespace-empty ((t (:background "#191919"))))
  (whitespace-line ((t (:background "#282a2e" :foreground unspecified)))))

(use-package rainbow-mode
  :diminish (rainbow-mode . "🌈"))

(use-package ligature
  :hook (after-init-hook . (lambda () (global-ligature-mode t)))
  :config
  (progn
    (let ((font (symbol-name (font-get (face-attribute 'default :font) :family))))
      (cond
       ((seq-some (lambda (s) (string-prefix-p s font)) '("FiraCode" "Casaydia" "Cascadia"))
        (ligature-set-ligatures
         t
         '("==" "===" "!=" "/=" "&&" "||"
           "->" "<-" "::" "__"
           "<=" ">=" "<=>"
           ))
        ;; Enable traditional ligature support in eww-mode, if the
        (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
        ;; Enable all Cascadia and Fira Code ligatures in programming modes
        (ligature-set-ligatures
         'prog-mode
         '(;; == === ==== => =| =>>=>=|=>==>> ==< =/=//=// =~
           ;; =:= =!=
           ("=" (rx (+ (or ">" "<" "|" "/" "~" ":" "!" "="))))
           ;; ;; ;;;
           (";" (rx (+ ";")))
           ;; && &&&
           ("&" (rx (+ "&")))
           ;; !! !!! !. !: !!. != !== !~
           ("!" (rx (+ (or "=" "!" "\." ":" "~"))))
           ;; ?? ??? ?:  ?=  ?.
           ("?" (rx (or ":" "=" "\." (+ "?"))))
           ;; %% %%%
           ("%" (rx (+ "%")))
           ;; |> ||> |||> ||||> |] |} || ||| |-> ||-||
           ;; |->>-||-<<-| |- |== ||=||
           ;; |==>>==<<==<=>==//==/=!==:===>
           ("|" (rx (+ (or ">" "<" "|" "/" ":" "!" "}" "\]" "-" "=" ))))
           ;; \\ \\\ \/
           ("\\" (rx (or "/" (+ "\\"))))
           ;; ++ +++ ++++ +>
           ("+" (rx (or ">" (+ "+"))))
           ;; :: ::: :::: :> :< := :// ::=
           (":" (rx (or ">" "<" "=" "//" ":=" (+ ":"))))
           ;; // /// //// /\ /* /> /===:===!=//===>>==>==/
           ("/" (rx (+ (or ">"  "<" "|" "/" "\\" "\*" ":" "!" "="))))
           ;; .. ... .... .= .- .? ..= ..<
           ("\." (rx (or "=" "-" "\?" "\.=" "\.<" (+ "\."))))
           ;; -- --- ---- -~ -> ->> -| -|->-->>->--<<-|
           ("-" (rx (+ (or ">" "<" "|" "~" "-"))))
           ;; *> */ *)  ** *** ****
           ("*" (rx (or ">" "/" ")" (+ "*"))))
           ;; www wwww
           ("w" (rx (+ "w")))
           ;; <> <!-- <|> <: <~ <~> <~~ <+ <* <$ </  <+> <*>
           ;; <$> </> <|  <||  <||| <|||| <- <-| <-<<-|-> <->>
           ;; <<-> <= <=> <<==<<==>=|=>==/==//=!==:=>
           ;; << <<< <<<<
           ("<" (rx (+ (or "\+" "\*" "\$" "<" ">" ":" "~"  "!" "-"  "/" "|" "="))))
           ;; >: >- >>- >--|-> >>-|-> >= >== >>== >=|=:=>> >> >>> >>>>
           (">" (rx (+ (or ">" "<" "|" "/" ":" "=" "-"))))
           ;; #: #= #! #( #? #[ #{ #_ #_( ## ### #####
           ("#" (rx (or ":" "=" "!" "(" "\?" "\[" "{" "_(" "_" (+ "#"))))
           ;; ~~ ~~~ ~=  ~-  ~@ ~> ~~>
           ("~" (rx (or ">" "=" "-" "@" "~>" (+ "~"))))
           ;; __ ___ ____ _|_ __|____|_
           ("_" (rx (+ (or "_" "|"))))
           ;; Fira code: 0xFF 0x12
           ("0" (rx (and "x" (+ (in "A-F" "a-f" "0-9")))))
           ;; Fira code:
           "Fl"  "Tl"  "fi"  "fj"  "fl"  "ft"
           ;; The few not covered by the regexps.
           "{|"  "[|"  "]#"  "(*"  "}#"  "$>"  "^=")))
       ((seq-some (lambda (s) (string-prefix-p s font)) '("Hasklug" "Hasklig"))
        (ligature-set-ligatures
         'prog-mode
         '("&&"  "***" "*>"  "\\\\" "||"  "|>"  "::"
           "=="  "===" "==>" "=>"   "=<<" "!!"  ">>"
           ">>=" ">>>" ">>-" ">-"   "->"  "-<"  "-<<"
           "<*"  "<*>" "<|"  "<|>"  "<$>" "<>"  "<-"
           "<<"  "<<<" "<+>" ".."   "..." "++"  "+++"
           "/="  ":::" ">=>" "->>"  "<=>" "<=<" "<->")))
       ((string-prefix-p "Iosevka" font)
        (ligature-set-ligatures
         'prog-mode
         '("<---" "<--" "<<-" "<-" "->" "-->" "--->" "<->" "<-->" "<--->" "<---->" "<!--"
           "<==" "<===" "<=" "=>" "=>>" "==>" "===>" ">=" "<=>" "<==>" "<===>" "<====>" "<!---"
           "<~~" "<~" "~>" "~~>" "::" ":::" "==" "!=" "===" "!=="
           ":=" ":-" ":+" "<*" "<*>" "*>" "<|" "<|>" "|>" "+:" "-:" "=:" "++" "+++")))
       (t (warn "Missing ligature setting for %s" font))))))

(use-package xterm-color
  :init
  (progn
    (setenv "TERM" "xterm-256color")

    (require 'compile)

    (defun init-xterm-color-compilation-filter (f proc string)
      (funcall f proc (xterm-color-filter string)))

    (advice-add 'compilation-filter
                :around #'init-xterm-color-compilation-filter)))
