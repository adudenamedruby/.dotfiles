;; [[file:../synthmacs.org::*JSON][JSON:1]]
(use-package json-mode
  :general
  (synthmacs/local-leader-keys
    :keymaps 'json-mode-map
    "p" 'json-pretty-print)
  )
;; JSON:1 ends here
