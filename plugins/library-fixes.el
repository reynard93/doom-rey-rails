(after! evil
  (defadvice
      evil-ex-search
      (after evil-search-forward-recenter activate)
    (recenter))
  (ad-activate 'evil-ex-search))
