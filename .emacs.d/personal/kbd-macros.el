(fset 'scala-macro-println-expression
      (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([67108896 5 67108896 1 134217847 112 114 105 110 116 108 110 40 34 25 32 34 32 43 32 40 5 41 41] 0 "%d")) arg)))

(fset 'scala-macro-class-fields-list
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([19 40 return 67108896 19 41 return 134217847 24 98 return 5 return 25] 0 "%d")) arg)))

(fset 'scala-macro-class-field-set-value
      (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([tab 67108896 19 58 18 134217847 11 32 61 32 25 134217849 44 14 1] 0 "%d")) arg)))

(fset 'scala-macro-remove-inner-scope
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([5 134217730 134217730 8388731 40 8388731 123 2 32 6 32 5 8388699] 0 "%d")) arg)))

(global-set-key (kbd "C-x C-k P") 'scala-macro-println-expression)
(global-set-key (kbd "C-x C-k C") 'scala-macro-class-fields-list)
(global-set-key (kbd "C-x C-k F") 'scala-macro-class-field-set-value)
(global-set-key (kbd "C-x C-k L") 'scala-macro-remove-inner-scope)
