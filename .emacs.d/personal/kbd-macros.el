(fset 'scala-macro-println-simple
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([5 67108896 1 23 25 67108896 1 40 34 25 6 32 32 backspace backspace 2 32 6 32 43 32 1 112 114 105 110 116 108 110 5] 0 "%d")) arg)))

(fset 'scala-macro-class-fields-list
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([19 40 return 67108896 19 41 return 134217847 24 98 return 5 return 25] 0 "%d")) arg)))

(fset 'scala-macro-class-field-set-value
      (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([tab 67108896 19 58 18 134217847 11 32 61 32 25 134217849 44 14 1] 0 "%d")) arg)))

(fset 'scala-macro-remove-inner-scope
      (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([5 134217730 134217730 8388731 40 8388731 123 2 32 6 32 5 8388699] 0 "%d")) arg)))

;; Must be run in *HTTP Response* buffer with point at the beggining of the buffer, and second buffer must be restclient with :my-auth = abc.token.xyz
(fset 'pure360-authorization
      (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([19 58 32 34 return 67108896 19 34 18 134217847 134217839 19 32 61 32 return 25 11] 0 "%d")) arg)))

(fset 'search-for-text-from-cursor-to-end-of-line
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([11 67108911 8388678 1 25 12 11 return] 0 "%d")) arg)))

(fset 'search-for-class-json-format
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([134217830 134217830 6 134217751 8388678 1 99 108 97 115 115 32 return] 0 "%d")) arg)))


(global-set-key (kbd "C-x C-k P") 'scala-macro-println-simple)
(global-set-key (kbd "C-x C-k C") 'scala-macro-class-fields-list)
(global-set-key (kbd "C-x C-k F") 'scala-macro-class-field-set-value)
(global-set-key (kbd "C-x C-k L") 'scala-macro-remove-inner-scope)
(global-set-key (kbd "C-x C-k A") 'pure360-authorization)
(global-set-key (kbd "C-x C-k B") 'search-for-text-from-cursor-to-end-of-line)
(global-set-key (kbd "C-x C-k M") 'search-for-class-json-format)
