
(defun dnd-roll (n d &optional b)
  "Rolls dice according to the form NdD+B. Returns a list (rslt (bonus rolls...))"
  (lexical-let*
      ((rolls0 (mapcar
                (lambda (ignore) (+ (random* d) 1))
		(number-sequence 1 n)))
       (rolls1 (if (and b (/= b 0))
		   (cons b rolls0)
		 rolls0))
       (res (apply '+ rolls1)))
    (list res rolls1)))

(defun dnd-rolld20 (&optional b) (dnd-roll 1 20 b))

;; regexps for searching
(setq dnd--regexp-1d2+3 "\\([0-9]+\\)d\\([0-9]+\\)\\+\\([0-9]+\\)") ;; 2d4+3
(setq dnd--regexp-1d2 "\\([0-9]+\\)d\\([0-9]+\\)")                  ;; 2d4
(setq dnd--regexp-d2 "d\\([0-9]+\\)")                               ;; d4
(setq dnd--regexp-+3 "\\+\\([0-9]+\\)")                             ;; +3
(setq dnd--regexp--3 "-\\([0-9]+\\)")                               ;; -3
(setq dnd--regexp-33% "\\([0-9]+\\)%")                              ;; 33%
(setq dnd--regexp-33-percent "\\([0-9]+\\) percent")                ;; 33 percent

(defun dnd--btn-data (str beg end n d b) (list str beg end n d b))
(defun dnd--btn-data-str (bd) (nth 0 bd))
(defun dnd--btn-data-beg (bd) (nth 1 bd))
(defun dnd--btn-data-end (bd) (nth 2 bd))
(defun dnd--btn-data-n (bd) (nth 3 bd))
(defun dnd--btn-data-d (bd) (nth 4 bd))
(defun dnd--btn-data-b (bd) (nth 5 bd))


;; after doing a regex search, use these matchers to extract info
(defun dnd--match-1d2+3 (end)
  (lexical-let* ((s (match-string 0))
		 (ns (match-string 1))
		 (ds (match-string 2))
		 (bs (match-string 3))
		 (beg (- end (length s))))
    (if (and ns ds bs)
	(dnd--btn-data s beg end
		       (string-to-number ns)
		       (string-to-number ds)
		       (string-to-number bs))
      nil)))

(defun dnd--match-1d2 (end)
  (lexical-let* ((s (match-string 0))
		 (ns (match-string 4))
		 (ds (match-string 5))
		 (b  0)
		 (beg (- end (length s))))
    (if (and ns ds)
	(dnd--btn-data s beg end
		       (string-to-number ns)
		       (string-to-number ds)
		       b)
      nil)))

(defun dnd--match-d2 (end)
  (lexical-let* ((s (match-string 0))
		 (n 1)
		 (ds (match-string 6))
		 (b 0)
		 (beg (- end (length s))))
    (if ds
	(dnd--btn-data s beg end
		       n
		       (string-to-number ds)
		       b)
      nil)))

(defun dnd--match-+3 (end)
  (lexical-let* ((s (match-string 0))
		 (n 1)
		 (d 20)
		 (bs (match-string 7))
		 (beg (- end (length s))))
    (if bs
	(dnd--btn-data s beg end
		       n
		       d
		       (string-to-number bs))
      nil)))

(defun dnd--match--3 (end)
  (lexical-let* ((s (match-string 0))
		 (n 1)
		 (d 20)
		 (bs (match-string 8))
		 (beg (- end (length s))))
    (if bs
	(dnd--btn-data s beg end
		       n
		       d
		       (* -1 (string-to-number bs)))
      nil)))

(defun dnd--match-% (end)
  (lexical-let* ((s (match-string 0))
		 (n 1)
		 (d 100)
		 (b 0)
		 (ps (match-string 9))
		 (beg (- end (length s))))
    (if ps
	(dnd--btn-data s beg end n d b)
      nil)))

(defun dnd--match-percent (end)
  (lexical-let* ((s (match-string 0))
		 (n 1)
		 (d 100)
		 (b 0)
		 (ps (match-string 10))
		 (beg (- end (length s))))
    (if ps
	(dnd--btn-data s beg end n d b)
      nil)))

(setq dnd--dice-regexp
      (mapconcat 'identity
                 (list dnd--regexp-1d2+3
                       dnd--regexp-1d2
                       dnd--regexp-d2
                       dnd--regexp-+3
                       dnd--regexp--3
                       dnd--regexp-33%
                       dnd--regexp-33-percent)
                 "\\|"))

(make-variable-buffer-local
 (defvar *dnd-mode-buttons* nil))

(defun dnd-highlight-dice-buttons ()
  "Returns a list of the buffer's dnd--btn-data, which is a list of lists containing 
the matching string, beginning positiong, end position, number of dice, the 
dice rolled (e.g. d12), and the bonus. ((string beg end num die bonus) ...)"
  (interactive)
  (save-excursion
    (goto-char 1)
    (lexical-let* ((end (search-forward-regexp dnd--dice-regexp nil t)))
      (while end
	(lexical-let* ((data (car
			      (seq-filter
			       'identity
			       (list 
				(dnd--match-1d2+3 end)
				(dnd--match-1d2 end)
				(dnd--match-d2 end)
				(dnd--match-+3 end)
				(dnd--match--3 end)
				(dnd--match-% end)
				(dnd--match-percent end))))))
	  (if data
	      (add-to-list '*dnd-mode-buttons*
			   (make-button (dnd--btn-data-beg data) end
					'action (lambda (b)
						  (print (dnd-roll
							  (dnd--btn-data-n data)
							  (dnd--btn-data-d data)
							  (dnd--btn-data-b data)))))))
	  (setq end (search-forward-regexp dnd--dice-regexp nil t)))))))

(defun dnd-unhighlight-dice-buttons ()
  (interactive)
  (mapcar 'delete-overlay *dnd-mode-buttons*)
  (setq *dnd-mode-buttons* nil))

(defun dnd-rehighlight-dice-buttons ()
  (interactive)
  (dnd-unhighlight-dice-buttons)
  (dnd-highlight-dice-buttons))

(defun dnd-next-dice-button ()
  (interactive)
  (lexical-let* ((pos (point))
		 (rest (seq-filter
			(lambda (beg) (< pos beg))
			(mapcar 'button-start *dnd-mode-buttons*)))
		 (next-pos (if rest (seq-min rest) pos)))
    (goto-char next-pos)))

(defun dnd-previous-dice-button ()
  (interactive)
  (lexical-let* ((pos (point))
		 (rest (seq-filter
			(lambda (beg) (> pos beg))
			(mapcar 'button-start *dnd-mode-buttons*)))
		 (next-pos (if rest (seq-max rest) pos)))
    (goto-char next-pos)))

(define-minor-mode dnd-mode
  :lighter " D&D"
  :keymap (let ((map (make-sparse-keymap)))
	    (define-key map (kbd "C-c d h") 'dnd-rehighlight-dice-buttons)
	    (define-key map (kbd "C-c d n") 'dnd-next-dice-button)
	    (define-key map (kbd "C-c d p") 'dnd-previous-dice-button)
            map))

(provide 'dnd-mode)
