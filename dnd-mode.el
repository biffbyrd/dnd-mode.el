
(define-minor-mode dnd-mode
  :lighter " D&D"
  :keymap (let ((map (make-sparse-keymap)))
	    (define-key map (kbd "C-c d h") 'dnd-rehighlight-dice-buttons)
	    (define-key map (kbd "C-c d n") 'dnd-next-dice-button)
	    (define-key map (kbd "C-c d p") 'dnd-previous-dice-button)
            map))

(provide 'dnd-mode)


(make-variable-buffer-local
 (defvar *dnd-mode-buttons* nil))


;; Highlighting dice buttons


(defun dnd-highlight-dice-buttons ()
  "Finds all dice-roll strings in the buffer and turns them into clickable 
buttons that will print the result of the dice roll depicted."
  (interactive)
  (save-excursion
    (goto-char 1)
    (lexical-let* ((b-data (dnd--find-next-button-data)))
      (while b-data
	(lexical-let* ((b-beg (dnd--btn-data-beg b-data))
		       (b-end (dnd--btn-data-end b-data))
		       (b-n (dnd--btn-data-n b-data))
		       (b-d (dnd--btn-data-d b-data))
		       (b-b (dnd--btn-data-b b-data)))
	 (add-to-list '*dnd-mode-buttons*
		     (make-button b-beg b-end
				  'action (lambda (b) (print (dnd-roll b-n b-d b-b))))))
	(setq b-data (dnd--find-next-button-data))))))

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
(setq dnd--regexp-1d2-3 "\\([0-9]+\\)d\\([0-9]+\\)-\\([0-9]+\\)")   ;; 2d4-3
(setq dnd--regexp-1d2 "\\([0-9]+\\)d\\([0-9]+\\)")                  ;; 2d4
(setq dnd--regexp-d2 "d\\([0-9]+\\)")                               ;; d4
(setq dnd--regexp-+3 "\\+\\([0-9]+\\)")                             ;; +3
(setq dnd--regexp--3 "-\\([0-9]+\\)")                               ;; -3
(setq dnd--regexp-33% "\\([0-9]+\\)%")                              ;; 33%
(setq dnd--regexp-33-percent "\\([0-9]+\\) percent")                ;; 33 percent

(setq dnd--dice-regexp
      (mapconcat (lambda (rx) (concat "[[:space:]\n(,]" rx "[[:space:]\n),]"))
                 (list dnd--regexp-1d2+3
		       dnd--regexp-1d2-3
                       dnd--regexp-1d2
                       dnd--regexp-d2
                       dnd--regexp-+3
                       dnd--regexp--3
                       dnd--regexp-33%
                       dnd--regexp-33-percent)
                 "\\|"))


(defun dnd--btn-data (str beg end n d b) (list str beg end n d b))
(defun dnd--btn-data-str (bd) (nth 0 bd))
(defun dnd--btn-data-beg (bd) (nth 1 bd))
(defun dnd--btn-data-end (bd) (nth 2 bd))
(defun dnd--btn-data-n (bd) (nth 3 bd))
(defun dnd--btn-data-d (bd) (nth 4 bd))
(defun dnd--btn-data-b (bd) (nth 5 bd))

(defun dnd--find-next-button-data ()
  (lexical-let* ((end (search-forward-regexp dnd--dice-regexp nil t)))
    (if (not end)
	nil
      (car ;; try out all these matchers and return the first one that works
       (seq-filter
	'identity
	(list
	 (dnd--match-1d2+3 end)
	 (dnd--match-1d2-3 end)
	 (dnd--match-1d2 end)
	 (dnd--match-d2 end)
	 (dnd--match-+3 end)
	 (dnd--match--3 end)
	 (dnd--match-% end)))))))

(defun dnd--match-1d2+3 (end)
  (dnd--match-btn-data end
		       (dnd--match-number 1)
		       (dnd--match-number 2)
		       (dnd--match-number 3)))

(defun dnd--match-1d2-3 (end)
  (dnd--match-btn-data end
		       (dnd--match-number 4)
		       (dnd--match-number 5)
		       (let ((b (dnd--match-number 6)))
			 (if b (* -1 b) b))))

(defun dnd--match-1d2 (end)
  (dnd--match-btn-data end
		       (dnd--match-number 7)
		       (dnd--match-number 8)
		       0))

(defun dnd--match-d2 (end)
  (dnd--match-btn-data end
		       1
		       (dnd--match-number 9)
		       0))

(defun dnd--match-+3 (end)
  (dnd--match-btn-data end
		       1
		       20
		       (dnd--match-number 10)))

(defun dnd--match--3 (end)
  (dnd--match-btn-data end
		       1
		       20
		       (let ((b (dnd--match-number 11)))
			 (if b (* -1 b) b))))

(defun dnd--match-% (end)
  (dnd--match-btn-data end
		       1
		       100
		       0))

(defun dnd--match-btn-data (end n d b)
  (lexical-let* ((s (match-string 0))
		 (beg (- end (length s))))
    (if (and n d b)
	(dnd--btn-data s (+ beg 1) (- end 1) n d b)
      nil)))

(defun dnd--match-number (count)
  (lexical-let* ((num-str (match-string count)))
    (if num-str
	(string-to-number num-str)
      nil)))
