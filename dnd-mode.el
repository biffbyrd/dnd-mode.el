
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
    (lexical-let* ((b-data (*dnd-find-next-button-data)))
      (while b-data
	(lexical-let* ((b-beg (*dnd-btn-data-beg b-data))
		       (b-end (*dnd-btn-data-end b-data))
		       (b-n (*dnd-btn-data-n b-data))
		       (b-d (*dnd-btn-data-d b-data))
		       (b-b (*dnd-btn-data-b b-data)))
	  (add-to-list '*dnd-mode-buttons*
		       (make-button b-beg b-end
				    'action (lambda (b) (print (dnd-roll b-n b-d b-b))))))
	(setq b-data (*dnd-find-next-button-data))))))

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
(setq *dnd-regexp-1d2+3 "\\([0-9]+\\)d\\([0-9]+\\) ?\\+ ?\\([0-9]+\\)") ;; 2d4+3, 2d4 + 3
(setq *dnd-regexp-1d2-3 "\\([0-9]+\\)d\\([0-9]+\\)-\\([0-9]+\\)")   ;; 2d4-3
(setq *dnd-regexp-1d2 "\\([0-9]+\\)d\\([0-9]+\\)")                  ;; 2d4
(setq *dnd-regexp-d2 "d\\([0-9]+\\)")                               ;; d4
(setq *dnd-regexp-+3 "\\+\\([0-9]+\\)")                             ;; +3
(setq *dnd-regexp--3 "-\\([0-9]+\\)")                               ;; -3
(setq *dnd-regexp-33% "\\([0-9]+\\)%")                              ;; 33%
(setq *dnd-regexp-33-percent "\\([0-9]+\\) percent")                ;; 33 percent

(setq *dnd-dice-regexp
      (mapconcat (lambda (rx) (concat "[[:space:]\n(,]" rx "[[:space:]\n),]"))
                 (list *dnd-regexp-1d2+3
		       *dnd-regexp-1d2-3
                       *dnd-regexp-1d2
                       *dnd-regexp-d2
                       *dnd-regexp-+3
                       *dnd-regexp--3
                       *dnd-regexp-33%
                       *dnd-regexp-33-percent)
                 "\\|"))


(defun *dnd-btn-data (str beg end n d b) (list str beg end n d b))
(defun *dnd-btn-data-str (bd) (nth 0 bd))
(defun *dnd-btn-data-beg (bd) (nth 1 bd))
(defun *dnd-btn-data-end (bd) (nth 2 bd))
(defun *dnd-btn-data-n (bd) (nth 3 bd))
(defun *dnd-btn-data-d (bd) (nth 4 bd))
(defun *dnd-btn-data-b (bd) (nth 5 bd))

(defun *dnd-find-next-button-data ()
  (lexical-let* ((end (search-forward-regexp *dnd-dice-regexp nil t)))
    (if (not end)
	nil
      (car ;; try out all these matchers and return the first one that works
       (seq-filter
	'identity
	(list
	 (*dnd-match-1d2+3 end)
	 (*dnd-match-1d2-3 end)
	 (*dnd-match-1d2 end)
	 (*dnd-match-d2 end)
	 (*dnd-match-+3 end)
	 (*dnd-match--3 end)
	 (*dnd-match-% end)))))))

(defun *dnd-match-1d2+3 (end)
  (*dnd-match-btn-data end
		       (*dnd-match-number 1)
		       (*dnd-match-number 2)
		       (*dnd-match-number 3)))

(defun *dnd-match-1d2-3 (end)
  (*dnd-match-btn-data end
		       (*dnd-match-number 4)
		       (*dnd-match-number 5)
		       (let ((b (*dnd-match-number 6)))
			 (if b (* -1 b) b))))

(defun *dnd-match-1d2 (end)
  (*dnd-match-btn-data end
		       (*dnd-match-number 7)
		       (*dnd-match-number 8)
		       0))

(defun *dnd-match-d2 (end)
  (*dnd-match-btn-data end
		       1
		       (*dnd-match-number 9)
		       0))

(defun *dnd-match-+3 (end)
  (*dnd-match-btn-data end
		       1
		       20
		       (*dnd-match-number 10)))

(defun *dnd-match--3 (end)
  (*dnd-match-btn-data end
		       1
		       20
		       (let ((b (*dnd-match-number 11)))
			 (if b (* -1 b) b))))

(defun *dnd-match-% (end)
  (*dnd-match-btn-data end
		       1
		       100
		       0))

(defun *dnd-match-btn-data (end n d b)
  (lexical-let* ((s (match-string 0))
		 (beg (- end (length s))))
    (if (and n d b)
	(*dnd-btn-data s (+ beg 1) (- end 1) n d b)
      nil)))

(defun *dnd-match-number (count)
  (lexical-let* ((num-str (match-string count)))
    (if num-str
	(string-to-number num-str)
      nil)))

;; SPELLS


(defconst *dnd-file-base (file-name-directory (or load-file-name "/home/biff/dnd-mode.el/dnd-mode.el")))

(defun *dnd-spells-json-file-name ()
  (expand-file-name "spells.json" *dnd-file-base))

(defconst *dnd-spell-list
  (let* ((json-array-type 'list)
	 (json-key-type 'string)
	 (json (json-read-file (*dnd-spells-json-file-name))))
    json))

(cl-defun dnd-lookup-spells (&key filter compare)
  (lexical-let* ((filter_ (or filter 'always-t))
		 (comp_ (or compare 'always-nil)))
    (seq-sort comp_ (seq-filter filter_ *dnd-spell-list))))

(defun always-t (a) t)
(defun always-nil (a b) nil)

(defun dnd-spell-filter-name (rgx)
  (lexical-let* ((r rgx))
    (lambda (spell)
      (let* ((sn (downcase (*dnd-assocdr "name" spell))))
	(string-match-p r sn)))))

(defun *dnd-assocdr (key alist)
  (cdr (assoc key alist)))

(defun dnd-display-spells (spells)
  (if (not spells) nil
    (lexical-let* ((buf (get-buffer-create (concat "dnd-" (*dnd-assocdr "name" (car spells))))))
      (split-window-horizontally)
      (other-window 1)
      (switch-to-buffer buf)
      (dolist (spell spells)
        (insert "-------------------------------------------------------\n")
	(insert (concat (*dnd-format-spell-title spell) "\n\n"))
	(insert (concat (*dnd-format-spell-cast-time spell) "\n"))
	(insert (concat (*dnd-format-spell-components spell) "\n"))
	(insert (concat (*dnd-format-spell-duration spell) "\n\n"))
	(insert (concat (*dnd-format-spell-entries spell) "\n\n"))
	(insert (*dnd-format-spell-higher-levels-entry spell))
	(insert (concat (*dnd-assocdr ""))))
      (dnd-rehighlight-dice-buttons))))

(defun *dnd-format-spell-title (spell)
  (concat "* " (*dnd-assocdr "name" spell) " (" (*dnd-format-spell-level spell) ", " (*dnd-format-spell-school spell) ")" ))

(defun *dnd-format-spell-level (spell)
  (let* ((lvl (number-to-string (*dnd-assocdr "level" spell))))
    (concat "Level " lvl)))

(defun *dnd-format-spell-school (spell)
  (let* ((sch-short (*dnd-assocdr "school" spell))
	 (sch-long (cond ((equal sch-short "A") "abjuration")
			 ((equal sch-short "C") "conjuration")
			 ((equal sch-short "D") "divination")
			 ((equal sch-short "E") "enchantment")
			 ((equal sch-short "I") "illusion")
                         ((equal sch-short "N") "necromancy")
                         ((equal sch-short "T") "transmutation")
                         ((equal sch-short "V") "evocation")
		         (t ""))))
    sch-long))

(defun *dnd-format-spell-cast-time (spell)
  (*dnd-wrap-string
   (concat
    "Time: "
    (lexical-let* ((times (*dnd-assocdr "time" spell)))
      (mapconcat
       (lambda (time)
         (lexical-let* ((number (number-to-string (*dnd-assocdr "number" time)))
			(unit (*dnd-assocdr "unit" time))
			(condition (*dnd-assocdr "condition" time))
			(rslt (concat number " " unit)))
           (if (not condition)
               rslt
             (concat rslt " (" condition ")"))))
       times
       " / ")))))

(defun *dnd-format-spell-range (spell)
  (lexical-let* ((range (*dnd-assocdr "range" spell))
                 (type (*dnd-assocdr "type" range))
                 (distance (*dnd-assocdr "distance" range))
                 (amount (number-to-string (*dnd-assocdr "amount" distance)))
                 (unit (*dnd-assocdr "type" distance)))
    (concat "Range: " amount " " unit " (" type ")")))

(defun *dnd-format-spell-components (spell)
  (*dnd-wrap-string
   (lexical-let* ((comps (*dnd-assocdr "components" spell))
                  (v (if (*dnd-assocdr "v" comps) "V" nil))
                  (s (if (*dnd-assocdr "s" comps) "S" nil))
                  (m? (*dnd-assocdr "m" comps))
                  (m (cond ((not m?) nil)
                           ((stringp m?) m?)
                           ((listp m?) (*dnd-assocdr "text" m?))
                           (t nil)))
                  (comp-list (seq-filter 'identity (list v s m)))
                  (comp-str (mapconcat 'identity comp-list ", ")))                 
     (concat "Components: " comp-str))))

(defun *dnd-wrap-string (str)
  (with-temp-buffer
    (set-fill-column 50)
    (insert str)
    (fill-region (point-min) (point-max))
    (buffer-string)))

(defun *dnd-format-spell-duration (spell)
  (*dnd-wrap-string
   (concat
    "Duration: "
    (mapconcat
     (lambda (dur)
       (lexical-let* ((type (*dnd-assocdr "type" dur)))
         (cond ((or (equal type "instant") (equal type "special")) type)
               ((equal type "permanent")
		(concat "until " (mapconcat 'identity (*dnd-assocdr "ends" dur) " or ")))
               ((equal type "timed")
		(let* ((dur2 (*dnd-assocdr "duration" dur))
                       (conc (if (*dnd-assocdr "concentration" dur) "Concentraion, " ""))
                       (upto (if (*dnd-assocdr "upTo" dur2) "up to " ""))
                       (amount (concat (number-to-string (*dnd-assocdr "amount" dur2)) " "))
                       (unit (*dnd-assocdr "type" dur2)))
                  (concat conc upto amount unit))))))
     (*dnd-assocdr "duration" spell)
     " / "))))


(defun *dnd-format-spell-entries (spell)
  (lexical-let* ((entries (*dnd-assocdr "entries" spell)))
    (mapconcat (lambda (entry)
		 (cond ((stringp entry) (*dnd-wrap-string entry))
		       ((listp entry) (*dnd-format-entry-object entry))
		       (t "???")))
	       entries
	       "\n\n")))

(defun *dnd-format-entry-object (entry)
  (let* ((name (*dnd-assocdr "name" entry))
         (type (*dnd-assocdr "type" entry)))
    (cond ((equal type "entries")
           (*dnd-wrap-string (concat name ". " (mapconcat 'identity (*dnd-assocdr "entries" entry) "\n\n"))))
          ((equal type "list") (mapconcat (lambda (i) (*dnd-wrap-string (concat "- " i))) (*dnd-assocdr "items" entry) "\n"))
          ((equal type "table") (*dnd-format-entry-object-table entry))
          (t ""))))

(defun *dnd-format-entry-object-table (table)
  (lexical-let* ((colLabels (*dnd-assocdr "colLabels" table))
                 (rows (*dnd-assocdr "rows" table)))
    (with-temp-buffer
      (goto-char 1)
      (dolist (lbl colLabels)
        (insert (concat "|" lbl)))
      (insert "\n|-\n")
      (dolist (row rows)
        (dolist (cell row)
          (if (stringp cell)
              (insert (concat "|" cell))
            (lexical-let* ((roll (*dnd-assocdr "roll" cell))
                           (exact (*dnd-assocdr "exact" roll))
                           (min (*dnd-assocdr "min" roll))
                           (max (*dnd-assocdr "max" roll)))
              (if exact
                  (insert (concat "|" (number-to-string exact)))
                (insert (concat "|" (number-to-string min) " - " (number-to-string max)))))))
        (insert "\n"))
      (goto-char 1)
      (org-mode)
      (org-table-align)
      (buffer-string))))

(defun *dnd-format-spell-higher-levels-entry (spell)
  (let* ((entriesHigherLevel (*dnd-assocdr "entriesHigherLevel" spell))
         (str (mapconcat '*dnd-format-entry-object entriesHigherLevel "\n\n")))
    (if (= (length str) 0)
        str
      (concat str "\n\n"))))
