#!/usr/bin/env newlisp

(context 'html)

(define-macro (html-token)
  (dolist (token (args))
    (letex (token (sym token)
		  (tag (replace "html-" (string token) "")) )
	   (define (token)
	     (letn ((is-option? (fn (x) (and (list? x) (list? (x 0)))))
		    (options (filter is-option? (args)))
		    (data    (filter (fn (x) (not (is-option? x))) (args)))
		    (option (if options
				(join (map
				       (fn (i) (format { %s="%s"} (replace "^MAIN:" (string (i 0)) "" 1) (string (i 1))))
				       (flat options 1)))))
		    (print-option (fn (x)
					  (format {<%s%s>%s</%s>} tag (if option option "") (string x) tag)))
		    (len (length data)))
		   (if (= len 1)
		       (letn ((arg (first data))
			      (arg (if (or (list? arg) (array? arg))
				       arg 
				     (list arg))))
			     (apply append (map 
					    (fn (i) (print-option i))
					    arg)))
		   (let (s  (join (map string data) ""))
			   (print-option s))
		     ))))))


(html-token html-html html-head html-body html-script html-style)
(html-token html-form html-frame html-frameset)
(html-token html-div html-span html-p)
(html-token html-h1 html-h2 html-h3 html-h4 html-h5 html-h6)
(html-token html-img)
(html-token html-ul html-ol html-li)
(html-token html-pre)
(html-token html-table html-thead html-tbody html-tfoot html-tr html-td)
(html-token html-a)
(html-token html-textarea)
(html-token html-area)
(html-token html-big html-small html-em html-strong html-u html-center)
(html-token html-button html-input html-select)
(html-token html-cite html-code)
(html-token html-dl html-dt html-dd)
(html-token html-svg html-text html-rect)
      
(define (html-test)
  (println
   (html-html
    (html-body
     (html-div "good day")
     (html-div
      (html-img '((src http:///1.jpg))))
     (dolist (x (sequence 1 10))
       (extend "" (html-div "abc")))
     (html-table '((class abcd))
		 (html-tr
		  (html-td (sequence 2 10))))))))

(context 'MAIN)

