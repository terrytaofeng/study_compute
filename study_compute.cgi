#!/usr/bin/env newlisp

;; macro
(define DEBUG (if (env "DEBUG") true nil))
(define UNIT_TEST_PREX "unit-test-")
(define UNIT_TEST_DOC  "doc_")
(define LINE_DIVISION (dup "-" 120))
(define MAX-LIST-LOG-COUNT 1000)
(define MAX-GRAPH-SVG-HIGHT 3)
(define MAX-GRAPH-SVG-COUNT 300)
(define HTML-GET-PROCESS-WITH-MAX 400)
(define HTML-GET-PROCESS-TIME-RATE 5)

(define DB-NAME "study_compute.db")
(define test-count 20)


(define-macro (load-module:load-module)
  (dolist (arg (args))
    (let ((r nil)
          (dirname (if (find "(.*/)" (real-path ((main-args) 1)) 1) $1 nil)))
      (or 
        (catch (load arg) 'r) 
        (catch (load (append dirname arg)) 'r)
        (catch (module arg) 'r)
        (catch (load (append "/usr/lib/bps/" arg)) 'r)
        (catch (load (append "/usr/local/lib/bps/" arg)) 'r)          
        ))))

;; load module
(module "sqlite3.lsp")
(module "getopts.lsp")
(load-module "html.lsp")


;; parse opt
(define (init-parse-opt)
  (shortopt "?" (getopts:usage) nil "help")
  (shortopt "h" (getopts:usage) nil "help")
  (shortopt "c" (setq test-count  (int getopts:arg)) "count" "Set test count")
  (shortopt "n" (setq test-number (int getopts:arg)) "number" "Set test number")
  (shortopt "d" (setq test-debug true) nil "Set debug")
  (getopts (2 (main-args)))
  )

;; db function


(define (init-db-sqls)
  (list
[text]
CREATE TABLE IF NOT EXISTS unit_test(
id INTEGER PRIMARY KEY AUTOINCREMENT, 
   sid VARCHAR,
   created INTEGER DEFAULT 0,
   type  VARCHAR,
   title VARCHAR,
   total INTEGER DEFAULT 0,
   right INTEGER DEFAULT 0,
   wrong INTEGER DEFAULT 0,
   score INTEGER DEFAULT 0,
   time  INTEGER DEFAULT 0)
[/text]
[text]
CREATE TABLE IF NOT EXISTS unit_test_log(
id INTEGER PRIMARY KEY AUTOINCREMENT,
sid VARCHAR,  
created INTEGER DEFAULT 0,
type  VARCHAR,
title VARCHAR,  
question VARCHAR,
answer   VARCHAR,
result   VARCHAR,
score INTEGER DEFAULT 0,
time  INTEGER DEFAULT 0)
[/text]
   ))

;; echo ".schema unit_test"|sqlite3 study_compute.db|grep -v CREATE|awk '{printf("\"%s\" ",$1)}'
(dolist (x (list "id" "sid" "created" "type" "title" "total" "right" "wrong" "score" "time" )) 
  (set (sym (append "U_" x)) $idx))

;; echo ".schema unit_test_log"|sqlite3 study_compute.db|grep -v CREATE|awk '{printf("\"%s\" ",$1)}'
(dolist (x (list "id" "sid" "created" "type" "title" "question" "answer" "result" "score" "time")) 
  (set (sym (append "L_" x)) $idx))


(define (init-db)
  (if (sql3:open DB-NAME) 
      (dolist (x (init-db-sqls))
        (if (sql3:sql x))))
  (sql3:close))

(define (log_compute_log sid type title question answer result score t)
  (if (sql3:open DB-NAME) 
      (if (sql3:sql "insert into unit_test_log(sid,created,type,title,question,answer,result,score,time) values (?, ?, ?, ?, ?, ?, ?, ?, ?);" 
                    (list sid (date-value (now)) type title question answer result score t))))
  (sql3:close))

(define (log_compute_test sid type title total right wrong score t)
  (if (sql3:open DB-NAME) 
      (if (sql3:sql "insert into unit_test(sid,created,type,title,total,right,wrong,score,time) values (? , ?, ?, ?, ?, ?, ?, ?, ?);" 
                    (list sid (date-value (now)) type title total right wrong score t))))
  (sql3:close))


(define (show-unit-result sid)
  (if (sql3:open DB-NAME) 
      (let (
            (unit (first (sql3:sql (format {select * from unit_test where sid='%s'} sid))))
            (qs   (sql3:sql (format {select * from unit_test_log where sid='%s' order by created} sid)))
            )
        (println LINE_DIVISION)
        (println (unit U_title) ":")
        (println)
        (dolist (q qs)
          (letn (r (= (q L_score) 1))
                (println (append
                          (if r "  " "x ") 
                          (format " [%d/%-2d] " (length qs) (+ 1 $idx))
                          (q L_question) 
                          (q L_answer) 
                          (if (not r) (format " (your answer:%s)" (q L_result)) "")
                          ))
                ))
        (println)
        (println (format "总共: %d 正确: %d 错误: %d  得分: %d 用时: %d"
                         (unit U_total) (unit U_right) (unit U_wrong) (unit U_score) (unit U_time) ))
        (println LINE_DIVISION)
        )))


;; unit and question definition

(define (get-name x)
  (let (result (replace "@.*" (copy (string x)) "" 1))
    (if (= "*" result)
        "x"
      result)))

(define (get-result question , answer)
  (if DEBUG
      (begin
       (println question)
       nil)
    (begin
     (do-while (empty? (set 'answer (read-line))) (print question))
     (if ( = answer "q")
         (exit)
       answer))))

;; s begin
;; s0 (x)
;; s1 (x op y)
;; s2 (x op1 y op2 z)
;; s3 (x op1 (y op2 z))
(define (compute-s sid title type ops total idx)
  (let (
        (question (cond  
                   ((= type "s0") (join (push "=" (map get-name ops) -1) " ")) 
                   ((= type "s1") (join (push "=" (map get-name ops) -1) " "))
                   ((= type "s2") (join (push "=" (map get-name ops) -1) " "))
                   ((= type "s3") (let (ops1 (list (ops 0) (ops 1) "(" (ops 2) (ops 3) (ops 4) ")" ))
                                    (join (push "=" (map get-name ops1) -1) " "))) 
                   ))
        (answer (cond
                 ((= type "s0") (string (ops 0)))
                 ((= type "s1") ((ops 1) (ops 0) (ops 2)))
                 ((= type "s2") ((ops 3) ((ops 1) (ops 0) (ops 2)) (ops 4))) 
                 ((= type "s3") ((ops 1) (ops 0) ((ops 3) (ops 2) (ops 4))))
                 ))
        (use_time 0)
        (result "")
        (score nil))
    (begin
     (set 'use_time (time (set 'result (get-result question))))
     (set 'score (cond
                  ((= type "s0") (= answer result))
                  (true (= answer (int result)))))
     (if test-debug
         (if score
             (println "ok")
           (println "error! answer is:" answer)))
     (log_compute_log sid type title question answer result (if score 1 0) use_time)
     (list score use_time))))


(define (get-scope ops idx total)
  (letn ((t (ops idx))
         (t (if (list? t) t (list t))))
        (slice (randomize t) 0 total)))

(define (list-s type ops rule-fun total, plist)
  (cond 
   ((= type "s0")
    (dolist (x   (get-scope ops 0 total))
      (if (or
           (not rule-fun)
           (rule-fun x))
          (push (list x) plist -1))))
   ((= type "s1")
    (dolist (x   (get-scope ops 0 total))
      (dolist (op (get-scope ops 1 total))
        (dolist (y (get-scope ops 2 total))
          (if (or
               (not rule-fun)
               (rule-fun x op y))
              (push (list x op y) plist -1))))))
   ((or (= type "s2") (= type "s3") )
    (dolist (x    (get-scope ops 0 total))
      (dolist (op1 (get-scope ops 1 total))
        (dolist (y  (get-scope ops 2 total))
          (dolist (op2 (get-scope ops 3 total))
            (dolist (z (get-scope ops 4 total))
              (if (or
                   (not rule-fun)
                   (rule-fun x op1 y op2 z))
                  (push (list x op1 y op2 z) plist -1))))))))

   )
  plist)


(define (process-s type title ops rule-fun total , sid a idx r right t)
  (begin
   (println title)
   (set 'sid (uuid))
   (set 'a (list-s type ops rule-fun total))
   (set 'a (slice (randomize a) 0 total))
   (set 'total (length a))
   (set 'idx 0)
   (set 'r (map (fn (i) 
                    (begin
                     (inc idx)
                     (print "[" total "/" idx "]  :  ")
                     (compute-s sid title type i total idx)))
                a))
   (set 'right (length (filter true? ((transpose r) 0))))
   (set 'score (/ (* 100 right) total))
   (set 't (/ (apply + ((transpose r) 1)) 1000))
   (log_compute_test sid type title total right (- total right) score t)
   (show-unit-result sid)
   (println "Enter to continue...")
   (read-line)
   (println "")))



(define autoid:autoid 0)
(define (auto-id id)
  (if (empty? id)
      (format "%03d" autoid:autoid)
    id))

(define-macro (M-S id type title ops rule-fun total)
  (letex
   ((fun (sym (append UNIT_TEST_PREX type "_" (auto-id id))))
    (fun_doc (sym (append UNIT_TEST_DOC UNIT_TEST_PREX type "_" (auto-id id))))
    (type type)
    (title title)
    (ops ops)
    (rule-fun rule-fun)
    (total total)
    (t1 (inc autoid:autoid))
    )
   (begin 
    (define fun_doc title)
    (define (fun) (process-s type title ops rule-fun total)))))

;;;  s end

(define (rule-jie-wei x op y) 
  (and
   (> x y)
   (> (% y 10) (% x 10))))

(define (rule-positive x op1 y) 
  (> (op1 x y) 0))

(define (rule-positive3 x op1 y op2 z) 
  (and
   (> (op1 x y) 0)
   (> (op2 (op1 x y) z) 0)))

(define (rule-positive-s3 x op1 y op2 z) 
  (and
   (> (op2 y z) 0)
   (> (op1 x (op2 y z)) 0)))

(define L10 (sequence 1 9))
(define L10_20 (sequence 11 19))
(define L20 (sequence 1 19))
(define L50 (sequence 1 49))
(define L100 (sequence 1 99))

                                        ;define unit-tests
(M-S "" "s0" "10以内按键"  (list L10) nil test-count)
(M-S "" "s0" "100以内按键"  (list L100) nil test-count)
(M-S "" "s0" "a-z以内按键"  (list (map char (sequence (char "a") (char "z")))) nil test-count)

(M-S "" "s1" "10以内加法"   (list L10 + L10) nil test-count)
(M-S "" "s1" "20以内加法"  (list L20 + L20) nil test-count)
(M-S "" "s1" "50以内加法"  (list L50 + L50) nil test-count)
(M-S "" "s1" "100以内加法"    (list L100 + L100) nil test-count)

(M-S "" "s1" "10以内减法"   (list L10 - L10) rule-positive test-count)
(M-S "" "s1" "20以内减法"  (list L20 - L20) rule-positive test-count)
(M-S "" "s1" "50以内减法"  (list L50 - L50) rule-positive test-count)
(M-S "" "s1" "100以内减法"    (list L100 - L100) rule-positive test-count)


(M-S "" "s1" "20内借位减法" (list L10_20 - L10) rule-jie-wei test-count)
(M-S "" "s1" "50内借位减法" (list L50 - L50) rule-jie-wei test-count)

(M-S "" "s2" "10以内加法(三项)"  (list L10 + L10  +  L10) nil test-count)
(M-S "" "s2" "20以内加法(三项)" (list L20  + L20  + L20) nil test-count)
(M-S "" "s2" "50以内加法(三项)" (list L20  + L20  + L20) nil test-count)
(M-S "" "s2" "100以内加法(三项)"   (list L100 + L100 + L100) nil test-count)
(M-S "" "s2" "20以内加减法(三项)" (list L20 (list + -) L20 (list + -)  L20) rule-positive3 test-count)
(M-S "" "s2" "50以内加减法(三项)" (list L50 (list + -) L50 (list + -)  L50) rule-positive3 test-count)
(M-S "" "s2" "100以内加减法(三项)" (list L100 (list + -) L100 (list + -)  L100) rule-positive3 test-count)

(M-S "" "s1" "2x10以内乘法"   (list (list 2) * L10)  nil test-count)
(M-S "" "s1" "3x10以内乘法"   (list (list 3) * L10)  nil test-count)
(M-S "" "s1" "4x10以内乘法"   (list (list 4) * L10)  nil test-count)
(M-S "" "s1" "5x10以内乘法"   (list (list 5) * L10)  nil test-count)
(M-S "" "s1" "6x10以内乘法"   (list (list 6) * L10)  nil test-count)
(M-S "" "s1" "7x10以内乘法"   (list (list 7) * L10)  nil test-count)
(M-S "" "s1" "8x10以内乘法"   (list (list 8) * L10)  nil test-count)
(M-S "" "s1" "9x10以内乘法"   (list (list 9) * L10)  nil test-count)
(M-S "" "s1" "5以内乘法"   (list (sequence 1 5) * (sequence 1 5))  nil test-count)
(M-S "" "s1" "10以内乘法"   (list L10 * L10)  nil test-count)
(M-S "" "s1" "20以内乘法"  (list L20 * L20)  nil test-count)

(M-S "" "s3" "10以内加法(三项括号)"  (list L10 + L10  +  L10) nil test-count)
(M-S "" "s3" "20以内加法(三项括号)" (list L20  + L20  + L20) nil test-count)
(M-S "" "s3" "50以内加法(三项括号)" (list L20  + L20  + L20) nil test-count)
(M-S "" "s3" "100以内加法(三项括号)"   (list L100 + L100 + L100) nil test-count)
(M-S "" "s3" "20以内加减法(三项括号)" (list L20 (list + -) L20 (list + -)  L20) rule-positive-s3 test-count)
(M-S "" "s3" "50以内加减法(三项括号)" (list L50 (list + -) L50 (list + -)  L50) rule-positive-s3 test-count)
(M-S "" "s3" "100以内加减法(三项括号)" (list L100 (list + -) L100 (list + -)  L100) rule-positive-s3 test-count)


                                        ; main 

(define (unit-tests , plist) 
  (dolist  (x (symbols))
    (if (find (append "^" UNIT_TEST_PREX) (string x) 0)
        (push x plist -1)))
  (sort plist))

(define (print-menu , idx)
  (letn (
         (n 3)
         (a (map (fn (x) (format "%2d. %s" (+ 1 $idx) (eval (sym (append UNIT_TEST_DOC (string x)))))) (unit-tests)))
         (a (append a (flat (dup '("")  (% (length a) n)))))
         (a (transpose (array n (/ (length a) n) a))))
        (dolist (i a)
          (dolist (j i)
            (print (format "%-28s" j))
            )
          (println)
          )))

(define (main , idx)
  (while true
    (begin
     (if test-number
         (define idx (int test-number))
       (begin
        (println LINE_DIVISION)
        (print-menu)
        (print "choice:")
        (define r (read-line))
        (if (= r "q") (exit))
        (define idx (int r))))
     (set 'tst ((unit-tests) (- idx 1) 0))
     (set 'tst1 (append "(" (string tst) ")"))
     (println "")
     (eval-string tst1)
     (println ""))))



;;; cgi
;;;
(constant 'REGEX_HTTP_SPECIAL_STR (regex-comp {([^.0-9a-z]+)} 1))  
(constant 'REGEX_HEX_ENCODED_CHAR (regex-comp {%([0-9A-F][0-9A-F])} 1))  

(define (hex-encode-str str , cnvrt)  
  (setf cnvrt (dup "%%%X" (length str)))  
  (eval (append '(format cnvrt) (unpack (dup "b" (length str)) str)))  
  )  

(define (utf8-urlencode str everything)  
  (if everything  
      (hex-encode-str str)  
    (replace REGEX_HTTP_SPECIAL_STR str (hex-encode-str $1) 0x10000)  
    )  
  )



(define (html-summary-nav)  
  (print [text]
         <div>
         <a href="?fun=unit-summary">unit-summary</a> 
         <a href="?fun=unit-list">unit-list</a> 
         <a href="?fun=question-summary">question-summary</a>
         <a href="?fun=question-list">question-list</a>
         </div>
         [/text]))

(new Tree 'MyColor)
(define (html-get-process head size color_key rate)
  (letn ( (r 150)
          (color (apply + (map (fn (x) (* (+ (- 256 r) x) (pow 256 $idx))) (rand r 3))))
          (color (if (MyColor color_key)
                     (MyColor color_key)
                   (MyColor color_key color)))
          (title head)
          (width-max (min (if rate (* size rate) (* size 1)) HTML-GET-PROCESS-WITH-MAX))
          (width-min (max width-max 20))
          (height (if html-get-process-height  html-get-process-height 20))
          )
        (html:html-svg  (list (list "width" width-min) (list "height" height) )
                        (html:html-rect (list (list "width" width-max) (list "height" 20) (list "style" (format "fill:#%02x" color))))
                        (html:html-text head '(("x" 0) ("y" 20)))
                        )))


(define (html-unit-list , filter_title s1)
  (begin
   (set 's1 "select * from unit_test ")
   (if (CGI:get "filter_title")
       (extend s1 (format { where title='%s' } (CGI:get "filter_title"))))
   (extend s1 " order by ")
   (if (CGI:get "order")
       (extend s1 " title,"))
   (extend s1 "id desc")
   (println {<h2>Unit List</h2>})
   (println {<table border=1>})
   (println (html:html-tr 
             (html:html-td (list "sn" "sid" "created" "type" 
                                 {<a href="?fun=unit-list&order=title">title</a>}
                                 "total" "right" "wrong" "score" "time"))))
   (if (sql3:open DB-NAME)
       (dolist (x (sql3:sql s1))
         (local (sn sid created type title total right wrong score _time)
                (bind (transpose (list '(sn sid created type title total right wrong score _time) x)))
                (letn ((created1 (date created 0 "%Y-%m-%d/%H:%M:%S"))
                       (title1 (format {<a href="?fun=unit-list&filter_title=%s">%s</a>} (utf8-urlencode title) (string title)))
                       (time1 (format "%.1f" (div _time total)))
                       (time1 (html-get-process time1 (div _time total)  title HTML-GET-PROCESS-TIME-RATE))
                       (sid1 (format {<a href="?fun=question-list&filter_sid=%s">%s</a>} (string sid) (slice sid 0 4)))
                       (score1 (format "%03d" (int score)))
                       (score1 (html-get-process score1 score title))
                       (x1 (list sn sid1 created1 type title1 total right wrong score1 time1)))
                      (println (html:html-tr
                                (html:html-td x1)))))))
   (println {</table>})
   (sql3:close)))


(define (avg a n)
  (if (not n)
      (letn ((l (length a)))
            (if l
                (div (apply + a) l)
              0))
    (cond
     ((> n 0) (avg (slice a 0 n)))
     ((< n 0) (avg (slice a (max n (- 0 (length a))))))
     (true (avg a)))))

(define (avg-last3 a)
  (avg (slice a (- 0 (min 3 (length a))))))

(define (last-n a n , t)
  (if (catch (slice a (- 0 n)) 't) t a))

(define (html-unit-summary)
  (begin
   (println {<h2>Unit Summary</h2>})
   (println {<table border=1>})
   (if (sql3:open DB-NAME)
       (begin
        (println (html:html-tr
                  (html:html-td (list "unit" "times" "score(,3)" "time(,3)" "last test time" "score list" "time list" "unit"))))
        (letn ((rs (sql3:sql "select * from unit_test"))
               (us (sort (unique (map (fn (x) (x U_title)) rs)))))
              (dolist (u us)
                (letn ((item (filter (fn (x) (= (x U_title) u)) rs))
                       (u1 (format {<a href="?fun=unit-list&filter_title=%s">%s</a>} (utf8-urlencode u) (string u)))
                       )
                      (local (sn sid created type title total right wrong score _time)
                             (bind (transpose (list '(sn sid created type title total right wrong score _time) (transpose item))))
                             (println 
                              (html:html-tr 
                               (html:html-td 
                                (list 
                                 u1 
                                 (length sn) 
                                 (let ((s1 (avg score))
                                       (s2 (avg score -3)))
                                   (append
                                    (html-get-process (int s1) s1 u)
                                    "<br/>"
                                    (html-get-process (int s2) s2 u)
                                    ))
                                 
                                 (let (t (map div _time total))
                                   (let ((s1 (avg t))
                                         (s2 (avg t -3)))
                                     (append
                                      (html-get-process (int s1) s1 u HTML-GET-PROCESS-TIME-RATE)
                                      "<br/>"
                                      (html-get-process (int s2) s2 u HTML-GET-PROCESS-TIME-RATE)
                                      )))                                               
                                 
                                 (date (last created) 0 "%Y-%m-%d/%H:%M:%S")
                                 
                                 (let (html-get-process-height MAX-GRAPH-SVG-HIGHT)
                                   (join (map (fn (x) (html-get-process "" x u) ) (last-n score MAX-GRAPH-SVG-COUNT)) "<br/>"))

                                 (let ((t (map div _time total))
                                       (html-get-process-height MAX-GRAPH-SVG-HIGHT))
                                   (join (map (fn (x) (html-get-process "" x u 3) ) (last-n t MAX-GRAPH-SVG-COUNT) ) "<br/>"))

                                 u1
                                 ))))
                             )))
              )))
   (println {</table>})
   (sql3:close)))


(define (html-question-list , condition)
  (begin
   (if (CGI:get "filter_question")
       (push (format {question="%s"} (CGI:get "filter_question")) condition))
   (if (CGI:get "filter_title")
       (push (format {title="%s"} (CGI:get "filter_title")) condition))
   (if (CGI:get "filter_sid")
       (push (format {sid="%s"} (CGI:get "filter_sid")) condition))
   (println (format {<h2>Question List</h2>}))
   (println {<table border=1>})
   (println (html:html-tr 
             (html:html-td (list "sn" "sid" "created" "type"
                                 {<a href="?fun=question-list&order=title">title</a>}
                                 {<a href="?fun=question-list&order=question">question</a>}
                                 "answer" "result" "score" "time"))))
   (if (sql3:open DB-NAME)
       (letn (
              (order (CGI:get "order"))
              (total (if (CGI:get "total") (CGI:get "total") MAX-LIST-LOG-COUNT))
              (s1 (append "select * from unit_test_log "
                          (if condition (append " where " (join condition " and ")) "")
                          (cond
                           ((= order "question") " order by question,id desc")
                           ((= order "title") " order by title,question,id desc")
                           (true " order by id desc "))
                          " limit "
                          (string total))))
             (dolist (x (sql3:sql s1))
               (local (sn sid created type title question answer result score _time)
                      (bind (transpose (list '(sn sid created type title question answer result score _time) x)))
                      (letn ((created1 (date created 0 "%Y-%m-%d/%H:%M:%S" ))
                             (title1 (format {<a href="?fun=question-list&filter_title=%s">%s</a>} (utf8-urlencode title) (string title)))
                             (time1 (format "%.1f" (div _time 1000)))
                             (time1 (html-get-process time1 (float time1) question HTML-GET-PROCESS-TIME-RATE))
                             (sid1 (format {<a href="?fun=question-list&filter_sid=%s">%s</a>} (string sid) (slice sid 0 4)))
                             (question1 (format {<a href="?fun=question-list&filter_question=%s">%s</a>} (utf8-urlencode question) (string question)))
                             (x1 (list sn sid1 created1 type title1 question1 answer result score time1)))
                            (println (html:html-tr
                                      (html:html-td x1))))))))
   (println {</table>})
   (sql3:close)))


(define (html-question-summary)
  (begin
   (println {<h2>Question Summary</h2>})
   (if (sql3:open DB-NAME)
       (begin
        (dolist (t '((0 "error count") (1 "right count")))
          (begin
           (println {<table border=1 style="float:left">})
           (println (html:html-tr
                     (html:html-td (list "question" (t 1)))))
           (dolist (x (sql3:sql (format "select question,count(question) as question_err_count from unit_test_log where score=%d group by question order by question_err_count desc;" (t 0))))
             (letn ((question (x 0))
                    (question1 (format {<a href="?fun=question-list&filter_question=%s">%s</a>} 
                                       (utf8-urlencode question)
                                       (string question)))
                    (_count (x 1))
                    (x1 (list question1 _count)))
                   (println (html:html-tr
                             (html:html-td x1)))))
           (println {</table>})))))
   (sql3:close)))


(define (cgi-main)
  (begin
   (module "cgi.lsp")
   (set 'fun (CGI:get "fun"))
   (print "Content-Type: text/html\r\n\r\n")
   (println {<html><head><meta http-equiv="Content-type" content="text/html; charset=utf8" /></head><body>})
   (html-summary-nav)
   (cond
    ((= fun "question-list") (html-question-list))
    ((= fun "question-summary") (html-question-summary))
    ((= fun "unit-list") (html-unit-list))
    ((= fun "unit-summary") (html-unit-summary))                
    (true (html-unit-summary))
    )
   ))

;; main process

(seed (time-of-day))
(setq is-cgi (env "QUERY_STRING"))

(init-parse-opt)
(init-db)


(if is-cgi 
  (cgi-main)
  (main)
  )
(exit)
