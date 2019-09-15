;; Since I couldn't seem to get any asdf packages working correctly
(ql:quickload :prove)

(defpackage :test-common-utils
  (:use :common-lisp)
  (:use :common-utils)
  (:use :prove))

(in-package :test-common-utils)

;; For my sanity since I can't get emacs slime to display colors correctly
(setf *enable-colors* nil)

(subtest "promises"
  (let ((p (delay (format t "hello"))))
    (is (realized? p) nil)
    (prove:is-print (force p) "hello")
    (is (realized? p) t)
    (prove:is-print (force p) "")
    (is (realized? p) t)))

(subtest "filter"
  (is (filter #'evenp '(0 1 2 3 4)) '(0 2 4)))

(subtest "hash-table related"
  (let ((grouped (common-utils::group-by #'evenp '(0 1 2 3 4))))
    (subtest "group-by (private)"
      (let ((expected (make-hash-table)))
        (setf (gethash t expected) '(4 2 0))
        (setf (gethash nil expected) '(3 1))
        (ok (equalp grouped expected))))
    (subtest "hash-table->entries"
      (is (common-utils::hash-table->entries grouped)
          '((nil (3 1)) (t (4 2 0)))))
    (subtest "copy-hash-table"
      (is grouped grouped)
      (let ((cp (common-utils::copy-hash-table grouped)))
        (isnt grouped cp)
        (ok (equalp grouped cp))))))

(subtest "strings"
  (is (str :a 'test "thing" (list '(x) :x) 99)
      ":ATESTthing((X) :X)99")
  ;; replace-all is stale
  (is (symb "thing") 'THING)
  (subtest "not sure if this is the expected behavior"
    (is (symb " wha t is this") 'wha)))

(subtest "gensyms"
  (eval (with-gensyms (a b)
          `(let* ((,a 1)
                  (a 2)
                  (,b 3))
             (is ,a 1)
             (is a 2)
             (is ,b 3)))))

(subtest "flatten"
  (subtest "Weird ambiguity with the empty list also being nil. I expected () to be removed..."
    (is (flatten '(1 (2 (3 4) 5 6 ) 7 () (((()))) ((((8))))))
        '(1 2 3 4 5 6 7 nil nil 8))))

(subtest "map-tree"
  (is (map-tree #'1+ '(1 (2)))
      '(2 (3))))


