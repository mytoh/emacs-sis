;;; sis --- sis -*- lexical-binding: t; coding: utf-8; -*-

;;; Commentary:

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'pcase)

(cl-defun sis:pair (x y)
  ;; couple, double, twin, dual
  (cons x y))

(cl-defun sis:fst (p)
  (pcase p
    ((seq x _)
     x)))

(cl-defun sis:snd (p)
  (pcase p
    ((seq _ &rest y)
     y)))

(cl-defun sis:curry (f &rest args)
  (pcase args
    (`()
      (lambda (x y)
        (funcall f (sis:pair x y))))
    ((seq x y)
     (funcall f (sis:pair x y)))))

(cl-defun sis:uncurry (f &optional p)
  (pcase p
    (`()
      (pcase-lambda ((and (app sis:fst x)
                (app sis:fst y)))
          (funcall f x y)))
    (_
     (funcall f (sis:fst p)
        (sis:snd p)))))

(cl-defun sis:swap (p)
  (sis:pair
   (sis:snd p)
   (sis:fst p)))

(cl-defun sis:pair-p (x)
  (pcase x
    ((and (pred seqp)
          (pred consp)
          `(,_ . ,(pred (lambda (x) (not (listp x))))))
     t)))

(cl-defun sis:first (f p)
  (sis:pair (funcall f (sis:fst p))
          (sis:snd p)))

(cl-defun sis:second(f p)
  (sis:pair (sis:fst p)
          (funcall f (sis:snd p))))

(cl-defun sis:bimap(f p)
  ;; both
  (sis:pair (funcal f (sis:fst p))
          (funcal f (sis:snd p))))
(defalias 'sis:both 'sis:bimap)

(cl-defun sis:fork (f g x)
  (sis:pair (funcall f x)
          (funcall g x)))
(defalias 'sis:&&& 'sis:fork)

(cl-defun sis:product (f g p)
  (sis:pair (funcall f (sis:fst p))
          (funcall g (sis:snd p))))
(defalias 'sis:*** 'sis:product)


;; //github.com/ekmett/bifunctiors/old-src/Data
;; lens, Control-Lens-Tuple

(provide 'sis)

;;; sis.el ends here

