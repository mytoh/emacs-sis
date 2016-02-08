;;; sis --- sis -*- lexical-binding: t; coding: utf-8; -*-

;;; Commentary:

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'pcase)

(cl-defun sis:pair (x y)
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

(provide 'sis)

;;; sis.el ends here

