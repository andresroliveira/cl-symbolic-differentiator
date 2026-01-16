;;; diferenciador.lisp

(defun deriv (expr var)
  (cond
    ;; Regra 1: Constante vira 0
    ((numberp expr) 0)

    ;; Regra 2: Variável vira 1 (se for a mesma) ou 0 (se for outra)
    ((symbolp expr)
     (if (eq expr var) 1 0))

    ;; Regra 3: Soma (+ u v) -> (+ du dv)
    ((eq (first expr) '+)
     (list '+
           (deriv (second expr) var)
           (deriv (third expr) var)))

    ;; Regra 4: Subtração (- u v) -> (- du dv)
    ((eq (first expr) '-)
     (list '-
           (deriv (second expr) var)
           (deriv (third expr) var)))

    ;; Regra 5: Produto (* u v) -> (+ (* u dv) (* v du))
    ((eq (first expr) '*)
     (let ((u (second expr))
           (v (third expr)))
       (list '+
             (list '* u (deriv v var))
             (list '* v (deriv u var)))))

    ;; Regra 6: Divisão (/ u v) -> (/ (- (* v du) (* u dv)) (* v v))
    ((eq (first expr) '/)
     (let ((u (second expr))
           (v (third expr)))
       (list '/
             (list '-
                   (list '* v (deriv u var))
                   (list '* u (deriv v var)))
             (list '* v v))))

    ;; Regra 7: Potência (expt u n) -> n * u^(n-1) * du
    ((eq (first expr) 'expt)
     (let ((u (second expr))
           (n (third expr)))
       (list '*
             (list '* n (list 'expt u (list '- n 1)))
             (deriv u var))))

    (t (error "Não sei derivar isso: ~a" expr))))

(defun simplificar (expr)
  (cond
    ;; Se for átomo (número ou símbolo), retorna ele mesmo
    ((atom expr) expr)

    (t
     (let ((op (first expr))
           ;; Simplifica os filhos primeiro (recursão)
           (u (simplificar (second expr)))
           (v (simplificar (third expr))))
       (cond
         ;; --- SOMA (+) ---
         ((eq op '+)
          (cond
            ((and (numberp u) (numberp v)) (+ u v))
            ((eq u 0) v)
            ((eq v 0) u)
            (t (list '+ u v))))

         ;; --- PRODUTO (*) ---
         ((eq op '*)
          (cond
            ((and (numberp u) (numberp v)) (* u v))
            ((or (eq u 0) (eq v 0)) 0)
            ((eq u 1) v)
            ((eq v 1) u)
            (t (list '* u v))))

         ;; --- SUBTRAÇÃO (-) [NOVO!] ---
         ((eq op '-)
          (cond
            ((and (numberp u) (numberp v)) (- u v)) ;; Resolve números: 3 - 2 = 1
            ((eq v 0) u)                            ;; u - 0 = u
            ((equal u v) 0)                         ;; u - u = 0
            (t (list '- u v))))

         ;; --- DIVISÃO (/) ---
         ((eq op '/)
          (cond
            ;; O Lisp é esperto: (/ 3 9) vira automaticamente 1/3 (rational)
            ((and (numberp u) (numberp v)) (/ u v))
            ((eq u 0) 0)                            ;; 0 / v = 0
            ((eq v 1) u)                            ;; u / 1 = u
            ((equal u v) 1)                         ;; u / u = 1
            (t (list '/ u v))))

         ;; --- POTÊNCIA (expt) ---
         ((eq op 'expt)
          (cond
            ((eq v 0) 1)               ;; Algo elevado a 0 é 1
            ((eq v 1) u)               ;; Algo elevado a 1 é ele mesmo
            ((and (numberp u) (numberp v)) (expt u v))
            (t (list 'expt u v))))

         ;; Padrão
         (t (list op u v)))))))
