; ###################################################################################################################################
; ## Apresentação do Grupo de Trabalho ##############################################################################################
; ###################################################################################################################################

; GRUPO DE TRABALHO Nº4
; Catarina Morais Gama, nº63873, Turma 6
; Valter Miguel Gaspar Nepomuceno, nº62599, Turma 6
; Vasco Alexandre da Silva Ramos, nº62587, Turma 6

; Licenciatura em Engenharia Informática e de Computadores, Alameda
; Instituto Superior Técnico, 19 de Novembro de 2007

; Fundamentos de Programação, Projecto Final
; Docentes: João Pavão Martins e Pedro Amaro de Matos
; Entrega até às 16h00 do dia 10 de Dezembro de 2007




; Compilação da Interface fornecida pela equipa da cadeira
(require "interface.zo")

; ###################################################################################################################################
; ## Operações Básicas de Matrizes ##################################################################################################
; ###################################################################################################################################

; Definição do procedimento (matriz d i) que recebe como argumentos o inteiro positivo -d- que define a dimensão da matriz e o
; inteiro -i- que determina o conteúdo dos elementos da matriz

(define (matriz d i)
  (let 
      ((coluna (make-vector d)))
    (define (matriz-aux d i pos)
      (define (faz-linha d i)
        (let 
            ((m (make-vector d)))
          (define (faz-linha-aux d pos i)
            (if (= pos d) 
                m
                (begin 
                  (vector-set! m pos i)
                  (faz-linha-aux d (add1 pos) i))))
          (faz-linha-aux d 0 i)))
      (if (= pos d) 
          coluna
          (begin 
            (vector-set! coluna pos (faz-linha d i))
            (matriz-aux d i (add1 pos)))))
    (matriz-aux d i 0)))

; Definição do procedimento (el-matriz m l c) que recebe como argumentos e seleciona a matriz -m-, a linha -l- e a coluna -c-

(define (el-matriz m l c)
  (vector-ref (vector-ref m c) l))

; Definição do procedimento (dimensao-matriz m) que recebe como argumentos a matriz m e devolve a sua dimensão

(define (dimensao-matriz m)
  (vector-length m))

; Definção do procedimento (coloca-matriz! m l c obj) que coloca -obj- na matriz -m-, linha -l- e coluna -c-

(define (coloca-matriz! m l c obj)
  (vector-set! (vector-ref m c) l obj))

; Definição do procedimento (matriz? arg) que verifica se o argumento -arg- se trata ou não de uma matriz

(define (matriz? arg)
  (define (verifica-colunas arg)
    (define (verifica-resto arg inicio fim num-linhas)
      (define (verifica-um c num-linhas)
        (and (vector? c) (= (vector-length c) num-linhas)))
      (if (< inicio fim)
          (if (verifica-um (vector-ref arg inicio) num-linhas)
              (verifica-resto arg (add1 inicio) fim num-linhas)
              #f)
          (verifica-um (vector-ref arg inicio) num-linhas)))
    (let* ((num-cols (vector-length arg))
           (primeira-coluna (vector-ref arg 0))
           (verifica-primeiro (vector? primeira-coluna)))
      (if verifica-primeiro
          (let ((num-linhas (vector-length primeira-coluna)))
            (verifica-resto arg 1 (sub1 num-cols) num-linhas))
          #f)))
  (if (vector? arg)
      (verifica-colunas arg)
      #f))

; Definição do procedimento (els-matriz-tipo? mat pred) que devolve o valor lógico #t caso todos os elementos de -mat- satisfaçam
; o predicado -pred-, ou devolvendo #f em caso contrário

(define (els-matriz-tipo? mat pred)
  (define (els-matriz-tipo?-aux linha ind1)
    (define (verifica-vector-pred? vector ind2)
      (if (= ind2 (vector-length vector)) 
          #t
          (if (pred (vector-ref vector ind2))
              (vector-ref vector (+ 1 ind2))
              #f)))
    (if (= ind1 (vector-length linha)) 
        #t
        (if
         (verifica-vector-pred? (vector-ref linha ind1) 0)
         (els-matriz-tipo?-aux linha (add1 ind1))
         #f)))
  (if (not (matriz? mat))
      (error "els-matriz-tipo?: Recebe uma matriz como primeiro argumento")
      (els-matriz-tipo?-aux mat 0)))

; ###################################################################################################################################
; ## Operações Básicas de Listas ####################################################################################################
; ###################################################################################################################################

; Definição do procedimento (insere el lst) que dada uma lista -lst-, insere nessa mesma lista o elemento -el

(define (insere el lst)
  (cons el lst))

; Definição do procedimento (comprimento lst) que dada uma lista -lst-, devolve o comprimento dessa mesma lista

(define (comprimento lst)
  (if (null? lst)
      0
      (+ 1 (comprimento (resto lst)))))

; Definição do procedimento (resto lst) que dada uma lista -lst-, devolve toda essa lista com excepção do primeiro elemento

(define (resto lst)
  (if (null? lst)
      (error "resto: a lista não tem elementos")
      (car lst)))

; Definição do procedimento (filter pred lis) que dada uma lista -lis-, verifica quais os elementos dessa lista que verificam o
; predicado -pred-, e cria uma nova lista com os elementos que o verificam
; Procedimento retirado de uma página web

(define (filter pred lis)
  (let recur ((lis lis))
    (if (null? lis) lis
        (let ((head (car lis))
              (tail (cdr lis)))
          (if (pred head)
              (let ((new-tail (recur tail)))
                (if (eq? tail new-tail) lis
                    (cons head new-tail)))
              (recur tail))))))

; Definição do procedimento (lset-intersection l1 . l) que dadas duas listas -l1- e -l-, verifica quais os elementos que se
; encontram nas duas listas e cria uma nova lista com esses elementos
; Procedimento retirado de uma página web

(define (lset-intersection l1 . l)
  (let loop ((l l) (r l1))
    (cond ((null? l)		r)
          ((null? (car l))	'())
          (else (loop (cdr l)
                      (filter (lambda (x) (member x (car l))) r))))))

; ###################################################################################################################################
; ## Procedimentos auxiliares utilizados por Procedimentos de Abstracção Superior ###################################################
; ###################################################################################################################################

; Definição do procedimento (digito-valido? d) que devolve #t se e só se -d- for um valor válido para a dimensão do problema

(define (digito-valido? d dim)
  (cond
    ((= 4 dim) (or (string=? d "1") (string=? d "2") (string=? d "3") (string=? d "4")))
    ((= 9 dim) (or (string=? d "1") (string=? d "2") (string=? d "3") (string=? d "4")
                   (string=? d "5") (string=? d "6") (string=? d "7") (string=? d "8")
                   (string=? d "9")))
    ((= 16 dim) (or (string=? d "1") (string=? d "2") (string=? d "3") (string=? d "4")
                    (string=? d "5") (string=? d "6") (string=? d "7") (string=? d "8")
                    (string=? d "9") (string=? d "A") (string=? d "B") (string=? d "C")
                    (string=? d "D") (string=? d "E") (string=? d "F") (string=? d "G")))
    (else (error "digito-valido?: A dimensão do problema é diferente de 4, 9 ou 16"))))

; Definição do procedimento (digito-possivel? d l c) que devolve #t caso a cadeia de caracteres de comprimento 1 -d- seja uma
; escolha válida para a posição da linha -l- e da coluna -c-

(define (digito-possivel? mat digito lin col)
  (define (percorre-coluna mat lin col)
    (define (percorre-coluna-aux mat lin col)
      (if (equal? col (dimensao-matriz mat))
          #t
          (if (equal? digito (el-matriz mat lin col))
              #f
              (percorre-coluna-aux mat lin (add1 col)))))
    (percorre-coluna-aux mat lin 0))
  (define (percorre-linha mat lin col)
    (define (percorre-linha-aux mat lin col)
      (if (= lin (dimensao-matriz mat))
          #t
          (if (equal? digito (el-matriz mat lin col))
              #f
              (percorre-linha-aux mat (add1 lin) col))))
    (percorre-linha-aux mat 0 col))
  (define (percorre-bloco mat lin col digito)
    (define (nº-bloco mat linha col)
      (cons (quotient linha (sqrt (dimensao-matriz mat))) (quotient col (sqrt (dimensao-matriz mat)))))
    (define (verifica-bloco-aux? mat lin col digito) 
      (define (verifica-bloco-digito? mat sup1 sup2 digito)
        (define (verifica-aux sup1 sup2 cont1 cont2)
          (if (= cont1 (sqrt (dimensao-matriz mat)))
              #t
              (if (= cont2 (sqrt (dimensao-matriz mat)))
                  (verifica-aux (sub1 sup1) (+ sup2 (sqrt (dimensao-matriz mat))) (add1 cont1) 0)
                  (begin (equal? digito (el-matriz mat sup1 sup2))
                         (verifica-aux sup1 (sub1 sup2) cont1 (add1 cont2))))))
        (verifica-aux sup1 sup2 0 0))
       (define (verifica-bloco16X16 n)
      (cond ((equal? n (cons 0 0))
             (verifica-bloco-digito? mat 3 3 digito))
            ((equal? n (cons 0 1))
             (verifica-bloco-digito? mat 3 7 digito))
            ((equal? n (cons 0 2))
             (verifica-bloco-digito? mat 3 11 digito))
            ((equal? n (cons 0 3))
             (verifica-bloco-digito? mat 3 15 digito))
            ((equal? n (cons 1 0))
             (verifica-bloco-digito? mat 7 3 digito))
            ((equal? n (cons 1 1))
             (verifica-bloco-digito? mat 7 7 digito))
            ((equal? n (cons 1 2))
             (verifica-bloco-digito? mat 7 12 digito))
            ((equal? n (cons 1 3))
             (verifica-bloco-digito? mat 7 15 digito))
            ((equal? n (cons 2 0))
             (verifica-bloco-digito? mat 11 3 digito))
            ((equal? n (cons 2 1))
             (verifica-bloco-digito? mat 11 7 digito))
            ((equal? n (cons 2 2))
             (verifica-bloco-digito? mat 11 11 digito))
            ((equal? n (cons 2 3))
             (verifica-bloco-digito? mat 11 15 digito))
            ((equal? n (cons 3 0))
             (verifica-bloco-digito? mat 15 3 digito))
            ((equal? n (cons 3 1))
             (verifica-bloco-digito? mat 15 7 digito))
            ((equal? n (cons 3 2)) 
             (verifica-bloco-digito? mat 15 11 digito))
            (else 
             (verifica-bloco-digito? mat 15 15 digito))))
    (define (verifica-bloco9X9 n)
      (cond
        ((equal? n (cons 0 0))
         (verifica-bloco-digito? mat 2 2 digito))
        ((equal? n (cons 0 1))
         (verifica-bloco-digito? mat 2 5 digito))
        ((equal? n (cons 0 2))
         (verifica-bloco-digito? mat 2 8 digito))
        ((equal? n (cons 1 0))
         (verifica-bloco-digito? mat 5 2 digito))
        ((equal? n (cons 1 1))
         (verifica-bloco-digito? mat 5 5 digito))
        ((equal? n (cons 1 2))
         (verifica-bloco-digito? mat 5 8 digito))
        ((equal? n (cons 2 0))
         (verifica-bloco-digito? mat 8 2 digito))
        ((equal? n (cons 2 1))
         (verifica-bloco-digito? mat 8 5 digito))
        (else 
         (verifica-bloco-digito? 8 8 digito))))
    (define (verifica-bloco4X4 n)
      (cond
        ((equal? n (cons 0 0))
         (verifica-bloco-digito? mat 1 1 digito))
        ((equal? n (cons 0 1))
         (verifica-bloco-digito? mat 1 3 digito)) 
        ((equal? n (cons 1 0))
         (verifica-bloco-digito? mat 3 1 digito))
        (else
         (verifica-bloco-digito? mat 3 3 digito))))
    (cond ((= (dimensao-matriz mat) 16)
           (verifica-bloco16x16 (nº-bloco mat lin col)))
          ((= (dimensao-matriz mat) 9)
           (verifica-bloco9X9 (nº-bloco mat lin col)))
          (else
           (verifica-bloco4X4 (nº-bloco mat lin col)))))
    (verifica-bloco-aux? mat lin col digito))
    (and (percorre-linha  mat lin col) (percorre-coluna mat lin col) (percorre-bloco mat lin col digito)))

; Definição do procedimento (grelha-vazia? mat) que devolve #t caso a matriz de dimensão (dimensao-matriz mat), -mat- esteja vazia,
; ou seja, todos os seus elementos são cadeias de caracteres de comprimento 0

(define (grelha-vazia? matriz)
  (define (grelha-vazia-aux d l c)
    (if (and (= (sub1 d) l) (= (sub1 d) c) (= 0 (string-length (el-matriz matriz l c))))
        #t
        (if (= 0 (if (number? (el-matriz matriz l c))
                     (string-length (number->string (el-matriz matriz l c)))
                     (string-length (el-matriz matriz l c))))
            (if (and (= (sub1 d) c) (not (= d l)))
                (grelha-vazia-aux d (add1 l) 0)
                (grelha-vazia-aux d l (add1 c)))
            #f)))
  (grelha-vazia-aux (dimensao-matriz matriz) 0 0))

; Definição do procedimento (grelha-cheia? d) que devolve #t caso a matriz de dimensão -d- -matriz- esteja completamente preenchida,
; ou seja, todos os seus elementos são cadeias de caracteres de comprimento diferente de 0

(define (grelha-cheia? matriz)
  (define (grelha-cheia-aux d l c)
    (cond ((= -1 l) #t)
          ((equal? "" (el-matriz matriz l c)) #f)
          ((= -1 c)(grelha-cheia-aux d (sub1 l) (sub1 d)))
          (else 
           (grelha-cheia-aux d (sub1 l) (sub1 c)))))
  (grelha-cheia-aux (dimensao-matriz matriz) (sub1 (dimensao-matriz matriz)) (sub1 (dimensao-matriz matriz))))

; Definição do procedimento (recebe-digito d l c) que caso o utilizador introduza um digito válido, este será inserido na grelha

(define (recebe-digito d l c)
  (define (recebe-aux)
    (if (not (digito-valido? d (dimensao-matriz matriz))) (error "recebe-digito: O dígito que inseriu não é válido") 
        (if (not (digito-possivel? d l c)) (error "recebe-digito: o digito que inseriu não é possivel")
        (coloca-matriz! matriz l c d))))
  (recebe-aux))

; Definição do procedimento (sudoku-resolvido? matriz) que dada uma matriz -matriz-, devolve #t caso essa matriz esteja completamente
; preenchida, de maneira correcta, ou seja, se cada dígito for possível para a posição em que está inserido

(define (sudoku-resolvido? matriz)
  (define (sudoku-resolvido-aux matriz l c)
        (if (and (= l (dimensao-matriz matriz)) (= c (dimensao-matriz matriz)))
            #t
            (if (and (= c (dimensao-matriz matriz)) (not (= l (dimensao-matriz matriz))))
                (if (digito-possivel? (if (integer? (el-matriz matriz l c)) (integer->string (el-matriz matriz l c))
                                          (symbol->string (el-matriz matriz l c))) l c)
                    (sudoku-resolvido-aux matriz l (add1 c))
                    #f)
                (if (digito-possivel? matriz (el-matriz matriz l c) l c)
                    (sudoku-resolvido-aux matriz 0 (add1 c))
                    #f))))
  (sudoku-resolvido-aux matriz 0 0))

; Definição do procedimento (numeros-aleatorios dim) que dada a dimensão de uma matriz -dim- devolve um número ou uma letra válidos
; para a dimensão dessa matriz

(define (numeros-aleatorios dim)
  (if (= 16 dim)
      (list-ref '("1" "2" "3" "4" "5" "6" "7" "8" "9" "A" "B" "C" "D" "E" "F" "G") (random 16))
      (if (= 9 dim)
          (list-ref '("1" "2" "3" "4" "5" "6" "7" "8" "9") (random 9))
          (list-ref '("1" "2" "3" "4") (random 4)))))

; Definição do procedimento (faz-bloco mat sup1 sup2) que dada uma matriz -mat- e os limites -sup1- e -sup2-, devolve uma matriz
; que corresponde ao bloco dentro desses limites

(define (faz-bloco mat sup1 sup2) 
  (let  
      ((ref2 (- sup2 (sqrt (dimensao-matriz mat))))
       (bloco (matriz (sqrt (dimensao-matriz mat)) 1)))
    (define (faz-bloco-aux inf1 inf2)
      (if (= inf1 sup1)
          bloco
          (if (= inf2 sup2)
              (faz-bloco-aux (add1 inf1) ref2)
              (begin (coloca-matriz! bloco inf1 inf2 (el-matriz mat inf1 inf2))
                     (faz-bloco-aux inf1 (add1 inf2))))))
    (faz-bloco-aux (- sup1 (sqrt (dimensao-matriz mat))) (- sup2 (sqrt (dimensao-matriz mat))))))

; Definição do procedimento (nºs-possiveis mat lin col) que dada uma matriz -mat-, devolve essa mesma matriz, em que o valor de
; cada elemento da matriz é uma lista de todos os dígitos possíveis para essa posição

(define (nºs-possiveis mat lin col)
  (define (nºs-possiveis-aux mat lin col)
    (if (= lin (dimensao-matriz mat))
        mat
        (if (= col (dimensao-matriz mat))
            (nºs-possiveis-aux mat (add1 lin) 0)
            (begin 
              (coloca-matriz! mat lin col (lset-intersection (numeros-coluna mat lin col) (numeros-linha mat lin col)
                                                             (numeros-bloco mat lin col)))
              (nºs-possiveis-aux mat lin (add1 col))))))
  (nºs-possiveis-aux mat lin col))

; Definição do procedimento (numeros-linha mat linha coluna) que dada uma matriz -mat- devolve uma lista com todos os números
; possíveis para a posição do elemento na -linha- e na -coluna-, tendo em conta a linha dada

(define (numeros-linha mat linha coluna)
  (let ((nova-lista ()))
    (define (numeros-linha-aux mat linha coluna)
      (if (= (dimensao-matriz mat) linha)
          nova-lista
          (begin 
            (insere nova-lista (el-matriz mat linha coluna))
            (numeros-linha-aux mat (add1 linha) coluna))))
    (numeros-linha-aux mat 0 0)))

; Definição do procedimento (numeros-coluna mat linha coluna) que dada uma matriz -mat- devolve uma lista com todos os números
; possíveis para a posição do elemento na -linha- e na -coluna-, tendo em conta a coluna dada

(define (numeros-coluna mat linha coluna)
  (let ((nova-lista ()))
    (define (numeros-coluna-aux mat linha coluna)
      (if (= (dimensao-matriz mat) coluna)
          nova-lista
          (begin
            (insere nova-lista (el-matriz mat linha coluna))
            (numeros-coluna-aux mat linha (add1 coluna)))))
    (numeros-coluna-aux mat 0 0)))

; Definição do procedimento (numeros-bloco mat linha coluna) que dada uma matriz -mat- devolve uma lista com todos os números
; possíveis para a posição do elemento na -linha- e na -coluna-, tendo em conta o bloco onde está inserido

(define (numeros-bloco mat linha coluna)
  (let ((nova-lista ())
        (bloco (faz-bloco mat linha coluna)))
    (define (numeros-bloco-aux lin col)
      (if (= (dimensao-matriz bloco) lin)
          nova-lista
          (if (= (dimensao-matriz bloco) col)
              (numeros-bloco-aux (add1 lin) 0)
              (insere nova-lista (el-matriz bloco lin col)))))
    (numeros-bloco-aux 0 0)))

; Definição do procedimento (insere-nºs mat) que dada uma matriz -mat-, é inserido nessa mesma matriz em posições aleatórias
; digítos possíveis aleatoriamente

(define (insere-nºs mat)
  (define (insere-nºs-aux mat lin col)
    (let ((digito-a-inserir (numeros-aleatorios (dimensao-matriz mat))))
      (if (= lin (dimensao-matriz mat))
          mat
          (if (= col (dimensao-matriz mat))
              (insere-nºs-aux mat (add1 lin) 0)
              (if (digito-possivel? mat digito-a-inserir lin col)
                  (coloca-matriz! mat lin col digito-a-inserir)
                  (insere-nºs-aux mat lin (add1 col)))))))
  (insere-nºs-aux mat 0 0))

; Definição do procedimento (resolve mat) que dada uma certa matriz -mat- preenche todos os elementos dessa matriz, devolvendo a
; solução correcta desse problema de sudoku, caso essa grelha esteja completamente vazia, ou semi-preenchida

(define (resolve mat)
  (let ((nova-matriz (matriz 4 ()))) 
    (define (digito-possivel? digito lin col)
  (define (percorre-coluna mat lin col)
    (define (percorre-coluna-aux mat lin col)
      (if (equal? col (dimensao-matriz mat))
          #t
          (if (equal? digito (el-matriz mat lin col))
              #f
              (percorre-coluna-aux mat lin (add1 col)))))
    (percorre-coluna-aux mat lin 0))
  (define (percorre-linha mat lin col)
    (define (percorre-linha-aux mat lin col)
      (if (= lin (dimensao-matriz mat))
          #t
          (if (equal? digito (el-matriz mat lin col))
              #f
              (percorre-linha-aux mat (add1 lin) col))))
    (percorre-linha-aux mat 0 col))
  (define (percorre-bloco mat lin col digito)
    (define (nº-bloco mat linha col)
      (cons (quotient linha (sqrt (dimensao-matriz mat))) (quotient col (sqrt (dimensao-matriz mat)))))
    (define (verifica-bloco-aux? mat lin col digito) 
      (define (verifica-bloco-digito? mat sup1 sup2 digito)
        (define (verifica-aux sup1 sup2 cont1 cont2)
          (if (= cont1 (sqrt (dimensao-matriz mat)))
              #t
              (if (= cont2 (sqrt (dimensao-matriz mat)))
                  (verifica-aux (sub1 sup1) (+ sup2 (sqrt (dimensao-matriz mat))) (add1 cont1) 0)
                  (begin (equal? digito (el-matriz mat sup1 sup2))
                         (verifica-aux sup1 (sub1 sup2) cont1 (add1 cont2))))))
        (verifica-aux sup1 sup2 0 0))
       (define (verifica-bloco16X16 n)
      (cond ((equal? n (cons 0 0))
             (verifica-bloco-digito? mat 3 3 digito))
            ((equal? n (cons 0 1))
             (verifica-bloco-digito? mat 3 7 digito))
            ((equal? n (cons 0 2))
             (verifica-bloco-digito? mat 3 11 digito))
            ((equal? n (cons 0 3))
             (verifica-bloco-digito? mat 3 15 digito))
            ((equal? n (cons 1 0))
             (verifica-bloco-digito? mat 7 3 digito))
            ((equal? n (cons 1 1))
             (verifica-bloco-digito? mat 7 7 digito))
            ((equal? n (cons 1 2))
             (verifica-bloco-digito? mat 7 12 digito))
            ((equal? n (cons 1 3))
             (verifica-bloco-digito? mat 7 15 digito))
            ((equal? n (cons 2 0))
             (verifica-bloco-digito? mat 11 3 digito))
            ((equal? n (cons 2 1))
             (verifica-bloco-digito? mat 11 7 digito))
            ((equal? n (cons 2 2))
             (verifica-bloco-digito? mat 11 11 digito))
            ((equal? n (cons 2 3))
             (verifica-bloco-digito? mat 11 15 digito))
            ((equal? n (cons 3 0))
             (verifica-bloco-digito? mat 15 3 digito))
            ((equal? n (cons 3 1))
             (verifica-bloco-digito? mat 15 7 digito))
            ((equal? n (cons 3 2)) 
             (verifica-bloco-digito? mat 15 11 digito))
            (else 
             (verifica-bloco-digito? mat 15 15 digito))))
    (define (verifica-bloco9X9 n)
      (cond
        ((equal? n (cons 0 0))
         (verifica-bloco-digito? mat 2 2 digito))
        ((equal? n (cons 0 1))
         (verifica-bloco-digito? mat 2 5 digito))
        ((equal? n (cons 0 2))
         (verifica-bloco-digito? mat 2 8 digito))
        ((equal? n (cons 1 0))
         (verifica-bloco-digito? mat 5 2 digito))
        ((equal? n (cons 1 1))
         (verifica-bloco-digito? mat 5 5 digito))
        ((equal? n (cons 1 2))
         (verifica-bloco-digito? mat 5 8 digito))
        ((equal? n (cons 2 0))
         (verifica-bloco-digito? mat 8 2 digito))
        ((equal? n (cons 2 1))
         (verifica-bloco-digito? mat 8 5 digito))
        (else 
         (verifica-bloco-digito? 8 8 digito))))
    (define (verifica-bloco4X4 n)
      (cond
        ((equal? n (cons 0 0))
         (verifica-bloco-digito? mat 1 1 digito))
        ((equal? n (cons 0 1))
         (verifica-bloco-digito? mat 1 3 digito)) 
        ((equal? n (cons 1 0))
         (verifica-bloco-digito? mat 3 1 digito))
        (else
         (verifica-bloco-digito? mat 3 3 digito))))
    (cond ((= (dimensao-matriz mat) 16)
           (verifica-bloco16x16 (nº-bloco mat lin col)))
          ((= (dimensao-matriz mat) 9)
           (verifica-bloco9X9 (nº-bloco mat lin col)))
          (else
           (verifica-bloco4X4 (nº-bloco mat lin col)))))
    (verifica-bloco-aux? mat lin col digito))
    (and (percorre-linha  mat lin col) (percorre-coluna mat lin col) (percorre-bloco mat lin col digito)))
    
    
    (define (resolve-aux mat lin col)
      (cond ((sudoku-resolvido? mat) mat)
            ((= col (dimensao-matriz mat)) (resolve-aux mat (add1 lin) 0))
            ((= col (dimensao-matriz mat)) (resolve-aux mat 0 0))
            ((= (nºs-possiveis mat lin col) 1)
             (begin
               (coloca-matriz! mat lin col (nºs-possiveis mat lin col))
               (resolve-aux mat lin (add1 col))))
            (else (algum-movimento?))))
    
    
    (cond 
      ((grelha-vazia? mat)
       (begin (insere-nºs nova-matriz)
              (resolve-aux nova-matriz 0 0)))
      ((not (grelha-cheia? mat))
       (resolve-aux mat 0 0))
      (else ((sudoku-resolvido? mat) "Sudoku Resolvido" (write-impossible-message))))))

; ###################################################################################################################################
; ## Procedimentos da Janela de Escolha #############################################################################################
; ###################################################################################################################################

; Definição do procedimento (utilizador-resolve d) que recebe como argumento o inteiro -d-
; Este procedimento abre uma janela com uma grelha de dimensão -d-, com um problema de sudoku semi-preenchido, em que o utilizador
; tem de completar. Caso este introduza um número não possível para a posição, o programa não o aceita

(define (utilizador-resolve dim)
  (let ((grelha (matriz dim "")))
    (begin (insere-nºs grelha)
           (create-grid-window dim #f)
           (draw-grid grelha))))

; Definição do procedimento (resolve-puzzle d) que recebe como argumento o inteiro -d-
; Este procedimento abre uma janela com uma grelha de dimensão -d- vazia, cujos valores dos seus elementos serão inseridos pelo
; utilizador, apresentando a janela um botão "Resolver" que após inseridos os valores, o programa resolverá o problema

(define (resolve-puzzle d)
  (create-grid-window d #t)
  (draw-grid (matriz d "")))

; Definição do procedimento (puzzle-resolvido d) que recebe como argumento o inteiro -d-
; Este procedimento abre uma janela com uma grelha de dimensão -d-, com um problema de sudoku já completamente resolvido

(define (puzzle-resolvido dim)
  (resolve (matriz dim "")))
