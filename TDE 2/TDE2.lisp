;GRAFOS
;Pajek (Grafo.net ou Grafo.paj)
;*Vertices 5
;1 "Curitiba"
;2 "Ponta Grossa"
;3 "Cascavel"
;4 "Pinhais"
;5 "Paranagua"

;*Edges
;/vertice 1/ vertice 2/ valor aresta/
;2	3	50
;1	2	44
;1	3	27
;5	4	80

;Vertices->		((1 "Curitiba") (2 "Ponta Grossa")...)
;Arestas->		((2 3 50) (1 2 40)...)
;Ficaria: (setq g '( ((1 "Curitiba") (2 "Ponta Grossa")...)   ((2 3 50) (1 2 40)...))
;1o: usar funcao (read-from-string )
;1-Ler de arquivo e salvar em arquivo
;2-Centralidade de Grau
;3-Centralidade de Proximidade (closeness)
;4-Centralidade de Intermediacao (betwenness)
;5-Tabela de valores

;Melhor caminho: A*
;INICIO
	;algoritmo A* (origem, destino, Candidatos)
	; if(vazia(Candidatos))
	;	return nil
	; else
	;	X = melhor_caminho(Candidatos)
	;	if (solucao(X,destino))
	;		return X;
	;	else
	;		L = expande(X);   -> gerar novos candidatos: antes: (100 ...... 1) depois de colocar vertice vizinho 2:  (125 ..... 1 2)
	;		adicionar L em Candidatos
	;		remover X de Candidatos
	;		return A*(origem, destino, Candidatos)
	;FIM
;Como representar um caminho: (104	1	2	4) *usar em Candidatos*
;							  custo/ vertices...


;Exemplo: Origem->1 Destino->5 Grafo->g  (A_estrela 1 5 g (expande '(0 1) g))
(defun A_estrela (origem destino g Candidatos)
	(let (X L)
		(if (= origem destino)
			(list 0 origem)
			(if (null Candidatos)
				nil
				(progn
					(setq X (melhor_candidato Candidatos))
					(if (eq (car (last X)) destino)
						X
						(progn 
							(setq Candidatos (append Candidatos (expande X g)))
							(setq Candidatos (remove X Candidatos :test #'equal))
							(A_estrela origem destino g Candidatos)
						)
					)			
				)
			)
		)
	)
)

(defun melhor_candidato (lista)
	(let ((melhor (car lista)))
		(dolist (e lista melhor)
			(if (< (car e) (car melhor))
				(setq melhor e)
			)
		)
	)
)

(defun expande (caminho g)
	(let ((aux caminho) v caminhos)
		(setq v (car (last caminho)))
		(dolist (v1 (cadr g) caminhos)
			(cond
				((= v (car v1)) 
					(unless (member (cadr v1) caminho)
						(setq caminhos (append caminhos (list (append (list (+ (car caminho) (caddr v1))) (cdr caminho) (list (cadr v1))))))
					)
				)
				((= v (cadr v1)) 
					(unless (member (car v1) caminho)
						(setq caminhos (append caminhos (list (append (list (+ (car caminho) (caddr v1))) (cdr caminho) (list (car v1))))))
					)
				)
			)
		)
	)
)

(defun load_graph(file)
	(let ((line "") (s "(( ") (asterisco 0))
		(with-open-file (a file :direction :input)
			(loop
				(setq line (read-line a nil nil))
				(if (null line) (return s))
				(if (equal "*" (subseq line 0 1))
					(if (= asterisco 0)
						(incf asterisco)
						(setq s (format nil "~A) (" s))
					)
					(setq s (format nil "~A (~A) " s line))
				)

			)
			(setq s (format nil "~A))" s))
		)
		(read-from-string s)
	)
)

(defun save_graph (g file)
	(with-open-file (str file :direction :output :if-exists :supersede :if-does-not-exist :create)
		(format str "*Vertices ~A ~%" (length (car g)))
		(dolist (v (car g))
			(format str "~A \"~A\"~%" (car v) (cadr v))	
		)
		(format str "*Edges ~%")
		(dolist (e (cadr g))
			(format str "~A ~A ~A~%" (car e) (cadr e) (caddr e))	
		)
	)
	T
)

(defun adjacentes (v g)
	(let ((adj))
		(dolist (v1 (cadr g) adj)
			(cond
				((= v (car v1)) (setq adj (append adj (list (cadr v1)))))
				((= v (cadr v1)) (setq adj (append adj (list (car v1)))))
			)
		)
	)
)

(defun grau (v g)
	(/ (length (adjacentes v g)) (- (length (car g)) 1))
)

(defun todos_graus(g)
	(let ((graus "("))
		(dolist (v (car g) graus)
			(setq graus (format nil "~A (~A ~A) " graus (car v) (grau (car v) g )))
		)
		(setq graus (sort (read-from-string (format nil "~A) " graus)) #'> :key #'cadr));ordena
		(setq graus (read-from-string (format nil "(\"Grau\"~A) " graus)))
	)
)

(defun soma_distancias (o g)
	(let ((soma 0))
		(dolist (v1 (car g) soma)
			(print (A_estrela o (car v1) g (expande (list 0 o) g)))
			(if (not (eq (car v1) o))
				(setq soma (+ soma (- (length (A_estrela o (car v1) g (expande (list 0 o) g))) 2)))
			)
		)
	)
)

(defun proximidade(v g)
	;segundo o quadro
	;(* (/ 1 (length (car g))) (soma_distancias v g))

	;segundo o documento
	(/ 1 (soma_distancias v g))

)

(defun todas_proximidades (g)
	(let ((prox "("))
		(dolist (v (car g) prox)
			(setq prox (format nil "~A (~A ~A) " prox (car v) (proximidade (car v) g )))
		)
		(setq prox (sort (read-from-string (format nil "~A) " prox)) #'> :key #'cadr));ordena
		(setq prox (read-from-string (format nil "(\"Proximidade\"~A) " prox)))
	)
)

(defun intermediarios (v g)
	(let ((soma 0) (aux (car g)))
		(dolist (v1 aux soma)
			(setq aux (cdr aux))
			(dolist (v2 aux soma)
				(setq caminho (A_estrela (car v1) (car v2) g (expande (list 0 (car v1)) g)))
				(if (< 3 (length caminho))
					;pega os do meio
					(dolist (e (remove (car (last caminho)) (cddr caminho)) soma)
						(if (eq v e)
							(incf soma)
						)
					)
				)
			)	
		)
	)
)

(defun intermediacao(v g)
	(/ (* 2 (intermediarios v g)) (* (- (length (car g)) 1) (- (length (car g)) 2)))
)

(defun todas_intermediacoes (g)
	(let ((inter "("))
		(dolist (v (car g) inter)
			(setq inter (format nil "~A (~A ~A) " inter (car v) (intermediacao (car v) g )))
		)
		(setq inter (sort (read-from-string (format nil "~A) " inter)) #'> :key #'cadr));ordena
		(setq inter (read-from-string (format nil "(\"Intermediacao\"~A) " inter)))
	)
)

(defun ordena (lista)
	(setq lista (append (car lista) (sort (cdr lista) #'> :key #'car)))
)


;Autores: 
;Bruno Cattalini
;Lucas Kaniak

; Lista no formato:
; ((Grau (Ver1 Val1) (Ver2 Val2) [...])(Proximidade (Ver1 Val1) (Ver2 Val2) [...])(Intermediacao (Ver1 Val1) (Ver2 Val2) [...]))
; Para teste: (setq teste '(("Grau" (9999 8) (156 6) (22 6))("Proximidade" (18 4.76) (170 4.52) (1500 4.44))("Intermediacao" (90 4.42) (76 38) (5 37))))

(defun tabela (g)
	(print_tabela (todos_graus g) (todas_proximidades g) (todas_intermediacoes g))
)
	
(defun print_tabela (coluna1 coluna2 coluna3)
	(setq cab1 "Vertice")
	(setq cab2 "Valor")
	(setq t1 (list (car coluna1)))
	(setq t2 (list (car coluna2)))
	(setq t3 (list (car coluna3)))
	(format t "~v,,,vA~%" 72 #\_ #\space)
	(format t "|~{          ~A         ~}" t1)
	(format t "|~{     ~A     ~}" t2)
	(format t "|~{      ~A      |~}~%" t3)
	(format t "~v,,,vA~%" 72 #\- #\space)
	(format t "~C" #\|)
	(dotimes (cont 2) (progn (format t "  ~A  |" cab1) (format t "   ~A   |" cab2)))
	(format t "  ~A  |" cab1)
	(format t "   ~A   |~%" cab2)
	(format t "~v,,,vA~%" 72 #\- #\space)
	(setq coluna1  (cadr coluna1))
	(setq coluna2 (cadr coluna2))
	(setq coluna3 (cadr coluna3))
	(setq tabela (list coluna1 coluna2 coluna3))
	;(mapcar (lambda (&rest arg)(format t "~A" arg)) tabela)
	(loop for x in coluna1
		  for y in coluna2
		  for z in coluna3
		  do
		  (format t "|") 
		  (format_control (car x))
		  (format_control (cadr x))
		  (format_control (car y))
		  (format_control (cadr y))
		  (format_control (car z))
		  (format_control (cadr z)) 
		  (format t "~%")
	)
	(format t "~v,,,vA~%" 72 #\- #\space)
	T
)

(defun format_control (num)
	(if (floatp num)
		(if (< (- num 10) 0)			(format t "    ~,2F   |" num) ;(un)
			(if (< (- num 100) 0)		(format t "   ~,2F   |" num) ;(dec)
				(if (< (- num 1000) 0)	(format t "   ~,2F  |" num) ;(cent)
										(format t "  ~,2F  |" num) ;(mili)
				)
			)
		)
		(if (< (- num 10) 0)			(format t "     ~A     |" num) ;(un)
			(if (< (- num 100) 0)		(format t "     ~A    |" num) ;(dec)
				(if (< (- num 1000) 0)	(format t "    ~A    |" num) ;(cent)
										(format t "    ~A   |" num) ;(mili)
				)
			)
		)
	)
)