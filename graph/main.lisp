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
	;	if ( solucao(X,destino))
	;		return X;
	;	else
	;		L = expande(X);   -> gerar novos candidatos: antes: (100 ...... 1) depois de colocar vertice vizinho 2:  (125 ..... 1 2)
	;		adicionar L em Candidatos
	;		remover X de Candidatos
	;		return A*(origem, destino, Candidatos)
	;FIM
;Como representar um caminho: (104	1	2	4) *usar em Candidatos*
;							  custo/ vertices...

;(defun A_estrela (origem destino Candidatos)
;	(let (X)
;		(if (not (null Candidatos))
;			(setq X (melhor_caminho Candidatos))
;			(if (eq X destino)
;				(return X)
;				(setq L (expande X))
;				(setq Candidatos (append (remover X Candidatos) L))
;				(return A_estrela (origem destino Candidatos))
;			)
;		)
;	)
;)


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
		(setq g (read-from-string s))
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
	(length (adjacentes v g))
)
