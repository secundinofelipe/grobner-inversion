# Critério de Invertibilidade de Aplicações Polinomiais via Bases de Gröbner

Implementação de um sistema de álgebra computacional (CAS) em OCaml para a decisão de invertibilidade de aplicações polinomiais e cálculo de inversas via Critério de van den Essen, com certificação formal em Lean4.

---

## Sobre o projeto

Este projeto tem como objetivo a implementação computacional, sem o uso de bibliotecas externas de álgebra computacional, do critério de van den Essen para decidir a invertibilidade de aplicações polinomiais, utilizando a teoria das bases de Gröbner. Desenvolvido no contexto de uma iniciação científica (PIBIC) na Universidade Federal de Uberlândia (UFU), o trabalho busca fornecer uma ferramenta para explorar problemas fundamentais da geometria algébrica.


---

## Fundamentação Matemática

O desenvolvimento fundamenta-se nos seguintes pilares teóricos:

* **Bases de Gröbner:** Conjuntos geradores especiais para ideais que permitem resolver problemas de decisão e idealidade.
* **Algoritmo de Buchberger:** O procedimento computacional para calcular bases de Gröbner, aqui otimizado para o cálculo de ideais associados a aplicações polinomiais.
* **Critério de van den Essen:** Método que utiliza uma ordem de eliminação específica para transformar a base de Gröbner de um ideal associado ao gráfico da aplicação em uma forma que revela explicitamente sua inversa.
* **Conjectura Jacobiana:** O problema em aberto central que motiva o estudo de invertibilidade de polinômios.
* **Aplicações Polinomiais de Hubbers:** Classificação de aplicações cúbicas homogêneas em dimensão quatro, utilizadas durante a iniciação científica como casos de teste para validação do sistema.

---

##  Funcionalidades

O CAS oferece as seguintes capacidades:

* Representação de polinômios multivariados;
* Aritmética exata com números racionais ($\mathbb{Q}$) para evitar erros de precisão;
* Implementação de ordens monomiais: Lex, GrLex e GrevLex;
* Algoritmo de pseudodivisão multivariada;
* Algoritmo de Buchberger para bases de Gröbner;
* Cálculo automático da base de Gröbner reduzida;
* Decisão de invertibilidade e extração da inversa polinomial;
* Geração automática de certificados formais para **Lean4**;
* Verificação formal da validade da inversa através de reflexão computacional (`decide`).

---

## Estrutura do Projeto

```text
.
├── src/
│   ├── rational.ml       # Implementação da aritmética de números racionais
│   ├── monomial.ml       # Definição e comparação de ordens monomiais
│   ├── polynomial.ml     # Estruturas, manipulação e normalização polinomial
│   ├── division.ml       # Algoritmo de pseudodivisão multivariada
│   ├── buchberger.ml     # Algoritmo de Buchberger (para cálculo de bases de Gröbner)
│   ├── van_den_essen.ml  # Critério de van den Essen e inversão
│   └── lean_export.ml    # Exportador de certificados formais para Lean4
├── README.md             # Documentação principal do projeto