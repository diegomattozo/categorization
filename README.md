> Tutorial para instalação de pacotes do R disponíveis no Github: https://cran.r-project.org/web/packages/githubinstall/vignettes/githubinstall.html

Duas classes de métodos de categorização de variáveis explicativas contínuas, para modelos com variável resposta binária estão disponíveis no pacote. A primeira classe de métodos é univariada e busca manter a associação entre a variável resposta e a covariável categorizada através de medidas de associação para variáveis qualitativas. A segunda classe de métodos é multivariada e tenta incorporar a estrutura de dependência entre as covariáveis do modelo através da categorização conjunta de todas as variáveis preditoras.

> IMPORTANTE: Os dois métodos consideram que a variável resposta está na última coluna da base de dados.

A primeira classe de métodos univariados pode ser utilizada da seguinte maneira:


> multdiscretization::discretize(db, meth = 4)

Onde db é a base de dados a ser discretizada e meth é a medida de associação a ser utilizada pela categorização. Há 5 medidas distintas. 1) Caim, 
2) Cacc 3) Ameva 4) Information Value e 5) Kendall's Tau-C. O método retorna uma lista com a base de dados categorizada e com os pontos de corte definidos.

O método de discretização multivariado pode ser utilizada da seguite maneira:

> multdiscretization::multdiscretization(train, validation)

Onde train é a base de treinamento e validation é a base de validação. O método retorna uma lista com as duas bases discretizadas e com o vetor de pontos de corte.


Há alguns método utilitários que podem ajudar no processo de utilização do pacote.

> O método train_test_split(db, test_percentual, seed) ajuda a dividir uma base de dados em treinamento e teste(validação) dado um percentual para
a base de testes.

> O método cutpoint_discretization(db, cutpoints) realiza a categorização de uma base de dados através do vetor de pontos de corte retornados no método multivariado.


Para mais informações digite ??multdiscretization no R.


