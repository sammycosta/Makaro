file = open('puzzle_01.txt', "r")

N = int(file.readline())

# Leitura arquivo
matriz_regioes = []
matriz_certezas = []
for i in range(N):
    matriz_regioes.append(file.readline().split())
file.readline()  # \n
for i in range(N):
    matriz_certezas.append([int(x) for x in file.readline().split()])


letras = ['R', 'L', 'D', 'U', 'X']
regioes = []


def encontra_regioes(row, col):
    '''Faz um parsing onde o resultado no fim das recursões deve ser o dicionário regiões preenchido com
    listas, uma para cada região, onde o primeiro item é o tamanho da região, o segundo é quantas posições
    faltam ser preenchidas e os demais itens são as posições da região.'''

    caracter = matriz_regioes[row][col]

    if caracter not in letras:
        regiao = int(caracter)
        if regiao > len(regioes):
            regioes.append([0, 0])  # TAMANHO, FALTA PREENCHER

        regioes[regiao-1][0] += 1  # Aumenta tamanho
        if matriz_certezas[row][col] == 0:
            regioes[regiao-1][1] += 1  # Aumenta falta preencher

        regioes[regiao-1].append((row, col))

    if col+1 == N:
        # Fim da linha, passar pra próxima se não acabou as linhas
        if row+1 != N:
            encontra_regioes(row+1, 0)
    else:
        encontra_regioes(row, col+1)


encontra_regioes(0, 0)
num_regioes = len(regioes)

# Provavelmente mudar a lógica de ver quais números possíveis de preencher no Haskell,
# ou ter um armazenamento disso pra não recalcular


def numeros_que_faltam(lista_regiao):
    size = lista_regiao[0]
    possibilidades = list(range(1, size+1))
    posicoes_vazias = []
    percorre_lista(possibilidades, posicoes_vazias, lista_regiao[2:])

    return possibilidades, posicoes_vazias


def percorre_lista(possibilidades, posicoes_vazias, lista):
    if len(lista) > 0:
        row, col = lista[0]
        num = matriz_certezas[row][col]
        if num != 0:
            possibilidades.remove(num)
        else:
            posicoes_vazias.append((row, col))

        percorre_lista(possibilidades, posicoes_vazias, lista[1:])


def preenche_falta_1(indice):
    if regioes[indice][1] == 1:
        # Verificar qual é o número que falta e qual é a posição
        faltam, posicoes = numeros_que_faltam(regioes[indice])
        row, col = posicoes[0]
        matriz_certezas[row][col] = faltam[0]
        regioes[indice][1] -= 1

    if indice+1 < num_regioes-1:
        # Continua até acabar as regiões. Mudar forma de percorrer para slice?
        print('chamou!')
        preenche_falta_1(indice+1)


def preenche_falta_2(indice):
    if regioes[indice][1] == 2:
        # Verificar qual é o número que falta e qual é a posição
        faltam, posicoes = numeros_que_faltam(regioes[indice])
        if (eh_adjacente(posicoes[0], faltam[0])):
            row1, col1 = posicoes[0]
            row2, col2 = posicoes[1]
            matriz_certezas[row1][col1] = faltam[1]
            matriz_certezas[row2][col2] = faltam[0]
            regioes[indice][1] -= 2
        elif (eh_adjacente(posicoes[1], faltam[0])):
            row1, col1 = posicoes[1]
            row2, col2 = posicoes[0]
            matriz_certezas[row1][col1] = faltam[1]
            matriz_certezas[row2][col2] = faltam[0]
            regioes[indice][1] -= 2
        elif (eh_adjacente(posicoes[0], faltam[1])):
            row1, col1 = posicoes[0]
            row2, col2 = posicoes[1]
            matriz_certezas[row1][col1] = faltam[0]
            matriz_certezas[row2][col2] = faltam[1]
            regioes[indice][1] -= 2
        elif (eh_adjacente(posicoes[1], faltam[0])):
            row1, col1 = posicoes[1]
            row2, col2 = posicoes[0]
            matriz_certezas[row1][col1] = faltam[0]
            matriz_certezas[row2][col2] = faltam[1]
            regioes[indice][1] -= 2

    if indice+1 < num_regioes-1:
        # Continua até acabar as regiões. Mudar forma de percorrer para slice?
        preenche_falta_2(indice+1)


# Coisas da primeira vez
def regiao_2_elementos(i1, j1, i2, j2):
    if matriz_certezas[i1][j1] == 0 and matriz_certezas[i2][j2] == 0:
        pass
    else:
        if matriz_certezas[i1][j1] == 1:
            matriz_certezas[i2][j2] = 2
        elif matriz_certezas[i1][j1] == 2:
            matriz_certezas[i2][j2] = 1
        elif matriz_certezas[i2][j2] == 1:
            matriz_certezas[i1][j1] = 2
        elif matriz_certezas[i2][j2] == 2:
            matriz_certezas[i1][j1] = 1


def eh_adjacente(posicao, n):
    if posicao[0] < len(matriz_certezas):
        if matriz_certezas[posicao[0]+1][posicao[1]] == n:
            return True
    if posicao[0] > 0:
        if matriz_certezas[posicao[0]-1][posicao[1]] == n:
            return True
    if posicao[1] < len(matriz_certezas[0]):
        if matriz_certezas[posicao[0]][posicao[1]+1] == n:
            return True
    if posicao[1] > 0:
        if matriz_certezas[posicao[0]][posicao[1]-1] == n:
            return True
    return False


preenche_falta_1(0)
preenche_falta_2(0)
for i in matriz_certezas:
    print(i)
