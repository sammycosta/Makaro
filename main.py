import copy

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


def numeros_que_faltam(lista_regiao, matriz):
    size = lista_regiao[0]
    possibilidades = list(range(1, size+1))
    posicoes_vazias = []
    percorre_lista(possibilidades, posicoes_vazias, lista_regiao[2:], matriz)

    return possibilidades, posicoes_vazias


def percorre_lista(possibilidades, posicoes_vazias, lista, matriz):
    if len(lista) > 0:
        row, col = lista[0]
        num = matriz[row][col]
        if num != 0:
            possibilidades.remove(num)
        else:
            posicoes_vazias.append((row, col))

        percorre_lista(possibilidades, posicoes_vazias, lista[1:], matriz)


def preenche_falta_1(indice, matriz):
    if regioes[indice][1] == 1:
        # Verificar qual é o número que falta e qual é a posição
        faltam, posicoes = numeros_que_faltam(regioes[indice], matriz)
        row, col = posicoes[0]
        matriz[row][col] = faltam[0]
        regioes[indice][1] -= 1

    if indice+1 < num_regioes:
        # Continua até acabar as regiões. Mudar forma de percorrer para slice?
        preenche_falta_1(indice+1, matriz)


def preenche_falta_2(indice, matriz):
    if regioes[indice][1] == 2:
        # Verificar qual é o número que falta e qual é a posição
        faltam, posicoes = numeros_que_faltam(regioes[indice], matriz)
        if (eh_adjacente(posicoes[0], faltam[0], matriz)):
            row1, col1 = posicoes[0]
            row2, col2 = posicoes[1]
            matriz[row1][col1] = faltam[1]
            matriz[row2][col2] = faltam[0]
            regioes[indice][1] -= 2
        elif (eh_adjacente(posicoes[1], faltam[0], matriz)):
            row1, col1 = posicoes[1]
            row2, col2 = posicoes[0]
            matriz[row1][col1] = faltam[1]
            matriz[row2][col2] = faltam[0]
            regioes[indice][1] -= 2
        elif (eh_adjacente(posicoes[0], faltam[1], matriz)):
            row1, col1 = posicoes[0]
            row2, col2 = posicoes[1]
            matriz[row1][col1] = faltam[0]
            matriz[row2][col2] = faltam[1]
            regioes[indice][1] -= 2
        elif (eh_adjacente(posicoes[1], faltam[0], matriz)):
            row1, col1 = posicoes[1]
            row2, col2 = posicoes[0]
            matriz[row1][col1] = faltam[0]
            matriz[row2][col2] = faltam[1]
            regioes[indice][1] -= 2
        # else:
        #     setas = setas_em_volta(posicoes[0])
        #     if len(setas) > 0:

    if indice+1 < num_regioes:
        # Continua até acabar as regiões. Mudar forma de percorrer para slice?
        preenche_falta_2(indice+1, matriz)


# o maior apontado nunca deve ser 1! a não ser que não tenha ninguém ao redor da seta além dele

def get_apontado(posicao_seta) -> tuple[int, int]:
    if matriz_regioes[posicao_seta[0]][posicao_seta[1]] == 'L':
        return (posicao_seta[0], posicao_seta[1]-1)
    if matriz_regioes[posicao_seta[0]][posicao_seta[1]] == 'R':
        return (posicao_seta[0], posicao_seta[1]+1)
    if matriz_regioes[posicao_seta[0]][posicao_seta[1]] == 'U':
        return (posicao_seta[0]-1, posicao_seta[1])
    if matriz_regioes[posicao_seta[0]][posicao_seta[1]] == 'D':
        return (posicao_seta[0]+1, posicao_seta[1])


def get_lista_redor(posicao_seta, posicao_apontado):
    lista_redor = []
    if posicao_seta[0] < (N-1) and (posicao_seta[0]+1, posicao_seta[1]) != posicao_apontado:
        lista_redor.append(matriz_certezas[posicao_seta[0]+1][posicao_seta[1]])
    if posicao_seta[0] > 0 and (posicao_seta[0]-1, posicao_seta[1]) != posicao_apontado:
        lista_redor.append(matriz_certezas[posicao_seta[0]-1][posicao_seta[1]])
    if posicao_seta[1] < (N-1) and (posicao_seta[0], posicao_seta[1]+1) != posicao_apontado:
        lista_redor.append(matriz_certezas[posicao_seta[0]][posicao_seta[1]+1])
    if posicao_seta[1] > 0 and (posicao_seta[0], posicao_seta[1]-1) != posicao_apontado:
        lista_redor.append(matriz_certezas[posicao_seta[0]][posicao_seta[1]-1])
    return lista_redor


def get_maior(n, lista):
    if len(lista) > 0:
        if n > lista[0]:
            n = get_maior(lista[1:], n)
        else:
            n = get_maior(lista[1:], lista[0])
    return n


def verifica_seta_maior(lista_redor):
    return get_maior(lista_redor[0], lista_redor[1:])


def valida_seta(matriz, posicao_seta, posicao_num, n):
    '''avalia se o número pode ser colocado na posição em função da seta'''
    apontado = get_apontado(posicao_seta)
    if apontado == posicao_num:
        lista_posicoes = get_lista_redor(posicao_seta, posicao_num)
        if n > verifica_seta_maior(lista_posicoes):
            return True
    else:
        if matriz[apontado[0]][apontado[1]] > 0:
            if n < matriz[apontado[0]][apontado[1]]:
                return True
            else:
                return False
        return True


def eh_seta(posicao) -> bool:
    return (matriz_regioes[posicao[0]][posicao[1]] == 'L' or
            matriz_regioes[posicao[0]][posicao[1]] == 'R' or
            matriz_regioes[posicao[0]][posicao[1]] == 'U' or
            matriz_regioes[posicao[0]][posicao[1]] == 'D')


def setas_em_volta(posicao_numero) -> list[tuple[int, int]]:
    lista_setas = []
    if posicao_numero[0] < (N-1) and eh_seta((posicao_numero[0]+1, posicao_numero[1])):
        lista_setas.append(
            (posicao_numero[0]+1, posicao_numero[1]))
    if posicao_numero[0] > 0 and eh_seta((posicao_numero[0]-1, posicao_numero[1])):
        lista_setas.append(
            (posicao_numero[0]-1, posicao_numero[1]))
    if posicao_numero[1] < (N-1) and eh_seta((posicao_numero[0], posicao_numero[1]+1)):
        lista_setas.append(
            (posicao_numero[0], posicao_numero[1]+1))
    if posicao_numero[1] > 0 and eh_seta((posicao_numero[0], posicao_numero[1]-1)):
        lista_setas.append(
            (posicao_numero[0], posicao_numero[1]-1))

    return lista_setas


def valida_em_setas(matriz, num, posicao, setas_em_volta) -> bool:
    ''' chama a função valida_seta para todas setas ao redor da posicao'''
    if len(setas_em_volta) == 0:
        return True

    seta = setas_em_volta[0]
    if valida_seta(matriz, seta, posicao, num):
        return valida_em_setas(matriz, num, posicao, setas_em_volta[1:])
    else:
        return False  # uma não é valida já


def valida_num_pos_setas(matriz, num, posicao) -> bool:
    '''Dado um número e uma posição, retorna se é válido colocar ele ali em função das setas ao redor da posição.'''
    setas_em_volta = setas_em_volta(posicao)


def eh_adjacente(posicao, n, matriz) -> bool:
    ''' Retorna True se um adjacente for igual, False se não'''
    if posicao[0] < len(matriz):
        if matriz[posicao[0]+1][posicao[1]] == n:
            return True
    if posicao[0] > 0:
        if matriz[posicao[0]-1][posicao[1]] == n:
            return True
    if posicao[1] < len(matriz[0]):
        if matriz[posicao[0]][posicao[1]+1] == n:
            return True
    if posicao[1] > 0:
        if matriz[posicao[0]][posicao[1]-1] == n:
            return True
    return False


def certezas(matriz):
    preenche_falta_1(0, matriz)
    preenche_falta_2(0, matriz)


certezas(matriz_certezas)
for i in matriz_certezas:
    print(i)

matriz_possib = copy.deepcopy(matriz_certezas)

# Inicio backtracking


def eh_valida(matriz, num, posicao) -> bool:
    ''' Checa todas as regras'''
    return (not eh_adjacente(posicao, num, matriz)) and valida_num_pos_setas(matriz, num, posicao)


def preenche_numero(matriz, num, lista_posicoes, lista_regiao) -> tuple[bool, tuple[int, int]]:
    '''Tenta preencher um número em todas as posições até conseguir na primeira que achar'''
    if len(lista_posicoes) == 0:  # Não preencheu em nenhuma posição
        return False, (-1, -1)

    row, col = lista_posicoes[0]

    if eh_valida(matriz, num, (row, col)):
        matriz[row][col] = num
        lista_regiao[1] -= 1
        return True, (row, col)
    else:
        # Tenta colocar o número na próxima posição
        return preenche_numero(matriz, num, lista_posicoes[1:], lista_regiao)


def preenche_toda_regiao(matriz, num_possiveis, vazias, lista_regiao) -> bool:
    if len(num_possiveis) == 0 and lista_regiao[1] != 0:
        return False  # Falhou
    elif len(num_possiveis) == 0 and lista_regiao[1] == 0:
        return True  # Regiao toda preenchida

    num = num_possiveis[0]

    conseguiu_preencher, pos = preenche_numero(
        matriz, num, vazias, lista_regiao, i)

    if conseguiu_preencher:
        vazias.remove(pos)  # Posição já preenchida
        return preenche_toda_regiao(matriz, num_possiveis[1:], vazias, lista_regiao)
    else:
        return False


def solve_by_regiao(matriz, lista_regioes):
    if len(lista_regioes) == 0:
        return
    regiao = lista_regioes[0]
    num_possiveis, vazias = numeros_que_faltam(regiao, matriz)

    # representa cada possibilidade (num, posicao) -> como utilizar?
    lista_possibilidades = [0] * (len(num_possiveis) * len(vazias))
    caminho = []  # Caminho com todas as ações feitas e aí eu voltaria na "árvore"? Avaliar
    if preenche_toda_regiao(matriz, num_possiveis, vazias, lista_regioes):
        matriz = certezas(matriz)
        return solve_by_regiao(matriz, lista_regioes[1:])
    else:
        return False
