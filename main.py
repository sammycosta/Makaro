import copy

file = open('puzzle_14.txt', "r")

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


def get_lista_redor(matriz, posicao_seta, posicao_apontado):
    lista_redor = []
    if posicao_seta[0] < (N-1) and (posicao_seta[0]+1, posicao_seta[1]) != posicao_apontado:
        lista_redor.append(matriz[posicao_seta[0]+1][posicao_seta[1]])
    if posicao_seta[0] > 0 and (posicao_seta[0]-1, posicao_seta[1]) != posicao_apontado:
        lista_redor.append(matriz[posicao_seta[0]-1][posicao_seta[1]])
    if posicao_seta[1] < (N-1) and (posicao_seta[0], posicao_seta[1]+1) != posicao_apontado:
        lista_redor.append(matriz[posicao_seta[0]][posicao_seta[1]+1])
    if posicao_seta[1] > 0 and (posicao_seta[0], posicao_seta[1]-1) != posicao_apontado:
        lista_redor.append(matriz[posicao_seta[0]][posicao_seta[1]-1])
    return lista_redor


def get_maior(n, lista):
    if len(lista) > 0:
        if n > lista[0]:
            n = get_maior(n, lista[1:])
        else:
            n = get_maior(lista[0], lista[1:])
    return n


def verifica_seta_maior(lista_redor):
    return get_maior(lista_redor[0], lista_redor[1:])


def valida_seta(matriz, posicao_seta, posicao_num, n):
    '''avalia se o número pode ser colocado na posição em função da seta'''
    apontado = get_apontado(posicao_seta)
    if apontado == posicao_num:
        lista_posicoes = get_lista_redor(matriz, posicao_seta, posicao_num)
        if n > verifica_seta_maior(lista_posicoes):
            return True
        return False
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
    setas = setas_em_volta(posicao)
    return valida_em_setas(matriz, num, posicao, setas)


def eh_adjacente(posicao, n, matriz) -> bool:
    ''' Retorna True se um adjacente for igual, False se não'''
    if posicao[0]+1 < len(matriz):
        if matriz[posicao[0]+1][posicao[1]] == n:
            return True
    if posicao[0] > 0:
        if matriz[posicao[0]-1][posicao[1]] == n:
            return True
    if posicao[1]+1 < len(matriz[0]):
        if matriz[posicao[0]][posicao[1]+1] == n:
            return True
    if posicao[1] > 0:
        if matriz[posicao[0]][posicao[1]-1] == n:
            return True
    return False


def certezas(matriz):
    preenche_falta_1(0, matriz)
    preenche_falta_2(0, matriz)
    return matriz


certezas(matriz_certezas)
for i in matriz_certezas:
    print(i)

matriz_possib = copy.deepcopy(matriz_certezas)

# Inicio backtracking


def posicao_numero(num, posicoes_possiveis, possibilidades):
    ''' junta o número com todas posicoes '''
    if len(posicoes_possiveis) == 0:
        return

    posicao = posicoes_possiveis[0]
    possibilidades.append((num, posicao))
    posicao_numero(num, posicoes_possiveis[1:], possibilidades)


def numero_posicao(num_possiveis, posicoes_possiveis, possibilidades):
    if len(num_possiveis) == 0:
        return
    num = num_possiveis[0]
    posicao_numero(num, posicoes_possiveis, possibilidades)
    numero_posicao(num_possiveis[1:], posicoes_possiveis, possibilidades)


def faz_lista_possibilidades(num_possiveis, posicoes_possiveis):
    possibilidades = []
    numero_posicao(num_possiveis, posicoes_possiveis, possibilidades)
    return possibilidades


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


def remove_itens_da_lista(lista_principal, lista_auxiliar):
    if len(lista_auxiliar) == 0:
        return lista_principal
    item = lista_auxiliar[0]
    if item in lista_principal:
        lista_principal.remove(item)
    return remove_itens_da_lista(lista_principal, lista_auxiliar[1:])


def eh_igual_a_algum(valor, lista):
    '''procuro na lista uma ocorrencia do valor'''
    if len(lista) == 0:
        return False

    valor_lista = lista[0]
    if valor == valor_lista:
        return True
    return eh_igual_a_algum(valor, lista[1:])


def para_todos_caminhos_errados(caminho, caminhos_errados, vazias_possiveis, possibilidades, alterou):
    if len(caminhos_errados) == 0:
        return vazias_possiveis, alterou
    errado = caminhos_errados[0]

    if caminho == errado[0:len(caminho)]:
        # Até aqui, o caminho é o mesmo.
        # Excluir possibilidade para tentar preencher de novo
        posicao = possibilidades[len(caminho) - 1][1]
        if posicao in vazias_possiveis:  # se nao removi já
            vazias_possiveis.remove(posicao)
        alterou = True

    return para_todos_caminhos_errados(caminho, caminhos_errados[1:], vazias_possiveis, possibilidades, alterou)


def backtracking_preenche_numero(matriz, num_possiveis, vazias, lista_regiao, possibilidades, caminho, lista_falhas, caminhos_errados):
    num = num_possiveis[0]
    vazias_possiveis = vazias.copy()

    # Vou procurar na lista de falhas se, de acordo com a possibilidade "pai",
    # Existem algumas posições que esse número já causou uma falha então não é possível.
    ordem = len(caminho)
    if (ordem > 0 and len(lista_falhas[ordem]) > 0 and lista_falhas[ordem][0] == caminho[-1]) or (ordem == 0 and len(lista_falhas[0]) > 0):
        vazias_possiveis = remove_itens_da_lista(
            vazias_possiveis, lista_falhas[ordem][1:])

    conseguiu_preencher, pos = preenche_numero(
        matriz, num, vazias_possiveis, lista_regiao)

    if conseguiu_preencher:
        caminho.append(possibilidades.index((num, (pos))))

        vazias_possiveis, alterou = para_todos_caminhos_errados(
            caminho, caminhos_errados, vazias_possiveis, possibilidades, False)

        if alterou:
            # limpa preenchimento anterior pra tentar de novo
            lista_regiao[1] += 1
            matriz[pos[0]][pos[1]] = 0
            old_pos = pos
            conseguiu_preencher, pos = preenche_numero(
                matriz, num, vazias_possiveis, lista_regiao)
            if conseguiu_preencher:
                # já me livrei de um caminho errado
                caminho.pop()
                caminho.append(possibilidades.index((num, (pos))))
            else:
                # retorna preenchimento antigo
                pos = old_pos
                lista_regiao[1] -= 1
                matriz[pos[0]][pos[1]] = num

        indice = vazias.index(pos)
        vazias.pop(indice)  # Posição já preenchida

        # Continua para os outros números
        preencheu = preenche_toda_regiao(
            matriz, num_possiveis[1:], vazias, lista_regiao, possibilidades, caminho, lista_falhas, caminhos_errados)

        if not preencheu:
            # Continuar a tentar preencher o número mas nas próximas posições
            # Limpar preenchimento
            lista_regiao[1] += 1
            matriz[pos[0]][pos[1]] = 0
            vazias.insert(indice, pos)

            ordem = len(caminho) - 1
            caminho.pop()  # falhou
            if len(caminho) > 0:
                # Tem pai no caminho
                if len(lista_falhas[ordem]) > 0:
                    if lista_falhas[ordem][0] == caminho[-1]:
                        # Mesma possibilidade pai, mais uma falha
                        lista_falhas[ordem].append((pos[0], pos[1]))
                    else:
                        # Pai diferente. reseto. avaliar depois se não preciso guardar isso
                        lista_falhas[ordem] = [caminho[-1], (pos[0], pos[1])]
                else:
                    # Primeira falha para esse número com x possibilidade pai
                    lista_falhas[ordem] = [caminho[-1], (pos[0], pos[1])]
            else:
                # é o primeiro
                if len(lista_falhas[ordem]) > 0:
                    lista_falhas[ordem].append((pos[0], pos[1]))
                else:
                    lista_falhas[ordem] = [-1, (pos[0], pos[1])]

            # Voltar: mesmo número, posições após a que já tentei
            return backtracking_preenche_numero(
                matriz, num_possiveis, vazias, lista_regiao, possibilidades, caminho, lista_falhas, caminhos_errados)
        else:
            return True
    else:
        # Não consegui colocar o número (vai retornar falso pro backtracking anterior)
        return False


def preenche_toda_regiao(matriz, num_possiveis, vazias, lista_regiao, possibilidades, caminho, lista_falhas, caminhos_errados) -> bool:
    '''itera pela lista de números pra colocar eles em cada posição '''
    if len(num_possiveis) == 0 and lista_regiao[1] != 0:
        return False  # Falhou
    elif len(num_possiveis) == 0 and lista_regiao[1] == 0:
        if eh_igual_a_algum(caminho, caminhos_errados):
            # Não conseguiu fazer um caminho que não fosse errado
            return False
        return True  # Regiao toda preenchida

    # [0] é o caminho pai, o resto é possibilidades falhas praquele caminho pai
    return backtracking_preenche_numero(
        matriz, num_possiveis, vazias, lista_regiao, possibilidades, caminho, lista_falhas, caminhos_errados)


def solve_by_regiao(matriz, lista_regioes, caminhos_regioes, caminhos_errados, num_possiveis, vazias):
    regiao = lista_regioes[0]
    possibilidades = faz_lista_possibilidades(num_possiveis, vazias)
    caminho = []
    lista_falhas = [[] for _ in range(len(num_possiveis))]

    if preenche_toda_regiao(matriz, num_possiveis, vazias, regiao, possibilidades, caminho, lista_falhas, caminhos_errados):
        matriz = certezas(matriz)
        caminhos_regioes.append(caminho)
        print(caminho, regiao, '\n')

        for i in matriz:
            print(i)
        return True
    else:
        print('\n não conseguiu preencher a', regiao)
        return False  # Não conseguiu preencher região, problema está antes


def clean_regiao(matriz, vazias):
    '''em todas as posicoes que eram vazias antes, colocar 0 na matriz'''
    if len(vazias) == 0:
        return matriz

    row, col = vazias[0]
    matriz[row][col] = 0
    return clean_regiao(matriz, vazias[1:])


def backtracking(matriz, lista_regioes, caminhos_regioes, lista_erros_regioes):
    ''' para cada região'''
    i = len(lista_regioes)
    if i == 0:
        # Validar?
        # Checa aqui, então vai voltar loucamente
        return True

    caminhos_errados = []
    ordem = len(caminhos_regioes)
    erros_atual = lista_erros_regioes[ordem]
    if (ordem > 0 and len(erros_atual) > 0 and erros_atual[0] == (i, caminhos_regioes[-1])) or (ordem == 0 and len(erros_atual) > 0):
        caminhos_errados = erros_atual[1:]

    # Tenta preencher uma próxima região
    num_possiveis, vazias = numeros_que_faltam(lista_regioes[0], matriz)
    # acho que haskell nao vai ter esse problema de ter que fazer copy pq é imutável
    vazias_inicio = vazias.copy()

    preencheu = solve_by_regiao(
        matriz, lista_regioes, caminhos_regioes, caminhos_errados, num_possiveis, vazias)

    if preencheu:
        preencheu = backtracking(
            matriz, lista_regioes[1:], caminhos_regioes, lista_erros_regioes)

        if not preencheu:
            # limpar preenchimento...
            if len(caminhos_regioes[-1]) > 0:
                matriz = clean_regiao(matriz, vazias_inicio)
                lista_regioes[0][1] = len(vazias_inicio)
                ordem = len(caminhos_regioes) - 1
                caminho_falha = caminhos_regioes.pop()
                print('caminho falha', caminho_falha)
                if len(caminhos_regioes) > 0:
                    if len(lista_erros_regioes[ordem]) > 0:
                        if lista_erros_regioes[ordem][0] == (i, caminhos_regioes[-1]):
                            # Mesma possibilidade pai, mais uma falha
                            lista_erros_regioes[ordem].append(caminho_falha)
                        else:
                            # Pai diferente. reseto.
                            lista_erros_regioes[ordem] = [
                                (i, caminhos_regioes[-1]), caminho_falha]
                    else:
                        # Primeira falha para esse número com x possibilidade pai
                        lista_erros_regioes[ordem] = [
                            (i, caminhos_regioes[-1]), caminho_falha]
                else:
                    # é o primeiro
                    return False

                # Tentar de novo essa região
                return backtracking(matriz, lista_regioes, caminhos_regioes, lista_erros_regioes)
            else:
                # caminho certeza, n adianta tentar dnv
                # problema aqui
                caminhos_regioes.pop()
                return False

        else:
            return True
    else:
        return False


caminhos_regioes = []
lista_erros_regioes = [[] for _ in range(len(regioes))]
backtracking(matriz_possib, regioes,
             caminhos_regioes, lista_erros_regioes)
print('\n')
for i in matriz_possib:
    print(i)
