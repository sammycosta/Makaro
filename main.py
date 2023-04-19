file = open('puzzle.txt', "r")

N = int(file.readline())

matriz_regioes = []
matriz_certezas = []
for i in range(N):
    matriz_regioes.append(file.readline().split())
file.readline()  # \n
for i in range(N):
    matriz_certezas.append([int(x) for x in file.readline().split()])


# solve

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


def eh_adjacente(i, j, n):
    if i < len(matriz_certezas):
        if matriz_certezas[i+1][j] == n:
            return True
    if i > 0:
        if matriz_certezas[i-1][j] == n:
            return True
    if j < len(matriz_certezas[0]):
        if matriz_certezas[i][j+1] == n:
            return True
    if j > 0:
        if matriz_certezas[i][j-1] == n:
            return True
    return False
