# Jogo Sudoku

#### Jogo SUDOKU desenvolvido em Fortran f90

O jogo possui 2 jogadores, sendo que um deve posicionar os números iniciais em um arquivo (jogador1.txt). O outro jogador deve resolver o tabuleiro. 

Os números que aparecem no tabuleiro devem ser passados a partir de um arquivo com o seguinte formato: A primeira linha indica quantos números serão inseridos. A partir daí, cada linha do arquivo representa uma posição no tabuleiro. O primeiro número representa a linha, o segundo representa a coluna, e o terceiro representa o valor da posição.
Ex:
4
1,3,9
4,5,1
5,1,7
5,6,8

#### Construindo o tabuleiro

```fortran
subroutine OrdenaTabuleiro (tabuleiro)
    implicit none
    integer :: i,j
    integer(1), dimension (1:9,1:9) :: tabuleiro
    character (*), parameter :: bar = '+-----+-----+-----+'
    character (*), parameter :: fmt = '(3 ("|", i0, 1x, i0, 1x, i0), "|")'

    write (*, '(a)') bar
    do j = 0, 6, 3
      do i = j + 1, j + 3
        write (*, fmt) tabuleiro (i, :)
      end do
      write (*, '(a)') bar
    end do
  end subroutine OrdenaTabuleiro
```



##### Imagens ilustrativas de desenvolvimento

![dev1](/.img/dev1.png)