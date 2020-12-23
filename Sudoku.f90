program Sudoku
implicit none

integer :: i,j,n, linha, coluna,valor, lp, cp, vp, verifica, tabuleiroCompleto
integer(1), dimension (1:9,1:9) :: tabuleiro
! tabuleirobase ******* Este tabuleiro esta sendo usado para não deixar o Jogador #2 alterar os valores definidos pelo Jogador #1, no arquivo 'jogador1.txt'
integer(1), dimension (1:9,1:9) :: tabuleirobase

  call OrdenaTabuleiro (tabuleiro)

contains
  
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

end program Sudoku
