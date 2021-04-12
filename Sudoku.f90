!
!Programa 1: Faça um programa que implemente o Jogo SUDOKU. O jogo deve ser jogado por 2 jogadores, sendo que um deve posicionar os números iniciais em um arquivo. O outro jogador deve resolver o tabuleiro. Os números que aparecem no tabuleiro devem ser passados a partir de um arquivo com o seguinte formato: A primeira linha indica quantos números serão inseridos. A partir daí, cada linha do arquivo representa uma posição no tabuleiro. O primeiro número representa a linha, o segundo representa a coluna, e o terceiro representa o valor da posição.
!Ex:
!4
!1,3,9
!4,5,1
!5,1,7
!5,6,8

program Sudoku
implicit none


integer :: i,j,n, linha, coluna,valor, lp, cp, vp, verifica, tabuleiroCompleto
integer(1), dimension (1:9,1:9) :: tabuleiro
! tabuleirobase ******* Este tabuleiro esta sendo usado para não deixar o Jogador #2 alterar os valores definidos pelo Jogador #1, no arquivo 'jogador1.txt'
integer(1), dimension (1:9,1:9) :: tabuleirobase

verifica = 0

!Inicializar tabuleiro
  do i=1,9,1
    do j=1,9,1
      tabuleirobase(i,j) = 0
      tabuleiro(i,j) = 0 
    end do    
  end do

  open(50,file="jogador1.txt")
  read (50,*) n
    do i=1,n
      read(50,*)linha,coluna,valor
      tabuleirobase(linha,coluna) = valor 
      tabuleiro(linha,coluna) = valor 
    end do
  close(50)

  lp = 1
  cp = 1
  vp = 1

  !Verifica se o JOGADOR #1 preencheu os valores corretos.
  call OrdenaTabuleiro (tabuleiro)
  call VerificaNumeroRepetido (tabuleiro)
  call VerificaBloco (tabuleiro, 1, 1, 3, 3)
  call VerificaBloco (tabuleiro, 1, 4, 3, 6)
  call VerificaBloco (tabuleiro, 1, 7, 3, 9)
  call VerificaBloco (tabuleiro, 4, 1, 6, 3)
  call VerificaBloco (tabuleiro, 4, 4, 6, 6)
  call VerificaBloco (tabuleiro, 4, 7, 6, 9)
  call VerificaBloco (tabuleiro, 7, 1, 9, 3)
  call VerificaBloco (tabuleiro, 7, 4, 9, 6)
  call VerificaBloco (tabuleiro, 7, 7, 9, 9)  

  do while(lp /= 0 .AND. cp /= 0 .AND. vp /= 0)
    
    write(*,*)"Para sair do programa digite 0 na atribuição de alguma variável!"
    write(*,*)"JOGADOR #2 !"
    write(*,*)"Digite uma linha de 1 a 9"
    read(*,*)lp
    write(*,*)"Digite uma coluna de 1 a 9"
    read(*,*)cp
    write(*,*)"Digite um valor para a posição "
    read(*,*)vp

    if(tabuleirobase(lp,cp) == 0 ) then 
      tabuleiro(lp,cp) = vp
    else
      write(*,*)"JOGADOR #2, Você não pode alterar os valores definidos pelo JOGADOR #1"
    end if

    call OrdenaTabuleiro (tabuleiro)
    call VerificaNumeroRepetido (tabuleiro)
    call VerificaBloco (tabuleiro, 1, 1, 3, 3)
    call VerificaBloco (tabuleiro, 1, 4, 3, 6)
    call VerificaBloco (tabuleiro, 1, 7, 3, 9)
    call VerificaBloco (tabuleiro, 4, 1, 6, 3)
    call VerificaBloco (tabuleiro, 4, 4, 6, 6)
    call VerificaBloco (tabuleiro, 4, 7, 6, 9)
    call VerificaBloco (tabuleiro, 7, 1, 9, 3)
    call VerificaBloco (tabuleiro, 7, 4, 9, 6)
    call VerificaBloco (tabuleiro, 7, 7, 9, 9)

   
    tabuleiroCompleto = 1
    do i=1,9,1
      do j=1,9,1
        if(tabuleiro(i,j) /= 0) then
          tabuleiroCompleto = tabuleiroCompleto + 1
        end if
      end do    
    end do

    if(tabuleiroCompleto == 82 .AND. verifica == 1) then
      lp = 0
      Write(*,*)"PARABÉNS!!! VOCÊ COMPLETOU O TABULEIRO!!!"
    end if 

  end do

! COMEÇO DAS SUBROTINAS====================================================================
contains
  
  subroutine VerificaNumeroRepetido (tabuleiro)
    implicit none
    integer(1), dimension (1:9,1:9), intent(out) :: tabuleiro
    integer :: num, i, j, cont

    verifica = 1
    cont = 1
    !Verifica se existe algum número repetido na "LINHA" do tabuleiro
    do i = 1, 9, 1
      do j = 1, 9, 1
        do while(cont <= 9)
          num = tabuleiro(i,cont)
          if(num == tabuleiro(i,j) .AND. num /= 0 .AND. cont /= j) then
            write(*,*)"Números iguais: LINHA = ",i," | COLUNA = ",j," | VALOR = ",num
            verifica = 0
          end if
          cont = cont + 1
        end do
        cont = 1 
      end do
    end do
 
    !Verifica se existe algum número repetido na "COLUNA" do tabuleiro
    do i = 1, 9, 1
      do j = 1, 9, 1
        do while(cont <= 9)
          num = tabuleiro(cont,i)
          if(num == tabuleiro(j,i) .AND. num /= 0 .AND. cont /= j) then
            write(*,*)"Números iguais: LINHA = ",i," | COLUNA = ",j," | VALOR = ",num
            verifica = 0
          end if
          cont = cont + 1
        end do
        cont = 1 
      end do
    end do
 
  end subroutine VerificaNumeroRepetido


  !Esta subrotina verifica se o Tabuleiro informado pelo Jogador #1 apresenta números iguais
  ! li = Linha Inicial
  ! lf = Linha Final
  ! ci = Coluna Inicial
  ! cf = Coluna Final
  subroutine VerificaBloco (tabuleiro,li,ci,lf,cf)
    implicit none
    integer(1), dimension (1:9,1:9), intent(out) :: tabuleiro
    integer(1), dimension (1:3,1:3) :: bloco
    integer :: l, c, num, i, j, liaux, ciaux, auxl, auxc
    integer, intent(in) :: li, ci, lf, cf

    liaux = li
    ciaux = ci
    auxl = li
    auxc = ci

    do i = 1, 3, 1
      do j = 1, 3, 1
        bloco(i,j) = tabuleiro(liaux,ciaux)
        ciaux = ciaux +1 
        end do
      ciaux = auxc 
      liaux = liaux + 1
    end do
  
    do l = li, lf, 1
      do c = ci, cf, 1
        num = tabuleiro(l,c)
        call BuscaNumeroBloco(bloco,num, l, c, tabuleiro)
      end do 
    end do 
  end subroutine VerificaBloco

  subroutine BuscaNumeroBloco (bloco, numero, l, c, tabuleiro)
    implicit none
    integer(1), dimension (1:9,1:9), intent(out) :: tabuleiro
    integer :: i,j,numero, k, z, l, c, numerocomp1, numerocomp2
    integer(1), dimension (1:3,1:3), intent(in) :: bloco
    character(len=99) :: char_a1, char_b1, char_c1
    character(len=99) :: char_a2, char_b2, char_c2

    k = l
    z = c
   
    if(l>3 .AND. l<7)then
      k = l-3
    end if
    if(l>6 .AND. l<10)then
      k = l-6
    end if
    if(c>3 .AND. c<7)then
      z = c-3
    end if
    if(c>6 .AND. c<10)then
      z = c-6
    end if

    write(unit=char_a1,fmt=*)k
    write(unit=char_b1,fmt=*)z

    char_c1 = trim(adjustl(char_a1))//trim(adjustl(char_b1))
    verifica = 1    

    do i = 1, 3, 1
      do j = 1, 3, 1
        write(unit=char_a2,fmt=*)i
        write(unit=char_b2,fmt=*)j

        char_c2 = trim(adjustl(char_a2))//trim(adjustl(char_b2))
        if (bloco(i,j) == numero .AND. bloco(i,j) /= 0 .AND. char_c1 /= char_c2) then
          write(*,*)" Números repetidos no bloco. LINHA = ",i," COLUNA = ",j
          verifica = 0
        end if
      end do
    end do
  end subroutine BuscaNumeroBloco

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
