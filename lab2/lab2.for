      program lab2
      common/in/x_min, x_max, dx, y_min, y_max, dy
      common/shared/deg2rad
      deg2rad = 3.141592652/180.0
      call input
      call output
      pause 'All done.'
      end

      subroutine input
      !Минимумы
      common/in/x_min, x_max, dx, y_min, y_max, dy
      open(1, file = 'in.txt', status = 'old', err = 1)
      read(1,  * , err = 1, end = 1)x_min, x_max, dx, y_min, y_max, dy
      close(1)
      if(x_min.gt.x_max.or.y_min.gt.y_max)goto 3
      if(dx.le.0.0.or.dy.le.0.0)goto 3
      goto 2
    1 continue
      pause 'Error. Reason: Reading error.'
      stop
    3 continue
      pause 'Error. Reason: Incorrect values.'
      stop
    2 continue
      end

      subroutine output
      common/in/x_min, x_max, dx, y_min, y_max, dy
      common/shared/deg2rad
      dimension X_array(1:10)
      open(2, file = 'out.txt', status = 'unknown', err = 4)

C     Погрешности x, y
      eps_x = 10**aint(log10(abs(dx)) - 4)
      eps_y = 10**aint(log10(abs(dy)) - 4)
      !Выбираем минимальную погрешность
      if(dx.lt.dy)then
        eps = eps_x * deg2rad
      else
        eps = eps_y * deg2rad
      end if

      iterator = 0
c     обход по иксу
      x = x_min
      x_prev = x - 2 * dx
      !Флаг конца выборки, x больше x_max
      is_x_picking_over = 0
      !Если x>x_max и x_prev достаточно близко к x_max
      use_max_x = 0

      m_exp_x = get_exp(x_max, -dx, eps_x)
      k_x = get_k(x_max)

  30  continue
      !Проверка на мнимый шаг, получение корректного x
      x = normalize_step(x_prev, x, dx, eps_x)

C       проверка на конец выборки, x вышел за пределы x_max
      if(x - x_max.ge.k_x * 10**m_exp_x)then
        is_x_picking_over = 1
C       Предыдущий x недостаточно близок к x_max
        if(abs(x_prev - x_max).ge.10**m_exp_x)then
        !Использовать x_max как последнее значение, если массив полон
          if(iterator.eq.10)then
            use_max_x = 1
          else
            !Предыдущий x близок к x_max, записываем x_max как последный
            X_array(iterator + 1) = x_max
            iterator = iterator + 1
          end if
        else
          if(iterator.gt.0)then
            X_array(iterator) = x_max
          end if
        end if
      end if
c     проверка на x  = 0 и запись текущего x
      if(is_x_picking_over.eq.0)then
        if(abs(x).lt.eps_x)then
          x = 0.0
        end if
        X_array(iterator + 1) = x
        iterator = iterator + 1
        exp_x = get_exp(x, dx, eps_x)
        !Исключение погрешности
        x_prev = anint(x * 10**( -exp_x)) * 10**exp_x
        x = x_prev + dx
      end if
      if(iterator.lt.10.and.is_x_picking_over.eq.0)then
        goto 30
      end if

   32 continue
c     печатаем заголовок
      if(iterator.eq.0)goto 33
c     вывод линии
      write(2, 100)
      write(2, 103)'-'
      do i = 0, iterator, 1
        write(2, 101)
      end do
      write(2, 102)
c     верхний звголовок
      write(2, 103)'|    y\\x     '
      do i = 1, iterator, 1
        write(2, 104)X_array(i)
      end do
      write(2, 103)' |'
      write(2, 102)
c     вывод линии
      write(2, 100)
      write(2, 103)'-'
      do i = 0, iterator, 1
        write(2, 101)
      end do
      write(2, 102)

c     обход по игреку
      y = y_min
      y_prev = y - 2 * dy
      is_y_picking_over = 0
      use_max_y = 0
      m_exp_y = get_exp(y_max, -dy, eps_y)
      k_y = get_k(y_max)

   31 continue
      !Проверка на мнимый шаг, получение корректного y
      y = normalize_step(y_prev, y, dy, eps_y)
      !проверка на конец выборки, y вышел за пределы y_max
      if(y - y_max.ge.k_y * 10**m_exp_y)then
        is_y_picking_over = 1
        if(abs(y_prev - y_max).ge.10**m_exp_y)then
          use_max_y = 1
          y = y_max
        end if
      end if

      if(is_y_picking_over.eq.0.or.use_max_y.eq.1)then
c       печатаем игрек, ловим 0 и инф
        if(abs(y).lt.eps_y)then
          y = 0.0
        end if
        write(2, 105)y
        do k = 1, iterator, 1
C           Вычисление функции
          c = TAN((y + X_array(k)) * deg2rad)
          !Ловим бесконечность
          if(abs(c).lt.eps)then
            write(2, 103)' |  infinity! '
          else
            write(2, 104) 1 / c
          end if
        end do
        y_prev = anint(y * 10**( -exp_y)) * 10**exp_y
        y = y + dy
        write(2, 103)' |'
        write(2, 102)
      end if
      if(is_y_picking_over.eq.0)then
        goto 31
      end if
c     вывод линии
      write(2, 100)
      write(2, 103)'-'
      do i = 0, iterator, 1
      write(2, 101)
      end do
      write(2, 102)

   33 continue
      if(is_x_picking_over.eq.0)then
        iterator = 0
        goto 30
      end if
      if(use_max_x.eq.1)then
        iterator = 1
        X_array(1) = x_max
        goto 32
      end if

      close(2)
      goto 5
c     один пробел
  100 format(1x$)
c     14 символов ' - '
  101 format(14('-')$)
c     переход на новую строку
  102 format(/1x$)
c     символьный формат
  103 format(a$)
c     ячейка с числом в экспоненциальной форме
  104 format(' | 'e11.4$)
c     первая ячейка
  105 format('| 'e11.4$)
    4 continue
      pause 'Error. Reason: Writing error.'
      stop
    5 continue
      end

      function get_exp(a, da, fnll)
      if(abs(a).lt.1.0)then
        if(abs(a).gt.fnll)then
          get_exp = aint(log10(abs(a))) - 4
        else
          get_exp = aint(log10(abs(a - da))) - 4
        end if
      else
        get_exp = aint(log10(abs(a))) - 3
      end if
      end

      function get_k(a)
      if(a.lt.0)then
        get_k = 0.6
      else
        get_k = 0.5
      end if
      end

C     Проверка на мнимый шаг
      function normalize_step(a_bk, a, da, eps_a)
  200 continue
      exp = get_exp(a, da, eps_a)
      k = get_k(a)
      if(abs(a - a_bk).lt.k * 10**exp)then
        a = a + da
        goto 200
      end if
      normalize_step = a
      end
